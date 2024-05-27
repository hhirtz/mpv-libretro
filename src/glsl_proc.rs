use crate::Error;
use glsl_lang::ast;
use glsl_lang::parse::DefaultParse;
use glsl_lang::transpiler::glsl::show_translation_unit;
use glsl_lang::transpiler::glsl::FormattingState;
use glsl_lang::visitor::HostMut;
use glsl_lang::visitor::Visit;
use glsl_lang::visitor::VisitorMut;
use std::collections::BTreeSet;
use std::collections::HashSet;

/// Convert libretro pass names to mpv's
///
/// See libretro slang docs
/// https://github.com/libretro/slang-shaders/blob/6cb93ce1be67b314a96f5e346befb2b58b63eb04/README.md#builtin-variables
fn mpv_pass_name<'a>(mut libretro_pass_name: &'a str, prev_mpv_pass_name: &'a str) -> &'a str {
    if let Some(history_n) = libretro_pass_name.strip_prefix("OriginalHistory") {
        if history_n != "0" {
            eprintln!("warning: unsupported access to previous frames");
        }
        libretro_pass_name = "Original";
    } else if let Some((_real_pass_name, _history_n)) = libretro_pass_name.split_once("Feedback") {
        eprintln!("warning: unsupported access to pass feedback");
        libretro_pass_name = "Source";
    }
    match libretro_pass_name {
        "Source" => prev_mpv_pass_name,
        "Original" => "MAIN_RGB",
        "Output" => "OUTPUT",
        pass => pass,
    }
}

/// Whether this pass alias (from libretro) is known and defined in the preset.
fn is_alias_known(pass: &str, known_aliases: &HashSet<String>) -> bool {
    match pass {
        "Source" | "Original" | "Output" => true,
        pass => known_aliases.contains(pass),
    }
}

/// Add "{prefix}_" to all global variables and functions.
fn into_namespace(shader_ast: &mut ast::TranslationUnit, prefix: &str) {
    struct NamespaceVisitor<'a> {
        prefix: &'a str,
        globals: HashSet<ast::Identifier>,
    }

    impl<'a> VisitorMut for NamespaceVisitor<'a> {
        fn visit_identifier(&mut self, ident: &mut ast::Identifier) -> Visit {
            if self.globals.contains(ident) {
                let s = format!("{}{}", self.prefix, ident);
                let data = ast::IdentifierData::from(&*s);
                *ident = ast::Identifier::from(data);
            }
            Visit::Parent
        }

        fn visit_expr(&mut self, expr: &mut ast::Expr) -> Visit {
            match &mut **expr {
                ast::ExprData::Dot(left, _) => {
                    // don't rename struct fields
                    left.visit_mut(self);
                    Visit::Parent
                }
                _ => Visit::Children,
            }
        }

        fn visit_struct_field_specifier(&mut self, _: &mut ast::StructFieldSpecifier) -> Visit {
            // don't rename struct fields
            Visit::Parent
        }
    }

    let mut globals = HashSet::new();

    for decl in &shader_ast.0 {
        match &**decl {
            ast::ExternalDeclarationData::Preprocessor(_) => {}
            ast::ExternalDeclarationData::FunctionDefinition(func_decl) => {
                let s = &func_decl.prototype.name;
                globals.insert(s.clone());
            }
            ast::ExternalDeclarationData::Declaration(decl) => match &**decl {
                ast::DeclarationData::FunctionPrototype(func_decl) => {
                    let s = &func_decl.name;
                    globals.insert(s.clone());
                }
                ast::DeclarationData::InitDeclaratorList(decl) => {
                    if let Some(qualifier) = &decl.head.ty.qualifier {
                        if qualifier.qualifiers.iter().any(|qualifier| {
                            matches!(&**qualifier, ast::TypeQualifierSpecData::Layout(_))
                        }) {
                            continue;
                        }
                    }
                    if let Some(s) = &decl.head.name {
                        globals.insert(s.clone());
                    }
                    for var in &decl.tail {
                        let s = &var.ident.ident;
                        globals.insert(s.clone());
                    }
                }
                ast::DeclarationData::Block(_) => {}
                ast::DeclarationData::Precision(_, _) => unreachable!(),
                ast::DeclarationData::Invariant(_) => unreachable!(),
            },
        }
    }

    shader_ast.visit_mut(&mut NamespaceVisitor { prefix, globals });
}

/// Process the layout(push_constant) Push & layout(std140) UBO blocks.
///
/// These blocks may contain several things:
/// - sizes of previous passes' textures (OriginalSize, SourceSize,
///   PASS_ALIASSize, ...)
/// - FrameCount, the frame count (TODO take into account modulo preset parameters)
/// - shader parameters
///
/// Make these a struct instead and initialize all values.
///
/// From this:
/// ```glsl
/// layout(push_constant) uniform Push {
///     vec4 SourceSize;
///     vec4 OutputSize;
///     float param1;
///     uint FrameCount;
/// } params;
/// ```
///
/// To this:
/// ```glsl
/// struct _params_ {
///     vec4 SourceSize;
///     vec4 OutputSize;
///     float param1;
///     uint FrameCount;
/// } params = _params_(
///     vec4(PREVIOUSPASS_size, PREVIOUSPASS_pt),
///     vec4(target_size, 1.0/target_size.x, 1.0/target_size.y),
///     float(param1_value),
///     uint(frame));
/// ```
fn uniform_block_as_struct(
    shader_ast: &mut ast::TranslationUnit,
    prev_pass_name: &str,
    current_pass_name: &str,
    pass_aliases: &HashSet<String>,
    parameter_names: &HashSet<String>,
) -> HashSet<String> {
    let mut dependencies = HashSet::new();
    for decl in &mut shader_ast.0 {
        let ast::ExternalDeclarationData::Declaration(decl) = &mut **decl else {
            continue;
        };
        let ast::DeclarationData::Block(block) = &**decl else {
            continue;
        };

        let mut is_layout = false;
        for qualifier in &block.qualifier.qualifiers {
            let ast::TypeQualifierSpecData::Layout(_) = &**qualifier else {
                continue;
            };
            is_layout = true;
            break;
        }
        if !is_layout || (block.name.as_str() != "UBO" && block.name.as_str() != "Push") {
            println!("{}", block.name.as_str());
            continue;
        }

        let Some(ident) = &block.identifier else {
            panic!("TODO remove block");
        };
        assert!(ident.array_spec.is_none());

        let new_deps = block
            .fields
            .iter()
            .flat_map(|field| &field.identifiers)
            .filter_map(|ident| {
                let s = ident.ident.as_str();
                let pass = s.strip_suffix("Size")?;
                if !is_alias_known(pass, pass_aliases)
                    || pass == current_pass_name
                    || pass == "Output"
                {
                    return None;
                }
                let texture_name = mpv_pass_name(pass, prev_pass_name);
                Some(texture_name.to_owned())
            });
        dependencies.extend(new_deps);

        let ident = &ident.ident;

        let new_decl_struct_name = ast::TypeNameData::from(&*format!("_{ident}_"));
        let new_decl_struct_fields: Vec<_> = block
            .fields
            .iter()
            .cloned()
            .filter_map(|mut field| {
                field.identifiers.retain(|ident| {
                    let s = ident.ident.as_str();
                    if let Some(alias) = s.strip_suffix("Size") {
                        return is_alias_known(alias, pass_aliases);
                    }
                    s == "FrameCount" || s == "MVP" || parameter_names.contains(s)
                });
                if field.identifiers.is_empty() {
                    None
                } else {
                    Some(field)
                }
            })
            .collect();
        let new_decl_struct = ast::StructSpecifierData {
            name: Some(new_decl_struct_name.clone().into()),
            fields: new_decl_struct_fields,
        };
        let new_decl_full_type = ast::FullySpecifiedTypeData {
            qualifier: None,
            ty: ast::TypeSpecifierData {
                ty: ast::TypeSpecifierNonArrayData::Struct(new_decl_struct.into()).into(),
                array_specifier: None,
            }
            .into(),
        };
        let new_decl_init_type = ast::TypeSpecifierData {
            ty: ast::TypeSpecifierNonArrayData::TypeName(new_decl_struct_name.into()).into(),
            array_specifier: None,
        };
        let new_decl_init_fun =
            ast::FunIdentifierData::TypeSpecifier(Box::new(new_decl_init_type.into()));
        let new_inits = block
            .fields
            .iter()
            .flat_map(|field| {
                let ty = ast::Node::from(ast::FunIdentifierData::TypeSpecifier(Box::new(
                    field.ty.clone(),
                )));
                field.identifiers.iter().filter_map(move |ident| {
                    assert!(ident.array_spec.is_none());
                    let s = ident.ident.as_str();
                    if s == "FrameCount" {
                        let uint = ast::TypeSpecifierData {
                            ty: ast::TypeSpecifierNonArrayData::UInt.into(),
                            array_specifier: None,
                        };
                        let uint = ast::FunIdentifierData::TypeSpecifier(Box::new(uint.into()));
                        // TODO take into account modulo shader preset param
                        let frame_ident = ast::ExprData::variable("frame");
                        let expr = ast::ExprData::FunCall(uint.into(), vec![frame_ident.into()]);
                        return Some(expr.into());
                    }
                    if s == "MVP" {
                        let expr = ast::ExprData::FunCall(
                            ty.clone(),
                            vec![ast::ExprData::FloatConst(1.0).into()],
                        );
                        return Some(expr.into());
                    }
                    if let Some(pass) = s.strip_suffix("Size") {
                        if !is_alias_known(pass, pass_aliases) {
                            return None;
                        }
                        let texture_name = mpv_pass_name(pass, prev_pass_name);
                        let vec4 = ast::TypeSpecifierData {
                            ty: ast::TypeSpecifierNonArrayData::Vec4.into(),
                            array_specifier: None,
                        };
                        let vec4 = ast::FunIdentifierData::TypeSpecifier(Box::new(vec4.into()));
                        let expr = if texture_name == "OUTPUT" || pass == current_pass_name {
                            let size_ident = ast::ExprData::variable("target_size");
                            let ptx_ident = ast::ExprData::Binary(
                                ast::BinaryOpData::Div.into(),
                                Box::new(ast::ExprData::FloatConst(1.0).into()),
                                Box::new(ast::ExprData::variable("target_size.x").into()),
                            );
                            let pty_ident = ast::ExprData::Binary(
                                ast::BinaryOpData::Div.into(),
                                Box::new(ast::ExprData::FloatConst(1.0).into()),
                                Box::new(ast::ExprData::variable("target_size.y").into()),
                            );
                            ast::ExprData::FunCall(
                                vec4.into(),
                                vec![size_ident.into(), ptx_ident.into(), pty_ident.into()],
                            )
                        } else {
                            let size_ident =
                                ast::ExprData::variable(&*format!("{texture_name}_size"));
                            let pt_ident = ast::ExprData::variable(&*format!("{texture_name}_pt"));
                            ast::ExprData::FunCall(
                                vec4.into(),
                                vec![size_ident.into(), pt_ident.into()],
                            )
                        };
                        return Some(expr.into());
                    }

                    if parameter_names.contains(s) {
                        let ident = ast::ExprData::variable(&*crate::normalize_parameter_name(s));
                        let expr = ast::ExprData::FunCall(ty.clone(), vec![ident.into()]).into();
                        return Some(expr);
                    }

                    None
                })
            })
            .collect();
        let new_decl_init = ast::ExprData::FunCall(new_decl_init_fun.into(), new_inits);
        let new_decl_head = ast::SingleDeclarationData {
            ty: new_decl_full_type.into(),
            name: Some(ident.clone()),
            array_specifier: None,
            initializer: Some(ast::InitializerData::Simple(Box::new(new_decl_init.into())).into()),
        };
        let new_decl_list = ast::InitDeclaratorListData {
            head: new_decl_head.into(),
            tail: vec![],
        };
        let new_decl = ast::DeclarationData::InitDeclaratorList(new_decl_list.into());
        *decl = new_decl.into();
    }
    dependencies
}

/// Redefine the vertex shader inputs:
///
/// - layout(location = 0) vec4: the position, redefined as `vec4(HOOKED_pos, 0.0, 1.0)`
/// - layout(location = 1) vec2: the texture coordinates, redefined as `HOOKED_pos`
///
/// See libretro docs
/// https://github.com/libretro/slang-shaders/blob/6cb93ce1be67b314a96f5e346befb2b58b63eb04/README.md#vertex-inputs
fn set_vertex_inputs(shader_ast: &mut ast::TranslationUnit) {
    for decl in &mut shader_ast.0 {
        let ast::ExternalDeclarationData::Declaration(decl) = &mut **decl else {
            continue;
        };
        let ast::DeclarationData::InitDeclaratorList(init) = &mut **decl else {
            continue;
        };
        let Some(qualifier) = &init.head.ty.qualifier else {
            continue;
        };

        let mut is_input = false;
        let mut location = None;
        for qualifier in &qualifier.qualifiers {
            match &**qualifier {
                ast::TypeQualifierSpecData::Storage(qual) => match &**qual {
                    ast::StorageQualifierData::InOut => todo!(),
                    ast::StorageQualifierData::In => is_input = true,
                    _ => {}
                },
                ast::TypeQualifierSpecData::Layout(layout_qual) => {
                    for item in &layout_qual.ids {
                        let ast::LayoutQualifierSpecData::Identifier(ident, idx) = &**item else {
                            continue;
                        };
                        if ident.as_str() != "location" {
                            continue;
                        }
                        let idx = idx.clone().expect("layout(location=) without an index");
                        let idx = match &**idx {
                            ast::ExprData::IntConst(n) if *n >= 0 => *n as u32,
                            ast::ExprData::UIntConst(n) => *n,
                            _ => panic!("unexpected location: {idx:?}"),
                        };
                        location = Some(idx);
                    }
                }
                _ => {}
            }
        }

        if !is_input {
            continue;
        }
        let Some(location) = location else {
            continue;
        };

        assert!(init.tail.is_empty(), "{init:?}");

        match location {
            // Position
            0 => {
                init.head = ast::SingleDeclarationData {
                    ty: ast::FullySpecifiedTypeData {
                        qualifier: None,
                        ty: init.head.ty.ty.clone(),
                    }
                    .into(),
                    initializer: Some(
                        ast::InitializerData::Simple(Box::new(
                            ast::ExprData::FunCall(
                                ast::FunIdentifierData::TypeSpecifier(Box::new(
                                    ast::TypeSpecifierData {
                                        ty: ast::TypeSpecifierNonArrayData::Vec4.into(),
                                        array_specifier: None,
                                    }
                                    .into(),
                                ))
                                .into(),
                                vec![
                                    ast::ExprData::variable("HOOKED_pos").into(),
                                    ast::ExprData::FloatConst(0.0).into(),
                                    ast::ExprData::FloatConst(1.0).into(),
                                ],
                            )
                            .into(),
                        ))
                        .into(),
                    ),
                    ..init.head.content.clone()
                }
                .into();
            }
            // TexCoord
            1 => {
                init.head = ast::SingleDeclarationData {
                    ty: ast::FullySpecifiedTypeData {
                        qualifier: None,
                        ty: init.head.ty.ty.clone(),
                    }
                    .into(),
                    initializer: Some(
                        ast::InitializerData::Simple(Box::new(
                            ast::ExprData::variable("HOOKED_pos").into(),
                        ))
                        .into(),
                    ),
                    ..init.head.content.clone()
                }
                .into();
            }
            _ => panic!("unexpected in location={location}"),
        }
    }
}

/// Make shader outputs (`layout() out`) as simple globals, that are shared
/// between the vertex and fragment shader codes.
fn outputs_as_simple_globals(shader_ast: &mut ast::TranslationUnit) {
    for decl in &mut shader_ast.0 {
        let ast::ExternalDeclarationData::Declaration(decl) = &mut **decl else {
            continue;
        };
        let ast::DeclarationData::InitDeclaratorList(init) = &mut **decl else {
            continue;
        };
        let Some(qualifier) = &init.head.ty.qualifier else {
            continue;
        };

        let mut is_output = false;
        let mut location = None;
        for qualifier in &qualifier.qualifiers {
            match &**qualifier {
                ast::TypeQualifierSpecData::Storage(qual) => match &**qual {
                    ast::StorageQualifierData::InOut => todo!(),
                    ast::StorageQualifierData::Out => is_output = true,
                    _ => {}
                },
                ast::TypeQualifierSpecData::Layout(layout_qual) => {
                    for item in &layout_qual.ids {
                        let ast::LayoutQualifierSpecData::Identifier(ident, idx) = &**item else {
                            continue;
                        };
                        if ident.as_str() != "location" {
                            continue;
                        }
                        let idx = idx.clone().expect("layout(location=) without an index");
                        let idx = match &**idx {
                            ast::ExprData::IntConst(n) if *n >= 0 => *n as u32,
                            ast::ExprData::UIntConst(n) => *n,
                            _ => panic!("unexpected location: {idx:?}"),
                        };
                        location = Some(idx);
                    }
                }
                _ => {}
            }
        }

        if !is_output || location.is_none() {
            continue;
        }

        assert!(init.tail.is_empty(), "{init:?}");

        init.head.ty.qualifier = None;
    }
}

/// Remove all `layout(push_constant)` and `layout() UBO` blocks.
///
/// Used on the fragment shader to avoid duplicate definitions, since it
/// (should?) share those with the vertex shader.
fn remove_push_constant_and_ubo(shader_ast: &mut ast::TranslationUnit) {
    let mut remove = BTreeSet::new();
    for (i, decl) in shader_ast.0.iter_mut().enumerate() {
        let ast::ExternalDeclarationData::Declaration(decl) = &mut **decl else {
            continue;
        };
        let ast::DeclarationData::Block(block) = &**decl else {
            continue;
        };

        let mut is_layout = false;
        for qualifier in &block.qualifier.qualifiers {
            if let ast::TypeQualifierSpecData::Layout(_) = &**qualifier {
                is_layout = true;
                break;
            }
        }
        if is_layout && matches!(block.name.as_str(), "UBO" | "Push") {
            remove.insert(i);
        } else {
            panic!("{block:#?}");
        }
    }
    for remove in remove.into_iter().rev() {
        shader_ast.0.remove(remove);
    }
}

/// Remove shader inputs (`layout() in`) from the shader.
///
/// Used on the fragment shader, since inputs are defined as simple global
/// variables and shared between the two shaders.
fn remove_inputs(shader_ast: &mut ast::TranslationUnit) {
    let mut remove = BTreeSet::new();
    for (i, decl) in shader_ast.0.iter_mut().enumerate() {
        let ast::ExternalDeclarationData::Declaration(decl) = &mut **decl else {
            continue;
        };
        let ast::DeclarationData::InitDeclaratorList(init) = &mut **decl else {
            continue;
        };
        let Some(qualifier) = &init.head.ty.qualifier else {
            continue;
        };

        let mut is_input = false;
        let mut location = None;
        for qualifier in &qualifier.qualifiers {
            match &**qualifier {
                ast::TypeQualifierSpecData::Storage(qual) => match &**qual {
                    ast::StorageQualifierData::InOut => todo!(),
                    ast::StorageQualifierData::In => is_input = true,
                    _ => {}
                },
                ast::TypeQualifierSpecData::Layout(layout_qual) => {
                    for item in &layout_qual.ids {
                        let ast::LayoutQualifierSpecData::Identifier(ident, idx) = &**item else {
                            continue;
                        };
                        if ident.as_str() != "location" {
                            continue;
                        }
                        let idx = idx.clone().expect("layout(location=) without an index");
                        let idx = match &**idx {
                            ast::ExprData::IntConst(n) if *n >= 0 => *n as u32,
                            ast::ExprData::UIntConst(n) => *n,
                            _ => panic!("unexpected location: {idx:?}"),
                        };
                        location = Some(idx);
                    }
                }
                _ => {}
            }
        }

        if !is_input || location.is_none() {
            continue;
        }

        remove.insert(i);
    }
    for remove in remove.into_iter().rev() {
        shader_ast.0.remove(remove);
    }
}

/// Create mpv's `hook` function.
///
/// This function calls the vertex shader, then the fragment shader, then
/// returns `FragColor` (or whatever is named the fragment shader output).
///
/// Is `is_last_pass` is true, then it also add a call to `delinearize` to
/// convert sRGB to linear RGB.
fn hook_fn(is_last_pass: bool) -> ast::ExternalDeclaration {
    let call_vertex_ident = ast::FunIdentifierData::ident("vertex_main").into();
    let call_vertex_stmt = ast::ExprData::FunCall(call_vertex_ident, vec![]);
    let call_vertex_stmt = ast::StatementData::Expression(
        ast::ExprStatementData(Some(call_vertex_stmt.into())).into(),
    );

    let call_fragment_ident = ast::FunIdentifierData::ident("fragment_main").into();
    let call_fragment_stmt = ast::ExprData::FunCall(call_fragment_ident, vec![]);
    let call_fragment_stmt = ast::StatementData::Expression(
        ast::ExprStatementData(Some(call_fragment_stmt.into())).into(),
    );

    let mut frag_color = Box::new(ast::ExprData::variable("FragColor").into());
    if is_last_pass {
        let delinearize =
            ast::FunIdentifierData::Expr(Box::new(ast::ExprData::variable("delinearize").into()))
                .into();
        frag_color = Box::new(ast::ExprData::FunCall(delinearize, vec![*frag_color]).into());
    }
    let return_frag_color_stmt =
        ast::StatementData::Jump(ast::JumpStatementData::Return(Some(frag_color)).into());

    let func = ast::FunctionDefinitionData {
        prototype: ast::FunctionPrototypeData {
            ty: ast::FullySpecifiedTypeData {
                qualifier: None,
                ty: ast::TypeSpecifierData {
                    ty: ast::TypeSpecifierNonArrayData::Vec4.into(),
                    array_specifier: None,
                }
                .into(),
            }
            .into(),
            name: ast::IdentifierData::from("hook").into(),
            parameters: vec![],
        }
        .into(),
        statement: ast::CompoundStatementData {
            statement_list: vec![
                call_vertex_stmt.into(),
                call_fragment_stmt.into(),
                return_frag_color_stmt.into(),
            ],
        }
        .into(),
    };
    ast::ExternalDeclarationData::FunctionDefinition(func.into()).into()
}

/// Add proper definitions for the samplers used in the fragment shader.
fn set_samplers(
    shader_ast: &mut ast::TranslationUnit,
    prev_pass_name: &str,
    texture_names: &HashSet<String>,
) -> HashSet<String> {
    let mut remove = BTreeSet::new();
    let mut samplers = HashSet::new();
    for (i, ext_decl) in shader_ast.0.iter_mut().enumerate() {
        let ast::ExternalDeclarationData::Declaration(decl) = &mut **ext_decl else {
            continue;
        };
        let ast::DeclarationData::InitDeclaratorList(init) = &mut **decl else {
            continue;
        };
        let Some(qualifier) = &init.head.ty.qualifier else {
            continue;
        };

        let mut is_uniform = false;
        for qualifier in &qualifier.qualifiers {
            if let ast::TypeQualifierSpecData::Storage(qual) = &**qualifier {
                if let ast::StorageQualifierData::Uniform = &**qual {
                    is_uniform = true;
                    break;
                }
            }
        }
        if !is_uniform {
            continue;
        }
        if init.head.ty.ty.ty.content != ast::TypeSpecifierNonArrayData::Sampler2D {
            continue;
        }
        assert!(init.tail.is_empty(), "{init:?}");

        let name = init.head.name.as_ref().unwrap().as_str().to_owned();
        let mpv_name = mpv_pass_name(&name, prev_pass_name);

        if texture_names.contains(mpv_name) {
            // mpv doesn't define texture samplers with a _raw suffix
            remove.insert(i);
        } else {
            *ext_decl = ast::ExternalDeclarationData::Preprocessor(
                ast::PreprocessorData::Define(
                    ast::PreprocessorDefineData::ObjectLike {
                        ident: init.head.name.clone().unwrap(),
                        value: format!("{mpv_name}_raw"),
                    }
                    .into(),
                )
                .into(),
            )
            .into();
        }

        samplers.insert(mpv_name.to_owned());
    }
    for remove in remove.into_iter().rev() {
        shader_ast.0.remove(remove);
    }
    samplers
}

pub struct MergeResult {
    pub shader: String,
    pub dependencies: HashSet<String>,
}

/// Merge a vertex shader with its fragment shader.
///
/// This creates a new mpv shader that call both shaders' main function, and
/// return the result of the fragment shader.
///
/// The vertex shader is included because some libretro shaders use it for some
/// things it seems...
pub fn merge_vertex_and_fragment(
    vertex: &str,
    fragment: &str,
    // we need this to replace the instances of `Source` in the original code
    prev_pass_name: &str,
    // we need this to replace the instances of this pass (mpv cannot bind a
    // pass to itself) to `target_size`
    current_pass_name: &str,
    // we need this to remove references unknown passes in the code
    pass_aliases: &HashSet<String>,
    // we need this to remove references unknown parameters in the code
    parameter_names: &HashSet<String>,
    // we need this because mpv defines texture and pass samplers differently.
    // pass samplers have a `_raw` suffix, while texture samplers don't.
    texture_names: &HashSet<String>,
    // whether we need to add a call to `delinearize` in the `hook` function.
    is_last_pass: bool,
) -> Result<MergeResult, Error> {
    let mut vertex_ast = ast::TranslationUnit::parse(vertex).map_err(|err| {
        for (lineno, line) in vertex.lines().enumerate() {
            eprintln!("{:>4}: {line}", lineno + 1);
        }
        err
    })?;
    vertex_ast.0.insert(
        0,
        ast::ExternalDeclarationData::Declaration(
            ast::DeclarationData::InitDeclaratorList(
                ast::InitDeclaratorListData {
                    head: ast::SingleDeclarationData {
                        ty: ast::FullySpecifiedTypeData {
                            qualifier: None,
                            ty: ast::TypeSpecifierData {
                                ty: ast::TypeSpecifierNonArrayData::Vec4.into(),
                                array_specifier: None,
                            }
                            .into(),
                        }
                        .into(),
                        name: Some(ast::IdentifierData::from("gl_Position").into()),
                        array_specifier: None,
                        initializer: None,
                    }
                    .into(),
                    tail: vec![],
                }
                .into(),
            )
            .into(),
        )
        .into(),
    );
    into_namespace(&mut vertex_ast, "vertex_");
    let mut dependencies = uniform_block_as_struct(
        &mut vertex_ast,
        prev_pass_name,
        current_pass_name,
        pass_aliases,
        parameter_names,
    );
    dependencies.insert("HOOKED".to_owned()); // HOOKED is needed for TexCoord init
    set_vertex_inputs(&mut vertex_ast);
    outputs_as_simple_globals(&mut vertex_ast);

    let mut fragment_ast = ast::TranslationUnit::parse(fragment).map_err(|err| {
        for (lineno, line) in fragment.lines().enumerate() {
            eprintln!("{:>4}: {line}", lineno + 1);
        }
        err
    })?;
    remove_push_constant_and_ubo(&mut fragment_ast);
    remove_inputs(&mut fragment_ast);
    into_namespace(&mut fragment_ast, "fragment_");
    let samplers = set_samplers(&mut fragment_ast, prev_pass_name, texture_names);
    dependencies.extend(samplers);
    outputs_as_simple_globals(&mut fragment_ast);

    for decl in fragment_ast.0 {
        vertex_ast.0.push(decl);
    }

    vertex_ast.0.push(hook_fn(is_last_pass));

    let mut shader = String::with_capacity(vertex.len() + fragment.len());
    show_translation_unit(&mut shader, &vertex_ast, FormattingState::default())?;
    Ok(MergeResult {
        shader,
        dependencies,
    })
}

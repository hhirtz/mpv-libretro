use librashader_common::FilterMode;
use librashader_common::WrapMode;
use librashader_preprocess::ShaderParameter;
use librashader_preprocess::ShaderSource;
use librashader_presets::ScaleFactor;
use librashader_presets::ScaleType;
use librashader_presets::ShaderPreset;
use std::backtrace::Backtrace;
use std::collections::BTreeMap;
use std::collections::HashSet;
use std::env;
use std::ffi::OsStr;
use std::fmt;

mod glsl_proc;

struct Error {
    err: Box<dyn std::error::Error + Send + Sync + 'static>,
    backtrace: Backtrace,
}

impl<E> From<E> for Error
where
    E: std::error::Error + Send + Sync + 'static,
{
    fn from(value: E) -> Self {
        Self {
            err: Box::new(value),
            backtrace: Backtrace::force_capture(),
        }
    }
}

impl fmt::Display for Error {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        writeln!(f, "{}", self.err)?;
        let mut maybe_err = self.err.source();
        while let Some(err) = maybe_err {
            writeln!(f, "  caused by: {err}")?;
            maybe_err = err.source();
        }
        write!(f, "Backtrace:\n{}", self.backtrace)
    }
}

impl fmt::Debug for Error {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        fmt::Display::fmt(self, f)
    }
}

/// Dummy name for passes that don't have aliases in the preset.
fn generated_pass_name(pass_no: usize) -> String {
    format!("_PASS_{pass_no}")
}

fn normalize_parameter_name(name: &str) -> String {
    match name {
        // These keywords are used by mpv and will conflict.
        "linearize" | "delinearize" => format!("{name}_"),
        _ => name.to_owned(),
    }
}

fn main() -> Result<(), Error> {
    let preset_file = env::args_os()
        .nth(1)
        .expect("usage: mpv-libretro PRESET_FILE");
    let mut preset = ShaderPreset::try_parse(preset_file)?;

    for pass in &mut preset.shaders {
        if let Some(alias) = &mut pass.alias {
            // TODO patch librashader to trim aliases
            *alias = alias.trim().to_owned();

            // TODO patch librashader to handle ""
            if alias == "\"\"" {
                pass.alias = None;
            }
        }
    }

    let sources: Result<Vec<ShaderSource>, Error> = preset
        .shaders
        .iter()
        .map(|pass| Ok(ShaderSource::load(&*pass.name)?))
        .collect();
    let sources = sources?;

    let pass_aliases: HashSet<String> = preset
        .shaders
        .iter()
        .filter_map(|pass| pass.alias.clone())
        .collect();

    let parameters: BTreeMap<String, ShaderParameter> = sources
        .iter()
        .flat_map(|source| {
            source
                .parameters
                .iter()
                .filter(|(param_name, _)| !param_name.contains('-'))
                .map(|(param_name, param_value)| {
                    let param_name = normalize_parameter_name(param_name);
                    (param_name, param_value.clone())
                })
        })
        .collect();
    let parameter_names: HashSet<String> = parameters.values().map(|p| p.id.clone()).collect();

    for source in &sources {
        for (param_name, param_value) in &source.parameters {
            if let Some(aggreg_value) = parameters.get(param_name) {
                assert_eq!(aggreg_value, param_value);
            }
        }
    }

    let texture_names: HashSet<String> = preset
        .textures
        .iter()
        .map(|texture| &texture.name)
        .cloned()
        .collect();

    println!("// Generated by mpv-libretro");

    for (param_name, param) in &parameters {
        println!();
        println!("//!PARAM {}", param_name);
        println!("//!DESC {}", param.description);
        println!("//!TYPE CONSTANT float");
        println!("//!MINIMUM {}", param.minimum);
        println!("//!MAXIMUM {}", param.maximum);
        println!("{}", param.initial);
    }

    for texture in preset.textures {
        eprintln!("{}", texture.path.display());

        let img = image::io::Reader::open(&*texture.path)?.decode()?;

        println!();
        println!("//!TEXTURE {}", texture.name);
        println!("//!SIZE {} {}", img.width(), img.height());

        let wrap_mode = match texture.wrap_mode {
            WrapMode::ClampToBorder | WrapMode::ClampToEdge => "CLAMP",
            WrapMode::Repeat => "REPEAT",
            WrapMode::MirroredRepeat => "MIRROR",
        };
        println!("//!BORDER {wrap_mode}");

        let filter_mode = match texture.filter_mode {
            FilterMode::Linear => "LINEAR",
            FilterMode::Nearest => "NEAREST",
        };
        println!("//!FILTER {filter_mode}");

        match img {
            image::DynamicImage::ImageRgb8(img) => {
                println!("//!FORMAT rgba8");
                for pixel in img.pixels() {
                    let [r, g, b] = pixel.0;
                    print!("{r:02x}{g:02x}{b:02x}ff");
                }
                println!();
            }
            image::DynamicImage::ImageRgba8(img) => {
                println!("//!FORMAT rgba8");
                for pixel in img.pixels() {
                    let [r, g, b, a] = pixel.0;
                    print!("{r:02x}{g:02x}{b:02x}{a:02x}");
                }
                println!();
            }
            _ => panic!("unsupported color type for texture: {:?}", img.color()),
        }
    }

    // libretro shaders expect linear RGB while mpv's MAIN texture is sRGB.
    println!();
    print!("{}", include_str!("linearize.glsl"));

    // some shaders need access to the size of the viewport/window.
    println!();
    print!("{}", include_str!("viewport_size.glsl"));

    for (i, (source, pass)) in sources.iter().zip(&preset.shaders).enumerate() {
        eprintln!("{}", pass.name.display());

        // TODO pass.wrap_mode
        if pass.float_framebuffer {
            eprintln!("warning: shader expects values not to be clamped");
        }
        // TODO pass.mipmap_input

        println!();
        println!("//!HOOK MAIN");
        println!("//!COMPONENTS 4");
        if let Some(name) = pass.name.file_name().map(OsStr::to_string_lossy) {
            println!("//!DESC {name}");
        }

        match pass.scaling.x.scale_type {
            ScaleType::Input if i != 0 => match pass.scaling.x.factor {
                ScaleFactor::Float(r) => {
                    let previous = preset.shaders[i - 1]
                        .alias
                        .clone()
                        .unwrap_or(generated_pass_name(i - 1));
                    println!("//!WIDTH {previous}.width {r} *");
                }
                ScaleFactor::Absolute(_) => unreachable!(),
            },
            ScaleType::Absolute => match pass.scaling.x.factor {
                ScaleFactor::Float(_) => unreachable!(),
                ScaleFactor::Absolute(r) => {
                    println!("//!WIDTH {r}");
                }
            },
            ScaleType::Viewport => match pass.scaling.x.factor {
                ScaleFactor::Float(r) => {
                    println!("//!WIDTH OUTPUT.width {r} *");
                }
                ScaleFactor::Absolute(_) => unreachable!(),
            },
            _ => match pass.scaling.x.factor {
                ScaleFactor::Float(r) => {
                    if r != 1.0 {
                        println!("//!WIDTH HOOKED.width {r} *");
                    }
                }
                ScaleFactor::Absolute(_) => unreachable!(),
            },
        }

        match pass.scaling.y.scale_type {
            ScaleType::Input if i != 0 => match pass.scaling.y.factor {
                ScaleFactor::Float(r) => {
                    let previous = preset.shaders[i - 1]
                        .alias
                        .clone()
                        .unwrap_or(generated_pass_name(i - 1));
                    println!("//!HEIGHT {previous}.height {r} *");
                }
                ScaleFactor::Absolute(_) => unreachable!(),
            },
            ScaleType::Absolute => match pass.scaling.y.factor {
                ScaleFactor::Float(_) => unreachable!(),
                ScaleFactor::Absolute(r) => {
                    println!("//!HEIGHT {r}");
                }
            },
            ScaleType::Viewport => match pass.scaling.y.factor {
                ScaleFactor::Float(r) => {
                    println!("//!HEIGHT OUTPUT.height {r} *");
                }
                ScaleFactor::Absolute(_) => unreachable!(),
            },
            _ => match pass.scaling.y.factor {
                ScaleFactor::Float(r) => {
                    if r != 1.0 {
                        println!("//!HEIGHT HOOKED.height {r} *");
                    }
                }
                ScaleFactor::Absolute(_) => unreachable!(),
            },
        }

        if i + 1 < sources.len() {
            if let Some(alias) = &pass.alias {
                println!("//!SAVE {alias}");
            } else {
                println!("//!SAVE {}", generated_pass_name(i));
            }
        }

        let previous = if i == 0 {
            String::from("MAIN_RGB")
        } else {
            preset.shaders[i - 1]
                .alias
                .clone()
                .unwrap_or(generated_pass_name(i - 1))
        };
        let current = pass.alias.clone().unwrap_or(generated_pass_name(i));
        let merged = glsl_proc::merge_vertex_and_fragment(
            &source.vertex,
            &source.fragment,
            &previous,
            &current,
            &pass_aliases,
            &parameter_names,
            &texture_names,
            i + 1 == sources.len(),
        )?;

        for dep in merged.dependencies {
            println!("//!BIND {dep}");
        }

        println!();
        println!("{}", merged.shader);
    }
    Ok(())
}

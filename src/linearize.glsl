//!HOOK MAIN
//!COMPONENTS 4
//!DESC sRGB to linear RGB
//!SAVE MAIN_RGB
//!BIND HOOKED

vec4 hook() {
	return linearize(HOOKED_tex(HOOKED_pos));
}

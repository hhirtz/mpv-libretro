//!HOOK MAIN
//!COMPONENTS 2
//!DESC store viewport size
//!SAVE VIEWPORT
//!WIDTH OUTPUT.width
//!HEIGHT OUTPUT.width

vec4 hook() {
	return vec4(target_size, 0.0, 0.0);
}

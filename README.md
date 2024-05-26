# mpv-libretro

Compile libretro shaders presets (.slangp) into MPV shaders.

Output shaders require `vo=gpu-next`.

## Dependencies

- shaderc
- glslang

## Usage

```
cargo run -- some_libretro_shader_preset.slangp >my_new_mpv_shader.glsl
mpv --vo=gpu-next --glsl-shader=my_new_mpv_shader.glsl video.mp4
```

## License

The code in this repository is distributed under the `MPL-2.0`.

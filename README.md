# mpv-libretro

Convert libretro shaders presets (.slangp) into MPV shaders.

Shaders created with this tool require `vo=gpu-next`.

## Usage

```
cargo run -- some_libretro_shader_preset.slangp >my_new_mpv_shader.glsl
mpv --vo=gpu-next --glsl-shader=my_new_mpv_shader.glsl video.mp4
```

## Limitations

mpv-libretro should process most libretro shaders successfully. However, it
doesn't mean mpv will be able run them. Some libretro shaders use features not
available in mpv, which mpv-libretro cannot fix:

- cyclic dependencies between passes: a pass depend on the output of another
  pass defined later in the preset. In this case, mpv will not run the shader.
- more than 16 bound textures per pass: a pass depend on the output of 17 or
  more passes or textures. In this case, mpv will throw an error.

## License

The code in this repository is distributed under the `MPL-2.0`.

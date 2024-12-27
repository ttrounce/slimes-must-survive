package main

import rl "vendor:raylib"

ShaderInfo :: struct {
	id:            string,
	load_uniforms: proc(shader: rl.Shader, info: ShaderInfo),
	variant:       union {
		ShaderVariant_PixelGlow,
	},
}

ShaderVariant_PixelGlow :: struct {
	glow_color:    rl.Color,
	glowSize:      f32,
	glowThreshold: f32,
	glowIntensity: f32,
}

make_shader_pixel_glow :: proc(color: rl.Color, size := f32(0.5), threshold := f32(0.5), intensity := f32(0.5)) -> ShaderInfo {
	return ShaderInfo {
		id = "pixel_glow",
		variant = ShaderVariant_PixelGlow{color, size, threshold, intensity},
		load_uniforms = proc(shader: rl.Shader, info: ShaderInfo) {
			pixel_glow, ok := info.variant.(ShaderVariant_PixelGlow)
			if !ok {
				return
			}

			normalized_color := rl.ColorNormalize(pixel_glow.glow_color)
			rl.SetShaderValueV(
				shader,
				rl.GetShaderLocation(shader, "glowColor"),
				raw_data(normalized_color[:]),
				.VEC3,
				1,
			)
            rl.SetShaderValue(shader, rl.GetShaderLocation(shader, "glowSize"), &pixel_glow.glowSize, .FLOAT)
            rl.SetShaderValue(shader, rl.GetShaderLocation(shader, "glowThreshold"), &pixel_glow.glowThreshold, .FLOAT)
            rl.SetShaderValue(shader, rl.GetShaderLocation(shader, "glowIntensity"), &pixel_glow.glowIntensity, .FLOAT)
		},
	}
}

get_entity_shader :: proc(
	game: ^Game,
	e: ^Entity,
) -> (
	shader: ^rl.Shader,
	info: ^ShaderInfo,
	exists: bool,
) {
	if shader_info, has_shader := &e.shader.?; has_shader {
		if shader, shader_exists := &game.shaders[shader_info.id]; shader_exists {
			return shader, shader_info, true
		}
	}
	return nil, nil, false
}
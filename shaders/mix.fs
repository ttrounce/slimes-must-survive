#version 330

in vec2 fragTexCoord;
in vec4 fragColor;

uniform sampler2D texture0;
uniform vec4 colDiffuse;

uniform vec3 mixColor = vec3(1,1,1);
uniform float intensity = 0.0;

out vec4 finalColor;

void main() {
    finalColor = texture(texture0, fragTexCoord)*fragColor*colDiffuse;
    finalColor = mix(finalColor, vec4(mixColor, finalColor.a), intensity);
}
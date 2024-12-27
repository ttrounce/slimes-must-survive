#version 330

in vec2 fragTexCoord;
in vec4 fragColor;

uniform sampler2D texture0;
uniform vec4 colDiffuse;

uniform vec3 glowColor = vec3(0, 1, 0);
uniform float glowSize = .5;
uniform float glowThreshold = .5;
uniform float glowIntensity = .5;

out vec4 finalColor;

void main() {
    finalColor = texture(texture0, fragTexCoord);

    if (finalColor.a <= glowThreshold)  {
        ivec2 size = textureSize(texture0, 0);

        float uvX = fragTexCoord.x * size.x;
        float uvY = fragTexCoord.y * size.y;

        float sum = 0.0;
        for (int n = 0; n < 9; ++n) {
            uvY = (fragTexCoord.y * size.y) + (glowSize * float(n - 4.5));
            float hSum = 0.0;
            hSum += texelFetch(texture0, ivec2(uvX - (4.0 * glowSize), uvY), 0).a;
            hSum += texelFetch(texture0, ivec2(uvX - (3.0 * glowSize), uvY), 0).a;
            hSum += texelFetch(texture0, ivec2(uvX - (2.0 * glowSize), uvY), 0).a;
            hSum += texelFetch(texture0, ivec2(uvX - glowSize, uvY), 0).a;
            hSum += texelFetch(texture0, ivec2(uvX, uvY), 0).a;
            hSum += texelFetch(texture0, ivec2(uvX + glowSize, uvY), 0).a;
            hSum += texelFetch(texture0, ivec2(uvX + (2.0 * glowSize), uvY), 0).a;
            hSum += texelFetch(texture0, ivec2(uvX + (3.0 * glowSize), uvY), 0).a;
            hSum += texelFetch(texture0, ivec2(uvX + (4.0 * glowSize), uvY), 0).a;
            sum += hSum / 9.0;
        }
        finalColor = vec4(glowColor.rgb, (sum/9.0)*glowIntensity);
    }
    finalColor = finalColor*fragColor*colDiffuse;
}
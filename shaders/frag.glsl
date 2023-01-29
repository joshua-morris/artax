#version 330 core
out vec4 FragColor;

in vec2 TexCoord;

uniform sampler2D ourTexture0;

void main()
{
     FragColor = texture(ourTexture0, TexCoord); 
}
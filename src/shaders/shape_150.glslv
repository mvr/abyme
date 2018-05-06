#version 150 core

in vec2 a_Pos;
in uint a_VertexType;
uniform mat3 i_Transform;
uniform vec3 i_Color;
out vec4 v_Color;

const uint FillVertex    = 0x00000000u;
const uint OutlineVertex = 0x00000001u;

void main() {
    v_Color = vec4(i_Color, 1.0);

    vec3 transformed = i_Transform * vec3(a_Pos, 1.0);

    gl_Position = vec4(transformed.x, transformed.y, 0.0, 1.0);

}

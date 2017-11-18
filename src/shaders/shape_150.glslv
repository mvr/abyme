#version 150 core

in vec2 a_Pos;
in uint a_VertexType;
uniform ivec2 i_Resolution;
uniform vec3 i_FillColor;
uniform vec3 i_OutlineColor;
out vec4 v_Color;

const uint FillVertex    = 0x00000000u;
const uint OutlineVertex = 0x00000001u;

void main() {
    if(a_VertexType == FillVertex) {
        v_Color = vec4(i_FillColor, 1.0);
    } else {
        v_Color = vec4(i_OutlineColor, 1.0);
    }

    float ratio = float(i_Resolution.y) / float(i_Resolution.x); // TODO: This should just be a constant/uniform

    gl_Position = vec4(a_Pos.x * ratio, a_Pos.y, 0.0, 1.0);

}

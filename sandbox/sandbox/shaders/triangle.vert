#version 450
#extension GL_ARB_separate_shader_objects : enable

layout(row_major, binding = 0) uniform UniformBufferObject {
  mat4 model;
  mat4 view;
  mat4 proj;
} ubo;

layout(location = 0) in vec2 inPosition;
layout(location = 1) in vec3 inColor;

layout(location = 0) out vec3 outColor;

out gl_PerVertex {
  vec4 gl_Position;
};

void main() {
  gl_Position = vec4(inPosition, 0.0, 1.0) * ubo.model * ubo.view * ubo.proj;
  outColor = inColor;
}

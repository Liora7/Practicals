varying vec3 col;
varying vec3 world_normal;

void main() {
  world_normal = normalize( mat3( modelMatrix[0].xyz, modelMatrix[1].xyz, modelMatrix[2].xyz ) * normal );
  col = vec3(uv, 0.0);
  gl_Position = projectionMatrix * modelViewMatrix * vec4(position,1.0);
}

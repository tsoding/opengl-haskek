#version 130

uniform vec2 objectPosition;

void main(void)
{
    // TODO: draw cube with gray code
    //   flutterlice: @tsoding, it is possible to do with quad_strip
    //   method o f drawing and doing draw call twice rotating gray
    //   code by 90 degrees second time. first gray code will draw
    //   front face bottom face and back face, second will draw right
    //   face top face and bottom face
    int gray = gl_VertexID ^ (gl_VertexID >> 1);
    gl_Position = vec4(
        0.5 + gray / 2 - 1.0 + objectPosition.x,
        0.5 + gray % 2 - 1.0 + objectPosition.y,
        0.0,
        1.0);
}

// (n-1) XOR (floor((n-1)/2))
// (n-1) XOR ((n-1)/2)

// 0 1
// 2 3

// 0 => 0
// 1 => 1
// 2 => 3
// 3 => 2

//   0 1 2 3
// % 0 1 0 1
// / 0 0 1 1

//   / %
// 0 0 0
// 1 0 1
// 2 1 1
// 3 1 0

// b = 0 =>
// 0.5 + -1.0 * (1 - b) =>
// 0.5 + b - 1

// -1.0, 1.0

// flutterlice: x = ID % 2 * 1.0
// flutterlice: y = ID / 2 * 1.0 

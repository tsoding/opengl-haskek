uniform vec2 u_resolution;

float random (vec2 st) {
    return fract(sin(dot(st.xy,
                         vec2(12.9898,78.233)))*
        43758.5453123);
}

//  0.0 .. 1.0 =>
// -0.5 .. 0.5 =>
// -1.0 .. 1.0

vec2 truchet_pattern(in vec2 _st, in float _index) {
    _index = fract((_index - 0.5) * 2.0);
    if (_index > 0.75) {
        _st = vec2(1.0) - _st;
    } else if (_index > 0.5) {
        _st = vec2(1.0 - _st.x, _st.y);
    } else if (_index > 0.25) {
        _st = 1.0 - vec2(1.0 - _st.x, _st.y);
    }
    return _st;
}

void main() {
    vec2 st = gl_FragCoord.xy / u_resolution.xy;
#define TILE_SIZE_INVERSED 10.0
    st *= TILE_SIZE_INVERSED;

    vec2 ipos = floor(st);
    vec2 fpos = fract(st);
    vec2 tile = truchet_pattern(fpos, random(ipos));

// #define LINE_THICCNESS 0.1
//     // Maze
//     float color = smoothstep(tile.x - LINE_THICCNESS, tile.x, tile.y) -
//         smoothstep(tile.x, tile.x + LINE_THICCNESS, tile.y);


#define CIRCLE_THICCNESS 0.6

    // Circle
    float color = (step(length(tile), CIRCLE_THICCNESS) -
                   step(length(tile), 1.0 - CIRCLE_THICCNESS)) +
        (step(length(tile - vec2(1.0)), CIRCLE_THICCNESS) -
         step(length(tile - vec2(1.0)), 1.0 - CIRCLE_THICCNESS));

    // Truchet (2 triangles)
    // float color = step(tile.x, tile.y);

    gl_FragColor = vec4(vec3(color), 1.0);
}

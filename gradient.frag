uniform vec2 u_resolution;
uniform vec2 u_mouse_position;

float plot(vec2 st, float pct, float thiccness) {
    return smoothstep(pct - thiccness, pct, st.y) -
        smoothstep(pct, pct + thiccness, st.y);
}

void main(void)
{
    vec2 st = gl_FragCoord.xy / u_resolution;

    float a = st.x - u_mouse_position.x / u_resolution.x;
    vec3 color = vec3(a);

    float pct = plot(st, a, 0.01);
    color = (1.0 - pct) * color + pct * vec3(0.0, 1.0, 0.0);
    gl_FragColor = vec4(color, 1.0);
}

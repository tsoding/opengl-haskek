# OpenGL Haskek

## Quick Start

```console
$ cabal v2-run
```

## pipeline.conf

Expected in `CWD`. See the [pipeline.conf](./pipeline.conf) for a usage example.

### Parameters

|Key|Description|
|---|---|
|`vertex`|File path to the vertex shader|
|`fragment`|File path to the fragment shader|

## Shortcuts

|Key|Description|
|---|---|
|<kbd>r</kbd>|Hot reload [`pipeline.conf`](#pipelineconf) and the shaders it refers to|
|<kbd>q</kbd>|Quit the application|

## Uniforms

These are [uniforms] that are passed to the shaders specified in
[pipeline.conf](#pipelineconf).

|Name|Description|
|---|---|
|`u_resolution`|Resolution of the window in pixels|
|`u_mouse`|Position of the mouse|
|`u_time`|How much time has passed in seconds|

[uniforms]: https://www.khronos.org/opengl/wiki/Uniform_(GLSL)

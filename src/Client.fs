namespace Samples

open System
open WebSharper
open WebSharper.GlMatrix
open WebSharper.JavaScript
open WebSharper.UI
open WebSharper.UI.Html
open WebSharper.UI.Client

[<JavaScript>]
module WebGL =
    
    let CreateContext (element: Elt) =
        let canvas = As<CanvasElement> element.Dom
        canvas.Width <- 300
        canvas.Height <- 200
        ["webgl"; "experimental-webgl"]
        |> List.tryPick (fun s ->
            let gl = As<WebGL.RenderingContext> (canvas.GetContext s)
            if gl = null then None else Some gl)

    let BasicVertexShader = "
        precision highp float;
        attribute vec3 position;
        attribute vec2 texCoord;
        uniform mat4 perspectiveMatrix;
        uniform mat4  modelViewMatrix;
        varying vec2 textureCoord;
        void main(void)
        {
            gl_Position = perspectiveMatrix * modelViewMatrix * vec4(position, 1.0);
            textureCoord = texCoord;
        }"

    let TexturingFragmentShader = "
        precision highp float;
        uniform sampler2D tex;
        varying vec2 textureCoord;
        void main(void)
        {
            gl_FragColor = texture2D(tex, textureCoord);
        }"

    let CreateProgram (gl : WebGL.RenderingContext, vertexSource, fragmentSource) =
        let vs = gl.CreateShader(gl.VERTEX_SHADER)
        gl.ShaderSource(vs, vertexSource)
        gl.CompileShader(vs)
        if not(As<bool>(gl.GetShaderParameter(vs, gl.COMPILE_STATUS))) then
            JS.Alert(
                "Couldn't compile the vertex shader:\n" +
                gl.GetShaderInfoLog(vs))
            gl.DeleteShader(vs)
        let fs = gl.CreateShader(gl.FRAGMENT_SHADER)
        gl.ShaderSource(fs, fragmentSource)
        gl.CompileShader(fs)
        if not(As<bool>(gl.GetShaderParameter(fs, gl.COMPILE_STATUS))) then
            JS.Alert(
                "Couldn't compile the fragment shader:\n" +
                gl.GetShaderInfoLog(fs))
            gl.DeleteShader(fs)
        let program = gl.CreateProgram()
        gl.AttachShader(program, vs)
        gl.AttachShader(program, fs)
        gl.LinkProgram(program)
        if not(As<bool>(gl.GetProgramParameter(program, gl.LINK_STATUS))) then
            JS.Alert(
                "Couldn't link the shader program:\n" +
                gl.GetProgramInfoLog(program))
            gl.DeleteProgram(program)
            gl.DeleteShader(fs)
            gl.DeleteShader(fs)
        program

    let SetupView (gl : WebGL.RenderingContext, program) =
        let fieldOfView = 45. * Math.PI / 180.
        let aspectRatio = 4./3.
        let nearPlane = 1.
        let farPlane = 10000.
        let perspectiveMatrix = Mat4.Perspective(Mat4.Create(), fieldOfView, aspectRatio, nearPlane, farPlane)
        let uPerspectiveMatrix = gl.GetUniformLocation(program, "perspectiveMatrix")
        gl.UniformMatrix4fv(uPerspectiveMatrix, false, perspectiveMatrix)

    let DrawRotatingObject (gl : WebGL.RenderingContext,
                            program, buf, numVertices) =
        gl.ClearColor(0., 0., 0., 0.)
        gl.UseProgram(program)
        SetupView(gl, program)
        let uModelViewMatrix = gl.GetUniformLocation(program, "modelViewMatrix")
        let rec RunFrame (i : int) () =
            let angle = 2. * float i * System.Math.PI / 1000.
            let modelViewMatrix = Mat4.Identity(Mat4.Create())
            Mat4.Translate(modelViewMatrix, modelViewMatrix, Vec3.FromValues(0., 0., -4.)) |> ignore
            Mat4.RotateY(modelViewMatrix, modelViewMatrix, angle) |> ignore
            gl.UniformMatrix4fv(uModelViewMatrix, false, modelViewMatrix)
            gl.Clear(gl.COLOR_BUFFER_BIT ||| gl.DEPTH_BUFFER_BIT)
            gl.BindBuffer(gl.ARRAY_BUFFER, buf)
            gl.DrawArrays(gl.TRIANGLES, 0, numVertices)
            gl.Flush()
            JS.SetTimeout (RunFrame ((i + 20) % 1000)) 20 |> ignore
        RunFrame 0 ()

    let MakeAndBindTexture (gl : WebGL.RenderingContext) f =
        let i =
            Elt.img [
                on.load (fun img ev ->
                    let tex = gl.CreateTexture()
                    gl.ActiveTexture(gl.TEXTURE0)
                    gl.BindTexture(gl.TEXTURE_2D, tex)
                    gl.PixelStorei(gl.UNPACK_FLIP_Y_WEBGL, 1)
                    gl.TexImage2D(gl.TEXTURE_2D, 0, gl.RGBA, gl.RGBA, gl.UNSIGNED_BYTE, img)
                    gl.TexParameteri(gl.TEXTURE_2D, gl.TEXTURE_MAG_FILTER, gl.LINEAR)
                    gl.TexParameteri(gl.TEXTURE_2D, gl.TEXTURE_MIN_FILTER, gl.LINEAR)
                    f ())
            ] []
        i.Dom.SetAttribute("src", "image.png")
        
    let CreateSquare (gl : WebGL.RenderingContext, program) =
        let vertexPosition = gl.GetAttribLocation(program, "position")
        gl.EnableVertexAttribArray(vertexPosition)
        let vertexTexCoord = gl.GetAttribLocation(program, "texCoord")
        gl.EnableVertexAttribArray(vertexTexCoord)
        let vertexBuffer = gl.CreateBuffer()
        let vertices = Float32Array([| -1.f; -1.f; 0.f;    0.f; 0.f;
                                       -1.f;  1.f; 0.f;    0.f; 1.f;
                                        1.f; -1.f; 0.f;    1.f; 0.f;
                                        1.f;  1.f; 0.f;    1.f; 1.f;
                                       -1.f;  1.f; 0.f;    0.f; 1.f;
                                        1.f; -1.f; 0.f;    1.f; 0.f; |])
        gl.BindBuffer(gl.ARRAY_BUFFER, vertexBuffer)
        gl.BufferData(gl.ARRAY_BUFFER, vertices, gl.STATIC_DRAW)
        let floatSize = Float32Array.BYTES_PER_ELEMENT
        gl.VertexAttribPointer(vertexPosition, 3, gl.FLOAT, false, 5 * int floatSize, 0)
        gl.VertexAttribPointer(vertexTexCoord, 2, gl.FLOAT, false, 5 * int floatSize, 3 * int floatSize)
        (vertexBuffer, 6)

    let DrawTexturedSquare () =
        let canvas = Elt.canvas [attr.style "z-index:-1; position:absolute;"] []
        match CreateContext canvas with
        | None -> JS.Alert "Couldn't create WebGL context."
        | Some gl ->
            let program = CreateProgram(gl, BasicVertexShader, TexturingFragmentShader)
            let vertexBuffer, numberVertices = CreateSquare(gl, program)
            MakeAndBindTexture gl <| fun () ->
                gl.UseProgram(program)
                let u_texture = gl.GetUniformLocation(program, "tex")
                gl.Uniform1i(u_texture, 0)
                DrawRotatingObject(gl, program, vertexBuffer, numberVertices)
        div [attr.style "position:relative; height: 300;"] [
            canvas
            p [] [text "This sample shows how you can insert 3D content in a web page."]
            p [] [text "Texture data is simply grabbed from an <img> element."]
            p [] [text "You can also see that a WebGL context can be placed anywhere, even behind text."]
        ]
        |> Doc.RunById "main"

    [<SPAEntryPoint>]
    let Main() =
        DrawTexturedSquare ()

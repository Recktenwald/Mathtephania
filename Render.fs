namespace Render

// Currently there are no options, just one thing that should hopefully work



module FFmpeg =
    open System
    open System.Diagnostics

    type Animation =
        { frames: float -> byte array // gets called between 0 and 1 inclusive
          fps: int
          duration: int } // in seconds

    // I'm only implementing what I need / think I'll need
    // This is not supposed to be comprehensive
    type Preset =
        | Ultrafast
        | Superfast
        | Veryfast
        | Faster
        | Fast
        | Medium // – default preset
        | Slow
        | Slower
        | Veryslow
        | Placebo // – ignore this as it is not usefull

    type Arguments =
        { Overwrite: bool
          Rate: int
          Input: unit // not implemented anything but pipe yet
          Preset: Preset
          Crf: int // only values between 0 and 51 do anything
          Faststart: bool // sets -movflags +faststart. See also https://trac.ffmpeg.org/wiki/Encode/H.264
          Output: string }

    let toArgumentsString args =
        [ if args.Overwrite then "-y"
          "-r " + args.Rate.ToString()
          "-i -"
          "-an"
          "-vcoded libx264"
          "-pix_fnt yuv420p"
          "-preset " + args.Preset.ToString().ToLower()
          "-crf " + args.Crf.ToString()
          if args.Faststart then
              "-movflags +faststart"
          args.Output ]
        |> String.concat " "

    let defaultArguments =
        { Overwrite = true
          Rate = 60
          Input = ()
          Preset = Medium
          Crf = 23
          Faststart = false
          Output = "output.mp4" }


    let runFfmpeg args (animation: Animation) =
        let timer = Stopwatch.StartNew()

        let procStartInfo =
            ProcessStartInfo(
                RedirectStandardOutput = true,
                RedirectStandardError = true,
                RedirectStandardInput = true,
                UseShellExecute = false,
                FileName = "ffmpeg",
                Arguments = toArgumentsString args
            )
        // match startDir with | Some d -> procStartInfo.WorkingDirectory <- d | _ -> ()
        // printfn "1"
        let outputs =
            System.Collections.Generic.List<string>()

        let errors =
            System.Collections.Generic.List<string>()

        let outputHandler f (_sender: obj) (args: DataReceivedEventArgs) = f args.Data
        let p = new Process(StartInfo = procStartInfo)
        p.OutputDataReceived.AddHandler(DataReceivedEventHandler(outputHandler outputs.Add))
        p.ErrorDataReceived.AddHandler(DataReceivedEventHandler(outputHandler errors.Add))

        let started =
            try
                p.Start()
            with
            | ex -> reraise ()

        if not started then
            failwithf "Failed to start ffmpeg"

        printfn "Started %s with pid %i" p.ProcessName p.Id
        p.BeginOutputReadLine()
        p.BeginErrorReadLine()
        let inStream = p.StandardInput.BaseStream
        let totalFrames = animation.fps * animation.duration
        let frames = animation.frames

        for frameNumber in [ 0 .. totalFrames ] do
            let frame =
                frames (float frameNumber / (float totalFrames))

            inStream.Write(frame)
        // let mutable counter = 0
        // for frame in frames do
        //     printfn $"{counter}"
        //     stream.Write(frame.ToByteArray())
        //     //frame.Write(p.StandardInput.BaseStream)
        //     counter <- counter + 1
        inStream.Flush() // Not entirely sure why I need this
        inStream.Close()
        p.WaitForExit()
        timer.Stop()
        printfn "Finished after %A milliseconds" timer.ElapsedMilliseconds

        let cleanOut l =
            l
            |> Seq.filter (fun o -> String.IsNullOrEmpty o |> not)

        cleanOut outputs, cleanOut errors

// module Svg =
//     open Curve
//     open ImageMagick

    // type Viewbox =
    //     { xmin: float
    //       ymin: float
    //       width: float
    //       height: float }
    //     override this.ToString() =
    //         $"{this.xmin} {this.ymin} {this.width} {this.height}"

    // let toPathString (commands: BezierPath seq) =
    //     let toPathCommand (command: BezierPath) =
    //         match command with
    //         | BezierPath.MoveTo (Vec2 (a, b)) -> $"M{a} {b}"
    //         | BezierPath.CurveTo (Vec2 (a, b), Vec2 (c, d), Vec2 (e, f)) -> $"C{a} {b} {c} {d} {e} {f}"
    //         | BezierPath.Close -> "Z"

    //     Seq.map toPathCommand commands |> String.concat ""

    // let shapeToSvgElement shape =
    //     $"""<path transform="scale(1 -1)" {shape.Attributes} d="{toPathString shape.Shape}"/>"""

    // let intoSvgTag width height viewBox content =
    //     $"""<svg xmlns="http://www.w3.org/2000/svg" width="{width}" height="{height}" viewBox="{viewBox}">"""
    //     + content
    //     + "</svg>"

    // let intoGroupTag attributes (transform: Transformation) content =
    //     $"""<g transform="{transform}" {attributes}>"""
    //     + content
    //     + "</g>"


    // let shapeToSvg (shape: Shape) =
    //     $"""<svg xmlns="http://www.w3.org/2000/svg" width="480px" height="270px" viewBox="-8 -4.5 16 9">
    //         <path transform="scale(1 -1)" {shape.Attributes} d="{toPathString shape.Shape}"/>
    //         </svg>
    //     """

    // let svgToQoiByteArray (svg: string) =
    //     let image =
    //         new MagickImage(System.Text.Encoding.ASCII.GetBytes(svg))

    //     image.Format <- MagickFormat.Qoi
    //     image.ToByteArray()

namespace Renderer



module Frame =

    type IRenderable =
        abstract member Render : unit -> System.Drawing.Bitmap

    let toByteArray (img: IRenderable) = img.Render()

module FFmpeg =
    open System
    open System.IO
    open System.Diagnostics

    type Animation =
        { Frames: float -> Drawing.Bitmap
          Rate: int
          Duration: int } // in seconds
    
    let bmpToBytes (bmp: Drawing.Bitmap) = 
        use stream = new MemoryStream()
        bmp.Save(stream, Drawing.Imaging.ImageFormat.Png)
        stream.ToArray()

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
          "-vcodec libx264"
          "-pix_fmt yuv420p"
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
        let totalFrames = animation.Rate * animation.Duration
        let frames = animation.Frames

        for frameNumber in [ 0 .. totalFrames ] do
            let t =
                (float frameNumber / (float totalFrames))

            let frame = frames t |> bmpToBytes
            inStream.Write(frame)
            inStream.Flush() // Not entirely sure why I need this

        inStream.Close()
        p.WaitForExit()
        timer.Stop()
        printfn "Finished after %A milliseconds" timer.ElapsedMilliseconds

        let cleanOut l =
            l
            |> Seq.filter (fun o -> String.IsNullOrEmpty o |> not)

        cleanOut outputs, cleanOut errors

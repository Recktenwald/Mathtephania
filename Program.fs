open ImageMagick
let img = new MagickImage(@"C:\AAPersonal\Programming\Mathtephania\Playground\test.svg")

[<EntryPoint>]
let main _ = 
    img.Format <- MagickFormat.Png
    img.Write(@"C:\AAPersonal\Programming\Mathtephania\Playground\test.png")
    printfn "hello" 
    0
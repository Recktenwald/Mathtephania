namespace Animo

module Attributes =
    [<RequireQualifiedAccess>]
    type Cap =
        | Butt
        | Round
        | Square
        override this.ToString() =
            match this with 
            | Round -> "round"
            | Butt -> "butt"
            | Square -> "square"


    [<RequireQualifiedAccess>]
    type LineJoin =
        | Bevel
        | Miter
        | Round
        override this.ToString() =
            match this with 
            | Bevel -> "bevel"
            | Miter -> "miter"
            | Round -> "round"

    type Paint =
        | RGB of int*int*int // https://www.w3.org/TR/2003/REC-SVG11-20030114/types.html#ColorKeywords to make a lot of constants.
        | LinearGradient // not implemented yet
        | RadialGradient // not implemented yet
        override this.ToString () = 
            match this with 
            | RGB (a,b,c) -> $"rgb({a},{b},{c})"
            | _ -> failwith "not implemented yet"
    
    type DrawAttributes =
        { StrokeWidth: float 
          StrokeColor: Paint option  
          StrokeOpacity: float 
          StrokeLineCap: Cap 
          StrokeLineJoin: LineJoin 
          StrokeMiterLimit : float option 
          FillColor: Paint option
          FillOpacity: float}
          with
        //   override this.ToString() = 
        //     $"stroke-width=\"{this.StrokeWidth}\"" 
        //     + match this.StrokeColor with | None -> "" | Some paint -> $"stroke=\"{paint}\""
        //     + $"stroke-opacity=\"{this.StrokeOpacity}\"" 
        //     + $"stroke-linecap=\"{this.StrokeLineCap}\"" 
        //     + $"stroke-linejoin=\"{this.StrokeLineJoin}\"" 
        //     + match this.StrokeMiterLimit with | None -> "" | Some limit -> $"stroke-miterlimit=\"{limit}\""
        //     + match this.FillColor with | None -> "" | Some paint -> $"fill=\"{paint}\""
        //     + $"fill-opacity=\"{this.FillOpacity}\""
            override this.ToString() = 
                [
                    $"stroke-width=\"{this.StrokeWidth}\"" 
                    match this.StrokeColor with | None -> "" | Some paint -> $"stroke=\"{paint}\""
                    $"stroke-opacity=\"{this.StrokeOpacity}\"" 
                    $"stroke-linecap=\"{this.StrokeLineCap}\"" 
                    $"stroke-linejoin=\"{this.StrokeLineJoin}\"" 
                    match this.StrokeMiterLimit with | None -> "" | Some limit -> $"stroke-miterlimit=\"{limit}\""
                    match this.FillColor with | None -> "fill=\"none\"" | Some paint -> $"fill=\"{paint}\""
                    $"fill-opacity=\"{this.FillOpacity}\""
                ]
                |> String.concat " "


    let defaultDrawAttributes = 
        { // This assumes a defaule resolution of roughly 1600x900
            StrokeWidth = 0.05
            StrokeColor = None 
            StrokeOpacity = 1.0
            StrokeLineCap = Cap.Butt
            StrokeLineJoin = LineJoin.Miter 
            StrokeMiterLimit = Some 4.0
            FillColor = Some (RGB (0,0,0))
            FillOpacity = 1.0
        }
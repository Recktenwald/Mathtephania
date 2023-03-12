namespace Curve



[<Struct>]
type Vec2 =
    | Vec2 of float * float
    // Additive Structure
    static member inline Zero = Vec2(0.0, 0.0)
    static member inline (+)(Vec2 (a, b), Vec2 (x, y)) = Vec2(a + x, b + y)
    static member inline (-)(Vec2 (a, b), Vec2 (x, y)) = Vec2(a - x, b - y)
    static member inline (~-)(Vec2 (a, b)) = Vec2(-a, -b)

    // Multiplicative Structure
    static member inline (.*)(Vec2 (a, b), lambda:float):Vec2 = Vec2(a * lambda, b * lambda)
    static member inline (.*)(lambda:float , v:Vec2):Vec2 = v .* lambda
    static member inline (/)(Vec2 (a, b), lambda:float):Vec2 = Vec2(a / lambda, b / lambda)
    static member inline (/)(lambda:float, Vec2 (a, b)):Vec2 = Vec2(lambda / a, lambda / b)

    // Convenience Functions
    member this.Item
        with get (n) =
            match this, n with
            | Vec2 (a, b), 0 -> a
            | Vec2 (a, b), 1 -> b
            | _ -> failwith "Vec2: Index out of bounds."

    // // This makes it very convenient to put this into SVG Path Commands later
    override this.ToString() =
        match this with
        | Vec2 (a, b) -> $"{a} {b}"

    // Euclidiean structure
    member this.Abs() = 
        match this with 
        | Vec2 (a, b) -> sqrt(a*a + b*b)
    member this.Turn() = match this with Vec2(a,b) -> Vec2(-b, a)

module Vec2 =
    let inline map f (Vec2 (a, b)) = Vec2(f a, f b)

    let inline dot (Vec2 (a, b)) (Vec2 (x, y)) = a * x + b * y

    let inline length (Vec2 (a, b)) (Vec2 (x, y)) = System.Double.Sqrt(a * x + b * y)

    let inline updateAt index value (Vec2 (a, b)) =
        match index with
        | 0 -> Vec2(value, b)
        | 1 -> Vec2(a, value)
        | _ -> failwith "Vec2: Index out of bounds."

[<Struct>]
type Mat2 = // Row Vectors
    | Mat2 of Vec2 * Vec2 
    // Additive Structure
    static member inline Zero = Mat2 (Vec2.Zero, Vec2.Zero)
    static member inline (+)(Mat2 (a, b), Mat2 (x, y)) = Mat2(a+x, b+y)
    static member inline (-)(Mat2 (a, b), Mat2 (x, y)) = Mat2(a-x, b-y)
    static member inline (~-)(Mat2 (a, b)) = Mat2(-a, -b)

    // Multiplicative Structure
    static member inline (.*)(Mat2 (a, b), lambda:float) = Mat2(a .* lambda, b .* lambda)
    static member inline (.*)(lambda:float, m:Mat2) = m .* lambda
    static member inline (.*)(Mat2 (v,w), x:Vec2) = Vec2( Vec2.dot v x, Vec2.dot w x)
    static member inline (/)(Mat2 (a, b), lambda) = Mat2(a / lambda, b / lambda)
    static member inline (/)(lambda, Mat2 (a, b)) = Mat2(lambda / a, lambda / b)
    
    // Convenience Functions
    member this.Item
        with get (n) =
            match this, n with
            | Mat2 (a, b), 0 -> a
            | Mat2 (a, b), 1 -> b
            | _ -> failwith "Vec2: Index out of bounds."
        
    override this.ToString() =
        match this with
        | Mat2 (Vec2(a, b), Vec2(x,y)) -> $"{a} {x}\n{b} {y}"

// type Vec3 =
//     | Vec3 of float * float * float
//     static member inline (+)(Vec3 (a, b, c), Vec3 (x, y, z)) = Vec3(a + x, b + y, c + z)
//     static member inline Zero = Vec3(0.0, 0.0, 0.0)
//     static member inline ( * )(Vec3 (a, b, c), lambda) = Vec3(a * lambda, b * lambda, c * lambda)
//     static member inline ( * )(lambda, v) = v*lambda


type Curve<'T> =
    { StartPoint: 'T
      StartControlPoint: 'T
      EndControlPoint: 'T
      EndPoint: 'T }
    static member inline (.*)(m:Mat2, c:Curve<Vec2>) = 
        {
            StartPoint = m .* c.StartPoint
            StartControlPoint = m .* c.StartControlPoint
            EndControlPoint = m .* c.EndControlPoint
            EndPoint = m .* c.EndPoint
        }

type Interpolation<'T> = 'T -> 'T -> float -> 'T


module Curve =
    let inline toTuple bc =
        bc.StartPoint, bc.StartControlPoint, bc.EndControlPoint, bc.EndPoint

    let inline ofTuple (a, b, c, d) =
        { StartPoint = a
          StartControlPoint = b
          EndControlPoint = c
          EndPoint = d }

    let inline lerp x y t = ((1.0 - t) .* x) + (t .* y)

    let inline splitBezier (bc: Curve<'T>) t =
        let P0, P1, P2, P3 = toTuple bc
        let Q1 = lerp P0 P1 t
        let middle = lerp P1 P2 t
        let R2 = lerp P2 P3 t
        let Q2 = lerp Q1 middle t
        let R1 = lerp middle R2 t
        let Q3 = lerp Q2 R1 t
        (ofTuple (P0, Q1, Q2, Q3)), (ofTuple (Q3, R1, R2, P3))

    let inline map f bc =
        { StartPoint = f bc.StartPoint
          StartControlPoint = f bc.StartControlPoint
          EndControlPoint = f bc.EndControlPoint
          EndPoint = f bc.EndPoint }

    let inline map2 f bc1 bc2 =
        { StartPoint = f bc1.StartPoint bc2.StartPoint
          StartControlPoint = f bc1.StartControlPoint bc2.StartControlPoint
          EndControlPoint = f bc1.EndControlPoint bc2.EndControlPoint
          EndPoint = f bc1.EndPoint bc2.EndPoint }

    let inline morph (interpolator: Interpolation<'T>) bc1 bc2 t =
        map2 interpolator bc1 bc2 |> map (fun f -> f t)

    let inline linearMorph bc1 bc2 t = morph lerp bc1 bc2 t


// type PathPart =
//     | MoveTo of Vec2
//     | CurveTo of startControlPoint: Vec2 * endControlPoint: Vec2 * endPoint: Vec2
//     | Close

type PathPart<'T> =
    | Curve of Curve<'T>
    | Close

type IPathable<'T> =
    abstract member ToPath : unit -> CurvePath<'T>

and CurvePath<'T> =
    | CurvePath of PathPart<'T> list
    interface IPathable<'T> with
        member this.ToPath() = this

// TODO
module Path =
    let toPath (p: IPathable<'T>) = p.ToPath()
// Some todos:
// Length Parametrization
// Morphing of Paths



// TODO
type Transformation = unit

module Transformation =
    let x = 0

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
        | Transperant // Note that having fill=none is different to not having a fill attribute at all!
        | RGB of int * int * int // https://www.w3.org/TR/2003/REC-SVG11-20030114/types.html#ColorKeywords to make a lot of constants.
        | LinearGradient // not implemented yet
        | RadialGradient // not implemented yet
        override this.ToString() =
            match this with
            | Transperant -> "none"
            | RGB (a, b, c) -> $"rgb({a},{b},{c})"
            | _ -> failwith "not implemented yet"

    type DrawAttributes =
        { StrokeWidth: float option
          StrokeColor: Paint option
          StrokeOpacity: float option 
          StrokeLineCap: Cap option 
          StrokeLineJoin: LineJoin option 
          StrokeMiterLimit: float option
          FillColor: Paint option 
          FillOpacity: float option }
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
            let toName attribute name = 
                match attribute with 
                | Some value -> $"{name}=\"{value}\""
                | None -> ""
            [ 
                toName this.StrokeWidth "stroke-width"
                toName this.StrokeColor "stroke"
                toName this.StrokeOpacity "stroke-opacity"
                toName this.StrokeLineCap "stroke-linecap"
                toName this.StrokeLineJoin "stroke-linejoin"
                toName this.FillColor "fill" 
                toName this.FillOpacity "fill-opacity" 
            ] |> String.concat " "


    let defaultDrawAttributes =
        { // This assumes a default resolution of roughly 1600x900
          StrokeWidth = Some 0.05
          StrokeColor = None
          StrokeOpacity = Some 1.0
          StrokeLineCap = Some Cap.Butt
          StrokeLineJoin = Some LineJoin.Miter
          StrokeMiterLimit = Some 4.0
          FillColor = Some(RGB(0, 0, 0))
          FillOpacity = Some 1.0 }

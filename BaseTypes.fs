namespace Animo

[<Struct>]
type Vec2 =
    | Vec2 of float * float
    // Additive Structure
    static member inline Zero = Vec2(0.0, 0.0)
    static member inline (+)(Vec2 (a, b), Vec2 (x, y)) = Vec2(a + x, b + y)
    static member inline (-)(Vec2 (a, b), Vec2 (x, y)) = Vec2(a - x, b - y)
    static member inline (~-)(Vec2 (a, b)) = Vec2(-a, -b)

    // Multiplicative Structure
    static member inline ( .* )(Vec2 (a, b), lambda) = Vec2(a * lambda, b * lambda)
    static member inline ( .* )(lambda, v) = v*lambda
    static member inline ( / )(Vec2 (a, b), lambda) = Vec2(a / lambda, b / lambda)
    static member inline ( / )(lambda, Vec2 (a, b)) = Vec2(lambda / a, lambda / b)

    // Convenience Functions
    member this.Item 
            with get(n) = 
                match this, n with 
                | Vec2 (a,b), 0 -> a
                | Vec2 (a,b), 1 -> b
                | _ -> failwith "Vec2: Index out of bounds."
    
    // // This makes it very convenient to put this into SVG Path Commands later
    override this.ToString () = 
            match this with 
            | Vec2 (a,b) -> $"{a} {b}"

    // // Euclidiean structure 
    // static member inline ( .* )(Vec2 (a, b), Vec2 (x, y)) = a*x + b*y

module Vec2 = 
    let inline map f (Vec2 (a, b)) = Vec2 (f a, f b)

    let inline dot (Vec2 (a, b)) (Vec2 (x, y)) = a*x + b*y

    let inline length (Vec2 (a, b)) (Vec2 (x, y)) = System.Double.Sqrt(a*x + b*y)

    let inline updateAt index value (Vec2 (a, b)) = 
        match index with 
        | 0 -> Vec2 (value, b)
        | 1 -> Vec2 (a, value)
        | _ -> failwith "Vec2: Index out of bounds."

    
// type Vec3 =
//     | Vec3 of float * float * float
//     static member inline (+)(Vec3 (a, b, c), Vec3 (x, y, z)) = Vec3(a + x, b + y, c + z)
//     static member inline Zero = Vec3(0.0, 0.0, 0.0)
//     static member inline ( * )(Vec3 (a, b, c), lambda) = Vec3(a * lambda, b * lambda, c * lambda)
//     static member inline ( * )(lambda, v) = v*lambda


type BezierCurve<'T> = 
    { 
        StartPoint : 'T 
        StartControlPoint : 'T 
        EndControlPoint : 'T
        EndPoint : 'T
    }

type Interpolation<'T> = 'T -> 'T -> float -> 'T

module BezierCurve = 
    let inline toTuple bc = 
        bc.StartPoint, bc.StartControlPoint, bc.EndControlPoint, bc.EndPoint
    let inline ofTuple (a,b,c,d) = 
        {StartPoint = a; StartControlPoint = b; EndControlPoint = c; EndPoint = d}
    
    let inline lerp x y t = ((1.0-t) .* x) + (t .* y)

    let inline splitBezier (bc:BezierCurve<'T>) t = 
        let P0, P1, P2, P3 = toTuple bc 
        let Q1 = lerp P0 P1 t
        let middle = lerp P1 P2 t 
        let R2 = lerp P2 P3 t 
        let Q2 = lerp Q1 middle t 
        let R1 = lerp middle R2 t 
        let Q3 = lerp Q2 R1 t 
        (ofTuple (P0, Q1, Q2, Q3)), (ofTuple (Q3, R1, R2, P3))
    
    let inline map f bc = 
        {
            StartPoint = f bc.StartPoint
            StartControlPoint = f bc.StartControlPoint
            EndControlPoint = f bc.EndControlPoint
            EndPoint = f bc.EndPoint
        }
    
    let inline map2 f bc1 bc2 = 
        {
            StartPoint = f bc1.StartPoint bc2.StartPoint 
            StartControlPoint = f bc1.StartControlPoint bc2.StartControlPoint 
            EndControlPoint = f bc1.EndControlPoint bc2.EndControlPoint
            EndPoint = f bc1.EndPoint bc2.EndPoint
        }

    let inline morph (interpolator:Interpolation<'T>) bc1 bc2 t= 
        map2 interpolator bc1 bc2 
        |> map (fun f -> f t)
    
    let inline linearMorph bc1 bc2 t = 
        morph lerp bc1 bc2 t


type BezierPath =
    | MoveTo of Vec2 
    | CurveTo of startControlPoint: Vec2 * endControlPoint: Vec2 * endPoint: Vec2
    | Close 

type Transformation = unit

type Shape = 
    {
        Shape : BezierPath seq 
        Attributes : Attributes.DrawAttributes
        Transformation : Transformation // not implemented yet
    }

// type CBP3 =
//     | MoveTo of Vec3
//     | CurveTo of startControlPoint: Vec3 * endControlPoint: Vec3 * endPoint: Vec3
//     | Close



// type Shape3 = 
//     {
//         Shape : CBP3 seq 
//         Attributes : Attributes.DrawAttributes
//         Transformation : unit // not implemented yet
//     }

// type Shape2 = 
//     {
//         Shape : CBP2 seq 
//         Attributes : Attributes.DrawAttributes
//         Transformation : unit // not implemented yet
//     }

// type Camera = CBP3 -> CBP2






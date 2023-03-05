namespace Parser

// module Transform =
//     type TMatrix = Matrix of a: float * b: float * c: float * d: float * e: float * f: float

//     type Transform =
//         | TMatrix of TMatrix
//         | Translate of Vec2d
//         | Scale of x: float * y: float
//         | Rotate of angle: float * point: Vec2d
//         | SkewX of float
//         | SkewY of float


// module Attributes =
//     [<RequireQualifiedAccess>]
//     type Cap =
//         | Round
//         | Butt
//         | Square

//     [<RequireQualifiedAccess>]
//     type LineJoin =
//         | Miter
//         | Bevel
//         | Round

//     [<RequireQualifiedAccess>]
//     type Paint =
//         | Color of string
//         | NoneValue

//     type DrawAttributes =
//         { StrokeWidth: float option
//           StrokeColor: string option
//           StrokeOpacity: float option
//           StrokeLineCap: Cap option
//           StrokeLineJoin: LineJoin option
//           StrokeMiterLimit : float option
//           FillColor: string option
//           FillOpacity: float option
//           Transform: Transform.Transform option }

module Path =
    type Point = float * float

    type ArcFlag =
        | Large
        | Small
        override this.ToString() =
            match this with
            | Large -> "1"
            | Small -> "0"

    type SweepFlag =
        | Clockwise
        | CounterClockwise
        override this.ToString() =
            match this with
            | Clockwise -> "1"
            | CounterClockwise -> "0"

    type PathPart =
        | MoveTo of Point // equivalent of M
        | MoveAlong of Point // equivalent of m
        | LineTo of Point // equivalent of L
        | LineAlong of Point // equivalent of l
        | HorizontalTo of float // equivalent of H
        | HorizontalAlong of float // equivalent of h
        | VerticalTo of float // equivalent of V
        | VerticalAlong of float // equivalent of v
        | CubicBezierAbsolute of startControlPoint: Point * endControlPoint: Point * endPoint: Point // equivalent of C
        | CubicBezierRelative of startControlPoint: Point * endControlPoint: Point * endPoint: Point // equivalent of c
        | SmoothCubicBezierAbsolute of endControlPoint: Point * endPoint: Point // equivalent of S
        | SmoothCubicBezierRelative of endControlPoint: Point * endPoint: Point // equivalent of s
        | QuadraticBezierAbsolute of controlPoint: Point * endPoint: Point // equivalent of Q
        | QuadraticBezierRelative of controlPoint: Point * endPoint: Point // equivalent of q
        | SmoothQuadraticBezierAbsolute of Point // equivalent of T
        | SmoothQuadraticBezierRelative of Point // equivalent of t
        | ArcAbsolute of
            radiusX: float *
            radiusY: float *
            rotation: float *
            largeArcFlag: ArcFlag *
            sweepFlag: SweepFlag *
            endPoint: Point // equivalent of A
        | ArcRelative of
            radiusX: float *
            radiusY: float *
            rotation: float *
            largeArcFlag: ArcFlag *
            sweepFlag: SweepFlag *
            endPoint: Point // equivalent of a
        | ClosePath

    and Path = PathPart list

    let rec toSvgPathString (path: Path) : string =
        match path with
        | [] -> System.String.Empty
        | p :: ps ->
            match p with
            | MoveTo (x, y) -> $"M{x} {y}" + toSvgPathString ps
            | MoveAlong (x, y) -> $"m{x} {y}" + toSvgPathString ps
            | LineTo (x, y) -> $"L{x} {y}" + toSvgPathString ps
            | LineAlong (x, y) -> $"l{x} {y}" + toSvgPathString ps
            | HorizontalTo x -> $"H{x}" + toSvgPathString ps // equivalent of H
            | HorizontalAlong x -> $"h{x}" + toSvgPathString ps // equivalent of h
            | VerticalTo y -> $"V{y}" + toSvgPathString ps // equivalent of V
            | VerticalAlong y -> $"v{y}" + toSvgPathString ps // equivalent of v
            | CubicBezierAbsolute ((x1, y1), (x2, y2), (x, y)) ->
                $"C{x1} {y1} {x2} {y2} {x} {y}"
                + toSvgPathString ps
            | CubicBezierRelative ((x1, y1), (x2, y2), (x, y)) ->
                $"c{x1} {y1} {x2} {y2} {x} {y}"
                + toSvgPathString ps
            | SmoothCubicBezierAbsolute ((x2, y2), (x, y)) -> $"S{x2} {y2} {x} {y}" + toSvgPathString ps
            | SmoothCubicBezierRelative ((x2, y2), (x, y)) -> $"s{x2} {y2} {x} {y}" + toSvgPathString ps
            | QuadraticBezierAbsolute ((x1, y1), (x, y)) -> $"Q{x1} {y1} {x} {y}" + toSvgPathString ps
            | QuadraticBezierRelative ((x1, y1), (x, y)) -> $"q{x1} {y1} {x} {y}" + toSvgPathString ps
            | SmoothQuadraticBezierAbsolute (x, y) -> $"T{x} {y}" + toSvgPathString ps
            | SmoothQuadraticBezierRelative (x, y) -> $"t{x} {y}" + toSvgPathString ps
            | ArcAbsolute (rx, ry, angle, largeArcFlag, sweepFlag, (x, y)) ->
                $"A{rx} {ry} {angle} {largeArcFlag} {sweepFlag} {x} {y}"
                + toSvgPathString ps
            | ArcRelative (rx, ry, angle, largeArcFlag, sweepFlag, (x, y)) ->
                $"a{rx} {ry} {angle} {largeArcFlag} {sweepFlag} {x} {y}"
                + toSvgPathString ps
            | ClosePath -> "Z" + toSvgPathString ps

module PathParser =
    open FParsec

    open Path

    type Pu<'T> = Parser<'T, unit>
    // pMyFloat also succeeds on .5
    let pMyFloat: Pu<float> =
        numberLiteral
            (NumberLiteralOptions.DefaultFloat
             ||| NumberLiteralOptions.AllowFractionWOIntegerPart)
            "float"
        |>> fun nl -> (float nl.String)

    let withSpaces p = between spaces spaces p

    let pPoint: Pu<_> =
        tuple2 (withSpaces pMyFloat) (withSpaces pMyFloat)

    let pfloatS: Pu<_> = withSpaces pMyFloat

    // Now we define Parsers for all the Path elements
    // In essance we do the following
    // // Check for the letter
    // // Depending on the letter check for the right amount of numbers after it
    // // Map each set of inputs, for the specific command, to the PathPart
    // MoveTo and MoveAlong are special because following commands are interpreted as LineTo and LineAlong
    let pMoveTo: Pu<_> =
        let toPath (vecs: Point list) =
            match vecs with
            | x :: xs ->
                [ MoveTo x
                  yield! (List.map LineTo xs) ]
            | [] -> failwith "At this point the parser should have failed"

        pstring "M" >>. (many (withSpaces pPoint))
        |>> toPath

    let pMoveAlong: Pu<_> =
        let toPath (vecs: Point list) =
            match vecs with
            | x :: xs ->
                [ MoveAlong x
                  yield! (List.map LineAlong xs) ]
            | [] -> failwith "At this point the parser should have failed"

        pstring "m" >>. (many (withSpaces pPoint))
        |>> toPath

    // This is a helper to create the others
    // The letter corresponds to the command, the parser parses it's inputs, we finally transform it into the PathPart
    let pAll (letter: string) (parser: Pu<'a>) (pathPart: 'a -> PathPart) =
        let toPath (xs: 'a list) = List.map pathPart xs

        pstring letter >>. (many (withSpaces parser))
        |>> toPath

    let pLineTo = pAll "L" pPoint LineTo
    let pLineAlong = pAll "l" pPoint LineAlong
    let pHorizontalTo = pAll "H" pfloatS HorizontalTo
    let pHorizontalAlong = pAll "h" pfloatS HorizontalAlong
    let pVerticalTo = pAll "V" pfloatS VerticalTo
    let pVerticalAlong = pAll "v" pfloatS VerticalAlong

    let pCubicBezierAbsolute =
        pAll "C" (tuple3 pPoint pPoint pPoint) CubicBezierAbsolute

    let pCubicBezierRelative =
        pAll "c" (tuple3 pPoint pPoint pPoint) CubicBezierRelative

    let pSmoothCubicBezierAbsolute =
        pAll "S" (tuple2 pPoint pPoint) SmoothCubicBezierAbsolute

    let pSmoothCubicBezierRelative =
        pAll "s" (tuple2 pPoint pPoint) SmoothCubicBezierRelative

    let pQuadraticBezierAbsolute =
        pAll "Q" (tuple2 pPoint pPoint) QuadraticBezierAbsolute

    let pQuadraticBezierRelative =
        pAll "q" (tuple2 pPoint pPoint) QuadraticBezierRelative

    let pSmoothQuadraticBezierAbsolute =
        pAll "T" pPoint SmoothQuadraticBezierAbsolute

    let pSmoothQuadraticBezierRelative =
        pAll "t" pPoint SmoothQuadraticBezierRelative

    // Arc needs 6 Inputs but FParsec only goes up to tuple5 so we define our own
    let pipe6 p1 p2 p3 p4 p5 p6 g =
        p1
        >>= fun a ->
                p2
                >>= fun b ->
                        p3
                        >>= fun c ->
                                p4
                                >>= fun d ->
                                        p5
                                        >>= fun e -> p6 >>= fun f -> preturn (g a b c d e f)

    let tuple6 p1 p2 p3 p4 p5 p6 =
        pipe6 p1 p2 p3 p4 p5 p6 (fun a b c d e f -> (a, b, c, d, e, f))

    // We need parsers for the flags
    let pArcFlag: Pu<_> =
        withSpaces (pstring "0" |>> (fun _ -> Small))
        <|> (pstring "1" |>> (fun _ -> Large))

    let pSweepFlag: Pu<_> =
        withSpaces (pstring "0" |>> (fun _ -> CounterClockwise))
        <|> (pstring "1" |>> (fun _ -> Clockwise))


    let pArcAbsolute: Pu<_> =
        pAll "A" (tuple6 pfloatS pfloatS pfloatS pArcFlag pSweepFlag pPoint) ArcAbsolute

    let pArcRelative: Pu<_> =
        pAll "A" (tuple6 pfloatS pfloatS pfloatS pArcFlag pSweepFlag pPoint) ArcRelative

    let pClose: Pu<_> =
        withSpaces (pstring "z" <|> pstring "Z")
        |>> (fun _ -> [ ClosePath ])

    // Now we can combine them to a parser for any of the path commands, and then just go through them one after another
    let pPathPart =
        choice [ pMoveTo
                 pMoveAlong
                 pLineTo
                 pLineAlong
                 pHorizontalTo
                 pHorizontalAlong
                 pVerticalTo
                 pVerticalAlong
                 pCubicBezierAbsolute
                 pCubicBezierRelative
                 pSmoothCubicBezierAbsolute
                 pSmoothCubicBezierRelative
                 pQuadraticBezierAbsolute
                 pQuadraticBezierRelative
                 pSmoothQuadraticBezierAbsolute
                 pSmoothQuadraticBezierRelative
                 pArcAbsolute
                 pArcRelative
                 pClose ]

    let pPath = many pPathPart

module LatexSvgParser =
    // THIS IS NOT A FULL SVG PARSER!
    // IT MAKES A LOT OF ASSUMPTIONS ON THE SPECIFIC FILE
    open System
    open System.Xml

    let nsManager = new XmlNamespaceManager(NameTable())
    nsManager.AddNamespace("svg", "http://www.w3.org/2000/svg")

    let extractIdAndPath (attributes: XmlAttributeCollection) =
        // Do I need more error handling here?
        match attributes.ItemOf("id").Value, attributes.ItemOf("d").Value with
        | null, null -> None, None
        | x, null -> Some x, None
        | null, y -> None, Some y
        | x, y -> Some x, Some y

    let getPathsInDefs (xml: XmlElement) =
        let nodes =
            xml.SelectNodes("svg:defs/svg:path", nsManager)

        nodes
        |> Seq.cast<XmlNode>
        |> Seq.map (fun node -> node.Attributes)
        |> Seq.map (extractIdAndPath)
        |> Map.ofSeq

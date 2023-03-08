namespace Curve

type ViewBox = 
    {
        MinX : float 
        MinY : float 
        Width : float 
        Height : float 
    }

type Camera2D =
    { 
        visibleArea: ViewBox
        // Position
        x: float
        y: float
        z: float
        outputHeight: float
        outputWidth: float
        background: Attributes.Paint 
    }

type Shape =
    { Path: IPathable<Vec2>
      Attributes: Attributes.DrawAttributes option
      Transformation: Transformation } // not implemented yet

type Scene =
    { Shapes: Shape seq
      Attributes: Attributes.DrawAttributes option
      Camera: Camera2D }

module SvgRenderer =
    open System.Drawing
    open Curve
    open Curve.Attributes
    open Svg
    open Svg.Pathing

    type SvgDocument with 
        static member draw (svgDoc:SvgDocument) =
            svgDoc.Draw()

    type Camera2D =
        { visibleArea: SvgViewBox
          // Position
          x: float
          y: float
          z: float
          outputHeight: float
          outputWidth: float
          background: Attributes.Paint }



    let toSvgColor (paint: Paint) =
        match paint with
        | Paint.Transperant -> new SvgColourServer(Color.Transparent)
        | Paint.RGB(a, b, c) -> new SvgColourServer(Color.FromArgb(a, b, c))
        | _ -> failwith "not implemented"

    type SvgElement with

        member this.SetAttributes(a: DrawAttributes) =
            if a.StrokeWidth.IsSome then
                this.StrokeWidth <- SvgUnit(float32 a.StrokeWidth.Value)

            if a.StrokeColor.IsSome then
                this.Stroke <- toSvgColor a.StrokeColor.Value

            if a.StrokeOpacity.IsSome then
                this.StrokeOpacity <- float32 a.StrokeOpacity.Value

            if a.StrokeLineCap.IsSome then
                this.StrokeLineCap <-
                    match a.StrokeLineCap.Value with
                    | Cap.Butt -> SvgStrokeLineCap.Butt
                    | Cap.Round -> SvgStrokeLineCap.Round
                    | Cap.Square -> SvgStrokeLineCap.Square

            if a.StrokeLineJoin.IsSome then
                this.StrokeLineJoin <-
                    match a.StrokeLineJoin.Value with
                    | LineJoin.Bevel -> SvgStrokeLineJoin.Bevel
                    | LineJoin.Miter -> SvgStrokeLineJoin.Miter
                    | LineJoin.Round -> SvgStrokeLineJoin.Round

            if a.StrokeMiterLimit.IsSome then
                this.StrokeMiterLimit <- float32 a.StrokeMiterLimit.Value

            if a.FillColor.IsSome then
                this.Fill <- toSvgColor a.FillColor.Value

            if a.FillOpacity.IsSome then
                this.FillOpacity <- float32 a.FillOpacity.Value
    // This takes care of flipping the y axis into the svg coordinate system
    let inline vecToPoint (Vec2(a, b)) : PointF = new PointF(float32 a, float32 -b)

    let toSvgPathSegments (pathable: IPathable<Vec2>) : SvgPathSegmentList =
        let curvePath = pathable.ToPath()
        // svgPath.PathData.Add(new SvgCubicCurveSegment(false, vecToPoint ))
        let rec loop (currentPoint: Vec2 option) (CurvePath remainingPath) (result: SvgPathSegmentList) =
            match remainingPath with
            | [] -> result
            | p :: ps ->
                match p with
                | Close -> //result.PathData.Add(new SvgClosePathSegment())
                    result.Add(new SvgClosePathSegment(false))
                    loop currentPoint (CurvePath ps) result
                | Curve c ->
                    if Some(c.StartPoint) <> currentPoint then
                        result.Add(new SvgMoveToSegment(false, vecToPoint c.StartPoint))
                    else
                        ()

                    result.Add(
                        new SvgCubicCurveSegment(
                            false,
                            vecToPoint c.StartControlPoint,
                            vecToPoint c.EndControlPoint,
                            vecToPoint c.EndPoint
                        )
                    )

                    loop (Some c.EndPoint) (CurvePath ps) result

        loop None curvePath (new SvgPathSegmentList())

    type Shape with
        static member toSvgElement shape =
            let svgPath = toSvgPathSegments shape.Path

            let attr = shape.Attributes
            let element = new SvgPath()
            element.PathData <- svgPath

            match attr with
            | None -> ()
            | Some a -> element.SetAttributes(a)

            element

        static member toSvg width height viewBox shape =
            let pathElement = Shape.toSvgElement shape
            let svgDoc = new SvgDocument(ViewBox = viewBox, Width = width, Height = height)
            svgDoc.Children.Add(pathElement)
            svgDoc

        static member toSvgString width height viewBox shape =
            let svgDoc = Shape.toSvg width height viewBox shape
            svgDoc.GetXML()






    let toSvgViewbox (vb:ViewBox) = 
        new SvgViewBox(
            MinX = float32 vb.MinX,
            MinY = float32 vb.MinY,
            Height = float32 vb.Height,
            Width = float32 vb.Width
        )

    type Scene with 
        static member toSvg scene =
            let camera = scene.Camera
            let visibleArea = toSvgViewbox camera.visibleArea
            let parentContent =
                new SvgDocument(
                    ViewBox = visibleArea,
                    Width = SvgUnit(float32 camera.outputWidth),
                    Height = SvgUnit(float32 camera.outputHeight)
                )

            match camera.background with
            | Paint.Transperant -> ()
            | RGB _ ->
                parentContent.Children.Add(
                    new SvgRectangle(
                        Width = SvgUnit(SvgUnitType.Percentage, 100f),
                        Height = SvgUnit(SvgUnitType.Percentage, 100f),
                        X = visibleArea.MinX,
                        Y = visibleArea.MinY,
                        Fill = toSvgColor (camera.background),
                        Stroke = toSvgColor (Paint.Transperant)
                    )
                )
            | _ -> failwith "Not implemented yet"

            if scene.Attributes.IsSome then
                parentContent.SetAttributes(scene.Attributes.Value)

            for shape in scene.Shapes do
                parentContent.Children.Add(Shape.toSvgElement shape)

            parentContent

        static member toSvgString scene =
            let svgDoc = Scene.toSvg scene
            svgDoc.GetXML()

    
// let childrenContent = new SvgGroup()
// for child in scene.Children do
//     childrenContent.Children.Add(Shape.toSvgElement)
//     // match scene.Children with
//     // | [] -> ""
//     // | _ -> // This match is actually unnecessary but I find it clearer
//     //     scene.Children
//     //     |> List.map ChildScene.toSvg
//     //     |> String.concat "\n"


// let backgroundRect =
//     match camera.background with
//     | Attributes.Paint.Transperant -> ""
//     | Attributes.Paint.RGB (a,b,c) ->
//         let xpos = camera.visibleArea.MinX
//         let ypos = camera.visibleArea.MinY
//         let color = camera.background.ToString()
//         let hundretPercent = "100%"
//         $"<rect x=\"{xpos}\" y=\"{ypos}\" width=\"{hundretPercent}\" height=\"{hundretPercent}\" fill=\"{color}\"/>"
//     | _ -> failwith "not implemented yet"

// backgroundRect + "\n" + parentContent + "\n" + childrenContent
// |> intoSvgTag camera.outputWidth camera.outputHeight camera.visibleArea

// interface Frame.IRenderable with
//     member this.Render() =
//         let svg = Scene.toSvg this

//         svgToQoi svg

// and ChildScene =
//     { xpos: float
//       ypos: float
//       width: float
//       height: float
//       Scene: Scene }
//     static member toSvg child =
//         let visibleArea = child.Scene.Camera.visibleArea

//         child.Scene.Shapes
//         |> Seq.map Shape.shapeToSvgElement
//         |> String.concat "\n"
//         |> intoGroupTag child.Scene.Attributes
//         |> intoSvgTagWithPos child.xpos child.ypos child.width child.height visibleArea
//     static member toSvg child =
//         match child.Scene with
//         | Scene s ->
//             let visibleArea = s.Camera.visibleArea

//             s.Shapes
//             |> Seq.map Shape.shapeToSvgElement
//             |> String.concat "\n"
//             |> intoGroupTag s.Attributes
//             |> intoSvgTagWithPos child.xpos child.ypos child.width child.height visibleArea
//         | _ -> failwith "a"


// and SceneTree =
//     | Scene of Scene
//     | WithChildren of parent: Scene * list<ChildScene>
//     static member toSvg st =
//         match st with
//         | Scene s -> Scene.toSvg s
//         | WithChildren (parent, []) -> Scene.toSvg parent
//         | WithChildren (parent, children) ->
//             let parentCamera = parent.Camera

//             let parentContent =
//                 parent.Shapes
//                 |> Seq.map Shape.shapeToSvgElement
//                 |> String.concat "\n"
//                 |> intoGroupTag parent.Attributes

//             let childrenContent =
//                 children
//                 |> List.map ChildScene.toSvg
//                 |> String.concat "\n"

//             parentContent + "\n" + childrenContent
//             |> intoSvgTag parentCamera.outputWidth parentCamera.outputHeight parentCamera.visibleArea

//     interface Frame.IRenderable with
//         member this.Render() =
//             match this with
//             | Scene s -> (s :> Frame.IRenderable).Render()
//             | WithChildren (parent, []) -> (parent :> Frame.IRenderable).Render()
//             | WithChildren (parent, children) ->
//                 let parentCamera = parent.Camera

//                 let parentContent =
//                     parent.Shapes
//                     |> Seq.map Shape.shapeToSvgElement
//                     |> String.concat "\n"
//                     |> intoGroupTag parent.Attributes

//                 let childrenContent =
//                     children
//                     |> List.map ChildScene.toSvg
//                     |> String.concat "\n"

//                 parentContent + "\n" + childrenContent
//                 |> intoSvgTag parentCamera.outputWidth parentCamera.outputHeight parentCamera.visibleArea
//                 |> svgToQoi

namespace Curve

module SvgRenderer =
    open Renderer
    open ImageMagick

    type Viewbox =
        { xmin: float
          ymin: float
          width: float
          height: float }
        override this.ToString() =
            $"{this.xmin} {this.ymin} {this.width} {this.height}"


    type Camera2D =
        { visibleArea: Viewbox
          // Position
          x: float
          y: float
          z: float
          outputHeight: float
          outputWidth: float }

    let toPathString (pathable: IPathable<Vec2>) : string =
        let path = pathable.ToPath()

        let rec loop (currentPoint: Vec2 option) (CurvePath remainingPath) result =
            match remainingPath with
            | [] -> result
            | p :: ps ->
                match p with
                | Close -> loop currentPoint (CurvePath ps) (result + " Z")
                | Curve { StartPoint = Vec2 (a, b)
                          StartControlPoint = Vec2 (c, d)
                          EndControlPoint = Vec2 (e, f)
                          EndPoint = Vec2 (g, h) } ->
                    let newResult =
                        if Some(Vec2(a, b)) <> currentPoint then
                            result + $" M{a} {b} C{c} {d} {e} {f} {g} {h}"
                        else
                            result + $" C{c} {d} {e} {f} {g} {h}"

                    loop (Some(Vec2(g, h))) (CurvePath ps) newResult

        loop None path ""

    let intoPathTag attributes content =
        match attributes with
        | None -> $"""<path d="{content}"/>"""
        | Some attributes -> $"""<path {attributes} d="{content}"/>"""

    let intoGroupTag attributes content =
        match attributes with
        | None -> $"""<g>""" + content + "</g>"
        | Some attributes -> $"""<g {attributes}>""" + content + "</g>"

    let intoSvgTag width height viewBox content =
        $"""<svg xmlns="http://www.w3.org/2000/svg" transform="scale(1 -1)" width="{width}" height="{height}" viewBox="{viewBox}">"""
        + content
        + "</svg>"

    let intoSvgTagWithPos xpos ypos width height viewBox content =
        $"""<svg xmlns="http://www.w3.org/2000/svg" transform="scale(1 -1)" x="{xpos}" y="{ypos}" width="{width}" height="{height}" viewBox="{viewBox}">"""
        + content
        + "</svg>"

    type Shape =
        { Path: IPathable<Vec2>
          Attributes: Attributes.DrawAttributes option
          Transformation: Transformation } // not implemented yet
        static member shapeToSvgElement shape =
            intoPathTag shape.Attributes (toPathString shape.Path)

        static member toSvg width height viewBox shape =
            shape
            |> Shape.shapeToSvgElement
            |> intoSvgTag width height viewBox


    let svgToQoi (svg: string) =
        let image =
            new MagickImage(System.Text.Encoding.ASCII.GetBytes(svg))

        image.Format <- MagickFormat.Qoi
        image

    type Scene =
        { Shapes: Shape seq
          Attributes: Attributes.DrawAttributes option
          Camera: Camera2D
          Children: ChildScene list } // not implemented yet
        static member toSvg scene =
            let camera = scene.Camera

            let parentContent =
                scene.Shapes
                |> Seq.map Shape.shapeToSvgElement
                |> String.concat "\n"
                |> intoGroupTag scene.Attributes

            let childrenContent =
                match scene.Children with
                | [] -> ""
                | _ -> // This match is actually unnecessary but I find it clearer
                    scene.Children
                    |> List.map ChildScene.toSvg
                    |> String.concat "\n"

            parentContent + "\n" + childrenContent
            |> intoSvgTag camera.outputWidth camera.outputHeight camera.visibleArea

        interface Frame.IRenderable with
            member this.Render() =
                let svg = Scene.toSvg this

                svgToQoi svg

    and ChildScene =
        { xpos: float
          ypos: float
          width: float
          height: float
          Scene: Scene }
        static member toSvg child =
            let visibleArea = child.Scene.Camera.visibleArea

            child.Scene.Shapes
            |> Seq.map Shape.shapeToSvgElement
            |> String.concat "\n"
            |> intoGroupTag child.Scene.Attributes
            |> intoSvgTagWithPos child.xpos child.ypos child.width child.height visibleArea
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

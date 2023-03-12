namespace Geometry

open System
open Curve 

module Line = 
    
    type Line =
        {
            Start : Vec2
            End : Vec2 
        }
        interface IPathable<Vec2> with 
            member this.ToPath () = CurvePath [Curve {StartPoint = this.Start; StartControlPoint = this.Start; EndControlPoint = this.End; EndPoint = this.End}]
        static member toPath (line:Line) = (line :> IPathable<Vec2>).ToPath() 

    let partial (t:float) line = 
        {line with End = line.Start + (t .* (line.End - line.Start))}
    
    let ofTuple (a,b) = {Start = a; End = b}
    
    

module Arc = 
    type Arc = 
        {
            Center : Vec2
            Rx : float 
            Ry : float 
            StartAngle : float 
            EndAngle : float 
            Rotation : float
        }
        member this.RotationMatrix = 
            Mat2(Vec2(cos this.Rotation, -sin this.Rotation), Vec2(sin this.Rotation, cos this.Rotation))
        
        member this.SkewMatrix = 
            Mat2(Vec2(this.Rx, 0), Vec2(0, this.Ry))

        member this.StartPoint:Vec2 = 
            this.RotationMatrix .* Vec2(this.Rx * cos this.StartAngle, this.Ry * sin this.StartAngle) + this.Center
        
        member this.EndPoint :Vec2 = 
            this.RotationMatrix .* Vec2(this.Rx * cos this.EndAngle, this.Ry * sin this.EndAngle) + this.Center

        member this.ArcFlag  = 
            abs ((this.EndAngle % System.Math.Tau) - (this.StartAngle % System.Math.Tau)) > System.Math.PI 
        
        member this.SweepFlag  = 
            (this.EndAngle % System.Math.Tau) - (this.StartAngle % System.Math.Tau) > 0

        member private this.SingleCurveApproximation() =
            let unitCircleApproximation =
                let phi = this.EndAngle - this.StartAngle
                let cosphi = cos(phi)
                let sinphi = sin(phi)
                // c is chosen such that the cubic bezier curve hits a unit circular arc at the endpoints and the middle
                let c = 4./3. * tan(phi/4.)
                let p0 = this.StartPoint
                let p1 = p0 + (c .* p0.Turn())
                let p3 = this.EndPoint // NOTE p3 not p2 
                let p2= p3 + (c .* (-p3.Turn()))
                Curve.ofTuple (p0, p1, p2, p3)
            this.RotationMatrix .* ( this.SkewMatrix .* unitCircleApproximation)

        member this.ToPath(num_segments:int) = 
            let deltaPhi = this.EndAngle - this.StartAngle
            let step = deltaPhi/float(num_segments)
            let mutable currentAngle = this.StartAngle
            [   
                for i in [1..num_segments] do
                    yield Curve (
                        {this with StartAngle = currentAngle; EndAngle = currentAngle + step}.SingleCurveApproximation()
                    )
                    currentAngle <- currentAngle + step
            ] |> CurvePath 
            

        interface IPathable<Vec2> with 
            member this.ToPath() = 
                let deltaPhi = this.EndAngle - this.StartAngle
                // We add segments because of longer angles and bigger ellipticity
                let pieces_from_angle = (5. * deltaPhi/Math.Tau) |> ceil |> int
                let pieces_from_ellipticity = (max (this.Rx/this.Ry) (this.Ry/this.Rx))/10. |> ceil |> int

                this.ToPath(pieces_from_angle * pieces_from_ellipticity)

        static member toPath num_segments (arc:Arc) = arc.ToPath(num_segments)

        
    let partial (t:float) arc = 
        {arc with EndAngle = arc.StartAngle + (t * (arc.EndAngle - arc.StartAngle))}


    


    













    















import Graphics.Element (Element)
import Graphics.Collage (..)
import Window
import Signal (..)
import Mouse
import Color (..)
import List

update : (Int,Int) -> List (Int,Int) -> List (Int,Int) 
update mouse balls = List.map (updateOne mouse) balls

updateOne : (Int,Int) -> (Int,Int) -> (Int,Int)
updateOne (mx,my) (x,y) =
  let d = dist (mx,my) (x,y)
  in if d > 10000
  then (x,y)
  else newPos (mx,my) (x,y)

dist : (Int,Int) -> (Int,Int) -> Int
dist (a,b) (c,d) = (a-c)^2 + (b-d)^2

newPos : (Int,Int) -> (Int,Int) -> (Int,Int)
newPos (mx,my) (x,y) =
  let d = 10000 / (dist (mx,my) (x,y) |> toFloat)
      x' = mx + round ((x - mx |> toFloat) * d)
      y' = my + round ((y - my |> toFloat) * d)
  in (x',y')


initial : List (Int,Int)
initial = 
  let points = List.map (\x -> x*100) [-5 .. 5]
  in cartesian points points

cartesian : List Int -> List Int -> List (Int,Int)
cartesian l1 l2 = List.concatMap (\a -> (List.map (\b -> (a,b)) l2)) l1

input : Signal (Int,Int)
input = map2 adjust Mouse.position Window.dimensions

adjust : (Int,Int) -> (Int,Int) -> (Int,Int)
adjust (cX,cY) (dX,dY) = (cX-dX//2,dY//2-cY)

state : Signal (List (Int,Int))
state = foldp update initial input

render : (Int,Int) -> List (Int,Int) -> Element
render (w',h') points =
  let (w,h) = (toFloat w', toFloat h')
  in List.map draw points |> collage w' h'

draw : (Int,Int) -> Form
draw (x,y) = circle 20 |> filled red |> move (toFloat x, toFloat y)

main : Signal Element
main = render <~ Window.dimensions ~ state

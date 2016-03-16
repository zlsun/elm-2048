
import Basics exposing (..)
import List exposing (..)
import String
import Maybe
import Random
import Debug

import Signal exposing ((<~))
import Keyboard
import Task

import Graphics.Element exposing (..)
import Html exposing (..)
import Html.Attributes exposing (..)

import StartApp
import Effects

-- Helpers

infixr 9 ?
(?) = flip Maybe.withDefault

infixl 1 <-:
infixl 1 :-
(<-:) = map
(:-) = map2 (<|)

infixl 1 :->
infixl 1 :-#
infixl 1 :-!
infixl 1 :-?
infixl 1 :-<
infixl 1 :->>
infixl 1 :-!!
infixl 1 :-??
(:->) = flip map
(:-#) = flip filter
(:-!) = flip all
(:-?) = flip any
(:-<) = flip String.join
(:->>) = flip matrixMap
(:-!!) l f = l :-> all f |> all identity
(:-??) l f = l :-> any f |> any identity

matrixMap f matrix = map (map f) matrix

infixl 0 =>
(=>) = (,)

infixl 9 !
(!) l n = unsafeHead <| drop n l

unsafeLast : List a -> a
unsafeLast xs = List.drop (List.length xs - 1) xs |> unsafeHead

unsafeHead : List a -> a
unsafeHead xs = case xs of
  (x::_) -> x
  _ -> Debug.crash "unsafeHead with empty list"

unsafeTail : List a -> List a
unsafeTail xs = case xs of
  (_::ys) -> ys
  _ -> Debug.crash "unsafeTail with empty list"

unsafeMaybe : Maybe a -> a
unsafeMaybe x = case x of
  Just y -> y
  _ -> Debug.crash "unsafeMaybe with Nothing"

transpose matrix =
    case matrix of
        ((x :: xs) :: xss) -> (x :: (map unsafeHead xss)) :: transpose (xs :: (map unsafeTail xss))
        otherwise          -> []

rotate = transpose >> map reverse

pcons xy xys =
    let (x, y) = xy
        (xs, ys) = xys
    in (x :: xs, y + ys)

sumBy f zero l =
    case l of
        (x :: xs) -> f x <| sumBy f zero xs
        []  -> zero

combine line =
    case line of
        (x :: y :: xs) -> if | x == y     -> (x * 2, x * 2) `pcons` combine xs
                             | otherwise  -> (x    , 0    ) `pcons` combine (y :: xs)
        x              -> (x, 0)

padding line = line ++ List.repeat (4 - length line) 0

mergeRowLeft line =
    let (newLine, score) = line :-# (\x -> x /= 0) |> combine
    in (newLine |> padding, score)

rotateFor n = if n == 0 then identity else rotate >> rotateFor (n - 1)

merge action grid =
    if [Nop, Init] :-? (\a -> a == action) then (grid, 0)
    else
        let rotateTime = case action of
                Left  -> 0
                Down  -> 1
                Right -> 2
                Up    -> 3
            rotated = rotateFor rotateTime grid
            (merged, score) = rotated :-> mergeRowLeft |> sumBy pcons ([], 0)
            newGrid = rotateFor (4 - rotateTime) merged
        in (newGrid, score)

replaceNth n v l =
    case l of
        (x :: xs) -> if n == 0 then (v :: xs) else (x :: replaceNth (n - 1) v xs)
        []        -> []

setCell grid x y v = replaceNth y (replaceNth x v (grid ! y)) grid

addCell grid seed =
    let posGen          = Random.pair (Random.int 0 3) (Random.int 0 3)
        ((x, y), seed') = Random.generate posGen seed
        (k, seed'')     = Random.generate (Random.int 1 10) seed'
    in if grid ! y ! x > 0
       then addCell grid seed''
       else (setCell grid x y (if k == 1 then 4 else 2), seed'')

fail grid = [Up, Down, Left, Right] :-! \d -> grid == (fst <| merge d grid)

won grid = grid :-?? (\x -> x == 128)

-- Model

type Action = Nop   | Init  | Up     | Down  | Left  | Right
type State  = Start | Play  | Failed | Won
type alias Grid = List (List Int)
type alias Model  =
    { grid : Grid
    , score : Int
    , state : State
    , seed : Random.Seed
    , lastAction : Action
    }

grid = [[0, 0, 0, 0], [0, 0, 0, 0], [0, 0, 0, 0], [0, 0, 0, 0]]
init = ({ grid = grid
        , score = 0
        , state = Start
        , seed = Random.initialSeed 0
        , lastAction = Nop
        }, Effects.none)

-- View

divText t = div [] [ text t ]

color kind str = kind ++ "(" ++ str ++ ")"

rgba r g b a = color "rgba" <| [r, g, b, a] :-> toString :-< ", "
rgb  r g b   = color "rgb"  <| [r, g, b   ] :-> toString :-< ", "

px n = toString n ++ "px"

bgColor n =
    case n of
        0    -> rgba 238 228 218 0.35
        2    -> rgb  238 228 218
        4    -> rgb  237 224 200
        8    -> rgb  242 177 121
        16   -> rgb  245 149  99
        32   -> rgb  246 124  95
        64   -> rgb  246 94   59
        128  -> rgb  237 207 114
        256  -> rgb  237 204  97
        512  -> rgb  237 200  80
        1024 -> rgb  237 197  63
        2048 -> rgb  237 194  46
        _    -> rgb  237 194  46

textSize n = px <|
    case n of
        128       -> 45
        256       -> 45
        512       -> 45
        1024      -> 35
        2048      -> 35
        otherwise -> 55

textColor n = if n >= 8 then (rgb 249 246 242) else (rgb 119 110 101)

cellStyle n = style
    [ "width"            => px 100
    , "height"           => px 100
    , "text-align"       => "center"
    , "background-color" => bgColor n
    , "font-family"      => "Helvetica Neue,Arial,sans-serif"
    , "font-weight"      => "bold"
    , "font-size"        => textSize n
    , "color"            => textColor n
    ]

emptyIfZero n = if n == 0 then "" else toString n

cell n = td [cellStyle n] [n |> emptyIfZero |> text]

displayScore score = divText <| "Score: " ++ toString score

displayState state = divText <|
    case state of
        Start  -> "Press space to start"
        Play   -> "Playing"
        Failed -> "You failed"
        Won    -> "You won"

displayAction = toString >> divText

displayBoard grid = grid :->> cell :-> tr [] |> table []

view address model = div []
    [ displayScore  model.score
    , displayState  model.state
    , displayAction model.lastAction
    , displayBoard  model.grid
    ]

-- Update

waitInit action model =
    if action /= Init
    then ({ model | lastAction <- action }, Effects.none)
    else
        let grid           = (fst init).grid
            (added, seed') = grid |> flip addCell model.seed
        in ({ model | grid <- added
                    , state <- Play
                    , seed <- seed'
                    , lastAction <- action
            }, Effects.none)

stepGame action model =
    let grid            = model.grid
        (merged, score) = grid |> merge action
        (added, seed')  = if grid == merged
                          then (merged, model.seed)
                          else merged |> flip addCell model.seed
        state           = if fail model.grid then Failed else
                          if won  model.grid then Won    else Play
    in ({ model | grid       <- added
                , state      <- state
                , seed       <- seed'
                , score      <- model.score + score
                , lastAction <- action
        }, Effects.none)

update action model =
    let step =
        case model.state of
            Start  -> waitInit
            Play   -> stepGame
            Failed -> waitInit
            Won    -> waitInit
    in step action model

-- Inputs

arrowsToAction arrow =
    if | arrow.y ==  1 -> Up
       | arrow.y == -1 -> Down
       | arrow.x == -1 -> Left
       | arrow.x ==  1 -> Right
       | otherwise     -> Nop

control = Signal.merge Keyboard.arrows Keyboard.wasd

inputs = [ arrowsToAction <~ control
         , (\x -> if x then Init else Nop) <~ Keyboard.space ]

-- App

app = StartApp.start { init = init, view = view, update = update, inputs = inputs }
main = app.html
port tasks : Signal (Task.Task Effects.Never ())
port tasks = app.tasks

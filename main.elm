import Signal
import Graphics.Element exposing (..)
import Graphics.Input exposing (button)
import Graphics.Collage exposing (collage, toForm)
import List exposing (..)
import Color
import Text
import Random
import Window
import Task exposing (Task)

type alias Model =
  { seed : Random.Seed
  , bingoTiles : List String
  , windowWidth : Int
  , windowHeight : Int
  }

type Action = NoOp
            | Shuffle
            | UpdateWindowSize (Int, Int)

randomFold : String -> (List (String, Float), Random.Seed) -> (List (String, Float), Random.Seed)
randomFold value (l, s) =
  let randomValue = Random.generate randomGenerator s
      randomFloat = fst randomValue
      seed = snd randomValue
  in
    ((value, randomFloat) :: l, seed)

shuffle : Random.Seed -> List String -> List String
shuffle seed list =
  foldr randomFold ([], seed) list
  |> fst
  |> sortBy snd
  |> map fst

generateRandomSeed : Random.Seed -> Random.Seed
generateRandomSeed seed =
  snd <| Random.generate randomGenerator seed

randomGenerator : Random.Generator Float
randomGenerator =
  Random.float 0 <| toFloat (length initialNumbers)

initialNumbers : List String
initialNumbers =
  [ "Always wants to fight"
  , "Autistic"
  , "Black and purple wardrobe"
  , "Choker collar"
  , "Communist"
  , "Dollmaker games"
  , "Furry"
  , "Gay"
  , "Kiss kiss fall in love"
  , "Linguist"
  , "Loves the moon"
  , "Loves weird animals"
  , "Neon Genesis Evangelion"
  , "Owns tabletop simulator"
  , "Owns thigh high socks"
  , "Plays competetive smash"
  , "Polyamorous"
  , "Programmer"
  , "Scene phase"
  , "Slime"
  , "Tired"
  , "Too much salt"
  , "Wants to be a robot"
  , "Went on /d/"
  , "Would date an alien"
  ]

main =
  Signal.map view model

update : Action -> Model -> Model
update action model =
  case action of
    NoOp ->
      model
    Shuffle ->
      { model
      | bingoTiles = shuffle model.seed model.bingoTiles
      , seed = generateRandomSeed model.seed
      }
    UpdateWindowSize size ->
      { model
      | windowWidth = fst size
      , windowHeight = snd size
      }

model : Signal Model
model =
  Signal.foldp
    update
    { seed = Random.initialSeed 42
    , bingoTiles = initialNumbers
    , windowWidth = 0
    , windowHeight = 0
    }
    actions.signal

slice : List a -> Int -> Int -> List a
slice xs s e = take (e - s + 1) <| drop (s - 1) xs

view : Model -> Element
view model =
  let boxSize = 150
      boxSpacing = 2
      boxSpacer = spacer boxSpacing boxSpacing
      sliceTilesAsText start end =
        slice model.bingoTiles start end
        |> map (centered << Text.height 10 << Text.fromString)
        |> map (container boxSize boxSize middle)
        |> map (color Color.lightGrey)
        |> intersperse boxSpacer
        |> flow right
      middleAtY = middleAt (relative 0.5)
      bingoTiles =
        [ sliceTilesAsText 1 5
        , sliceTilesAsText 6 10
        , sliceTilesAsText 11 15
        , sliceTilesAsText 16 20
        , sliceTilesAsText 21 25
        ]
        |> intersperse boxSpacer
        |> flow down
  in
    collage model.windowWidth model.windowHeight
    <| map toForm
      [ container ((widthOf bingoTiles) + boxSpacing * 2) ((heightOf bingoTiles) + boxSpacing * 2) middle bingoTiles
        |> color Color.black
        |> below (container ((widthOf bingoTiles) + boxSpacing * 2)(button (Signal.message actions.address Shuffle) "Shuffle"))
      ]

actions : Signal.Mailbox Action
actions =
  Signal.mailbox NoOp

port windowSizeUpdate : Signal (Task x ())
port windowSizeUpdate =
  Signal.map UpdateWindowSize Window.dimensions
  |> Signal.map (Signal.send actions.address)

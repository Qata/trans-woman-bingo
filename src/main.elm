import Signal
import Graphics.Element exposing (..)
import Graphics.Input exposing (button)
import Graphics.Collage exposing (collage, toForm)
import List exposing (..)
import Color
import Text
import Random
import Random.Array
import Window
import Array

type alias Model =
  { seed : Random.Seed
  , bingoTiles : List String
  , generator : Random.Generator (List String)
  }

type Action = NoOp
            | Shuffle

randomGenerator : Random.Generator (List String)
randomGenerator =
  initialNumbers
  |> Random.Array.shuffle << Array.fromList
  |> Random.map Array.toList

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
  , "Loves spicy food"
  ]

main =
  Signal.map2 view model Window.dimensions

update : Action -> Model -> Model
update action model =
  case action of
    NoOp ->
      model
    Shuffle ->
      let (tiles, newSeed) = Random.generate model.generator model.seed
      in
        { model
        | bingoTiles = tiles
        , seed = newSeed
        }

initialModel : Model
initialModel =
  { seed = Random.initialSeed 42
  , bingoTiles = initialNumbers
  , generator = randomGenerator
  }

model : Signal Model
model =
  Signal.foldp update initialModel actions.signal

slice : List a -> Int -> Int -> List a
slice xs s e = take (e - s + 1) <| drop (s - 1) xs

view : Model -> (Int, Int) -> Element
view model (windowWidth, windowHeight) =
  let boxSize = (min windowWidth windowHeight) |> toFloat |> flip (/) 6 |> round
      boxSpacing = 2
      boxSpacer = spacer boxSpacing boxSpacing
      shuffleButton = button (Signal.message actions.address Shuffle) "Shuffle"
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
    collage windowWidth windowHeight
    <| map toForm
      [ container ((widthOf bingoTiles) + boxSpacing * 2) ((heightOf bingoTiles) + boxSpacing * 2) middle bingoTiles
        |> color Color.black
        |> below (container ((widthOf bingoTiles) + boxSpacing * 2) ((heightOf shuffleButton) * 2) middle shuffleButton)
      ]

actions : Signal.Mailbox Action
actions =
  Signal.mailbox NoOp

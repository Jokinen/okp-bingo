module Main exposing (..)
import Array exposing (Array)
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing ( onClick )
import Random exposing (Seed, step, int, initialSeed)

-- APP
main : Program Never Model Msg
main =
  Html.program
    { init = ( model, Cmd.none )
    , view = view
    , update = update
    , subscriptions = subscriptions
    }

-- MODEL
type alias Entry =
  {
    id: Int,
    value: String
  }
type alias Cell =
  {
    coordinates: (Int, Int),
    value: Int,
    selected: Bool
  }
type alias Board =
  {
    cells: List Cell,
    availableCellNumbers: Array (Int),
    won: Bool
  }

type alias Model =
  {
    entryRandom: Int,
    cellRandom: Int,
    availableEntries: Array (Int),
    entries: List Entry,
    board: Board,
    loaded: Bool
  }

model : Model
model =
  {
     entryRandom = 0,
     cellRandom = 0,
     availableEntries = Array.initialize 32 (\n -> n),
     entries = [],
     board = {
       cells = [],
       availableCellNumbers = Array.initialize 32 (\n -> n),
       won = False
     },
     loaded = False
  }

defaultModel : Model
defaultModel = model

cells : List (Int, Int)
cells =
  [ (0, 0), (1, 0), (2, 0), (3, 0), (4, 0)
  , (0, 1), (1, 1), (2, 1), (3, 1), (4, 1)
  , (0, 2), (1, 2), (2, 2), (3, 2), (4, 2)
  , (0, 3), (1, 3), (2, 3), (3, 3), (4, 3)
  , (0, 4), (1, 4), (2, 4), (3, 4), (4, 4)
  ]

getCells : Int -> List (Cell)
getCells random = List.map (makeCell random) cells

makeCell : Int -> (Int, Int) -> Cell
makeCell random (x, y) =
  { coordinates = (x, y)
  , value = random
  , selected = False
  }


-- UPDATE
type Msg =
  CallNewNumber
  | MarkCells Int
  | NewEntryRandom Int
  | NewRandomCell (List (Int, Int)) Int
  | ResetBoard
  | GetCells (List (Int, Int))

update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
  case msg of
    CallNewNumber ->
      let
        randomNumber = model.entryRandom
        entriesLeftToChooseFrom = model.availableEntries
        (entryValue, newAvailableEntries) = pickMemberAndRemoveIt randomNumber entriesLeftToChooseFrom
        newEntryList = List.reverse (newEntry entryValue :: (List.reverse model.entries))
      in
        { model
        | entries
          = newEntryList
          , availableEntries = newAvailableEntries
        }
        |> update (MarkCells entryValue)
    MarkCells withValue ->
      let
        newCells = List.map (markSelected withValue) model.board.cells
      in
        ( { model | board =
            { cells = newCells
            , won = hasBingo newCells
            , availableCellNumbers = model.board.availableCellNumbers
            }
          }
        , Random.generate NewEntryRandom (Random.int 0 ((Array.length model.availableEntries) - 1))
        )
    ResetBoard ->
      defaultModel
      |> update (GetCells cells)
    GetCells cells ->
      case List.head cells of
        Just val ->
          let
            randomNumber = model.cellRandom
            cellsLeftToChooseFrom = model.board.availableCellNumbers
            (cellValue, newAvailableCellNumbers) = pickMemberAndRemoveIt randomNumber cellsLeftToChooseFrom
            newBuiltCells = List.append model.board.cells [(makeCell cellValue val)]
            cellsLeftToProcess = Maybe.withDefault [] (List.tail cells)
          in
            ( { model | board =
                { cells = newBuiltCells
                , won = model.board.won
                , availableCellNumbers = newAvailableCellNumbers
                }
                , loaded = True
            }
            , Random.generate (NewRandomCell cellsLeftToProcess) (Random.int 0 ((Array.length newAvailableCellNumbers) - 1))
            )
        Nothing ->
          ( model, Cmd.none )
    NewEntryRandom r ->
      ( { model | entryRandom = r }, Cmd.none )
    NewRandomCell cells r ->
      { model | cellRandom = r }
      |> update (GetCells cells)

removeFromList : Int -> List a -> List a
removeFromList i xs =
  (List.take i xs) ++ (List.drop (i+1) xs)

removeFromArray : Int -> Array a -> Array a
removeFromArray i =
  Array.toList >> removeFromList i >> Array.fromList

pickMemberAndRemoveIt : Int -> Array (Int) -> (Int, Array (Int))
pickMemberAndRemoveIt nth array =
  let
    numberPacked = Array.get nth array
    numberUnpacked = Maybe.withDefault 0 numberPacked
    newArray = removeFromArray nth array
  in
    (numberUnpacked, newArray)

markSelected : Int -> Cell -> Cell
markSelected ifValue cell = { cell | selected = (cell.value == ifValue) || cell.selected  }

newEntry : Int -> Entry
newEntry value = { id = value, value = (toString value) }

resetBoard : Model -> Model
resetBoard model = defaultModel

rotate : List (Cell) -> List (Cell)
rotate list = list

hasBingo : List (Cell) -> Bool
hasBingo cells =
  let
    hasRowBingo = checkRows cells 0
    hasColumnBingo = checkColumns (rotate cells) 0
  in
    hasRowBingo
    || hasColumnBingo

trimIntoSelected : Cell -> Bool
trimIntoSelected cell = cell.selected

checkRows : List (Cell) -> Int -> Bool
checkRows cells depth =
  let
    row = List.filter (\x -> (Tuple.second x.coordinates) == depth) cells
    rowTruths = List.map (\x -> x.selected) row
    rowHasBingo = List.foldr (&&) True rowTruths
  in
    case depth of
      4 ->
        rowHasBingo
      val ->
        rowHasBingo || (checkRows cells (val + 1))

checkColumns : List (Cell) -> Int -> Bool
checkColumns cells depth =
  let
    column = List.filter (\x -> (Tuple.first x.coordinates) == depth) cells
    columnTruths = List.map (\x -> x.selected) column
    columnHasBingo = List.foldr (&&) True columnTruths
  in
    case depth of
      4 ->
        columnHasBingo
      val ->
        columnHasBingo || (checkColumns cells (val + 1))

-- VIEW
-- Html is defined as: elem [ attribs ][ children ]
-- CSS can be applied via class names
view : Model -> Html Msg
view model =
  div [ class "wrapper" ]
    [ div [ class "wrapper-row" ] [ createBoardButton model.loaded ]
    , div [ class "wrapper-row" ] [ callNumberButton model.loaded ]
    , div [ class "wrapper-row" ] [ calledNumbers model.entries ]
    , div [ class "wrapper-row" ] [ bingoBoard model.board ]
    , div [ class "wrapper-row" ] [ won model.board ]
    ]

createBoardButton : Bool -> Html Msg
createBoardButton loaded =
  if not loaded then
    button [ onClick (GetCells cells), class "call-number-button" ] [ text "Create board" ]
  else
    button [ onClick (ResetBoard), class "call-number-button" ] [ text "Reset board" ]

callNumberButton : Bool -> Html Msg
callNumberButton loaded =
  if loaded then
    button [ onClick CallNewNumber, class "call-number-button" ] [ text "Get the next number" ]
  else
    button [ disabled True ] [ text "Get the next number" ]

renderNumber : Entry -> Html a
renderNumber entry =
  text (entry.value ++ ",")

calledNumbers : List (Entry) -> Html a
calledNumbers entryList =
  div [ class "called-numbers" ] (List.map renderNumber entryList)

bingoSquare : Cell -> Html Msg
bingoSquare cell =
  let
    (column, row) = cell.coordinates
  in
    div
      [ classList
        [ ("bingo-square", True)
        , ("selected", cell.selected)
        ]
      ]
      [ (text (toString column)), (text ((toString row) ++ "(" ++ (toString cell.value) ++ ")")) ]

bingoBoard : Board -> Html Msg
bingoBoard board =
  let
    cells = board.cells
    bingoItems = List.map (bingoSquare) cells
  in
    div [ class "bingo-board" ] bingoItems

won : Board -> Html a
won board =
  div [ class "won" ] [ text (if board.won then "Voitit!" else "Et ole vielä voittanut") ]

-- Subscriptions
subscriptions : a -> Sub msg
subscriptions x =
    Sub.none


-- CSS STYLES
styles : { img : List ( String, String ) }
styles =
  {
    img =
      [ ( "width", "33%" )
      , ( "border", "4px solid #337AB7")
      ]
  }

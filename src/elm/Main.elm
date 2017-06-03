module Main exposing (..)
import Array exposing (Array)
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing ( onClick )
import Random exposing (Seed, step, int, initialSeed)

-- component import example
import Components.Hello exposing ( hello )

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
    board: Board
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
     }
  }

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
  | GetCells (List (Int, Int))

update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
  case msg of
    CallNewNumber ->
      let
        randomM = Array.get model.entryRandom model.availableEntries
        random = (Maybe.withDefault 0 randomM)
        newEntryList = List.reverse (newEntry random :: (List.reverse model.entries))
        newAvailableEntries = removeFromArray random model.availableEntries

        mor1 = Debug.log "entries" model.availableEntries
        mor0 = Debug.log "entryRandom" model.entryRandom
        mor2 = Debug.log "random" random
        mor3 = Debug.log "3ntries" newAvailableEntries
        mor4 = Debug.log "-----------" ""
      in
        { model
        | entries
          = newEntryList
          , availableEntries = newAvailableEntries
        }
        |> update (MarkCells random)
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
    GetCells cells ->
      case List.head cells of
        Just val ->
          let
            cellRandomM = Array.get model.cellRandom model.board.availableCellNumbers
            cellRandom = Maybe.withDefault 0 cellRandomM
            newCells = List.append model.board.cells [(makeCell cellRandom val)]
            newAvailableCellNumbers = removeFromArray cellRandom model.board.availableCellNumbers
            tail = Maybe.withDefault [] (List.tail cells)
          in
            ( { model | board =
                { cells = newCells
                , won = model.board.won
                , availableCellNumbers = newAvailableCellNumbers
                }
            }
            , Random.generate (NewRandomCell tail) (Random.int 0 ((Array.length newAvailableCellNumbers) - 1))
            )
        Nothing ->
          ( model, Cmd.none )
    NewEntryRandom r ->
      ( { model | entryRandom = r }, Cmd.none )
    NewRandomCell cells r ->
      { model | cellRandom = r }
      |> update (GetCells cells)

markSelected : Int -> Cell -> Cell
markSelected ifValue cell = { cell | selected = (cell.value == ifValue) || cell.selected  }

newEntry : Int -> Entry
newEntry value = { id = value, value = (toString value) }

hasBingo : List (Cell) -> Bool
hasBingo cells =
  let
    hasRowBingo = checkRows cells 0
    hasColumnBingo = checkColumns cells 0
  in
    hasRowBingo
    || hasColumnBingo
--  || hasDiagonalBingo

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

removeFromArray : Int -> Array a -> Array a
removeFromArray i a =
  let
    index = if i == 0 then i else (i - 1)
    a1 = Array.slice 0 index a
    a2 = Array.slice (index + 1) (Array.length a) a
  in
    Array.append a1 a2

-- VIEW
-- Html is defined as: elem [ attribs ][ children ]
-- CSS can be applied via class names
view : Model -> Html Msg
view model =
  div [ class "wrapper" ]
    [ div [ class "wrapper-row" ] [ createBoardButton () ]
    , div [ class "wrapper-row" ] [ callNumberButton () ]
    , div [ class "wrapper-row" ] [ calledNumbers model.entries ]
    , div [ class "wrapper-row" ] [ bingoBoard model.board ]
    , div [ class "wrapper-row" ] [ won model.board ]
    ]

createBoardButton : () -> Html Msg
createBoardButton () =
  button [ onClick (GetCells cells), class "call-number-button" ] [ text "Create board" ]

callNumberButton : () -> Html Msg
callNumberButton () =
  button [ onClick CallNewNumber, class "call-number-button" ] [ text "Get the next number" ]

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
  div [ class "won" ] [ text (toString board.won) ]

-- Subscriptions
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

module Main exposing (main)

import Browser
import Debug exposing (toString)
import Html exposing (Html, text)
import Html.Attributes exposing (size, style, value)
import Html.Events exposing (onClick)
import Maybe exposing (map)
import Platform.Cmd


type alias Model =
    { sudoku : List Cell }


type alias Cell =
    { value : Maybe Int
    , candidates : List Candidate
    , index : Int
    , col : Int
    , row : Int
    , square : Int
    }


type alias Candidate =
    { value : Int
    , countSquare : Int
    , countRow : Int
    , countCol : Int
    }


hard =
    [ 0
    , 0
    , 0
    , 0
    , 0
    , 0
    , 0
    , 0
    , 0
    , --
      0
    , 0
    , 1
    , 7
    , 0
    , 4
    , 9
    , 0
    , 0
    , --
      0
    , 5
    , 0
    , 0
    , 2
    , 0
    , 0
    , 7
    , 0
    , --
      0
    , 2
    , 0
    , 0
    , 0
    , 7
    , 0
    , 6
    , 0
    , --
      0
    , 0
    , 9
    , 0
    , 0
    , 0
    , 1
    , 0
    , 0
    , --
      0
    , 8
    , 0
    , 4
    , 0
    , 0
    , 0
    , 5
    , 0
    , --
      0
    , 6
    , 0
    , 0
    , 7
    , 0
    , 0
    , 2
    , 0
    , --
      0
    , 0
    , 8
    , 9
    , 0
    , 1
    , 7
    , 0
    , 0
    , --
      0
    , 0
    , 0
    , 0
    , 0
    , 0
    , 0
    , 0
    , 0
    ]


harder =
    [ 9
    , 2
    , 0
    , 0
    , 0
    , 0
    , 0
    , 0
    , 5
    , --
      0
    , 0
    , 3
    , 0
    , 7
    , 8
    , 4
    , 0
    , 0
    , --
      0
    , 0
    , 0
    , 6
    , 0
    , 0
    , 0
    , 0
    , 0
    , --
      0
    , 0
    , 0
    , 1
    , 0
    , 0
    , 9
    , 0
    , 0
    , --
      0
    , 0
    , 6
    , 4
    , 0
    , 0
    , 0
    , 0
    , 0
    , --
      0
    , 3
    , 0
    , 0
    , 9
    , 6
    , 0
    , 2
    , 0
    , --
      7
    , 0
    , 0
    , 0
    , 0
    , 0
    , 0
    , 4
    , 0
    , --
      0
    , 0
    , 8
    , 0
    , 3
    , 9
    , 7
    , 0
    , 0
    , --
      0
    , 0
    , 0
    , 0
    , 1
    , 0
    , 0
    , 0
    , 0
    ]


emptyCell index =
    newCell Nothing (List.map (\v -> Candidate v 0 0 0) [ 1, 2, 3, 4, 5, 6, 7, 8, 9 ]) index


valuedCell v index =
    newCell (Just v) [] index


newCell value candidates index =
    Cell value candidates index (modBy 9 index) (index // 9) (squareIndex index)


main =
    Browser.element { init = init, view = mainView, update = update, subscriptions = subscriptions }


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none


mainView model =
    Html.div []
        [ Html.button [ onClick NextStep ] [ text "Next Step" ]
        , Html.div
            [ style "display" "flex"
            , style "flex-wrap" "wrap"
            , style "border" "2px solid black"
            , style "width" "750px"
            , style "height" "750px"
            ]
            (List.map viewCell model.sudoku)
        ]


hardGame =
    harder
        |> List.indexedMap
            (\i ->
                \v ->
                    if v == 0 then
                        emptyCell i

                    else
                        valuedCell v i
            )


type Msg
    = NextStep


init : () -> ( Model, Cmd Msg )
init _ =
    ( Model hardGame, Cmd.none )


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        NextStep ->
            ( { model | sudoku = solveSudoku model.sudoku }, Cmd.none )


solveSudoku cells =
    cells
        |> placeCandidates
        |> removeCandidatesByRow
        |> removeCandidatesByColumn
        |> removeCandidatesBySquare
        |> updateCandidatesCount


updateCandidatesCount cells =
    cells
        |> List.map (\c -> { c | candidates = List.map (\cand -> { cand | countSquare = countCandidatesPerSquare c.square cells cand.value }) c.candidates })
        |> List.map (\c -> { c | candidates = List.map (\cand -> { cand | countRow = countCandidatesPerRow c.row cells cand.value }) c.candidates })
        |> List.map (\c -> { c | candidates = List.map (\cand -> { cand | countCol = countCandidatesPerCol c.col cells cand.value }) c.candidates })


countCandidatesPerRow : Int -> List Cell -> Int -> Int
countCandidatesPerRow row cells value =
    cells
        |> List.filter (\c -> c.row == row)
        |> List.map (\c -> c.candidates)
        |> List.foldl (++) []
        |> List.map (\c -> c.value)
        |> List.filter (\v -> v == value)
        |> List.length


countCandidatesPerCol : Int -> List Cell -> Int -> Int
countCandidatesPerCol col cells value =
    cells
        |> List.filter (\c -> c.col == col)
        |> List.map (\c -> c.candidates)
        |> List.foldl (++) []
        |> List.map (\c -> c.value)
        |> List.filter (\v -> v == value)
        |> List.length


countCandidatesPerSquare : Int -> List Cell -> Int -> Int
countCandidatesPerSquare square cells value =
    cells
        |> List.filter (\c -> c.square == square)
        |> List.map (\c -> c.candidates)
        |> List.foldl (++) []
        |> List.map (\c -> c.value)
        |> List.filter (\v -> v == value)
        |> List.length


placeCandidates cells =
    cells
        |> List.map
            (\c ->
                if List.length c.candidates == 1 then
                    placeCandidate (List.head c.candidates |> Maybe.withDefault (Candidate 0 0 0 0) |> .value |> Just) c

                else
                    placeSingleCandidate c
            )


placeCandidate : Maybe Int -> Cell -> Cell
placeCandidate candidateValue cell =
    { cell
        | candidates = []
        , value = candidateValue
    }


placeSingleCandidate cell =
    let
        squareCandidate =
            cell.candidates |> List.filter (\c -> c.countSquare == 1) |> List.head

        rowCandidate =
            cell.candidates |> List.filter (\c -> c.countRow == 1) |> List.head

        colCandidate =
            cell.candidates |> List.filter (\c -> c.countCol == 1) |> List.head
    in
    case squareCandidate of
        Nothing ->
            case rowCandidate of
                Nothing ->
                    case colCandidate of
                        Nothing ->
                            cell

                        Just candidate ->
                            placeCandidate (Just candidate.value) cell

                Just candidate ->
                    placeCandidate (Just candidate.value) cell

        Just candidate ->
            placeCandidate (Just candidate.value) cell


removeCandidatesBySquare : List Cell -> List Cell
removeCandidatesBySquare cells =
    let
        grouped =
            cells |> cellBySquare
    in
    grouped
        |> List.map (\( square, cell ) -> { cell | candidates = removeAll (solvedInGroup square grouped) cell.candidates })


cellBySquare cells =
    cells |> List.map (\c -> ( c.square, c ))


squareIndex i =
    modBy 3 (i // 3) + ((i // 27) * 3)


removeCandidatesByColumn : List Cell -> List Cell
removeCandidatesByColumn cells =
    let
        cellByColumn =
            cells |> List.map (\c -> ( c.col, c ))
    in
    cellByColumn
        |> List.map (\( col, cell ) -> { cell | candidates = removeAll (solvedInGroup col cellByColumn) cell.candidates })


removeCandidatesByRow : List Cell -> List Cell
removeCandidatesByRow cells =
    let
        cellByRow =
            cells |> List.map (\c -> ( c.row, c ))
    in
    cellByRow
        |> List.map (\( row, cell ) -> { cell | candidates = removeAll (solvedInGroup row cellByRow) cell.candidates })


solvedInGroup group cellByGroup =
    cellByGroup
        |> List.filter (\( g, c ) -> g == group)
        |> List.map (\( r, c ) -> c.value |> Maybe.withDefault 0)
        |> List.filter (\v -> v > 0)


removeAll : List Int -> List Candidate -> List Candidate
removeAll toRemove fromList =
    fromList
        |> List.filter (\value -> not (List.member value.value toRemove))


viewCell : Cell -> Html Msg
viewCell cell =
    Html.div
        [ style "display" "flex"
        , style "min-width" "10.97%"
        , style "border-bottom" "1px solid black"
        , style "border-left" "1px solid black"
        , style "min-height" "10.97%"
        , style "flex-flow" "row wrap"
        , style "flex-direction" "column"
        ]
        [ cell.candidates |> List.take 5 |> viewCandidates True
        , Html.div [ style "flex" "4" ] [ viewCellValue cell ]
        , cell.candidates |> List.drop 5 |> viewCandidates False
        ]


viewCandidates withColor cells =
    let
        color =
            if withColor && List.length cells == 1 then
                "lightgreen"

            else if withColor && List.length cells == 2 then
                "orange"

            else
                "transparent"
    in
    Html.div
        [ style "flex" "1"
        , style "display" "flex"
        , style "justify-content" "space-evenly"
        , style "background-color" color
        ]
        (List.map viewPossibleValue cells)


viewCellValue cell =
    case cell.value of
        Just x ->
            Html.input
                [ style "border" "0"
                , style "background-color" "lightblue"
                , style "font-size" "30px"
                , style "text-align" "center"
                , style "width" "100%"
                , style "height" "100%"
                , size 1
                , value (toString x)
                ]
                []

        Nothing ->
            Html.input
                [ style "border" "0"
                , style "background-color" "lightblue"
                , style "font-size" "30px"
                , style "text-align" "center"
                , style "width" "100%"
                , style "height" "100%"
                , size 1
                ]
                []


viewPossibleValue value =
    let
        color =
            if value.countSquare == 1 then
                "green"

            else
                "lightgray"
    in
    Html.div
        [ style "flex" "1 100%"
        , style "border" "1px solid lightgray"
        , style "text-align" "center"
        , style "font-size" "8px"
        , style "color" color
        ]
        [ text (toString value.value), text ", ", text (toString value.countSquare) ]

import Html exposing (div, input, span, text, img)
import Html.Attributes exposing (placeholder, src, style, rel, href, autofocus, class, value)
import Html.Events exposing (onInput, onClick)

type alias Model =
  { containedSugarInG : String
  , contentInMl : String
  }

type Message
  = InputSugarWeight String
  | InputContentWeight String
  | ChangeContentTo String

main = Html.program
  { init = init
  , view = view
  , update = update
  , subscriptions = \_ -> Sub.none
  }

init =
  ( Model "" "", Cmd.none )

view model =
  div []
    [  Html.node "link" [ rel "stylesheet", href "style.css" ] []
    , input [ onInput InputSugarWeight, placeholder "Sugar in g per 100ml", autofocus True ] []
    , input [ onInput InputContentWeight, placeholder "ml", value model.contentInMl ] []
    , div [ class "contentButtons" ]
        [ div [ onClick <| ChangeContentTo "200" ]
            [ img [ src "glass.svg", style [("width", "3rem")] ] []
            , div [ class "label" ] [ text "200ml" ]
            ]
        , div [ onClick <| ChangeContentTo "330" ]
            [ img [ src "cup.svg", style [("width", "4rem")] ] []
            , div [ class "label" ] [ text "330ml" ]
            ]
        , div [ onClick <| ChangeContentTo "500" ]
            [ img [ src "bottle.svg", style [("width", "5rem")] ] []
            , div [ class "label" ] [ text "500ml" ]
            ]
        ]
    , renderCubes model
    ]

renderCubes : Model -> Html.Html Message
renderCubes model =
  case transformAndCalculate model of
    Ok cubes ->
      div [ class "cubes" ]
        [ div []
            [ img [ src "sugarcube.svg", style [("width", "5em")] ] []
            ]
        , div [ class "cubecount" ] [ text <| toString cubes ]
        , div [] [ text "cubes" ] 
        ]
    Err _ ->
      span [] []

transformAndCalculate model =
  let
    sugarAsNumber = String.toFloat model.containedSugarInG
    contentAsNumber = String.toFloat model.contentInMl
  in
    Result.map2 calculateCubes sugarAsNumber contentAsNumber

calculateCubes sugar content =
  content / 100.0 * sugar / 4.0

update : Message -> Model -> ( Model, Cmd Message )
update message model =
  case message of
    InputSugarWeight weight ->
      ( { model | containedSugarInG = weight }, Cmd.none )
    InputContentWeight weight ->
      ( { model | contentInMl = weight }, Cmd.none )
    ChangeContentTo content ->
      ( { model | contentInMl = content }, Cmd.none )

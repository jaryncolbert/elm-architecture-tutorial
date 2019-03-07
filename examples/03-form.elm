import Browser
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onInput)



-- MAIN


main =
  Browser.sandbox { init = init, update = update, view = view }



-- MODEL


type alias Model =
  { name : String
  , password : String
  , passwordAgain : String
  }


init : Model
init =
  Model "" "" ""



-- UPDATE


type Msg
  = Name String
  | Password String
  | PasswordAgain String

update : Msg -> Model -> Model
update msg model =
  case msg of
    Name name ->
      { model | name = name }

    Password password ->
      { model | password = password }

    PasswordAgain password ->
      { model | passwordAgain = password }



-- VIEW


view : Model -> Html Msg
view model =
  div []
    [ viewInput "text" "Name" model.name Name
    , viewInput "password" "Password" model.password Password
    , viewInput "password" "Re-enter Password" model.passwordAgain PasswordAgain
    ]


viewInput : String -> String -> String -> (String -> msg) -> Html msg
viewInput t p v toMsg =
  input [ type_ t, placeholder p, value v, onInput toMsg ] []


viewValidation : Model -> Html msg
viewValidation model =
  if model.password == model.passwordAgain && has8Digits model.password then
    div [ style "color" "green" ] [ text "OK" ]
  else if not (has8Digits model.password) then
    div [ style "color" "red" ] [ text "Passwords must be at least 8 characters!" ]
  else if not (hasUpperLowerNumeric model.password) then
    div [ style "color" "red" ] [ text "Passwords must contain at least one uppercase, lowercase, and numeric character!" ]
  else
    div [ style "color" "red" ] [ text "Passwords do not match!" ]

stringSatisfies : (Char -> Bool) -> String -> Bool
stringSatisfies fn input =
  List.any fn (String.toList input)

hasUpperLowerNumeric : String -> Bool
hasUpperLowerNumeric input =
    stringSatisfies Char.isUpper input &&
    stringSatisfies Char.isLower input &&
    stringSatisfies Char.isDigit input

has8Digits : String -> Bool
has8Digits input =
    String.length input >= 8
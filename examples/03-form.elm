import Browser
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onInput, onClick)



-- MAIN


main =
  Browser.sandbox { init = init, update = update, view = view }



-- MODEL

type alias ValidationResult =
    { color: String
    , message: String
    }

type alias Model =
  { name : String
  , password : String
  , passwordAgain : String
  , validationResult : ValidationResult
  }


init : Model
init =
  Model "" "" "" { color = "", message = "" }



-- UPDATE


type Msg
  = Name String
  | Password String
  | PasswordAgain String
  | Submit { color: String, message: String }


update : Msg -> Model -> Model
update msg model =
  case msg of
    Name name ->
      { model | name = name }

    Password password ->
      { model | password = password, validationResult = clearValidationMsg }

    PasswordAgain password ->
      { model | passwordAgain = password, validationResult = clearValidationMsg }

    Submit result ->
      { model | validationResult = result }


clearValidationMsg: ValidationResult
clearValidationMsg = { color = "", message = "" }


validateInput : Model -> Msg
validateInput model =
  if model.password == model.passwordAgain && has8Digits model.password && hasUpperLowerNumeric model.password then
    Submit { color = "green", message = "OK" }
  else if not (has8Digits model.password) then
    Submit { color = "red", message = "Passwords must be at least 8 characters!" }
  else if not (hasUpperLowerNumeric model.password) then
    Submit { color = "red", message = "Passwords must contain at least one uppercase, lowercase, and numeric character!" }
  else
    Submit { color = "red", message = "Passwords do not match!" }


-- VIEW


view : Model -> Html Msg
view model =
  div []
    [ viewInput "text" "Name" model.name Name
    , viewInput "password" "Password" model.password Password
    , viewInput "password" "Re-enter Password" model.passwordAgain PasswordAgain
    , button [ onClick (validateInput model) ] [ text "Submit" ]
    , viewValidation model.validationResult
    ]


viewInput : String -> String -> String -> (String -> msg) -> Html msg
viewInput t p v toMsg =
  input [ type_ t, placeholder p, value v, onInput toMsg ] []


viewValidation : ValidationResult -> Html msg
viewValidation result =
  div [ style "color" result.color ] [ text result.message ]


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
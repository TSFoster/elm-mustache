module Mustache exposing (Template, parser)

import Parser exposing ((|.), (|=), Parser, Step(..), spaces, symbol)
import Set
import Tuple2
import Tuple3


type Template
    = Template (List Element)


type alias Model =
    ( Delimeters, List Element )


type alias Delimeters =
    ( Delimeter, Delimeter )


type alias Delimeter =
    String


type Element
    = Static String
    | Tag Tag


type Tag
    = SingleTag SingleTag
    | Section SectionRule String (List Element)
    | SetDelimeters Delimeters -- {{=begin end=}} (kept for re-encoding)


type SingleTag
    = Variable EscapeMethod String
    | Partial String -- {{> partial }}
    | Comment String -- {{! ignore }} (kept for re-encoding)


type EscapeMethod
    = Escape -- {{ thing }}
    | Unescape -- {{& thing }}
    | Raw -- {{{ thing }}}


type SectionRule
    = IsTrueOrForEach -- {{# thing }} {{/ thing }}
    | IsFalseOrIsEmpty -- {{^ thing }} {{/ thing }}
    | IsPresent -- {{# thing? }} {{/ thing? }}



-- PARSER


parser : Parser Template
parser =
    Parser.loop ( defaultDelimeters, [] ) parserLoop
        |> Parser.map Tuple.second
        |> Parser.map Template


defaultDelimeters : Delimeters
defaultDelimeters =
    ( "{{", "}}" )



--- MAIN PARSER LOOP


parserLoop : Model -> Parser (Step Model Model)
parserLoop ( delimeters, elements ) =
    Parser.map (withElement elements) <|
        Parser.oneOf
            [ delimeters |> tagParser |> Parser.map (Tuple.mapSecond Tag)
            , delimeters |> staticParser |> Parser.map Static |> Parser.map (Tuple.pair delimeters)
            ]


withElement : List Element -> ( Delimeters, Element ) -> Step Model Model
withElement elements ( delimeters, element ) =
    case element of
        Static "" ->
            Done ( defaultDelimeters, List.reverse elements )

        _ ->
            Loop ( delimeters, element :: elements )



--- STATIC PARSER


staticParser : Delimeters -> Parser String
staticParser ( begin, _ ) =
    Parser.chompUntilEndOr begin
        |> Parser.getChompedString



--- TAG PARSER


tagParser : Delimeters -> Parser ( Delimeters, Tag )
tagParser delimeters =
    Parser.oneOf
        [ delimeters |> singleTagParser |> Parser.map SingleTag |> Parser.map (Tuple.pair delimeters)
        , delimeters |> setDelimetersParser |> Parser.map Tuple2.double |> Parser.map (Tuple.mapSecond SetDelimeters)
        , delimeters |> sectionParser |> Parser.map (Tuple.mapSecond (Tuple3.uncurry Section))
        ]


variableTagParser : Delimeters -> Parser String
variableTagParser delimeters =
    makeTagParser delimeters spaces variableNameParser


variableNameParser : Parser String
variableNameParser =
    -- The spec  does not define what  a valid variable  name is, so I  assume these
    -- rules, which are common rules to many languages.
    Parser.variable
        { start = \char -> char == '_' || Char.isAlpha char
        , inner = \char -> char == '_' || Char.isAlphaNum char
        , reserved = Set.empty
        }


makeTagParser : Delimeters -> Parser () -> Parser String -> Parser String
makeTagParser ( begin, end ) before stringParser =
    Parser.succeed identity
        |. symbol begin
        |. before
        |= stringParser
        |. spaces
        |. symbol end



---- SINGLE TAG PARSER


singleTagParser : Delimeters -> Parser SingleTag
singleTagParser delimeters =
    Parser.oneOf
        [ delimeters |> variableParser |> Parser.map (Tuple2.uncurry Variable)
        , delimeters |> partialParser |> Parser.map Partial
        , delimeters |> commentParser |> Parser.map Comment
        ]


variableParser : Delimeters -> Parser ( EscapeMethod, String )
variableParser delimeters =
    Parser.oneOf
        [ delimeters |> escapeVariableParser |> Parser.map (Tuple.pair Escape)
        , delimeters |> unescapeVariableParser |> Parser.map (Tuple.pair Unescape)
        , delimeters |> rawVariableParser |> Parser.map (Tuple.pair Raw)
        ]


escapeVariableParser : Delimeters -> Parser String
escapeVariableParser =
    variableTagParser


unescapeVariableParser : Delimeters -> Parser String
unescapeVariableParser ( begin, end ) =
    variableTagParser ( begin ++ "&", end )


rawVariableParser : Delimeters -> Parser String
rawVariableParser ( begin, end ) =
    variableTagParser ( begin ++ "{", "}" ++ end )


partialParser : Delimeters -> Parser String
partialParser ( begin, end ) =
    variableTagParser ( begin ++ ">", end )


commentParser : Delimeters -> Parser String
commentParser ( begin, end ) =
    Parser.chompUntil end
        |> Parser.getChompedString
        |> makeTagParser ( begin ++ "!", end ) (Parser.succeed ())



---- SET DELIMETER PARSER


setDelimetersParser : Delimeters -> Parser Delimeters
setDelimetersParser ( begin, end ) =
    Parser.succeed Tuple.pair
        |. symbol begin
        |= notWhitespaceOrEquals
        |. spaces
        |= notWhitespaceOrEquals
        |. symbol end


notWhitespaceOrEquals : Parser String
notWhitespaceOrEquals =
    Parser.chompWhile isNotWhitespaceOrEquals
        |> Parser.getChompedString


isNotWhitespaceOrEquals : Char -> Bool
isNotWhitespaceOrEquals char =
    List.member char [ ' ', '\u{000D}', '\n', 't', '=' ]



---- SECTION PARSER


sectionParser : Delimeters -> Parser ( Delimeters, ( SectionRule, String, List Element ) )
sectionParser delimeters =
    Parser.map toSection <|
        Parser.oneOf
            [ delimeters |> isTrueOrForEachParser |> Parser.map (Tuple.pair IsTrueOrForEach)
            , delimeters |> isFalseOrIsEmptyParser |> Parser.map (Tuple.pair IsFalseOrIsEmpty)
            , delimeters |> isPresentParser |> Parser.map (Tuple.pair IsPresent)
            ]


toSection : ( SectionRule, ( String, Model ) ) -> ( Delimeters, ( SectionRule, String, List Element ) )
toSection ( rule, ( name, ( delimeters, elements ) ) ) =
    ( delimeters, ( rule, name, elements ) )


isTrueOrForEachParser : Delimeters -> Parser ( String, Model )
isTrueOrForEachParser ( begin, end ) =
    variableTagParser ( begin ++ "#", end )
        |> Parser.andThen (innerSectionParser ( begin, end ))


isFalseOrIsEmptyParser : Delimeters -> Parser ( String, Model )
isFalseOrIsEmptyParser ( begin, end ) =
    variableTagParser ( begin ++ "^", end )
        |> Parser.andThen (innerSectionParser ( begin, end ))


isPresentParser : Delimeters -> Parser ( String, Model )
isPresentParser ( begin, end ) =
    makeTagParser ( begin ++ "#", end ) spaces (variableNameParser |. symbol "?")
        |> Parser.andThen (innerSectionParser ( begin, end ))


innerSectionParser : Delimeters -> String -> Parser ( String, Model )
innerSectionParser delimeters variableName =
    Parser.loop ( delimeters, [] ) parserLoop
        |> Parser.andThen (endSectionParser variableName)


endSectionParser : String -> Model -> Parser ( String, Model )
endSectionParser variableName ( ( begin, end ), elements ) =
    Parser.succeed ( variableName, ( ( begin, end ), elements ) )
        |. makeTagParser ( begin ++ "/", end ) spaces (Parser.succeed "" |. symbol variableName)

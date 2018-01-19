module Inflexio.Pluralize
    exposing
        ( singularize
        , pluralize
        )

import Regex exposing (Regex, regex)
import Set exposing (Set)


uncountable : Set String
uncountable =
    Set.fromList
        [ "aircraft"
        , "bellows"
        , "bison"
        , "deer"
        , "equipment"
        , "fish"
        , "hovercraft"
        , "information"
        , "jeans"
        , "means"
        , "measles"
        , "money"
        , "moose"
        , "news"
        , "pants"
        , "police"
        , "rice"
        , "series"
        , "sheep"
        , "spacecraft"
        , "species"
        , "swine"
        , "tights"
        , "tongs"
        , "trousers"
        ]


firstCaptureAppending : String -> Regex.Match -> String
firstCaptureAppending suffix match =
    case match.submatches of
        (Just captured) :: tl ->
            captured ++ suffix

        _ ->
            suffix


patternAndSuffix : String -> String -> ( Regex.Regex, Regex.Match -> String )
patternAndSuffix pattern suffix =
    ( pattern |> regex |> Regex.caseInsensitive, firstCaptureAppending suffix )


patternAndSuffixOneOrTwo : String -> String -> ( Regex.Regex, Regex.Match -> String )
patternAndSuffixOneOrTwo pattern suffix =
    let
        extractFromMatch : Regex.Match -> String
        extractFromMatch match =
            case match.submatches of
                Nothing :: (Just captured) :: tl ->
                   captured ++ suffix
                
                (Just captured) :: Nothing :: tl ->
                   captured ++ suffix

                _ ->
                    suffix
    in
        ( pattern |> regex |> Regex.caseInsensitive, extractFromMatch )


pattern : String -> ( Regex.Regex, Regex.Match -> String )
pattern pattern =
    patternAndSuffix pattern ""


singular : List ( Regex, Regex.Match -> String )
singular =
    [ patternAndSuffix "(alumn|cact|fung|radi|stimul|syllab)i" "us"
    , patternAndSuffix "(alg|antenn|amoeb|larv|vertebr)ae" "a"
    , patternAndSuffix "^(gen)era$" "us"
    , patternAndSuffix "(pe)ople" "rson"
    , pattern "^(zombie)s$"
    , patternAndSuffix "(g)eese" "oose"
    , patternAndSuffix "(criteri)a" "on"
    , patternAndSuffix "^(m)en$" "an"
    , pattern "^(echo)es"
    , pattern "^(hero)es"
    , pattern "^(potato)es"
    , pattern "^(tomato)es"
    , patternAndSuffix "^(t)eeth" "ooth"
    , patternAndSuffix "^(l)ice$" "ouse"
    , patternAndSuffix "^(addend|bacteri|curricul|dat|memorand|quant)a$" "um"
    , patternAndSuffix "^(di)ce" "e"
    , patternAndSuffix "^(f)eet" "oot"
    , patternAndSuffix "^(phenomen)a" "on"
    , pattern "(child)ren"
    , patternAndSuffix "(wo|sea)men" "man"
    , patternAndSuffix "(m|l)ice" "ouse"
    , pattern "(bus|canvas|status|alias)(es)"
    , pattern "(ss)$"
    , pattern "(database)s$"
    , pattern "([ti])a$"
    , patternAndSuffix "((a)naly|(b)a|(d)iagno|(p)arenthe|(p)rogno|(s)ynop|(t)he)(sis|ses)$" "sis"
    , patternAndSuffix "(analy)(sis|ses)$" "sis"
    , patternAndSuffix "(octop|vir)i$" "us"
    , pattern "(hive)s$"
    , pattern "(tive)s$"
    , patternAndSuffix "([lora])ves$" "f"
    , patternAndSuffix "([^f])ves$" "fe"
    , patternAndSuffix "([^aeiouy]|qu)ies$" "y"
    , patternAndSuffix "(m)ovies$" "ovie"
    , pattern "(x|ch|ss|sh)es$"
    , pattern "(shoe)s$"
    , pattern "(o)es$"
    , pattern "()(s)$"
    ]


plural : List ( Regex, Regex.Match -> String )
plural =
    [ patternAndSuffix "(alumn|cact|fung|radi|stimul|syllab)us" "i"
    , patternAndSuffix "(alg|antenn|amoeb|larv|vertebr)a" "ae"
    , patternAndSuffix "^(gen)us$" "era"
    , patternAndSuffix "(pe)rson" "ople"
    , pattern "^(zombie)s$"
    , patternAndSuffix "(g)oose" "eese"
    , patternAndSuffix "(criteri)on" "a"
    , pattern "^(men)$"
    , pattern "^(women)"
    , patternAndSuffix "^(echo)" "es"
    , patternAndSuffix "^(hero)" "es"
    , patternAndSuffix "^(potato)" "es"
    , patternAndSuffix "^(tomato)" "es"
    , patternAndSuffix "^(t)ooth" "eeth"
    , patternAndSuffix "^(l)ouse" "ice"
    , patternAndSuffix "^(addend|bacteri|curricul|dat|memorand|quant)um$" "a"
    , patternAndSuffix "^(di)e" "ce"
    , patternAndSuffix "^(f)oot" "eet"
    , patternAndSuffix "^(phenomen)on" "a"
    , patternAndSuffix "(child)$" "ren"
    , patternAndSuffix "(m)an$" "en"
    , patternAndSuffix "(m|l)ouse" "ice"
    , patternAndSuffix "(database)s$" ""
    , patternAndSuffix "(quiz)$" "zes"
    , patternAndSuffix "^(ox)$" "en"
    , patternAndSuffix "(matr|vert|ind)ix|ex$" "ices"
    , patternAndSuffix "(x|ch|ss|sh)$" "es"
    , patternAndSuffix "([^aeiouy]|qu)y$" "ies"
    , patternAndSuffix "(hive)$" "s"
    , patternAndSuffix "(sc[au]rf)$" "s"
    , patternAndSuffixOneOrTwo "(?:([^f])fe|((hoo)|([lra]))f)$" "ves"
    , patternAndSuffix "sis$" "ses"
    , patternAndSuffix "([ti])um$" "a"
    , patternAndSuffix "(buffal|tomat)o$" "oes"
    , patternAndSuffix "(octop|vir)us$" "i"
    , patternAndSuffix "(bus|alias|status|canvas)$" "es"
    , patternAndSuffix "(ax|test)is$" "es"
    , patternAndSuffix "s$" "s"
    , patternAndSuffix "data$" "data"
    , patternAndSuffix "()$" "s"
    ]


find_match : List ( Regex, Regex.Match -> String ) -> String -> String
find_match regexes word =
    if Set.member word uncountable then
        word
    else
        replace_match regexes word


replace_match : List ( Regex, Regex.Match -> String ) -> String -> String
replace_match regexes word =
    let
        test regex word =
            Regex.contains regex word

        findRegex word =
            List.filter (\( regex, _ ) -> test regex word) regexes
                |> List.head

        regexAndReplacer =
            findRegex word
    in
        case regexAndReplacer of
            Just ( regex, replacer ) ->
                Regex.replace Regex.All regex replacer word

            Nothing ->
                word


singularize : String -> String
singularize word =
    find_match singular word


pluralize : String -> String
pluralize word =
    find_match plural word

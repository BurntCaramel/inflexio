module PluralizeTest exposing (..)

import Expect exposing (Expectation)
import Test exposing (..)
import Inflexio.Pluralize exposing (singularize, pluralize)


suite : Test
suite =
    describe "Inflexio.Pluralize."
        [ describe "singularize"
            [ test "common plurals" <|
                \() ->
                    [ "users", "dogs", "cats", "rabbits", "birds", "owls" ]
                        |> List.map singularize
                        |> Expect.equal
                            [ "user", "dog", "cat", "rabbit", "bird", "owl" ]
            , test "interesting plurals" <|
                \() ->
                    [ "children", "women", "seamen", "mice", "lice", "buses", "canvases", "statuses", "aliases", "databases", "synopses", "theses", "analyses" ]
                        |> List.map singularize
                        |> Expect.equal
                            [ "child", "woman", "seaman", "mouse", "louse", "bus", "canvas", "status", "alias", "database", "synopsis", "thesis", "analysis" ]
            , test "special case nouns ending in f or fe" <|
                \() ->
                    [ "lives", "knives", "hooves", "roofs", "loofs", "wolves", "calves", "scarfs", "scurfs", "wharves", "loaves", "leaves" ]
                        |> List.map singularize
                        |> Expect.equal
                            [ "life", "knife", "hoof", "roof", "loof", "wolf", "calf", "scarf", "scurf", "wharf", "loaf", "leaf" ]
            , test "special case nouns ending in o" <|
                \() ->
                    [ "echoes", "heroes", "potatoes", "tomatoes" ]
                        |> List.map singularize
                        |> Expect.equal
                            [ "echo", "hero", "potato", "tomato" ]
            , test "special case misc" <|
                \() ->
                    [ "teeth", "sheep", "mice", "children", "women", "seamen", "mice", "lice", "buses", "canvases", "statuses", "aliases", "databases", "synopses", "theses", "analyses", "addenda", "algae", "alumni", "amoebae", "antennae", "bacteria", "cacti", "curricula", "data", "fungi", "genera", "larvae", "memoranda", "menus", "stimuli", "syllabi", "vertebrae", "buses", "databases", "shoes", "zombies", "viri", "geese", "criteria", "radii", "classes", "women", "seamen", "men", "abdomens", "abdomen", "specimens", "specimen", "manager", "managers", "People" ]
                        |> List.map singularize
                        |> Expect.equal
                            [ "tooth", "sheep", "mouse", "child", "woman", "seaman", "mouse", "louse", "bus", "canvas", "status", "alias", "database", "synopsis", "thesis", "analysis", "addendum", "alga", "alumnus", "amoeba", "antenna", "bacterium", "cactus", "curriculum", "datum", "fungus", "genus", "larva", "memorandum", "menu", "stimulus", "syllabus", "vertebra", "bus", "database", "shoe", "zombie", "virus", "goose", "criterion", "radius", "class", "woman", "seaman", "man", "abdomen", "abdomen", "specimen", "specimen", "manager", "manager", "Person" ]
            ]
        , describe "pluralize"
            [ test "common plurals" <|
                \() ->
                    [ "user", "dog", "cat", "rabbit", "bird", "owl" ]
                        |> List.map pluralize
                        |> Expect.equal
                            [ "users", "dogs", "cats", "rabbits", "birds", "owls" ]
            , test "special case nouns ending in f or fe" <|
                \() ->
                    [ "life", "knife", "hoof", "roof", "loof", "wolf", "calf", "scarf", "scurf", "wharf", "loaf", "leaf" ]
                        |> List.map pluralize
                        |> Expect.equal
                            [ "lives", "knives", "hooves", "roofs", "loofs", "wolves", "calves", "scarfs", "scurfs", "wharves", "loaves", "leaves" ]
            , test "special case nouns ending in o" <|
                \() ->
                    [ "echo", "hero", "potato", "tomato" ]
                        |> List.map pluralize
                        |> Expect.equal
                            [ "echoes", "heroes", "potatoes", "tomatoes" ]
            , test "special case misc" <|
                \() ->
                    [ "tooth", "sheep", "mouse", "child", "woman", "seaman", "mouse", "louse", "bus", "canvas", "status", "alias", "database", "synopsis", "thesis", "analysis", "addendum", "alga", "alumnus", "amoeba", "antenna", "bacterium", "cactus", "curriculum", "datum", "fungus", "genus", "larva", "memorandum", "menu", "stimulus", "syllabus", "vertebra", "bus", "database", "shoe", "zombie", "virus", "goose", "criterion", "radius", "class", "woman", "seaman", "man", "abdomen", "abdomens", "specimen", "specimens", "manager", "managers", "Person" ]
                        |> List.map pluralize
                        |> Expect.equal
                            [ "teeth", "sheep", "mice", "children", "women", "seamen", "mice", "lice", "buses", "canvases", "statuses", "aliases", "databases", "synopses", "theses", "analyses", "addenda", "algae", "alumni", "amoebae", "antennae", "bacteria", "cacti", "curricula", "data", "fungi", "genera", "larvae", "memoranda", "menus", "stimuli", "syllabi", "vertebrae", "buses", "databases", "shoes", "zombies", "viri", "geese", "criteria", "radii", "classes", "women", "seamen", "men", "abdomens", "abdomens", "specimens", "specimens", "managers", "managers", "People" ]
            ]
        ]

-- | Haskell language pragma
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module View (
  viewModel
  ) where

-- | Miso framework import
import Miso hiding (on)
import Miso.String (MisoString, ms)

import Model

import Data.Char (toUpper)
import qualified Data.Map as M
import Data.Bool (bool)
import Data.Function (on)
import Data.List (sortBy)

-- | Constructs a virtual DOM from a model
viewModel :: Model -> View Action
viewModel g@Model{..} = div_ [ class_ "mainDiv sredina" ] [
  section_ [] [
    img_ [ src_ "./logo-320.png" ]
  , case state of -- MANUAL ROUTING
    MainMenu  -> viewMainMenu g

    -- BasicRules    -> viewBasicRules g
    -- AliasRules    -> viewAliasRules g
    -- CharadesRules -> viewCharadesRules g
    -- OneWordRules  -> viewOneWordRules g

    Settings  -> viewSettings g -- TODO dodati polja za unos

    TeamInput -> viewTeamInput g inputField

    SynInput  -> viewSynInput g inputField numberField

    RoundPrep -> viewRoundPrep g

    Gameplay  -> viewGameplay g

    TimeOut   -> viewTimeOut g

    GameOver  -> viewGameOver g

    _         -> viewRules g

  -- , text "-----------------------------------"
  -- , div_ [] [
  --     text (ms (show state))
  --   , ul_ [] $
  --       flip map syntagmas $ \t -> li_ [] [text (ms t)]
  --   ]
    ]
  ]


viewMainMenu :: Model -> View Action
viewMainMenu g@Model{..} =
    ul_ [ class_ "mainMenuList"] [
      li_ [] [ button_ [ class_ "regularButton", onClick ToTeamInput  ] [ text "NOVA IGRA" ] ]
    , li_ [] [ button_ [ class_ "regularButton", onClick ToBasicRules ] [ text "PRAVILA"   ] ]
    , li_ [] [ button_ [ class_ "regularButton", onClick ToSettings   ] [ text "POSTAVKE"  ] ]
    ]


viewRules :: Model -> View Action
viewRules g@Model{..} =
  div_ [] [
    div_ [] [
      button_ [ class_ "rulesButton"
              , onClick ToBasicRules ]
              [ text "OSNOVE" ]
    , button_ [ class_ "rulesButton"
              , onClick ToAliasRules ]
              [ text "ALIAS   " ]
    ]
  , div_ [] [
      button_ [ class_ "rulesButton"
              , onClick ToCharadesRules ]
              [ text "PANTOMIMA" ]
    , button_ [ class_ "rulesButton"
              , onClick ToOneWordRules ]
              [ text "JEDNA RIJEČ" ]
    ]
  , case state of
      BasicRules    -> viewBasicRules
      AliasRules    -> viewAliasRules
      CharadesRules -> viewCharadesRules
      OneWordRules  -> viewOneWordRules
  , div_ [ class_ "bottomButtons" ] [
      button_ [ class_ "regularButton"
              , onClick BackToMainMenu ]
              [ text "POČETAK" ]
      ]
  ]

viewBasicRules :: View Action
viewBasicRules =
  div_ [] [
    p_ [] [ text "Tri su kruga igre:", br_ []
          , text "1. Alias", br_ []
          , text "2. Pantomima", br_ []
          , text "3. Jedna riječ", br_ []
          ]
  , p_ [] [ text "Sintagme se ponavljaju u svakom krugu." ]
  , p_ [] [ text "" ]
  ]

viewAliasRules :: View Action
viewAliasRules =
  div_ [] [
    p_ [] [ text "Sintagmu opisujete riječima." ]
  , p_ [] [ text "Ne smijete koristiti korijene riječi niti strane jezike." ]
  , p_ [] [ text "Za veću težinu igre probajte igrati bez gestikuliranja." ]
  ]

viewCharadesRules :: View Action
viewCharadesRules =
  div_ [] [
    p_ [] [ text "Sintagmu opisujete kretnjom." ]
  , p_ [] [ text "Ne smijete proizvoditi zvukove." ]
  , p_ [] [ text "Za veću težinu igre probajte ne pokazivati prstom na stvari oko sebe." ]
  ]

viewOneWordRules :: View Action
viewOneWordRules =
  div_ [] [
    p_ [] [ text "Sintagmu opisujete samo JEDNOM riječju." ]
  , p_ [] [ text "Ne smijete koristiti korijene riječi niti strane jezike." ]
  , p_ [] [ text "Za veću težinu igre probajte igrati bez gestikuliranja." ]
  ]

viewSettings :: Model -> View Action
viewSettings g@Model{..} =
  div_ [] [
    text "tu treba settingse dodati"
  , div_ [ class_ "bottomButtons" ] [
      button_ [ class_ "regularButton"
              , onClick BackToMainMenu ]
              [ text "POČETAK" ]
      ]
  ]

viewTeamInput :: Model -> MisoString -> View Action
viewTeamInput g@Model{..} teamName =
  div_ [] [
    div_ [] [
      text "Molimo unesite timove.", br_ []
    , text "Minimalno 2 tima!"
    ]
  , div_ [ style_ (M.singleton "visibility" (bool "hidden" "visible" invalidTeamName))
         , class_ "backButton" ] [
      text "Unijeli ste prazno polje ili tim koji već postoji!"
    ]
  , div_ [] [
    input_
      [ placeholder_ "ImeTima"
      , type_ "text"
      , autofocus_ True
      , maxlength_ "15"
      , value_ teamName
      , name_ "newTeamName"
      , onInput UpdateTeamField
      , onEnter AddTeam
      ]
    ]
  , ul_ [] $ flip map (take 4 (teams)) $ \t ->
      li_ [] [ text (ms (fst t)) ]
  , div_ [ class_ "bottomButtons" ] [
      button_ [ class_ "backButton", onClick BackToMainMenu ] [ text "NATRAG" ]
    , button_ [ class_ "forwardButton", onClick ToSynInput ] [ text "DALJE" ]
    ]
  ]

viewSynInput :: Model -> MisoString -> MisoString -> View Action
viewSynInput g@Model{..} syn num =
  div_ [] [
    div_ [] [
        span_ [] [ text "Broj sintagmi: " ]
      , span_ [ style_ (M.singleton "color" (bool "#EC676E" "#18BC9C" (0 /= length syntagmas)))]
              [ text (ms (show (length syntagmas)))]
      , span_ [] [ text " (max 50)"]
      ]
  , div_ [ style_ (M.singleton "visibility" (bool "hidden" "visible" invalidSyntagma))
         , class_ "backButton"]
         [ text "Sintagma treba sadržavati dvije riječi koje sadrže samo slova."
         ]
  , div_ [] [
      text "Unesite sintagmu..."
    ]
  , input_
      [ placeholder_ "Semantička devijantnost"
      , type_ "text"
      , autofocus_ True
      , value_ syn
      , name_ "newSyn"
      , onInput UpdateSynField
      , onEnter AddSyn
      ]
  , br_ [], br_ []
  , div_ [] [
      text "... ili broj koliko ih želite generirati."
      -- text "Ili unesite broj sintagmi koliko želite imati pa će se one automatski generirati i nadodati."
    , br_ []
    -- , text "Za vaše psihičko zdravlje, ograničit ćemo ukupan broj na maksimalno 50." , br_ []
    , input_ [
        type_ "number"
      , defaultValue_ "30"
      , autofocus_ False
      , value_ num
      , min_ "1"
      , max_ "50"
      , name_ "numberToGenerate"
      , onInput UpdateNumberField
      , onEnter GenerateSyntagmas
      ]
    , button_ [ class_ "regularButton", onClick GenerateSyntagmas ] [ text "GENERIRAJ" ]
    ]
  , div_ [ class_ "bottomButtons" ] [
      button_ [ class_ "backButton", onClick ToTeamInput ] [ text "NATRAG" ]
    , button_ [ class_ "forwardButton", onClick ToRoundPrep ] [ text "ZAPOČNI IGRU" ]
    ]
  ]

onEnter :: Action -> Attribute Action
onEnter action =
  onKeyDown $ bool NoOp action . (== KeyCode 13)

viewRoundPrep :: Model -> View Action
viewRoundPrep g@Model{..} =
  div_ [] [
    div_ [] [
        text (ms ("Runda: " ++ show runda)), br_ []
      , displayRoundDescription runda , br_ [], br_ []
      , viewTeamScores teams
      , text (ms ("Tim " ++ map toUpper (fst (teams !! currentTeam)) ++ ", jeste spremni? Imate 60 sekundi")), br_ []
    ]
  , div_ [ class_ "bottomButtons" ] [
      button_ [ class_ "backButton", onClick ToSynInput ] [ text "NATRAG" ]
    , button_ [ autofocus_ True, class_ "forwardButton", onClick ToGameplay ] [ text "SPREMNI!" ]
    ]
  ]

displayRoundDescription :: Int -> View Action
displayRoundDescription 1 = text "ALIAS"
displayRoundDescription 2 = text "PANTOMIMA"
displayRoundDescription 3 = text "JEDNA RIJEČ"
displayRoundDescription _ = text "Još nismo smislili što ćemo za više runde"

viewGameplay :: Model -> View Action
viewGameplay g@Model{..} =
  div_ [] [
      div_ [ class_ "syntagma sredina" ] [
          text (ms (head (words (head (syntagmas))))), br_ []
        , text (ms (last (words (head (syntagmas))))), br_ []
        , text (ms (show time))
      ]
    , div_ [ class_ "bottomButtons" ] [
          button_ [ disabled_ buttonEnabled
                    , class_ "backButton"
                    , onClick SkipWord ]
                  [ text "DALJE!" ]
        , button_ [ disabled_ buttonEnabled
                    , autofocus_ True
                    , class_ "forwardButton"
                    , onClick NextWord ]
                  [ text "POGODAK!" ]
    ]
  ]

viewTimeOut :: Model -> View Action
viewTimeOut g@Model{..} =
  div_ [] [
    div_ [ class_ "sredina" ] [
        text "Vrijeme je isteklo", br_ [], br_ []
      , viewTeamScores teams
      , text (ms ("Tim " ++ map toUpper (fst (teams !! currentTeam)) ++ ", jeste spremni? Imate 60 sekundi")), br_ []
    ]
  , div_ [ class_ "bottomButtons" ] [
      button_ [ autofocus_ True, class_ "regularButton", onClick ToGameplay ] [ text "SPREMNI!" ]
    ]
  ]

viewGameOver :: Model -> View Action
viewGameOver g@Model{..} =
  div_ [] [
    div_ [ class_ "sredina" ] [
        text "Igra je gotova!" , br_ [], br_ []
      , viewTeamScores teams
    ]
  , div_ [ class_ "bottomButtons" ] [
      button_ [ autofocus_ True, class_ "regularButton", onClick ToMainMenu ] [ text "POČETAK"]
    ]
  ]

top5 :: [(String, Int)] -> [(String, Int)]
top5 = take 4 . sortBy (flip compare `on` snd)

viewTeamScores :: [(String, Int)] -> View Action
viewTeamScores teams =
  div_ [] [
    ul_ [] $ flip map (top5 teams) $ \t ->
        li_ [] [ text (ms ((fst t) ++ ": " ++ show (snd t))) ]
    , br_ []
  ]

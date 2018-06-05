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
import Data.Map (singleton)
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

    Rules     -> viewRules g

    Settings  -> viewSettings g -- TODO dodati polja za unos

    TeamInput -> viewTeamInput g inputField

    SynInput  -> viewSynInput g inputField numberField

    RoundPrep -> viewRoundPrep g

    Gameplay  -> viewGameplay g

    TimeOut   -> viewTimeOut g

    GameOver  -> viewGameOver g

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
      li_ [] [ button_ [ class_ "regularButton", onClick ToTeamInput ] [ text "NOVA IGRA" ] ]
    , li_ [] [ button_ [ class_ "regularButton", onClick ToRules ] [ text "PRAVILA" ] ]
    , li_ [] [ button_ [ class_ "regularButton", onClick ToSettings ] [ text "POSTAVKE" ] ]
    ]


viewRules :: Model -> View Action
viewRules _ =
  div_ [] [
    text "tu treba pravila napisati", br_ []
  , button_ [ onClick BackToMainMenu ] [ text "POČETAK" ]
  ]

viewSettings :: Model -> View Action
viewSettings g@Model{..} =
  div_ [] [
    text "tu treba settingse dodati", br_ []
    , button_ [ onClick BackToMainMenu ] [ text "POČETAK" ]
  ]

viewTeamInput :: Model -> MisoString -> View Action
viewTeamInput g@Model{..} teamName =
  div_ [] [
    div_ [] [
      text "Molimo unesite timove.", br_ []
    , text "Minimalno 2 tima!"
    ]
  , div_ [ style_ (singleton "visibility" (bool "hidden" "visible" invalidTeamName))
         , class_ "backButton" ] [
      text "Postoji već tim sa takvim imenom! (ili ste unijeli prazno polje)"
    ]
  , div_ [] [
    input_
      [ placeholder_ "ImeTima"
      , type_ "text"
      , autofocus_ True
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
      , span_ [ style_ (singleton "color" (bool "#EC676E" "#18BC9C" (0 /= length syntagmas)))]
              [ text (ms (show (length syntagmas)))]
      , span_ [] [ text " (max 50)"]
      ]
  , div_ [ style_ (singleton "visibility" (bool "hidden" "visible" invalidSyntagma))
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
      , defaultValue_ "1"
      , autofocus_ False
      , value_ num
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

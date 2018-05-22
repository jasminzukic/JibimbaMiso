-- | Haskell language pragma
-- {-# LANGUAGE BangPatterns #-} -- TODO izbrisati
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

-- | Haskell module declaration
module Main where

-- | Miso framework import
import Miso hiding (on)
import Miso.String (MisoString, ms, fromMisoString)

import Data.Bool
import Data.Char (isLetter)
import Data.Function (on)
import Data.List (sortBy)
import Data.Monoid
import Data.Maybe (fromJust)
import Control.Monad
import Control.Monad.Random.Class (MonadRandom)
import Control.Concurrent
import System.Random
import System.Random.Shuffle

data States = MainMenu
            | TeamInput
            | SynInput
            | RoundPrep
            | Gameplay
            | TimeOut
            | GameOver
            deriving (Eq, Show)

-- | Type synonym for an application model
data Model = Model
  { state :: States
  , seed :: Int
  , inputField :: MisoString
  , numberField :: MisoString
  , invalidSyntagma :: Bool
  , syntagmas :: [String]
  , guessed :: [String]
  , teams :: [(String, Int)]
  , currentTeam :: Int
  , time :: Int
  , runda :: Int
  } deriving (Eq, Show)


initialModel :: Model
initialModel = Model
  { state = MainMenu
  , seed = 1
  , inputField = mempty
  , numberField = "1"
  , invalidSyntagma = False
  , syntagmas = [
      "manjinske supstance" -- TODO izbrisati
    , "dočekani čarobnjaci"
    ]
  , guessed = []
  , teams = [("anata va", 3), ("unko", 7)] -- TODO izbrisati
  , currentTeam = 0
  , time = 0
  , runda = 1 }

-- | Sum type for application events
data Action
  = NextWord
  | ToTeamInput
  | ToSynInput
  | ToRoundPrep
  | ToGameplay
  | ToGameOver
  | ToMainMenu
  | UpdateTeamField MisoString
  | UpdateSynField MisoString
  | UpdateNumberField MisoString
  | AddTeam
  | AddSyn
  | ShuffleTeams
  | Tick !Int
  | NoOp
  | SayHelloWorld
  deriving (Show, Eq)

-- | Entry point for a miso application
main :: IO ()
main = do
  gen <- getStdGen
  let s = fst . random $ gen :: Int
  let m = initialModel { seed = s }
  startApp App { model = m , ..}
  where
    initialAction = SayHelloWorld -- initial action to be executed on application load
    -- model         = m           -- initial model
    update        = updateModel   -- update function
    view          = viewModel     -- view function
    events        = defaultEvents -- default delegated events
    subs          = [  ]            -- empty subscription list -- every 1000000 Tick
    mountPoint    = Nothing       -- mount point for application (Nothing defaults to 'body')


-- every :: Int -> (Double -> action) -> Sub action model
-- every n f _ sink = void . forkIO . forever $ do
--   threadDelay n
--   sink =<< f <$> now


-- | Updates model, optionally introduces side effects
updateModel :: Action -> Model -> Effect Action Model
updateModel ToTeamInput g = noEff g { state = TeamInput }
updateModel ToMainMenu  g = noEff g { state = MainMenu }
updateModel ToSynInput  g@Model{..} =
  noEff g {
    state = if length (teams) >= 2
      then SynInput
      else TeamInput
  , teams = snd $ shuffleTeams seed teams
  , seed = fst $ shuffleTeams seed teams
  }

updateModel ToRoundPrep g@Model{..} =
  noEff g {
    state = RoundPrep
  , syntagmas = snd $ shuffleSyntagmas seed syntagmas
  , seed = fst $ shuffleSyntagmas seed syntagmas
  }


updateModel ToGameplay g@Model{..} =
  noEff g {
    state = Gameplay
  , time = 60
  , syntagmas = snd $ shuffleSyntagmas seed syntagmas
  , seed = fst $ shuffleSyntagmas seed syntagmas
  }

updateModel NextWord g@Model{..} =
  if length syntagmas == 1
    then noEff g { syntagmas = guessed ++ syntagmas
                 , guessed = []
                 , teams = updatedTeams
                 , currentTeam = (currentTeam + 1) `mod` (length teams)
                 , runda = (runda+1)
                 , state = if runda >= 3 then GameOver else RoundPrep
                 }
    else noEff g { syntagmas = newSyns
                 , guessed = newGuessed
                 , teams = updatedTeams
                 }
  where
    newSyns = tail $ syntagmas
    newGuessed = (head (syntagmas)) : guessed
    updatedTeams = updateTeams g

updateModel AddTeam g@Model{..} =
  noEff g {
    teams = (fromMisoString inputField, 0) : teams
  , inputField = mempty
  }

updateModel AddSyn g@Model{..} =
  noEff g {
    syntagmas = if isValid syn then syn:syntagmas else syntagmas
  , inputField = if isValid syn then mempty else inputField
  , invalidSyntagma = not $ isValid syn
  }
  where syn = fromMisoString inputField

updateModel (UpdateTeamField str) g = noEff g { inputField = str }
updateModel (UpdateSynField str) g = noEff g { inputField = str }
updateModel (UpdateNumberField str) g = noEff g { numberField = str }


updateModel (Tick _) g@Model{..} =
  if time == 1
    then noEff g {
      time = 0
    , state = TimeOut
    , currentTeam = (currentTeam + 1) `mod` (length teams)
    }
    else noEff g { time = time - 1 }

updateModel NoOp m = noEff m
updateModel SayHelloWorld m = m <# do
  putStrLn "Hello World" >> pure NoOp


-- | Constructs a virtual DOM from a model
viewModel :: Model -> View Action
viewModel g@Model{..} = div_ [] [
  case state of -- MANUAL ROUTING
    MainMenu -> viewMainMenu g

    TeamInput -> viewTeamInput g inputField

    SynInput -> viewSynInput g inputField numberField

    RoundPrep -> viewRoundPrep g

    Gameplay -> viewGameplay g

    TimeOut -> viewTimeOut g

    GameOver -> viewGameOver g

    -- _ -> div_ [] [ text "Not Implemented yet" ]



  , text "-----------------------------------"
  , div_ [] [
      text (ms (show state))
    , ul_ [] $
        flip map syntagmas $ \t -> li_ [] [text (ms t)]
    ]
  ]


shuffleTeams :: Int -> [(String, Int)] -> (Int, [(String, Int)])
shuffleTeams seed teams = ( fst (random (gen))
                          , shuffle' teams (length teams) gen)
  where
    gen = mkStdGen (seed + l)
    l = sum $ map (length . fst) teams

shuffleSyntagmas :: Int -> [String] -> (Int, [String])
shuffleSyntagmas seed syns = ( fst (random gen)
                             , shuffle' syns (length syns) gen)
  where
    gen = mkStdGen (seed + l)
    l = sum $ map length syns

isValid :: String -> Bool
isValid syn = (length s == 2) && (and $ map (all isLetter) s)
  where s = words syn

updateTeams :: Model -> [(String, Int)]
updateTeams g = (take n teamList) ++ [updatedTeam] ++ (drop (n+1) teamList)
  where
    teamList = teams g
    updatedTeam = (fst currentT, (snd currentT) + 1)
    currentT = teamList !! n
    n = currentTeam g


viewMainMenu :: Model -> View Action
viewMainMenu g@Model{..} =
  ul_ [] [
    button_ [ onClick ToTeamInput ] [ text "NOVA IGRA" ]
  ]

viewTeamInput :: Model -> MisoString -> View Action
viewTeamInput g@Model{..} teamName =
  div_ [] [
    div_ [] [ text "Molimo unesite timove. Potrebno je barem 2 tima za nastaviti dalje"]
  , input_
      [ placeholder_ "ImeTima"
      , autofocus_ True
      , value_ teamName
      , name_ "newTeamName"
      , onInput UpdateTeamField
      , onEnter AddTeam
      ]
  , ul_ [] $ flip map teams $ \t ->
      li_ [] [ text (ms (fst t)) ]
  , button_ [ onClick ToSynInput ] [ text "DALJE" ]
  ]

viewSynInput :: Model -> MisoString -> MisoString -> View Action
viewSynInput g@Model{..} syn num =
  div_ [] [
    div_ [] [
      text (ms ("Broj sintagmi: " ++ show (length syntagmas) ++ " (max 50)")) , br_ [], br_ []
    , text (if invalidSyntagma then "Unesena sintagma je neispravna. Sintagma treba sadržavati dvije riječi koje sadrže samo slova." else "")
    , br_ [], br_ []
    , text "Unesite sintagmu."
    ]
  , input_
      [ placeholder_ "Semantička devijantnost"
      , autofocus_ True
      , value_ syn
      , name_ "newSyn"
      , onInput UpdateSynField
      , onEnter AddSyn
      ]

  , div_ [] [
      text "Ili unesite broj sintagmi koliko želite imati pa će se one automatski generirati i nadodati."
    , br_ []
    -- , text "Za vaše psihičko zdravlje, ograničit ćemo ukupan broj na maksimalno 50." , br_ []
    , input_ [
        type_ "number"
      , defaultValue_ "1"
      , autofocus_ False
      , value_ num
      , name_ "numberToGenerate"
      , onInput UpdateNumberField
      ]
    , button_ [] [ text "GENERIRAJ" ]
    ]
  , div_ [] [
      button_ [ onClick ToRoundPrep ] [ text "ZAPOČNI IGRU" ]
    ]
  ]

onEnter :: Action -> Attribute Action
onEnter action =
  onKeyDown $ bool NoOp action . (== KeyCode 13)

viewRoundPrep :: Model -> View Action
viewRoundPrep g@Model{..} =
  div_ [] [
    text (ms ("Runda: " ++ show runda)), br_ []
  , displayRoundDescription runda , br_ [], br_ []
  , ul_ [] $ flip map (top5 teams) $ \t ->
      li_ [] [ text (ms ((fst t) ++ ": " ++ show (snd t)))]
  , text (ms ("Tim " ++ fst (teams !! currentTeam) ++ ", jeste spremni? Imate 60 sekundi")), br_ []
  , button_ [ onClick ToGameplay ] [ text "SPREMNI!" ]
  ]

displayRoundDescription :: Int -> View Action
displayRoundDescription 1 = text "ALIAS"
displayRoundDescription 2 = text "PANTOMIMA"
displayRoundDescription 3 = text "JEDNA RIJEČ"
displayRoundDescription _ = text "Još nismo smislili što ćemo za više runde"

viewGameplay :: Model -> View Action
viewGameplay g@Model{..} =
  div_ [] [
    text (ms (head (syntagmas))) , br_ [], br_ []
  , text (ms (show time)), br_[], br_ []
  , button_ [ onClick NextWord ] [ text "DALJE!" ]
  ]

viewTimeOut :: Model -> View Action
viewTimeOut g@Model{..} =
  div_ [] [
    text "Vrijeme je isteklo", br_ [], br_ []
  , ul_ [] $ flip map (top5 teams) $ \t ->
      li_ [] [ text (ms ((fst t) ++ ": " ++ show (snd t))) ]
  ]

viewGameOver :: Model -> View Action
viewGameOver g@Model{..} =
  div_ [] [
    text "Igra je gotova!" , br_ [], br_ []
  , ul_ [] $ flip map (top5 teams) $ \t ->
      li_ [] [ text (ms ((fst t) ++ ": " ++ show (snd t))) ]
  , button_ [ onClick ToMainMenu ] [ text "POČETAK"]
  ]

top5 :: [(String, Int)] -> [(String, Int)]
top5 = take 5 . sortBy (flip compare `on` snd)

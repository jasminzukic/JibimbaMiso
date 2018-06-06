{-# LANGUAGE OverloadedStrings #-}

module Model where

import Miso.String (MisoString)

-- | Type synonym for an application model
data Model = Model
  { state :: States
  , inputField :: MisoString
  , numberField :: MisoString
  , invalidTeamName :: Bool
  , invalidSyntagma :: Bool
  , buttonEnabled :: Bool
  , syntagmas :: [String]
  , guessed :: [String]
  , teams :: [(String, Int)]
  , currentTeam :: Int
  , time :: Int
  , runda :: Int
  } deriving (Eq, Show)


data States = MainMenu
            | BasicRules
            | AliasRules
            | CharadesRules
            | OneWordRules
            | Settings
            | TeamInput
            | SynInput
            | RoundPrep
            | Gameplay
            | TimeOut
            | GameOver
            deriving (Eq, Show)

-- | Sum type for application events
data Action
  = ToRules
  | ToBasicRules
  | ToAliasRules
  | ToCharadesRules
  | ToOneWordRules
  | ToSettings
  | ToTeamInput
  | ToSynInput
  | ToRoundPrep
  | ToGameplay
  | ToGameOver
  | ToMainMenu
  | BackToMainMenu

  | UpdateTeamField MisoString
  | UpdateSynField MisoString
  | UpdateNumberField MisoString

  | NextWord
  | SkipWord

  | AddTeam
  | AddSyn
  | GenerateSyntagmas
  | ShuffleTeams [(String, Int)]
  | ShuffleSyns [String]
  | StartCounter
  | Tick
  | NoOp
  deriving (Show, Eq)


initialModel :: Model
initialModel = Model
  { state = MainMenu
  , inputField = mempty
  , numberField = "25"
  , invalidTeamName = False
  , invalidSyntagma = False
  , buttonEnabled = False
  , syntagmas = []
  , guessed = []
  , teams = []
  , currentTeam = 0
  , time = -1
  , runda = 1 }

-- | Haskell language pragma
-- {-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Update (
  updateModel
  ) where

-- | Miso framework import
import Miso hiding (on)
import Miso.String (fromMisoString, ms)

import Model

import Nouns
import CommonSyns
import FpnAttributes
import FsnAttributes
import MpnAttributes
import MsnAttributes
import MsmpAttributes
import NpnAttributes
import NsnAttributes

import Data.Char (isLetter)
import Data.List  (delete)
import Control.Monad (forM_)
import Control.Concurrent (threadDelay)
import System.Random (randomRIO)
import System.Random.Shuffle (shuffleM)


-- | Updates model, optionally introduces side effects
updateModel :: Action -> Model -> Effect Action Model

-- UPDATE state variable
updateModel ToTeamInput g = noEff g {
    state = TeamInput
  , invalidTeamName = False
  }

updateModel ToBasicRules     g = noEff g { state = BasicRules    }
updateModel ToAliasRules     g = noEff g { state = AliasRules    }
updateModel ToCharadesRules g = noEff g { state = CharadesRules }
updateModel ToOneWordRules   g = noEff g { state = OneWordRules  }

updateModel ToSettings g = noEff g { state = Settings }
updateModel ToSynInput  g@Model{..} =
  g {
    state = if length teams >= 2
      then SynInput
      else TeamInput
  } <# pure AddTeam

updateModel ToRoundPrep g@Model{..} =
  g {
    state = if length syntagmas > 0
      then RoundPrep
      else SynInput
  } <# do
    shuff <- shuffleM teams
    return $ ShuffleTeams shuff


updateModel ToGameplay g@Model{..} =
  g {
    state = Gameplay
  , time = timer
  } <# do
    shuff <- shuffleM syntagmas
    return $ ShuffleSyns shuff

updateModel ToMainMenu  g = -- noEff initialModel umjesto ovoga dolje.
  noEff g {                 -- ne treba ako ću dodati settingse
    state = MainMenu
  , runda = 1
  , syntagmas = []
  , teams = []
  , guessed = []
  , currentTeam = 0
  , invalidSyntagma = False
  , invalidTeamName = False
  , inputField = mempty
  , numberField = ms $ show 1
  }

updateModel BackToMainMenu g = noEff g { state = MainMenu }

-- UPDATE nonstate variables

updateModel (ShuffleTeams t) g@Model{..} = noEff g { teams = t }
updateModel (ShuffleSyns  s) g@Model{..} = noEff g { syntagmas = s }

updateModel NextWord g@Model{..} =
  if length syntagmas == 1
    then noEff g { syntagmas = guessed ++ syntagmas
                 , guessed = []
                 , teams = updatedTeams
                 , currentTeam = (currentTeam + 1) `mod` (length teams)
                 , time = 0
                 , runda = (runda+1)
                 , state = if runda >= 3 then GameOver else RoundPrep
                 }
    else noEff g { syntagmas = newSyns
                 , guessed = newGuessed
                 , teams = updatedTeams
                 , buttonEnabled = True
                 }
  where
    newSyns = tail $ syntagmas
    newGuessed = (head (syntagmas)) : guessed
    updatedTeams = updateTeams teams currentTeam 1

updateModel SkipWord g@Model{..} =
  if length syntagmas == 1
    then noEff g
    else noEff g { syntagmas = tail syntagmas ++ [head syntagmas]
                 , teams = updateTeams teams currentTeam (-1)
                 }

updateModel AddTeam g@Model{..} =
  noEff g {
    teams = if isValidTeam team teams then (team, 0):teams else teams
  , inputField = if isValidTeam team teams then mempty else inputField
  , invalidTeamName = not $ isValidTeam team teams
  }
  where team = fromMisoString inputField

updateModel AddSyn g@Model{..} =
  noEff g {
    syntagmas = if isValidSyntagma syn then syn:syntagmas else syntagmas
  , inputField = if isValidSyntagma syn then mempty else inputField
  , invalidSyntagma = not $ isValidSyntagma syn
  }
  where syn = fromMisoString inputField

updateModel GenerateSyntagmas g@Model{..} =
  g <# do
    nouns <- sample realNumber nouns
    syns <- addAttributes nouns
    return $ ShuffleSyns (syntagmas ++ syns)
    where
      number = (read (fromMisoString numberField) :: Int)
      realNumber = if (number + length syntagmas) > 50
        then 50 - length syntagmas
        else if number < 0
          then 0
          else number

updateModel (UpdateTeamField   str) g = noEff g { inputField  = str }
updateModel (UpdateSynField    str) g = noEff g { inputField  = str }
updateModel (UpdateNumberField num) g = noEff g { numberField = num }
updateModel (UpdateTimerField  num) g = noEff g { timerField  = num }

updateModel SetTime g@Model{..} = noEff g { timer = t }
  where
    t' = fromMisoString timerField
    t = if t' < 0
          then 10
          else if t' > 120
            then 120
            else t'

updateModel StartCounter g@Model{..} = effectSub g $ \sink ->
  forM_ [0..] $ \n -> do
    threadDelay 1000000
    sink Tick

updateModel Tick g@Model{..} =
  if time <= timer && time > 1
    then noEff g {
      time = time - 1
    , buttonEnabled = False
    }
    else if time == 1
      then g {
          state = TimeOut
        , time = 0
        , currentTeam = (currentTeam + 1) `mod` (length teams)
        , buttonEnabled = False
        } <# do
          shuff <- shuffleM syntagmas
          return $ ShuffleSyns shuff
      else noEff g { buttonEnabled = False }

updateModel NoOp m = noEff m


isValidSyntagma :: String -> Bool
isValidSyntagma syn = (length s == 2) && (and $ map (all isLetter) s)
  where s = words syn

isValidTeam :: String -> [(String, Int)] -> Bool
isValidTeam team teams = not (null team) && (team, 0) `notElem` teams

updateTeams :: [(String,Int)] -> Int -> Int -> [(String, Int)]
updateTeams teamList idx n = (take idx teamList) ++ [updatedTeam] ++ (drop (idx+1) teamList)
  where
    updatedTeam = (fst currentT, (snd currentT) + n)
    currentT = teamList !! idx


addAttributes :: [String] -> IO [String]
addAttributes nouns = mapM newAttribute nouns

newAttribute :: String -> IO String
newAttribute x = do
  attr <- sample1 (getAttributes x)
  let syn = attr ++ " " ++ exactWord x
  if syn `elem` commonSyns
    then newAttribute x
    else return syn
  where
    wordType x = (words x) !! 1
    exactWord x = head $ words x

getAttributes :: String -> [String]
getAttributes x
  | wordType x == "np" = npnAttributes ++ fsnAttributes
  | wordType x == "ms" = msmpAttributes ++ msnAttributes
  | wordType x == "mp" = mpnAttributes ++ msmpAttributes
  | wordType x == "ns" = nsnAttributes
  | wordType x == "fs" = fsnAttributes
  | otherwise          = fpnAttributes
  where
    wordType x = (words x) !! 1

sample1 :: [a] -> IO a
sample1 xs = do
  let l = length xs - 1
  idx <- randomRIO (0, l)
  return $ xs !! idx

sample :: Eq a => Int -> [a] -> IO [a]
sample 0 xs = return []
sample n xs = do
  let l = min n (length xs)
  val <- sample1 xs
  (:) <$> (pure val) <*> (sample (l-1) (delete val xs))

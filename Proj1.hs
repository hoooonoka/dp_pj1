-- author: Zhouhui Wu
-- username: zhouhuiw
-- email: zhouhuiw@student.unimelb.edu.au
-- This file contains code for a game, 'Musician'. In this game, one player
-- acts as a composer, who chooses a three-pitch musical chord first. The 
-- other player acts as a performer, who try to guess the correct chord. 
-- Each turn, the performer guesses a chord and the composer gives a feedback.
-- This process is performed iteratively. When the performer finds the 
-- correct chord, the game finishs.
-- For this program, we complete several functions for the game, including a
-- feedback function to generate feedback, a guess function to make a guess 
-- based on current game state ... We also design several data structure for
-- this game, including a Pitch structure to represent a pitch and a GameState
-- to represent a game state ...

module Proj1 (Pitch, toPitch, feedback, GameState, initialGuess, nextGuess)
 where

import Data.List

---------------------------------------------
--------- section 1. data structure ---------
---------------------------------------------

-- In this section, we define the data structure used in the project,
-- including the Pitch, Note, Octave and GameState, several helper functions
-- for are also defined here

-- description: data structure Pitch consists 2 components: Note and Octave,
--              represnets the pitch
data Pitch = Pitch Note Octave deriving Eq


-- description: data strucutre Note consists 7 values, from A to G, represents
--              the note
data Note = A | B | C | D | E | F | G deriving (Eq, Show)


-- description: data structure Octave consists 3 values, One, Two and Three,
--              represents the octave
data Octave = One | Two | Three deriving Eq


-- description: data structure GameState, consists a list of list of Pitch,
--              denoting the left candidates as the game process goes
-- note: for the game state, we store all combinations of 3-length pitches
--       (such as [A1,B1,C1]) still legal in current step, these combinations
--       are defined ascandidates, which is used in the below comment
data GameState = GameState [[Pitch]] deriving Show


getCandidate :: GameState -> [[Pitch]]
-- description: get candidates set from a game state
getCandidate (GameState candidates) = candidates


getNote :: Pitch -> Note
-- description: get Note from a Pitch
getNote (Pitch note octave) = note


getOctave :: Pitch -> Octave
-- description: get Octave from a Pitch
getOctave (Pitch note octave) = octave


---------------------------------------------
--------- section 2. show functions ---------
---------------------------------------------

-- In this section, we define the show functions for above data structures,
-- including Pitch and Octave

instance Show Pitch where show = showPitch


showOctave :: Octave -> String
-- description: show function for Octave structure
showOctave One = "1"
showOctave Two = "2"
showOctave Three = "3"


showPitch :: Pitch -> String
-- description: show function for Pitch structure
showPitch (Pitch note octave) = (show note) ++ (showOctave octave)


------------------------------------------------------------------
--------- section 3. to note, octave and pitch functions ---------
------------------------------------------------------------------

-- In this section, we define the toPitch function, which take a String 
-- type parameter as input and return a Just Pitch if it is legal and Nothing
-- if it is illegal. Several helper functions are also defined


toNote :: Char -> Note
-- description: transfer a Char (denoting the Note) to the corresponding Note
toNote 'A' = A
toNote 'B' = B
toNote 'C' = C
toNote 'D' = D
toNote 'E' = E
toNote 'F' = F
toNote 'G' = G


toOctave :: Char -> Octave
-- description: transfer a Char (denoting the Octave) to the corresponding 
--              Octave
toOctave '1' = One
toOctave '2' = Two
toOctave '3' = Three


isNote :: Char -> Bool
-- description: check if a Char (denoting the Note) is legal representation
isNote 'A' = True
isNote 'B' = True
isNote 'C' = True
isNote 'D' = True
isNote 'E' = True
isNote 'F' = True
isNote 'G' = True
isNote _ =False


isOctave :: Char -> Bool
-- description: check if a Char (denoting the Octave) is legal representation
isOctave '1' = True
isOctave '2' = True
isOctave '3' = True
isOctave _ = False


toPitch :: String -> Maybe Pitch
-- description: from a String denoting the Pitch, check if it is legal, 
--              if it is legal, then transfer it to the corresponding Pitch, 
--              or return Nothing
toPitch pitch = 
 let legalLength = 2 in
  if length pitch /= legalLength then Nothing
   else 
    let 
       notePosition = 0
       octavePosition = 1
       noteChar = pitch!!notePosition
       octaveChar = pitch!!octavePosition
       isnote = isNote noteChar
       isoctave = isOctave octaveChar
    in if isnote && isoctave 
     then Just (Pitch (toNote noteChar) (toOctave octaveChar))
     else Nothing


-------------------------------------------------------------------
--------- section 4. compute feedback relevant statistics ---------
-------------------------------------------------------------------

-- In this section, we define the function feedback, which take target and 
-- guess as input and return a tuple of statistics. Several helper functions
-- are also defined here to help with the computation of feedback function


sameNumber :: [Pitch] -> [Pitch] -> (Int, Int)
-- description: from the target pitches and guess pitches, find the correct
--              number of notes and octaves
-- input: parameter 1: a list of Pitch denoting the target;
--        parameter 2: a list of Pitch denoting the guess
-- output: a 2-element tuple consisting the correct number of note and the 
--         correct number of octave
-- note: the intuition behind this function is to compute different note and
--       octave first, and use this to find the correct note and octave.
--       Directly using set intersection operation will not always receive a
--       correct result due to sometimes there will be same note or octave,
--       however, using this apporach, we can guarantee the correctness
sameNumber target guess = 
 let (targetNotes,targetOctaves) = splitPitch target
     (guessNotes,guessOctaves) = splitPitch guess
     diffNotes = targetNotes \\ guessNotes
     diffOctaves = targetOctaves \\ guessOctaves
 in (length target - length diffNotes, length target - length diffOctaves)


splitPitch :: [Pitch] -> ([Note],[Octave])
-- description: split a list of Pitch into a list of Note and a list of Octave
splitPitch [] = ([],[])
splitPitch ((Pitch note octave):pitches) = 
 let (notes,octaves) = splitPitch pitches in (note:notes,octave:octaves)


feedback :: [Pitch] -> [Pitch] -> (Int,Int,Int)
-- description: generate feedback statistics from the guess and target
-- input: parameter 1: the target pitches; parameter 2: the guess pitches
-- output: a 3 element tuple, consisting correct Pitch number, correct Note 
--         number and correct Octave number
-- note: the correct Pitch is computed by doing a set intersection operation
--       the correct Note and Octave are computed by first removing the  
--       correct Pitch,then use the function 'sameNumber', which is defined 
--       above and could compute the same Note and Octave
feedback target guess = 
 let commonPitch = intersect target guess
     targetLeft = target \\ commonPitch
     guessLeft = guess \\ commonPitch
     (correctNote, correctOctave) = sameNumber targetLeft guessLeft
 in (length commonPitch, correctNote, correctOctave)


--------------------------------------------
--------- section 5. initial guess ---------
--------------------------------------------

-- In this section, we define the initialGuess function, which gives the 
-- first guess with no information acquired


initialGuess :: ([Pitch],GameState)
-- description: give the first guess and corresponding game state
-- Note: the guess here is a hardcoded result: A1, B2, C3, this might not 
--       be the best guess which could eliminate the largest number of 
--       candidates, but as it use 3 different Note and Octave, it could 
--       give a good-enough result potentially
initialGuess = 
 let pitches = [Pitch A One, Pitch B Two, Pitch C Three]
     state = GameState fullCandidates
 in (pitches,state)


fullCandidates :: [[Pitch]]
-- description: generate the full 1330 candidates set by calling the function
--              'combination' which is defined below
-- output: a list of 1330 list of Pitch, denoting the full candidates set
fullCandidates = 
 let all = allPitches in combination all 3


combination :: [Pitch] -> Int -> [[Pitch]]
-- description: generate all combinations of pitches, giving the combination 
--              length
-- input: parameter 1: a list of Pitch denoting all possible Pitch 
--        parameter 2: the combination length
-- output: a list of list of Pitch, denoting all combinations
combination _ 0 = [[]]
combination [] _ = [] 
combination (item:items) n
 | length items > n-1 = map (item:) (combination items (n-1))
                        ++ (combination items n)
 | length items == n-1 = map (item:) (combination items (n-1))
 | otherwise = []


allPitches :: [Pitch]
-- description: generate all possible pitches: from A1 to G3
allPitches = 
 let notes = [A, B, C, D, E, F, G]
     octaves = [One, Two, Three]
 in [Pitch note octave|note<-notes,octave<-octaves]


-----------------------------------------
--------- section 6. next guess ---------
-----------------------------------------

-- In this section, we define the nextGuess function, which is used to 
-- generate a new guess given the previous guess and its corresponding 
-- game state and the feedback. Several helper functions is also defined
-- here.

-- For finding the best guess, we first filter the all items do not 
-- generate the same feedback from the previous candidate set. After
-- this phase, all candidates left could meet the requirement so far.

-- The second phase is to iteratively choosing one item in the candidates
-- set as the guess and compute the average number could be eliminated.
-- Finally, we simply return the item with highest eliminated candidate 
-- numbers. In math, bestCandidate = argmax E[numberOfEliminatedCandidate]

-- This approach could guarantee finding a good enough guess, however, will
-- suffer from high computing cost. Therefore, we apply a prune-like approach,
-- where we simply return the first element in the new candidate set if there
-- are too much candidates inside, otherwise we follow the former approach and
-- find the best one with highest average eliminate number.


nextGuess :: ([Pitch],GameState) -> (Int,Int,Int) -> ([Pitch],GameState)
-- description: from the last guess, game state and corresponding feedback, 
--              generate a new guess and the corresponding game state
-- input: parameter 1: a 2-element tuple, the first element is a list of 
--                     Pitch denoting the guess and the second element is 
--                     the corresponding GameState
--        parameter 2: a tuple representing the corresponding feedback
-- output: a 2-element tuple, the first element is a list of Pitch 
--         denoting the guess and the second element is the corresponding
--         GameState
nextGuess (guess,previousState) result = 
 let candidates = getCandidate previousState
     state = GameState (filter (sameFeedback result guess) candidates)
     newCandidates = getCandidate state
     limit = 180
 in if length (newCandidates) > limit then ((newCandidates)!!0,state)
     else (getBestGuess state,state)


sameFeedback :: (Int,Int,Int) -> [Pitch] -> [Pitch] -> Bool
-- description: for a given feedback and a guess, check if a target candidate 
--              can get the same feedback with the guess
-- input: parameter 1: a tuple representing the feedback;
--        parameter 2: a list of Pitch denoting the guess
--        parameter 3: a list of Pitch denoting a target candidate
-- output: a boolean value show if the target candidate meet the requirement
sameFeedback fb guess target
 | fb == feedback guess target = True
 | otherwise = False


getBestGuess :: GameState -> [Pitch]
-- description: give the current game state, find best guess potentially (the
--              guess which can eliminate most number of candidate in average)
-- input: parameter 1: a GameState denoting current game state
-- output: a Guess denoting the best guess
getBestGuess state = 
 let pitches = getCandidate state
     (score,bestGuess) = findBestGuess pitches pitches
 in bestGuess


findBestGuess :: [[Pitch]] -> [[Pitch]] -> (Int,[Pitch])
-- description: iteratively check each candidate for its potentially eliminate
--              candidate number and return the candidate with the largest one
-- input: parameter 1: list of list of Pitch, which is used for recursion
--        parameter 2: list of list of Pitch, which is the full candidates set
-- output: a tuple consist the best guess with its potential eliminate
--         candidate number
findBestGuess [] _ = (0,[])
findBestGuess (candidate:candidates) allCandidates = 
 let (previousscore,previouspitch) = findBestGuess candidates allCandidates
     currentscore = numberEliminate candidate allCandidates allCandidates
 in if currentscore >= previousscore then (currentscore,candidate)
     else (previousscore,previouspitch)


numberEliminate :: [Pitch] -> [[Pitch]] -> [[Pitch]] -> Int
-- description: given an element in candidate set, suppose it is the guess,
--              compute the average number of eliminate items in the candidate
--              set for assuming each item in candidate set has an equal 
--              probability to be the true target. We ignore multiplying the
--              probability since all probability are the same and it will 
--              indicate the average.
-- input: parameter 1: a list of Pitch, denoting the guess
--        parameter 2: a list of list of Pitch, denoting candidate set, usd
--                     for recursion
--        parameter 3: a list of list of Pitch, denoting the full candidate
--                     set
-- output: total eliminate items number
numberEliminate _ [] _ = 0
numberEliminate guess (candidate:candidates) all = 
 let fb = feedback guess candidate
     leftCandidates = filter (sameFeedback fb guess) all
     eliminateNumber = length all - length (leftCandidates)
 in eliminateNumber + numberEliminate guess candidates all



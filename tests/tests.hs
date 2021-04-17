module Main where

import           Data.Maybe                     ( catMaybes
                                                , isNothing
                                                )
import           Logic                          ( Puzzle(..)
                                                , alreadyGuessed
                                                , charInWord
                                                , fillInCharacter
                                                , freshPuzzle
                                                , handleGuess
                                                )
import           Test.QuickCheck

regularChar :: Gen Char
regularChar = elements ['a' .. 'z']

regularWord :: Gen String
regularWord =
  listOf regularChar `suchThat` (\xs -> length xs > 0 && length xs < 10)

puzzle :: Gen Puzzle
puzzle = do
  word <- regularWord
  return $ freshPuzzle word

puzzleAndCharInWord :: Gen (Puzzle, Char)
puzzleAndCharInWord = do
  p <- puzzle
  c <- regularChar `suchThat` charInWord p
  return (p, c)

puzzleAndCharNotInWord :: Gen (Puzzle, Char)
puzzleAndCharNotInWord = do
  p <- puzzle
  let charNotInWord = not . charInWord p
  c <- regularChar `suchThat` charNotInWord
  return (p, c)

puzzleAndCharAlreadyGuessed :: Gen (Puzzle, Char)
puzzleAndCharAlreadyGuessed = do
  (p, c) <- puzzleAndCharInWord
  return (fillInCharacter p c, c)


filledInEmpty :: Puzzle -> Bool
filledInEmpty (Puzzle _ filledIn _) = all isNothing filledIn

characterIsFilledIn :: Puzzle -> Char -> Bool
characterIsFilledIn (Puzzle _ filledIn _) c = c `elem` catMaybes filledIn

prop_filledInSoFarEmptyAtBeginning :: Property
prop_filledInSoFarEmptyAtBeginning = forAll puzzle filledInEmpty

prop_charactersInWordAreFilledIn :: Property
prop_charactersInWordAreFilledIn = forAll
  puzzleAndCharInWord
  (\(p, c) ->
    let newPuzzle = fillInCharacter p c in characterIsFilledIn newPuzzle c
  )

prop_charactersNotInWordAreNotFilledIn :: Property
prop_charactersNotInWordAreNotFilledIn = forAll
  puzzleAndCharNotInWord
  (\(p, c) ->
    let newPuzzle = fillInCharacter p c
    in  not $ characterIsFilledIn newPuzzle c && alreadyGuessed newPuzzle c
  )


main :: IO ()
main = do
  quickCheck prop_filledInSoFarEmptyAtBeginning
  quickCheck prop_charactersInWordAreFilledIn
  quickCheck prop_charactersNotInWordAreNotFilledIn

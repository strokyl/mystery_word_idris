module Main

import Effects
import Effect.StdIO
import Effect.File
import Effect.Exception
import Effect.Random
import Effect.System
import Data.SortedSet
import Data.Fin
import Effect.State

dic_file : String
dic_file = "words.txt"

consumeFileEff : (t : Type) -> Type
consumeFileEff t = Eff t [FILE (FileHandle Read), EXCEPTION String] [FILE (), EXCEPTION String]

closeAndFail : String -> consumeFileEff t
closeAndFail err = do close
                      raise err

closeAndPure : t -> consumeFileEff t
closeAndPure e = do close
                    pure e

countLine: Nat -> consumeFileEff Nat
countLine acc = do False <- eof | True => closeAndPure acc
                   Result _ <- readLine | FError err => closeAndFail (show err)
                   countLine $ S acc

readLineAtNum: Nat -> consumeFileEff String
readLineAtNum acc = do False <- eof | True => closeAndFail "dic file has been altered"
                       Result word <- readLine | FError err =>  closeAndFail (show err)
                       case acc of
                            Z => if word == "" then closeAndFail "Bad dic" else closeAndPure word
                            S k => readLineAtNum k

minusOneOrFail : Nat -> Eff Nat [EXCEPTION String]
minusOneOrFail Z = raise "Empty dic"
minusOneOrFail (S k) = pure k

getWord : Eff String [FILE (), EXCEPTION String, RND, SYSTEM, RND]
getWord = do Success <- open dic_file Read | FError err => raise (show err)
             size <- minusOneOrFail !(countLine Z)
             srand !(time)
             line_num <- rndFin size
             Success <- open dic_file Read | FError err => raise (show err)
             readLineAtNum $ finToNat line_num



record GuessState where
       constructor MkGuessState
       goodGuess : SortedSet Char
       badGuess : SortedSet Char

implementation Default GuessState where
               default = MkGuessState empty empty

numGuess: GuessState -> Nat
numGuess = length . Data.SortedSet.toList . badGuess

displaySet : SortedSet Char -> String
displaySet = pack . Data.SortedSet. toList

displaySecretWord : GuessState -> String -> String
displaySecretWord st = pack . map displayChar . unpack where
                    displayChar l = if contains l (goodGuess st) then l else '*'

showState: String -> GuessState -> String
showState secret st = "Bad letter: " ++ (displaySet $ badGuess st) ++ "\n" ++
                "Num of try: " ++ (show $ numGuess st) ++ "\n" ++
                "Word: " ++ (displaySecretWord st secret)

evolveState: String -> GuessState -> Char -> (GuessState, String)
evolveState w st l = if alreadyGuess then
                        (st, (cast l) ++ " was already asked")
                     else
                         newState ()
                     where
                        alreadyGuess = contains l $ (goodGuess st `union` badGuess st)
                        newState () = if l `elem` (unpack w) then
                                         (record {goodGuess $= insert l} st, "good guess")
                                      else
                                         (record {badGuess $= insert l} st, "bad guess")

askChar: Eff Char [STDIO]
askChar = do line <- getStr
             case (unpack line )of
               [c] => pure c
               _ => do {putStrLn "Please enter one and only one letter"; askChar}

max_try : Nat
max_try = 10

game : String -> Eff () [STDIO, STATE GuessState]
game word = do putStrLn $ showState word !(get)
               False <- userWin | True => putStrLn "You wine"
               False <- userLoose | True => putStrLn ("You loose, secret was " ++ word)
               putStrLn "Enter a letter: "
               char <- askChar
               let (new_st, mess) = evolveState word !(get) char
               putStrLn mess
               put new_st
               game word
            where
               userWin : Eff Bool [STATE GuessState]
               userWin = do pure $ displaySecretWord !(get) word == word

               userLoose : Eff Bool [STATE GuessState]
               userLoose = do pure $ (numGuess !(get)) == max_try


mainEffect : Eff () [STDIO, FILE (), EXCEPTION String, RND, SYSTEM, STATE GuessState]
mainEffect = do game !(getWord)

main : IO ()
main = run mainEffect

-- Local Variables:
-- idris-load-packages: ("effects" "contrib")
-- End:

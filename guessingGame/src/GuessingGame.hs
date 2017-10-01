module GuessingGame where

starman :: String -> Int -> IO ()
starman secretWord numStarsRemaining = turn secretWord ['-' | x <- secretWord] numStarsRemaining

check :: String -> String -> Char -> (Bool,String)
check secretWord currentDisplayedWord c = 
	let
	  containsChar = c `elem` secretWord
	  newDisplayedWord = [(if x==c then c else y) | (x,y) <- zip secretWord currentDisplayedWord]
	in (containsChar, newDisplayedWord)

turn :: String -> String -> Int -> IO ()
turn secretWord currentDisplayedWord numStarsRemaining =
  do if numStarsRemaining==0
       then putStrLn "You lose"
       else if secretWord==currentDisplayedWord
              then putStrLn "You win!"
              else mkguess secretWord currentDisplayedWord numStarsRemaining

mkguess :: String -> String -> Int -> IO ()
mkguess secretWord currentDisplayedWord numStarsRemaining =
  do putStrLn (currentDisplayedWord ++ "  " ++ take numStarsRemaining (repeat '*'))
     putStr "  Enter your guess: "
     q <- getLine
     let (correct, currentDisplayedWord') = check secretWord currentDisplayedWord (q!!0)
     let numStarsRemaining' = if correct then numStarsRemaining else numStarsRemaining-1
     turn secretWord currentDisplayedWord' numStarsRemaining'

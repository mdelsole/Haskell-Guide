module Main where

-- Running: runaskell Main.hs path_to_test_file

import Interpret
import System.Environment

main :: IO ()
main = do
	-- Read in file name from program arguments
    (fileName:tl) <- getArgs
    -- Read the contents of the file to "contents"
    contents <- readFile fileName
    -- Interpret file contents, return output and remaining stack 
    let (stack, output) = interpret contents
    -- Print the output
    putStrLn output

    -- Check if the stack is empty. If it is, print its contents.
    if not (null stack) 
    -- "then" can't do more than one statement, so we use a "do" to make a monad
	then do
		putStrLn "Stack was not empty upon program completion. Its contents were:"
		print stack
	else return ()
    	

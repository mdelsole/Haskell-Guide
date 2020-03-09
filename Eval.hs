module Eval where
-- This file contains definitions for functions and operators

import Val
import Data.Char   


-- main evaluation function for operators and 
-- built-in FORTH functions with no output
-- takes a string and a stack and returns the stack
-- resulting from evaluation of the function
eval :: String -> [Val] -> [Val]

-- Multiplication
-- if arguments are integers, keep result as integer
eval "*" (Integer x: Integer y:tl) = Integer (x*y) : tl
-- if any argument is float, make result a float
eval "*" (x:y:tl) = (Real $ toFloat x * toFloat y) : tl 
-- any remaining cases are stacks too short
eval "*" _ = error("Stack underflow")

-- Division
-- If integers are entered, perform integer division
eval "/" (Integer x: Integer y:tl) = Integer (x `div` y) : tl
-- If floats are entered/ perform float division
eval "/" (x:y:tl) = (Real $ toFloat x / toFloat y) : tl 
-- any remaining cases are stacks too short
eval "/" _ = error("Stack underflow")

-- Addition
-- if arguments are integers, keep result as integer
eval "+" (Integer x: Integer y:tl) = Integer (x+y) : tl
-- if any argument is float, make result a float
eval "+" (x:y:tl) = (Real $ toFloat x + toFloat y) : tl 
-- any remaining cases are stacks too short
eval "+" _ = error("Stack underflow")

-- Subtraction
-- if arguments are integers, keep result as integer
eval "-" (Integer x: Integer y:tl) = Integer (x-y) : tl
-- if any argument is float, make result a float
eval "-" (x:y:tl) = (Real $ toFloat x - toFloat y) : tl 
-- any remaining cases are stacks too short
eval "-" _ = error("Stack underflow")

-- Power
-- if arguments are integers, keep result as integer
eval "^" (Integer x: Integer y:tl) = Integer (x^y) : tl
-- if any argument is float, make result a float
eval "^" (x:y:tl) = (Real $ toFloat x ** toFloat y) : tl 
-- any remaining cases are stacks too short
eval "^" _ = error("Stack underflow")


-- Duplicate the element at the top of the stack
eval "DUP" (x:tl) = (x:x:tl)
eval "DUP" [] = error("Stack underflow")

-- STR = Convert to string
eval "STR" (Integer x:tl) = Id (show x) : tl
eval "STR" (Real x:tl) = Id (show x) : tl
eval "STR" (Id x:tl) = Id x : tl
eval "STR" _ = error("Stack underflow")

-- CONCAT2 = Concatenate 2 strings
eval "CONCAT2" (Id x: Id y:tl) = Id (x ++ y) : tl
eval "CONCAT2" (Real x: Real y:tl) =  error("Arguments are not strings")
eval "CONCAT2" (Integer x: Integer y:tl) =  error("Arguments are not strings")
eval "CONCAT2" _ = error("Stack underflow")

-- CONCAT3 = Concatenate 3 strings
eval "CONCAT3" (Id x: Id y: Id z: tl) = Id (x ++ y ++ z) : tl
eval "CONCAT3" (Real x: Real y: Real z: tl) =  error("Arguments are not strings")
eval "CONCAT3" (Integer x: Integer y: Integer z: tl) =  error("Arguments are not strings")
-- I could cover every combination here but I don't think that's really necessary for this
eval "CONCAT3" _ = error("Stack underflow")

-- this must be the last rule
-- it assumes that no match is made and preserves the string as argument
eval s l = Id s : l 


-- variant of eval with output
-- state is a stack and string pair
evalOut :: String -> ([Val], String) -> ([Val], String) 
-- print element at the top of the stack
evalOut "." (Id x:tl, out) = (tl, out ++ x)
evalOut "." (Integer i:tl, out) = (tl, out ++ (show i))
evalOut "." (Real x:tl, out) = (tl, out ++ (show x))
evalOut "." ([], _) = error "Stack underflow"

-- EMIT =  take the integer value of the top of the stack and convert to ascii
evalOut "EMIT" (Integer x:tl, out) = (tl, out ++ [(chr x)] ++ "\n")
evalOut "EMIT" (Real x:tl, out) =  (tl, out ++ " " ++ "\n")
evalOut "EMIT" (Id x:tl, out) = (tl, out ++ " " ++ "\n")
evalOut "EMIT" ([], _) = error("Stack underflow")

-- 10 is the ascii code for new line
evalOut "CR" (tl, out) = (tl, out ++ "\n")


-- this has to be the last case
-- If no special case, ask eval to deal with it and propagate output
evalOut op (stack, out) = (eval op stack, out)
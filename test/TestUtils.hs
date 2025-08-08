module TestUtils (testEq) where

import Debug.Trace

infix 4 `testEq`

testEq :: (Show a, Eq a) => a -> a -> Bool
testEq a b =
    if a == b
        then True
        else trace
            ("\n---\n" ++ show a ++ "\n---\n" ++ show b ++ "\n---")
            False
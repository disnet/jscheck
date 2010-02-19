{-# OPTIONS -fglasgow-exts #-}
module HJS.Interpreter.VarArgs where

class BuildList a r  | r-> a where
    build' :: [a] -> a -> r

instance Show a => BuildList a [String] where
    build' l x = reverse $ (show x):(map show l)

instance BuildList a b => BuildList a (a->b) where
    build' l x y = build' (x:l) y

--argsList x = build' [] x


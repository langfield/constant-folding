module Main where

import Control.Monad
import Control.Applicative

import System.IO
import System.IO.Unsafe
import System.Process

import Data.List
import Data.List.NonEmpty (NonEmpty (..), (<|))

import Data.Set (Set)
import Data.Maybe
import Data.Functor
import Data.Function
import Data.Map.Strict (Map)

import Test.Hspec
import Test.QuickCheck

import qualified Data.Set as S
import qualified Data.Foldable1 as F1
import qualified Data.Map.Strict as M
import qualified Data.List.NonEmpty as NE

import Debug.Trace (trace)

type Handles = (Handle,Handle,Handle)

-- Solution ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

data Expr = Name String | Value Int | Mul Expr Expr deriving (Eq, Show)

eval :: Map String Expr -> Expr -> Maybe Int
eval exprs = go []
  where
    go :: [String] -> Expr -> Maybe Int
    go _    (Value n)    = Just n
    go seen (Mul e1 e2)  = (*) <$> go seen e1 <*> go seen e2
    go seen (Name name)
      | name `elem` seen = Nothing
      | otherwise        = go (name:seen) =<< M.lookup name exprs

unmaybe :: Show a => (a -> Maybe b) -> a -> b
unmaybe f x = fromMaybe (error $ "cycle detected while evaluating: " ++ show x) (f x)

solution :: Map String Expr -> Map String Int
solution m = M.map (unmaybe (eval m)) m

-- Tests ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

sed :: String -> String -> String
sed pat = unsafePerformIO . readProcess "sed" ["-E", pat]

test :: Handles -> Int -> IO ()
test (hi,ho,_) x = do
  hPutStr hi $ unlines [show x]
  shouldBe True . read =<< hGetLine ho

main :: IO ()
main = do
  (hi,ho,he,_) <- runInteractiveCommand "python3 a.py"
  hSetBuffering hi NoBuffering
  hspec $ do
    describe "solution" $ do
      it "handles example 1" $ do
        solution namespace `shouldBe` M.fromList
          [ ("Y", 2)
          , ("X", 12)
          , ("Z", 24)
          , ("W", 0)
          , ("G", 0)
          ]
      xit "matches a reference implementation" $ do
        property . withMaxSuccess 1000 . test $ (hi,ho,he)
  where
    namespace = M.fromList
      [ ("Y", Value 2)
      , ("X", Mul (Value 3) (Value 4))
      , ("Z", Mul (Name "X") (Name "Y"))
      , ("W", Name "G")
      , ("G", Value 0)
      ]

module Main where

import Data.Maybe
import Text.Read (readMaybe)
import Text.Printf (printf)
import Control.Monad (msum)
import qualified Data.Map as Map

type Operator a = [a] -> Result [a]
type Result a = Either String a
data Elem a = Item a | Instruction (Operator a)
type Program a = [Elem a]

withStackElements :: Int -> ([a] -> [a]) -> (Operator a)
withStackElements n f = fx
  where
    fx stack =
      if (length stack) >= n
      then
        let (before, after) = splitAt n stack
            newBefore = f before
            in Right $ newBefore ++ after
      else Left "Not enough elements on stack"

plus :: Num a => Operator a
plus = withStackElements 2 f
  where f (x:y:[]) = [x + y]
        f _        = error "invalid"

minus :: Num a => Operator a
minus = withStackElements 2 f
  where f (x:y:[]) = [x - y]
        f _        = error "invalid"

times :: Num a => Operator a
times = withStackElements 2 f
  where f (x:y:[]) = [x * y]
        f _        = error "invalid"

divide :: (Num a, Fractional a) => Operator a
divide = withStackElements 2 f
  where f (x:y:[]) = [x / y]
        f _        = error "invalid"

log' :: Floating a => Operator a
log' = withStackElements 1 f
  where f (x:[]) = [log x]
        f _      = error "invalid"

logBase' :: Floating a => Operator a
logBase' = withStackElements 2 f
  where f (x:y:[]) = [logBase x y]
        f _        = error "invalid"

sum' :: (Num a) => Operator a
sum' = Right . return . sum

operatorStringToOperator :: (Floating a) => Map.Map String (Operator a)
operatorStringToOperator =
  Map.fromList [
    ("+", plus),
    ("-", minus),
    ("*", times),
    ("/", divide),
    ("sum", sum'),
    ("ln", log'),
    ("logBase", logBase')
  ]

parseElem :: (Floating a, Read a) => String -> Result (Elem a)
parseElem s =
  fromMaybe (Left $ printf "Could not read elem '%s'" s) $
    fmap Right $ msum [readOperator s, readItem s]
  where
    readOperator x = fmap Instruction $ operatorStringToOperator Map.!? x
    readItem     x = fmap Item $ readMaybe x

parseProgram :: (Floating a, Read a) => String -> Result (Program a)
parseProgram s = sequence $ map parseElem $ words s

evalProgram :: (Floating a) => Program a -> a
evalProgram = head . foldl f []
  where
    f acc (Item x)     = x:acc
    f acc (Instruction i) =
      case i acc of
        Left err -> error err
        Right newAcc -> newAcc

main :: IO ()
main = do
  contents <- getContents
  let program = parseProgram contents :: Result (Program Double)
      result  = fmap evalProgram program
      in case result of
        Left  err -> putStrLn err
        Right res -> putStrLn $ show res


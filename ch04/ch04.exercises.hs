-- file: ch04/ch04.exercises.hs

import System.Environment (getArgs)
import Data.Char (digitToInt)
import Data.List (foldl')

safeHead :: [a] -> Maybe a
safeHead (x:_) = Just x
safeHead _     = Nothing

safeTail :: [a] -> Maybe [a]
safeTail (x:xs) = Just xs
safeTail _      = Nothing

safeLast :: [a] -> Maybe a
safeLast [x]    = Just x
safeLast (_:xs) = safeLast xs
safeLast _      = Nothing


safeInit :: [a] -> Maybe [a]
safeInit xs = if not (null xs)
              then Just (init xs)
              else Nothing

splitWith :: (a -> Bool) -> [a] -> [[a]]
splitWith fun xs    = case dropWhile (not.fun) xs of
                        []  -> []
                        xs' -> w : splitWith fun xs''
                            where (w, xs'') = break (not.fun) xs'



interactWith function inputFile outputFile = do
    input <- readFile inputFile
    writeFile outputFile (function input)

main = mainWith myFunction
    where mainWith function = do
            args <- getArgs
            case args of
                [input, output] -> interactWith function input output
                _ -> putStrLn "error: exactly two arguments needed"

          -- exercise 3
          -- myFunction = unlines.(map (head.words)).lines
          -- exercise 4
          myFunction = unlines.transpose.lines


transpose :: [String] -> [String]
transpose xs = if (concat xs == "")
               then []
               else (map ((takeOffMaybe ' ').safeHead) xs) : (transpose (map ((takeOffMaybe "").safeTail) xs))

takeOffMaybe :: a -> (Maybe a) -> a
takeOffMaybe def (Just a) = a
takeOffMaybe def Nothing  = def 


myHead :: String -> Char
myHead (x:_) = x
myHead _     = ' '

myTail :: String -> String
myTail (x:xs) = xs
myTail _      = ""

myTranspose :: [String] -> [String]
myTranspose xs = if (concat xs == "")
                 then []
                 else (map myHead xs) : (transpose (map myTail xs))


asInt :: String -> Int
asInt xs = foldl step 0 xs
    where step zero x = zero*10 + digitToInt x
            
asInt_fold :: String -> Int
asInt_fold []           = error "String is empty."
asInt_fold "-"          = error "Dash is not a number."
asInt_fold ('-':xs)     = (-1) * asInt_fold xs
asInt_fold xs           = foldl' step 0 xs
    where step zero x
            | zero > maxis = error "Overflow of Int."
            | otherwise    = zero*10 + digitToInt x
          maxis = (maxBound :: Int) `div` 10

asInt_either :: String -> (Either String Int)
asInt_either []         = Left "String is empty."
asInt_either xs
    | legalInt xs       = Right (asInt xs)
    | otherwise         = Left "illegal Int string."

legalInt :: String -> Bool
legalInt xs@(x:xs')
    | x == '-'  = onlyDigits xs'
    | otherwise = onlyDigits xs

onlyDigits :: String -> Bool
onlyDigits (x:xs)
    | x >= '0' && x <= '9' = onlyDigits xs
    | otherwise            = False

myConcat :: [[a]] -> [a]
myConcat xs = foldr (++) [] xs

myTakeWhile :: (a -> Bool) -> [a] -> [a]
myTakeWhile f (x:xs)
    | f x       = x : myTakeWhile f xs
    | otherwise = []

myTakeWhile_foldr :: (a -> Bool) -> [a] -> [a]
myTakeWhile_foldr f xs = foldr step [] xs
    where step x ys = if f x then x:ys else []

myGroupBy :: (a -> a -> Bool) -> [a] -> [[a]]
myGroupBy _ []      = []
myGroupBy eq (x:xs) = (x:ys) : myGroupBy eq zs
                      where (ys, zs) = span (eq x) xs

-- so so, we should return when we first meet the True case
myAny :: (a -> Bool) -> [a] -> Bool
myAny f xs = foldr step False xs
    where step x hasAny = hasAny || f x

-- ugly
myCycle :: [a] -> [a]
myCycle xs = foldr step xs xs
    where step x ys = ys ++ (step x ys)

-- clear
myWords :: String -> [String]
myWords xs = snd (foldr step ([], []) xs)
    where
        step ' '    ([]  , words) = ([], words)
        step ' '    (word, words) = ([], word:words)
        step letter (word, words) = (letter:word, words)

-- clear
myUnlines :: [String] -> String
myUnlines lines = foldr step "" lines
    where step newline lines = newline ++ "\n" ++ lines

-- for fun
合并多行 :: [String] -> String
合并多行 很多行 = foldr 接上一行 "" 很多行
    where 接上一行 新行 接在这里 = 新行 ++ "\n" ++ 接在这里

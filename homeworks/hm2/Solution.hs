import Data.Foldable
import Data.Sequence (Seq)

data Sequence a = Empty | Single a | Append (Sequence a) (Sequence a)
  deriving (Show, Eq)

-- #1
instance Functor Sequence where
  fmap _ Empty = Empty
  fmap f (Single x) = Single (f x)
  fmap f (Append s1 s2) = Append (fmap f s1) (fmap f s2)

-- #2
instance Foldable Sequence where
  foldMap :: (Monoid m) => (a -> m) -> Sequence a -> m
  foldMap _ Empty = mempty
  foldMap f (Single x) = f x
  foldMap f (Append s1 s2) = foldMap f s1 <> foldMap f s2

seqToList :: Sequence a -> [a]
seqToList = toList

seqLength :: Sequence a -> Int
seqLength = length

-- #3
instance Semigroup (Sequence a) where
  Empty <> s = s
  s <> Empty = s
  s1 <> s2 = Append s1 s2

instance Monoid (Sequence a) where
  mempty = Empty

-- #4
tailElem :: (Eq a) => a -> Sequence a -> Bool
tailElem target seq = go [seq]
  where
    go [] = False
    go (s : stack) = case s of
      Empty -> go stack
      Single x -> if x == target then True else go stack
      Append s1 s2 -> go (s1 : s2 : stack)

-- #5
tailToList :: Sequence a -> [a]
tailToList seq = reverse (go [seq] [])
  where
    go [] acc = acc
    go (s : stack) acc = case s of
      Empty -> go stack acc
      Single x -> go stack (x : acc)
      Append s1 s2 -> go (s1 : s2 : stack) acc

-- #6
data Token = TNum Int | TAdd | TSub | TMul | TDiv
  deriving (Show, Eq)

tailRPN :: [Token] -> Maybe Int
tailRPN tokens = go tokens []
  where
    go [] [result] = Just result
    go [] _ = Nothing
    go (t : ts) stack = case t of
      TNum n -> go ts (n : stack)
      TAdd -> applyOp (+) ts stack
      TSub -> applyOp (-) ts stack
      TMul -> applyOp (*) ts stack
      TDiv -> applyDiv ts stack

    applyOp op ts (y : x : stack) = go ts (x `op` y : stack)
    applyOp _ _ _ = Nothing

    applyDiv ts (y : x : stack) = if y == 0 then Nothing else go ts (x `div` y : stack)
    applyDiv _ _ = Nothing

-- #7
-- a
myReverse :: [a] -> [a]
myReverse = foldl (flip (:)) []

-- b
myTakeWhile :: (a -> Bool) -> [a] -> [a]
myTakeWhile p = foldr (\x acc -> if p x then x : acc else []) []

-- c
decimal :: [Int] -> Int
decimal = foldl (\acc x -> acc * 10 + x) 0

-- #8
-- a
encode :: (Eq a) => [a] -> [(a, Int)]
encode = foldr go []
  where
    go x [] = [(x, 1)]
    go x ((y, count) : rest)
      | x == y = (y, count + 1) : rest
      | otherwise = (x, 1) : (y, count) : rest

-- b
decode :: [(a, Int)] -> [a]
decode = foldr (\(x, count) acc -> replicate count x ++ acc) []

main :: IO ()
main = do
  let seqA :: Sequence Int
      seqA = Append (Single 1) (Append (Single 2) (Single 3))

  let seqB :: Sequence Int
      seqB = Append Empty (Append (Single 4) Empty)

  let seqC :: Sequence Int
      seqC = Single 5 <> Empty <> Single 6

  let seqD :: Sequence Int
      seqD =
        Append
          (Append (Single 1) (Single 2))
          (Append (Single 3) (Single 4))

  let seqE :: Sequence Int
      seqE = Append Empty Empty

  let seqF :: Sequence Int
      seqF =
        Append
          (Single 10)
          ( Append
              (Append Empty (Single 20))
              (Append (Single 30) Empty)
          )

  let printSection title tests = do
        putStrLn title
        mapM_ (\(name, ok) -> putStrLn (name ++ ": " ++ show ok)) tests
        putStrLn ""

  -- #1
  let task1Tests =
        [ ( "fmap (+1) seqA",
            fmap (+ 1) seqA == Append (Single 2) (Append (Single 3) (Single 4))
          ),
          ( "fmap (*2) seqD",
            fmap (* 2) seqD
              == Append
                (Append (Single 2) (Single 4))
                (Append (Single 6) (Single 8))
          )
        ]

  -- #2
  let task2Tests =
        [ ("seqToList seqA", seqToList seqA == [1, 2, 3]),
          ("seqToList seqD", seqToList seqD == [1, 2, 3, 4]),
          ("seqToList seqE", seqToList seqE == []),
          ("seqToList seqF", seqToList seqF == [10, 20, 30]),
          ("seqLength seqA", seqLength seqA == 3),
          ("seqLength seqD", seqLength seqD == 4),
          ("seqLength seqE", seqLength seqE == 0),
          ("seqLength seqF", seqLength seqF == 3)
        ]

  -- #3
  let task3Tests =
        [ ("mempty <> seqA", mempty <> seqA == seqA),
          ("seqA <> mempty", seqA <> mempty == seqA),
          ("seqA <> seqB", seqToList (seqA <> seqB) == [1, 2, 3, 4]),
          ("seqE <> seqA", seqToList (seqE <> seqA) == [1, 2, 3]),
          ("seqA <> seqE", seqToList (seqA <> seqE) == [1, 2, 3]),
          ("seqToList seqC", seqToList seqC == [5, 6])
        ]

  -- #4
  let task4Tests =
        [ ("find 2 in seqA", tailElem 2 seqA == True),
          ("find 99 in seqA", tailElem 99 seqA == False),
          ("find 4 in seqB", tailElem 4 seqB == True),
          ("find 3 in seqD", tailElem 3 seqD == True),
          ("find 0 in seqE", tailElem 0 seqE == False),
          ("find 20 in seqF", tailElem 20 seqF == True)
        ]

  -- #5
  let task5Tests =
        [ ("tailToList seqA", tailToList seqA == [1, 2, 3]),
          ("tailToList seqD", tailToList seqD == [1, 2, 3, 4]),
          ("tailToList seqE", tailToList seqE == []),
          ("tailToList seqF", tailToList seqF == [10, 20, 30])
        ]

  -- #6
  let task6Tests =
        [ ("4 5 *", tailRPN [TNum 4, TNum 5, TMul] == Just 20),
          ("8 2 /", tailRPN [TNum 8, TNum 2, TDiv] == Just 4),
          ("7 3 -", tailRPN [TNum 7, TNum 3, TSub] == Just 4),
          ("1 2 3 + *", tailRPN [TNum 1, TNum 2, TNum 3, TAdd, TMul] == Just 5),
          ("bad expression", tailRPN [TNum 6, TDiv] == Nothing),
          ("empty input", tailRPN [] == Nothing)
        ]

  -- #7
  let task7Tests =
        [ ("myReverse [1,2,3]", myReverse [1, 2, 3] == [3, 2, 1]),
          ("myReverse []", myReverse ([] :: [Int]) == []),
          ("myTakeWhile (<3)", myTakeWhile (< 3) [1, 2, 3, 1] == [1, 2]),
          ("myTakeWhile even", myTakeWhile even [2, 4, 6, 7] == [2, 4, 6]),
          ("decimal [1,2,3,4]", decimal [1, 2, 3, 4] == 1234),
          ("decimal []", decimal [] == 0)
        ]

  -- #8
  let task8Tests =
        [ ( "encode aaabccca",
            encode "aaabccca" == [('a', 3), ('b', 1), ('c', 3), ('a', 1)]
          ),
          ( "encode numbers",
            encode [1, 1, 1, 2, 2, 3, 1, 1] == [(1, 3), (2, 2), (3, 1), (1, 2)]
          ),
          ( "decode chars",
            decode [('x', 2), ('y', 3), ('z', 1)] == "xxyyyz"
          ),
          ( "decode ints",
            decode [(1, 3), (2, 2), (3, 1)] == [1, 1, 1, 2, 2, 3]
          ),
          ( "decode . encode",
            decode (encode "hhheeelllo") == "hhheeelllo"
          )
        ]

  printSection "Task 1" task1Tests
  printSection "Task 2" task2Tests
  printSection "Task 3" task3Tests
  printSection "Task 4" task4Tests
  printSection "Task 5" task5Tests
  printSection "Task 6" task6Tests
  printSection "Task 7" task7Tests
  printSection "Task 8" task8Tests
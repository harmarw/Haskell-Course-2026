import Control.Monad (foldM, guard)
import Control.Monad.Writer (Writer, runWriter, tell)
import Data.List (permutations)
import Data.Map (Map)
import qualified Data.Map as Map
import qualified Data.Set as Set

-- Task 1
type Pos = (Int, Int)

data Dir = N | S | E | W
    deriving (Eq, Ord, Show)

type Maze = Map Pos (Map Dir Pos)

-- (a)
move :: Maze -> Pos -> Dir -> Maybe Pos
move maze pos dir = do
    neighbors <- Map.lookup pos maze
    Map.lookup dir neighbors

-- (b)
followPath :: Maze -> Pos -> [Dir] -> Maybe Pos
followPath maze start dirs = foldM (move maze) start dirs

-- (c)
safePath :: Maze -> Pos -> [Dir] -> Maybe [Pos]
safePath maze start dirs = do
    (_, visited) <- foldM step (start, [start]) dirs
    pure visited
  where
    step :: (Pos, [Pos]) -> Dir -> Maybe (Pos, [Pos])
    step (pos, path) dir = do
        next <- move maze pos dir
        pure (next, path ++ [next])

-- Task 2
type Key = Map Char Char

decrypt :: Key -> String -> Maybe String
decrypt key = traverse (`Map.lookup` key)

decryptWords :: Key -> [String] -> Maybe [String]
decryptWords key = traverse (decrypt key)

-- Task 3
type Guest = String
type Conflict = (Guest, Guest)

seatings :: [Guest] -> [Conflict] -> [[Guest]]
seatings guests conflicts = do
    seating <- permutations guests
    guard (isValid seating)
    pure seating
  where
    conflictSet :: Set.Set (Guest, Guest)
    conflictSet =
        Set.fromList $
            concatMap (\(g1, g2) -> [(g1, g2), (g2, g1)]) conflicts

    isValid :: [Guest] -> Bool
    isValid seating = all (`Set.notMember` conflictSet) (adjacentPairs seating)

    adjacentPairs :: [Guest] -> [(Guest, Guest)]
    adjacentPairs [] = []
    adjacentPairs xs = zip xs (tail xs ++ [head xs])

-- Task 4
data Result a = Failure String | Success a [String]
    deriving (Show, Eq)

-- (a)
instance Functor Result where
    fmap _ (Failure msg) = Failure msg
    fmap f (Success x warnings) = Success (f x) warnings

instance Applicative Result where
    pure x = Success x []

    Failure msg <*> _ = Failure msg
    Success _ _ <*> Failure msg = Failure msg
    Success f warnings1 <*> Success x warnings2 =
        Success (f x) (warnings1 ++ warnings2)

instance Monad Result where
    Failure msg >>= _ = Failure msg
    Success x warnings1 >>= f =
        case f x of
            Failure msg -> Failure msg
            Success y warnings2 -> Success y (warnings1 ++ warnings2)

-- (b)
warn :: String -> Result ()
warn msg = Success () [msg]

failure :: String -> Result a
failure = Failure

-- (c)
validateAge :: Int -> Result Int
validateAge age
    | age < 0 = failure "Age cannot be negative"
    | age > 150 = do
        warn "Age is unusually high"
        pure age
    | otherwise = pure age

validateAges :: [Int] -> Result [Int]
validateAges = mapM validateAge

-- Task 5
data Expr = Lit Int | Add Expr Expr | Mul Expr Expr | Neg Expr
    deriving (Show, Eq)

simplify :: Expr -> Writer [String] Expr
simplify (Lit n) = pure (Lit n)

simplify (Add e1 e2) = do
    s1 <- simplify e1
    s2 <- simplify e2
    case (s1, s2) of
        (Lit 0, e) -> do
            tell ["Add identity: 0 + e -> e"]
            pure e
        (e, Lit 0) -> do
            tell ["Add identity: e + 0 -> e"]
            pure e
        (Lit a, Lit b) -> do
            tell ["Add constant folding: a + b -> (a+b)"]
            pure (Lit (a + b))
        _ ->
            pure (Add s1 s2)

simplify (Mul e1 e2) = do
    s1 <- simplify e1
    s2 <- simplify e2
    case (s1, s2) of
        (Lit 1, e) -> do
            tell ["Mul identity: 1 * e -> e"]
            pure e
        (e, Lit 1) -> do
            tell ["Mul identity: e * 1 -> e"]
            pure e
        (Lit 0, _) -> do
            tell ["Mul zero absorption: 0 * e -> 0"]
            pure (Lit 0)
        (_, Lit 0) -> do
            tell ["Mul zero absorption: e * 0 -> 0"]
            pure (Lit 0)
        (Lit a, Lit b) -> do
            tell ["Mul constant folding: a * b -> (a*b)"]
            pure (Lit (a * b))
        _ ->
            pure (Mul s1 s2)

simplify (Neg e) = do
    s <- simplify e
    case s of
        Neg inner -> do
            tell ["Double negation: -(-e) -> e"]
            pure inner
        _ ->
            pure (Neg s)

-- Task 6
newtype ZipList a = ZipList { getZipList :: [a] }
    deriving (Show, Eq)

-- (a)
instance Functor ZipList where
    fmap f (ZipList xs) = ZipList (map f xs)

instance Applicative ZipList where
    pure x = ZipList (repeat x)
    ZipList fs <*> ZipList xs = ZipList (zipWith ($) fs xs)

-- (b)
zipTest1 :: ZipList Int
zipTest1 = pure id <*> ZipList [1, 2, 3]

zipTest2 :: ZipList Int
zipTest2 = pure (+) <*> ZipList [1, 2, 3] <*> ZipList [10, 20, 30]

-- (c)
-- ZipList cannot be a Monad because (>>=) would have to merge results
-- produced for each element. These results may have different lengths,
-- so there is no consistent way to keep the position-by-position (zipping)
-- behavior. Any way of combining them would break the Applicative semantics
-- or violate the monad laws.


-- Test data
sampleMaze :: Maze
sampleMaze =
    Map.fromList
        [ ( (0, 0)
          , Map.fromList [(E, (0, 1)), (S, (1, 0))]
          )
        , ( (0, 1)
          , Map.fromList [(W, (0, 0)), (S, (1, 1))]
          )
        , ( (1, 0)
          , Map.fromList [(N, (0, 0)), (E, (1, 1))]
          )
        , ( (1, 1)
          , Map.fromList [(N, (0, 1)), (W, (1, 0))]
          )
        ]

sampleKey :: Key
sampleKey =
    Map.fromList
        [ ('a', 'x')
        , ('b', 'y')
        , ('c', 'z')
        , ('x', 'a')
        , ('y', 'b')
        , ('z', 'c')
        ]

sampleExpr1 :: Expr
sampleExpr1 = Add (Mul (Lit 1) (Add (Lit 2) (Lit 3))) (Lit 0)

sampleExpr2 :: Expr
sampleExpr2 = Mul (Lit 0) (Add (Lit 5) (Lit 8))

sampleExpr3 :: Expr
sampleExpr3 = Neg (Neg (Lit 7))

sampleExpr4 :: Expr
sampleExpr4 = Add (Lit 0) (Mul (Lit 1) (Lit 9))

main :: IO ()
main = do
    putStrLn "=== Task 1: Maze navigation ==="
    putStrLn "move tests:"
    print (move sampleMaze (0, 0) E)
    print (move sampleMaze (0, 0) S)
    print (move sampleMaze (0, 0) W)
    print (move sampleMaze (5, 5) N)

    putStrLn "\nfollowPath tests:"
    print (followPath sampleMaze (0, 0) [])
    print (followPath sampleMaze (0, 0) [E])
    print (followPath sampleMaze (0, 0) [E, S])
    print (followPath sampleMaze (0, 0) [S, E])
    print (followPath sampleMaze (0, 0) [E, E])
    print (followPath sampleMaze (0, 0) [N])

    putStrLn "\nsafePath tests:"
    print (safePath sampleMaze (0, 0) [])
    print (safePath sampleMaze (0, 0) [E])
    print (safePath sampleMaze (0, 0) [E, S])
    print (safePath sampleMaze (0, 0) [S, E])
    print (safePath sampleMaze (0, 0) [E, E])

    putStrLn "\n=== Task 2: Decryption ==="
    putStrLn "decrypt tests:"
    print (decrypt sampleKey "abc")
    print (decrypt sampleKey "cba")      
    print (decrypt sampleKey "")
    print (decrypt sampleKey "abd")

    putStrLn "\ndecryptWords tests:"
    print (decryptWords sampleKey ["abc", "cba"])
    print (decryptWords sampleKey [])
    print (decryptWords sampleKey ["abc", ""])
    print (decryptWords sampleKey ["abc", "dog"])

    putStrLn "\n=== Task 3: Seatings ==="
    print (seatings [] []) 
    print (seatings ["Alice"] [])
    print (seatings ["Alice", "Bob"] [])
    print (seatings ["Alice", "Bob", "Carol"] [])
    print (seatings ["Alice", "Bob", "Carol"] [("Alice", "Bob")])
    print (seatings ["Alice", "Bob", "Carol", "Dave"] [("Alice", "Bob"), ("Carol", "Dave")])

    putStrLn "\n=== Task 4: Result monad ==="
    putStrLn "validateAge tests:"
    print (validateAge 25)
    print (validateAge 0)
    print (validateAge 150)
    print (validateAge 151)
    print (validateAge 200)
    print (validateAge (-3))

    putStrLn "\nvalidateAges tests:"
    print (validateAges [])
    print (validateAges [20, 30, 40])
    print (validateAges [20, 180, 35])
    print (validateAges [170, 180])
    print (validateAges [20, -1, 35])

    putStrLn "\n=== Task 5: Simplify with Writer ==="
    print (runWriter (simplify (Lit 5)))
    print (runWriter (simplify sampleExpr1))
    print (runWriter (simplify sampleExpr2))
    print (runWriter (simplify sampleExpr3))
    print (runWriter (simplify sampleExpr4))
    print (runWriter (simplify (Add (Lit 2) (Lit 3))))
    print (runWriter (simplify (Mul (Lit 4) (Lit 5))))
    print (runWriter (simplify (Neg (Neg (Add (Lit 1) (Lit 2))))))

    putStrLn "\n=== Task 6: ZipList ==="
    print zipTest1
    print zipTest2
    print (fmap (+1) (ZipList [1,2,3]))
    print ((ZipList [(+1), (*2), (\x -> x - 1)]) <*> ZipList [10,20,30])

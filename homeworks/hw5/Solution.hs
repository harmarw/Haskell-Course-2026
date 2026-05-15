import Control.Monad.State
import Control.Monad (unless)
import qualified Data.Map as Map
import Data.Map (Map)

data Instr = PUSH Int | POP | DUP | SWAP | ADD | MUL | NEG
    deriving Show

execInstr :: Instr -> State [Int] ()
execInstr instr =
    case instr of
        PUSH n -> modify (n :)
        POP -> modify $ \stack ->
            case stack of
                (_:xs) -> xs
                [] -> []
        DUP -> modify $ \stack ->
            case stack of
                (x:xs) -> x:x:xs
                [] -> []
        SWAP -> modify $ \stack ->
            case stack of
                (x:y:xs) -> y:x:xs
                _ -> stack
        ADD -> binaryOp (+)
        MUL -> binaryOp (*)
        NEG -> modify $ \stack ->
            case stack of
                (x:xs) -> (-x):xs
                [] -> []

binaryOp :: (Int -> Int -> Int) -> State [Int] ()
binaryOp op = modify $ \stack ->
    case stack of
        (x:y:xs) -> op x y : xs
        _ -> stack

execProg :: [Instr] -> State [Int] ()
execProg = mapM_ execInstr

runProg :: [Instr] -> [Int]
runProg prog = execState (execProg prog) []

data Expr
    = Num Int
    | Var String
    | Add Expr Expr
    | Mul Expr Expr
    | Neg Expr
    | Assign String Expr
    | Seq Expr Expr
    deriving Show

eval :: Expr -> State (Map String Int) Int
eval expr =
    case expr of
        Num n -> return n
        Var name -> do
            env <- get
            return (env Map.! name)
        Add e1 e2 -> do
            v1 <- eval e1
            v2 <- eval e2
            return (v1 + v2)
        Mul e1 e2 -> do
            v1 <- eval e1
            v2 <- eval e2
            return (v1 * v2)
        Neg e -> do
            v <- eval e
            return (-v)
        Assign name e -> do
            v <- eval e
            modify (Map.insert name v)
            return v
        Seq e1 e2 -> do
            _ <- eval e1
            eval e2

runEval :: Expr -> Int
runEval expr = evalState (eval expr) Map.empty

editDistM :: String -> String -> Int -> Int -> State (Map (Int, Int) Int) Int
editDistM xs ys i j = do
    cache <- get
    case Map.lookup (i, j) cache of
        Just value -> return value
        Nothing -> do
            value <- computeDistance
            modify (Map.insert (i, j) value)
            return value
  where
    computeDistance
        | i == 0 = return j
        | j == 0 = return i
        | xs !! (i - 1) == ys !! (j - 1) =
            editDistM xs ys (i - 1) (j - 1)
        | otherwise = do
            deletion <- editDistM xs ys (i - 1) j
            insertion <- editDistM xs ys i (j - 1)
            substitution <- editDistM xs ys (i - 1) (j - 1)

            return (1 + minimum [deletion, insertion, substitution])

editDistance :: String -> String -> Int
editDistance xs ys =
    evalState (editDistM xs ys (length xs) (length ys)) Map.empty

data Location = Normal | DecisionPoint | Obstacle | Treasure | Trap | Goal deriving (Show, Eq)

data GameState = GameState { position :: Int, energy   :: Int, score    :: Int, location :: Location} deriving Show

type AdventureGame a = StateT GameState IO a

board :: Map Int Location
board =
    Map.fromList
        [ (3, Treasure)
        , (5, Obstacle)
        , (6, DecisionPoint)
        , (8, Trap)
        , (10, Treasure)
        , (12, Obstacle)
        , (14, DecisionPoint)
        , (15, Trap)
        , (17, Treasure)
        ]

getLocation :: Int -> Location
getLocation pos
    | pos >= 20 = Goal
    | otherwise = Map.findWithDefault Normal pos board

movePlayer :: Int -> AdventureGame Int
movePlayer moves = do
    st <- get
    let newPos = position st + moves
    put st { position = newPos, energy = energy st - moves, location = getLocation newPos}

    return moves

makeDecision :: [String] -> AdventureGame String
makeDecision options = lift (getPlayerChoice options)

handleLocation :: AdventureGame Bool
handleLocation = do
    st <- get
    case location st of
        Normal ->
            return False
        DecisionPoint -> do
            choice <- makeDecision ["Left path", "Right path"]
            lift $ putStrLn ("You chose: " ++ choice)
            return False
        Obstacle -> do
            modify $ \s -> s { score = max 0 (score s - 1) }
            lift $ putStrLn "Obstacle! You lost 1 point."
            return False
        Treasure -> do
            modify $ \s -> s { score = score s + 1 }
            lift $ putStrLn "Treasure! You gained 1 point."
            return False
        Trap -> do
            modify $ \s -> s { score = max 0 (score s - 3) }
            lift $ putStrLn "Trap! You lost 3 points."
            return False
        Goal -> do
            lift $ putStrLn "You reached the treasure!"
            return True

playTurn :: AdventureGame Bool
playTurn = do
    roll <- lift getDiceRoll
    _ <- movePlayer roll
    st <- get
    lift (displayGameState st)
    if energy st <= 0
        then do
            lift $ putStrLn "You ran out of energy!"
            return True
        else handleLocation

playGame :: AdventureGame ()
playGame = do
    ended <- playTurn
    unless ended playGame

getDiceRoll :: IO Int
getDiceRoll = do
    putStrLn "Provide a dice roll:"
    read <$> getLine

displayGameState :: GameState -> IO ()
displayGameState st = do
    putStrLn "Current game state:"
    print st

getPlayerChoice :: [String] -> IO String
getPlayerChoice options = do
    putStrLn "Choose one option:"
    mapM_
        (\(i, option) -> putStrLn (show i ++ ". " ++ option))
        (zip [1..] options)

    input <- getLine
    let index = read input - 1
    return (options !! index)


assertEqual :: (Eq a, Show a) => String -> a -> a -> IO ()
assertEqual name expected actual =
    if expected == actual
        then putStrLn ("[PASS] " ++ name)
        else putStrLn ("[FAIL] " ++ name ++
                       " expected: " ++ show expected ++
                       ", got: " ++ show actual)

initialGameState :: GameState
initialGameState =
    GameState
        { position = 0
        , energy = 10
        , score = 0
        , location = Normal
        }

runTests :: IO ()
runTests = do
    putStrLn "\nTask 1 tests:"
    assertEqual "push/add" [7] $
        runProg [PUSH 3, PUSH 4, ADD]

    assertEqual "mul/neg" [-10] $
        runProg [PUSH 2, PUSH 5, MUL, NEG]

    assertEqual "invalid pop skipped" [2] $
        runProg [POP, PUSH 1, DUP, ADD]

    assertEqual "swap" [1,2] $
        runProg [PUSH 1, PUSH 2, SWAP]

    assertEqual "not enough operands skipped" [5] $
        runProg [PUSH 5, ADD, MUL, SWAP]


    putStrLn "\nTask 2 tests:"
    assertEqual "simple add" 5 $
        runEval (Add (Num 2) (Num 3))

    assertEqual "assign and use variable" 15 $
        runEval
            (Seq
                (Assign "x" (Num 10))
                (Add (Var "x") (Num 5)))

    assertEqual "nested assignments" 16 $
        runEval
            (Seq
                (Assign "x" (Num 4))
                (Seq
                    (Assign "y" (Mul (Var "x") (Num 3)))
                    (Add (Var "x") (Var "y"))))

    assertEqual "reassign variable" 25 $
        runEval
            (Seq
                (Assign "x" (Num 10))
                (Seq
                    (Assign "x" (Num 20))
                    (Add (Var "x") (Num 5))))

    assertEqual "negative expression" (-12) $
        runEval
            (Neg (Mul (Num 3) (Num 4)))


    putStrLn "\nTask 3 tests:"
    assertEqual "cat -> cut" 1 $
        editDistance "cat" "cut"

    assertEqual "kitten -> sitting" 3 $
        editDistance "kitten" "sitting"

    assertEqual "empty -> abc" 3 $
        editDistance "" "abc"

    assertEqual "same strings" 0 $
        editDistance "haskell" "haskell"

    assertEqual "flaw -> lawn" 2 $
        editDistance "flaw" "lawn"


    putStrLn "\nTask 4 tests:"
    finalState1 <- execStateT (movePlayer 3) initialGameState
    assertEqual "move to treasure position" Treasure (location finalState1)
    assertEqual "position after move" 3 (position finalState1)
    assertEqual "energy after move" 7 (energy finalState1)

    finalState2 <- execStateT (movePlayer 20) initialGameState
    assertEqual "move to goal" Goal (location finalState2)


main :: IO ()
main = runTests
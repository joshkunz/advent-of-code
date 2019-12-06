 module Main where

import qualified Data.Map.Strict as Map
import Data.Maybe
import Debug.Trace
import Control.Monad
import Data.List.Split
import Data.Function
import Data.Maybe
import Data.List

type Address = Int
type Memory = Map.Map Address Int
data Computer = CState
     { memory :: Memory
     , pc     :: Int
     , input  :: [Int]
     , output :: [Int]
     } | Halted deriving (Show)
data Val = Addr Address | Imm Int deriving (Ord,Eq,Show)
data BOp = Add | Mul deriving (Eq,Show)
data Instruction = BinOp BOp Val Val Address
                 | Input Address
                 | Output Val
                 | Halt deriving (Show) 

maybeToRight :: b -> Maybe a -> Either b a
maybeToRight def Nothing = Left def
maybeToRight _ (Just v) = Right v

loadPCRel :: Computer -> Int -> Maybe Int
loadPCRel Halted _ = Nothing
loadPCRel c offset = load c $ Addr $ (pc c) + offset

load :: Computer -> Val -> Maybe Int
load Halted _ = Nothing
load c (Addr addr) = Map.lookup addr (memory c)
load _ (Imm i) = Just i

store :: Computer -> Address -> Int -> Computer
store c addr val =
    setMem $ Map.alter (Just . const val) addr mem 
    where mem = memory c
          setMem newMem = c { memory = newMem }

nextInput :: Computer -> Maybe (Int, Computer)
nextInput c = 
    pure . flip (,) popC <=< listToMaybe . input $ c
    where popC = c { input = (tail . input $ c) }

-- This function takes a
-- 1. function that produces "Val" constructors based on the index
--    of the argument.
-- 2. a list of parameter values. It returns a list of values assuming they
--    have constructors, and the values exist.
nextVals :: (Int -> Maybe (Int -> Val)) -> [Maybe Int] -> [Maybe Val]
nextVals amode ps = 
    zipWith (<*>) modeL memoryL 
    where memoryL = (++ repeat Nothing) ps 
          modeL = amode <$> [0..]

binOp :: BOp -> [Val] -> Maybe Instruction
binOp op (a:b:(Addr dest):_) = Just $ BinOp op a b dest
binOp _ _ = Nothing

inputOp :: [Val] -> Maybe Instruction
inputOp ((Addr v):_) = Just $ Input v
inputOp _ = Nothing

outputOp :: [Val] -> Maybe Instruction
outputOp (v:_) = Just $ Output v
outputOp _ = Nothing

addrMode :: Int -> Maybe (Int -> Val)
addrMode 0 = Just $ Addr
addrMode 1 = Just $ Imm
addrMode _ = Nothing
        
parseInstruction :: Computer -> Int -> Either String Instruction
parseInstruction c i =
    tryParse $ case opNum i of
                1  -> binOp Add =<< args 3
                2  -> binOp Mul =<< args 3 
                3  -> inputOp =<< args 1 
                4  -> outputOp =<< args 1
                99 -> Just $ Halt
                _ -> Nothing
    where opNum = (`mod` 100)
          amode = addrMode . (`mod` 10) . div (i `div` 100) . (^) 10
          pcMemory = loadPCRel c <$> [1..]
          args n = sequence $ take n $ nextVals amode pcMemory
          tryParse = maybeToRight $ "failed to parse opcode " ++ (show i) ++ " at pc " ++ (show . pc $ c)

nextInstruction :: Computer -> Either String Instruction
nextInstruction Halted = Left "computer halted"
nextInstruction c = 
    case loadPCRel c 0 of
        Just v -> parseInstruction c v
        Nothing -> Left $ "unable to load opcode at pc " ++ (show . pc $ c)

sizeI :: Instruction -> Int
sizeI (BinOp _ _ _ _) = 4
sizeI (Input _) = 2
sizeI (Output _) = 2
sizeI Halt = 1

runI :: Computer -> Instruction -> Either String Computer
runI Halted _ = Right Halted
runI _ Halt = Right Halted
runI c (BinOp opT a1 a2 dest) =
    maybeToRight "invalid source address" $ op 
    where doOp Add = (+)
          doOp Mul = (*)
          op = do a <- load c a1
                  b <- load c a2
                  pure $ store c dest $ (doOp opT) a b
runI c (Input addr) =
    maybeToRight "no input items" $ op
    where op = do (inP, newC) <- nextInput c
                  pure $ store newC addr inP
runI c (Output val) = 
    maybeToRight "invalid memory addr on output" $ op
    where op = do v <- load c val 
                  pure $ c { output = ((v :) . output $ c) }

incPC :: Int -> Computer -> Computer
incPC _ Halted = Halted
incPC i c = c { pc = ((pc c) + i) }

step :: Computer -> Either String Computer
step Halted = Right $ Halted
step c = do isn <- nextInstruction c
            newC <- runI c isn
            return . incPC (sizeI isn) $ newC

isHalted :: Computer -> Bool
isHalted Halted = True
isHalted _ = False

runUntilHalt :: Computer -> Either String Computer
runUntilHalt c =
    rr c c  -- recursive run until halt.
    where rr prev Halted = Right prev
          rr _ c         = step c >>= rr c

parse :: String -> Computer
parse s =
    CState { memory = mem, pc = 0, input = [], output = []}
    where splitMem = map (read :: String -> Int) . splitOn ","
          mem = Map.fromList $ zip [0..] (splitMem s)
                            
fetch0 :: Computer -> Int
fetch0 = fromJust . (\c -> load c (Addr 0))

showC :: Computer -> String
showC c =
    intercalate "\n" $ [ "Mem0: " ++ (show $ fetch0 c)
                       , "Output:" ] ++ (reverse $ show <$> output c)

runC :: [Int] -> Computer -> Either String Computer
runC inputs c = runUntilHalt $ c { input = inputs }

fromEither :: Either a a -> a
fromEither (Left v) = v
fromEither (Right v) = v

main = interact $ fromEither . fmap showC . runC [1] . parse

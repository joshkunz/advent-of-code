module Main where
import qualified Data.Map.Strict as Map;
import Data.Maybe;
import Debug.Trace;
import Control.Monad;
import Data.List.Split;
import Data.Function;

newtype Addr = Addr Int deriving (Ord,Eq,Show)
type Memory = Map.Map Addr Int
data Computer = CState { memory :: Memory, pc :: Int } | Halted deriving (Show)
data Instruction = Add Addr Addr Addr | Mul Addr Addr Addr | Halt deriving (Show) 

loadPCRel :: Computer -> Int -> Maybe Int
loadPCRel Halted _ = Nothing
loadPCRel c offset = load c . Addr $ (pc c) + offset

load :: Computer -> Addr -> Maybe Int
load Halted _ = Nothing
load c addr = Map.lookup addr (memory c)

store :: Computer -> Addr -> Int -> Computer
store c addr val =
    setMem $ Map.alter (Just . const val) addr mem 
    where mem = memory c
          setMem newMem = c { memory = newMem }

parseInstruction :: [Int] -> Either String Instruction
parseInstruction (99:_) = Right $ Halt
parseInstruction [opcode, a1, a2, dest] | opcode == 1 = Right $ Add (Addr a1) (Addr a2) (Addr dest)
                                        | opcode == 2 = Right $ Mul (Addr a1) (Addr a2) (Addr dest)
                                        | otherwise = Left $ "invalid opcode " ++ (show opcode)
parseInstruction _ = Left "invalid instruction"

nextInstruction :: Computer -> Either String Instruction
nextInstruction Halted = Left "computer halted"
nextInstruction c = parseInstruction $ catMaybes $ map (loadPCRel c) [0..3]

maybeToRight :: b -> Maybe a -> Either b a
maybeToRight def Nothing = Left def
maybeToRight _ (Just v) = Right v

runI :: Computer -> Instruction -> Either String Computer
runI Halted _ = Right Halted
runI _ Halt = Right Halted
runI c (Add a1 a2 dest) =
    maybeToRight "invalid source address" $ op 
    where op = do a <- load c a1
                  b <- load c a2
                  Just $ store c dest (a + b) 
runI c (Mul a1 a2 dest) =
    maybeToRight "invalid source address" $ op 
    where op = do a <- load c a1
                  b <- load c a2
                  Just $ store c dest (a * b)

incPC :: Computer -> Computer
incPC Halted = Halted
incPC c = c { pc = ((pc c) + 4) }

step :: Computer -> Either String Computer
step Halted = Right $ Halted
step c = nextInstruction c >>= runI c >>= return . incPC

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
    CState { memory = mem, pc = 0 }
    where splitMem = map (read :: String -> Int) . splitOn ","
          mem = Map.fromList $ zip (map Addr [0..]) (splitMem s)
                            
setup :: Computer -> (Int, Int) -> Computer
setup c (a, b) = foldl (&) c [ \c -> store c (Addr 1) a
                             , \c -> store c (Addr 2) b ]

-- candidates generates all the two-element pairs of numbers between [0, max]
candidates :: Int -> [(Int, Int)]
candidates max =
    candidatesR max
    where candidatesR 0 = zip (repeat 0) [1..max]
          candidatesR n = candidatesR (n - 1) ++ (zip (repeat n) [1..max])

fetch0 :: Computer -> Int
fetch0 = fromJust . (\c -> load c (Addr 0))

search :: Int -> Computer -> Maybe (Int, Int)
search target c =
    searchR cds
    where maxAddr = (\x -> x - 1) . Map.size . memory
          cds = candidates $ maxAddr c
          searchR [] = Nothing
          searchR (x:xs) = case result of
                            (Left e) -> error e
                            (Right c) -> if c == target then Just x else searchR xs
                           where result = (liftM fetch0) $ runUntilHalt $ setup c x

final :: (Int, Int) -> Int
final (noun, verb) = (100 * noun) + verb

main = interact $ show . (liftM final) . search 19690720 . parse

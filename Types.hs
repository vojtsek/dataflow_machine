module Types where

import Data.Char

parse :: String -> [(String, a)] -> [(a, String)]
--dostane radek a pokud v nem najde retezec ze druheho parametru,
--vrati prislusny datovy konstruktor a zbytek radku
parse input ((pattern, value):rest) =
	if (take (length pattern) input) == pattern
		then [(value, drop (length pattern) input)]
		else parse input rest
parse _ [] = []

--definice typu registru
data RegType = Fal | Tr | No | Const | In
	deriving (Eq, Show)

--cteni typu registru
instance Read RegType where
	readsPrec _ input =
		parse input [("Fal", Fal), ("Tr", Tr), ("No", No), ("Const", Const), ("In", In)]

--mozne stavy registru
data StateType = Off | On | T | F
	deriving (Eq, Show)

--cteni stavu registru
instance Read StateType where
	readsPrec _ input =
		parse input [("off", Off), ("on", On), ("T", T), ("F", F)]

--definice registru
data Register = Register {
	number :: Int,
	value :: Double,
	t :: RegType,
	s1 :: StateType,
	s2 :: StateType,
	output :: Bool,
	enabled :: Bool
}

--zobrazeni registru
instance Show Register where
	show (Register n v t s1 s2 o e) = "r" ++ (show n) ++ ": " ++ (show v) -- ++ (show e)

--aritmeticka funkce
data InstrFunc = InstrFunc (Double -> Double -> Double) String

--logicka funkce
data BoolFunc = BoolFunc (Double -> Double -> Bool) String

--instrukcni bunka
data ICell = ICell {
	targets :: [Int],
	op1 :: Int,
	op2 :: Int,
	func :: InstrFunc
}
	deriving (Show)

--kontrolni bunka
data CCell = CCell {
	trgs :: [Int],
	o1 :: Int,
	o2 :: Int,
	fnc :: BoolFunc
}
	deriving (Show)

instance Show InstrFunc where
	show (InstrFunc _ str) = str

instance Show BoolFunc where
	show (BoolFunc _ str) = str

--extrakce funkce z bunky
processIFnc (InstrFunc fnc _) = fnc
processBFnc (BoolFunc fnc _) = fnc

getRegValue :: [(String, String)] -> String -> Double
--pokud v prvnim parametru najde registr s cislem z druheho parametru, vrati jeho hodnotu
--pouziva se pro nacteni vstupu
getRegValue ((n, val):rest) reg
	| n == reg = read val :: Double
	| otherwise = getRegValue rest reg
getRegValue [] reg = 0 

parseRegister :: [String] -> [(String, String)] -> Register
--z predane radky precte a vytvori registr, dostava korektni vstup
parseRegister [n, v, t, s1, s2, o, e] inp
	| t == "In" = Register (read n :: Int) (getRegValue inp n) (read t :: RegType) (read s1 :: StateType) (read s2 :: StateType) (read o :: Bool) (read e :: Bool)
	| otherwise = Register (read n :: Int) (read v :: Double) (read t :: RegType) (read s1 :: StateType) (read s2 :: StateType) (read o :: Bool) (read e :: Bool)

parseRegs :: [String] -> [(String, String)] -> [Register] -> [Register]
--projde seznam radek; ty, ktere zacinaji "Register" povazuje za korektni vstup a preda parseRegister
--vrati seznam vytvorenych registru
parseRegs (line:rest) inp regs
	| line == [] = parseRegs rest inp regs
	| (head (words line)) == "Register" = parseRegs rest inp (regs ++ [new_reg])
	| otherwise = parseRegs rest inp regs
	where new_reg = parseRegister (drop 1 (words line)) inp
parseRegs [] _ regs = regs

parseICell :: [String] -> [String] -> String -> ICell
--dostane definici nove instrukcni bunky a vytvori ji
--obsahuje seznam funkci, ktere se mapuji na symbolicka jmena
--pocita s korektnim vstupem
parseICell dests (o1:o2) f = ICell (map (\x -> read x :: Int) dests) (read o1 :: Int) (read (head o2) :: Int) (InstrFunc (mkFnc f) f)
	where mkFnc f
		| f == "plus" = \x -> \y -> (x + y)
		| f == "minus" = \x -> \y -> (x - y)
		| f == "times" = \x -> \y -> (x * y)
		| f == "div" = \x -> \y -> x / y
		|otherwise = \x -> \y -> x 

parseICells :: [String] -> [ICell] -> [ICell]
--prochazi vstup, pokud narazi na radku zacinajici "ICell"
--preda ji spolu s dvema nasledujicimi funkci parseICell
parseICells (l1:l2:l3:rest) cells
	| l1 == [] = parseICells (l2:l3:rest) cells
	| (head (words l1)) == "ICell" = parseICells rest (new_cell:cells)
	| otherwise = parseICells (l2:l3:rest) cells
	where new_cell = parseICell (drop 1 (words l1)) (words l2) l3
parseICells _ cells = cells

parseCCell :: [String] -> [String] -> String -> CCell
--dostane definici nove kontrolni bunky a vytvori ji
--obsahuje seznam funkci, ktere se mapuji na symbolicka jmena
--pocita s korektnim vstupem
parseCCell dests (o1:o2) f = CCell (map (\x -> read x :: Int) dests) (read o1 :: Int) (read (head o2) :: Int) (BoolFunc (mkFnc f) f)
	where mkFnc f
		| f == "lt" = \x -> \y -> (x < y)
		| f == "gt" = \x -> \y -> (x > y)
		| f == "ge" = \x -> \y -> (x >= y)
		| f == "le" = \x -> \y -> (x <= y)
		| f == "eq" = \x -> \y -> (x == y)
		| f == "ne" = \x -> \y -> (x /= y)
		|otherwise = \x -> \y -> True

parseCCells :: [String] -> [CCell] -> [CCell]
--prochazi vstup, pokud narazi na radku zacinajici "CCell"
--preda ji spolu s dvema nasledujicimi funkci parseCCell
parseCCells (l1:l2:l3:rest) cells
	| l1 == [] = parseCCells (l2:l3:rest) cells
	| (head (words l1)) == "CCell" = parseCCells rest (new_cell:cells)
	| otherwise = parseCCells (l2:l3:rest) cells
	where new_cell = parseCCell (drop 1 (words l1)) (words l2) l3
parseCCells _ cells = cells
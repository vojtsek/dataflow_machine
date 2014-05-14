module Core where

import Types
import System.Environment
import System.IO

printPrompt :: [String] -> IO ()
--vytiskne zbytek radku zadajiciho o vstup
printPrompt (w:ws) = do
	putStr (w ++ " ")
	printPrompt ws
printPrompt [] = do putStr "\n"

readInput :: [String] -> [(String, String)] -> IO [(String, String)]
--diky radkam programu zadajicich vstup nacte cislo ze standartniho vstupu
--vrati seznam nactenych cisel a registru, kterym jsou adresovany
readInput (l:ls) values
	| l == [] = readInput ls values
	| otherwise = do
	let line = words l
	if (head line) == "In"
		then do
			let num = head (drop 1 line)
			printPrompt (drop 2 line)
			input <- getLine
			let d = read input :: Double
			readInput ls ((num, input):values)
		else readInput ls values
readInput [] values = do return values

printRegs :: [Register] -> Bool -> IO ()
--vypise registry po radcich
printRegs (r:rs) out_only = do
	if out_only && (output r)
		then print r
		else do
			if (not out_only)
				then print r
				else return ()
	printRegs rs out_only
printRegs [] _ = return ()

getOp :: Int -> [Register] -> Maybe Double
--ze seznamu registru vezme hodnotu toho, ktery ma cislo z prvniho parametru
--registr musi byt povolen
getOp addr ((Register a val _ _ _ _ True):rs)
	| a == addr = Just val
	| otherwise = getOp addr rs

getOp addr (r:rs) = getOp addr rs

getOp _ [] = Nothing

{--initRegisters ((Register a val In s1 s2 out en):rs) regs = do
	d <- readIODouble
	initRegisters rs (regs ++ (Register a d In s1 s2 out en))
initRegisters [] regs = regs
--}

processIC :: ICell -> [Register] -> Maybe (Double, [Int])
--pokud jsou oba vstupy dane bunky povolene, vykona nad nimi operaci
--vrati vysledek a seznam ovlivnenych registru
processIC c regs = case oper1 of
	Nothing -> Nothing
	Just v1 ->
		case oper2 of
			Nothing -> Nothing
			Just v2 -> Just (((processIFnc (func c)) v1 v2), targets c)
	where
		oper1 = (getOp (op1 c) regs)
		oper2 = (getOp (op2 c) regs)

processCC :: CCell -> [Register] -> Maybe (Bool, [Int])
--pokud jsou oba vstupy dane bunky povolene, vykona nad nimi operaci
--vrati vysledek a seznam ovlivnenych registru
processCC c regs = case oper1 of
	Nothing -> Nothing
	Just v1 ->
		case oper2 of
			Nothing -> Nothing
			Just v2 -> Just (((processBFnc (fnc c)) v1 v2), trgs c)
	where
		oper1 = (getOp (o1 c) regs)
		oper2 = (getOp (o2 c) regs)


processICells :: [ICell] -> [Maybe (Double, [Int])] -> [Register] -> [Maybe (Double, [Int])]
--projde seznam bunek a kazdou se pokusi provest
--vrati seznam vysledku a ovlivnenych cilu
processICells [] processed regs = processed
processICells (c:cs) processed regs = processICells cs ((processIC c regs):processed) regs

processCCells :: [CCell] -> [Maybe (Bool, [Int])] -> [Register] -> [Maybe (Bool, [Int])]
--projde seznam bunek a kazdou se pokusi provest
--vrati seznam vysledku a ovlivnenych cilu
processCCells [] processed regs = processed
processCCells (c:cs) processed regs = processCCells cs ((processCC c regs):processed) (enableRegisters (updateRegistersCtrl regs ((processCC c regs):processed) [] ) [])

updateRegisters :: [Register] -> [Maybe (Double, [Int])] -> [Register] -> [Register]
--projde registry a ty, ktere jsou ovlivneny vysledkem nektere aritmeticke operace upravi
--vrati seznam aktualizovanych registru
updateRegisters	(r:rs) new new_regs = updateRegisters rs new (new_regs ++ [updateReg r new])
updateRegisters [] _ new_regs = new_regs

updateRegistersCtrl :: [Register] -> [Maybe (Bool, [Int])] -> [Register] -> [Register]
--projde registry a ty, ktere jsou ovlivneny vysledkem nektere booleovske operace upravi
--vrati seznam aktualizovanych registru
updateRegistersCtrl	(r:rs) new new_regs = updateRegistersCtrl rs new (new_regs ++ [updateRegCtrl r new])
updateRegistersCtrl [] _ new_regs = new_regs

updateReg :: Register -> [Maybe (Double, [Int])] -> Register
--pokud je registr v seznamu cilu ovlivnenych aritmetickou operaci, je upraven
-- vraci upraveny registr
updateReg r [] = r
updateReg r (Nothing:xs) = updateReg r xs
updateReg r ((Just (v, affected)):xs)
	| elem (number r) affected = updateReg (Register (number r) v (t r) (s1 r) On (output r) (enabled r)) xs
	| otherwise = updateReg r xs

updateRegCtrl :: Register -> [Maybe (Bool, [Int])] -> Register
--pokud je registr v seznamu cilu ovlivnenych booleovskou operaci, je upraven
-- vraci upraveny registr
updateRegCtrl r [] = r
updateRegCtrl r (Nothing:xs) = updateRegCtrl r xs
updateRegCtrl r ((Just (v, affected)):xs)
	| elem (number r) affected = 
		if v
			then updateRegCtrl (Register (number r) (value r) (t r) T (s2 r) (output r) (enabled r)) xs
			else updateRegCtrl (Register (number r) (value r) (t r) F (s2 r) (output r) (enabled r)) xs
	| otherwise = updateRegCtrl r xs

enableRegisters :: [Register] -> [Register] -> [Register]
--na zaklade stavu registru je povoli nebo zakaze
--vraci seznam upravenych registru
enableRegisters [] regs = regs
enableRegisters ((Register n v No s1 On out _):rs) regs = enableRegisters rs (regs ++ [Register n v No s1 On out True])
enableRegisters ((Register n v Tr T On out _):rs) regs = enableRegisters rs (regs ++ [Register n v Tr T On out True])
enableRegisters ((Register n v Tr s1 s2 out _):rs) regs = enableRegisters rs (regs ++ [Register n v Tr s1 s2 out False])
enableRegisters ((Register n v In F s2 out _):rs) regs = enableRegisters rs (regs ++ [Register n v In F s2 out False])
enableRegisters ((Register n v Fal F On out _):rs) regs = enableRegisters rs (regs ++ [Register n v Fal F On out True])
enableRegisters ((Register n v Fal s1 s2 out _):rs) regs = enableRegisters rs (regs ++ [Register n v Fal s1 s2 out False])
enableRegisters ((Register n v t s1 s2 out en):rs) regs = enableRegisters rs (regs ++ [Register n v t s1 s2 out en])

existWaiting :: [Register] -> Bool
--vrati True prave kdyz existuje nepovoleny vystupni registr
existWaiting ((Register _ _ _ _ _ True False):rs) = True
existWaiting (r:rs) = existWaiting rs
existWaiting [] = False
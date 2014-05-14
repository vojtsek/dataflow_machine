module Main where
import Types
import Core
import System.Environment
import System.IO

mainloop :: [ICell] -> [CCell] -> [Register] -> IO ()
--mainloop simuluje vypocet programu tim,
--ze postupne iteruje stridave aplikaci booleovskych a aritmetickych funkci
mainloop icells ccells registers = do
	--getLine
	let updated_ctrl = updateRegistersCtrl (registers) (processCCells ccells [] registers) []
	let enabled_regs1 = enableRegisters	updated_ctrl []
	--printRegs enabled_regs1 False
	if (not (existWaiting enabled_regs1))
		then do
			let final = updateRegisters (enabled_regs1) (processICells icells [] enabled_regs1) []
			printRegs final True
			return ()
		else do
			let updated = updateRegisters (enableRegisters updated_ctrl []) (processICells icells [] enabled_regs1) []
			let enabled_regs2 = enableRegisters updated []
			--print "--"
			--printRegs enabled_regs2 False
			if (existWaiting enabled_regs2)
				then mainloop icells ccells enabled_regs2
				else do
					printRegs enabled_regs2 True
					return ()

main :: IO ()
main = do
	fname <- getArgs
	file_content <- readFile (head fname)
	let splitted = lines file_content
	input <- readInput splitted []
	let registers = parseRegs splitted input []
	let icells = parseICells splitted []
	let ccells = parseCCells splitted []
	mainloop icells ccells registers
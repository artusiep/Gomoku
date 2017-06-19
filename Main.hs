import Gomoku
import Text.Read
import System.Random

default_board = initBoard  


main = do
	putStrLn "1. Gra z graczem \n2.Gra z A.I."
	x <- getLine
	if x == "1"
		then do
			loopPlayers default_board Cross 1
	else if x == "2"
		then do		
			loopAI default_board Cross 2
	else do  
		main
		
loopPlayers :: Board -> Element -> Int-> IO()
loopPlayers board color tryb = do 
		putStrLn (show board)
		if color == Cross then putStrLn "Ruch Krzyżyka" else putStrLn "Ruch Kółka"
		putStrLn "Wiersz: "
		ystr <- getLine
		putStrLn "Kolumna: "
		xstr <- getLine
		let y = (read ystr::Int) - 1 
		let x = (read xstr::Int) - 1
		check board y x color tryb

loopAI :: Board -> Element -> Int -> IO()
loopAI board color tryb = do
		putStrLn (show board)
		if color == Cross
			then do
				putStrLn "Twój ruch"
				putStrLn "Wiersz: "
				xstr <- getLine
				putStrLn "Kolumna: "
				ystr <- getLine
				let y = (read ystr::Int) - 1 
				let x = (read xstr::Int) - 1
				check board x y color tryb
  		else do
			x <- randomRIO(1,19 :: Int)
			y <- randomRIO(1,19 :: Int)
			check board x y color tryb


check :: Board -> Int -> Int -> Element -> Int -> IO()
check board x y color tryb = do
	if (x<19) && (y<19) && (x>=0) && (y>=0)
		then do
			if positionIsEmpty board x y 
				then do  
					let newboard = updateBoard board color x y
					if checkFive(checkList (elements newboard) x y) color 
						then do 
							end color newboard
					else do
						let newcolor = (if color == Circle then Cross else Circle)
						if tryb == 1  then (loopPlayers newboard newcolor tryb) else (loopAI newboard newcolor tryb)
			else do
				putStrLn "Pozycja zajęta"
				if tryb == 1  then (loopPlayers board color tryb) else (loopAI board color tryb)
	else do
		putStrLn "Nie właściwa wartość x lub y"
		if tryb == 1  then (loopPlayers board color tryb) else (loopAI board color tryb)

end :: Element -> Board -> IO()
end color board = do
	if color == Cross then putStrLn "Wygrał Krzyżyk" else putStrLn "Wygrało Kółko"
	putStrLn (show board)
	putStrLn "Jeszcze raz? (tak/nie)"
	x <- getLine
	if(x == "tak") then main
	else do putStrLn "To Pa"
	

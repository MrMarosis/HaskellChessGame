module Chess where
    import Moves
    import ChessBoard
    import BoardState

    x = readBoard initialBoardStr
    y = flattenBoard x 
    z = (Piece White Queen)
    z1 = Just z
    d = addSquare z1 12 y 
    e = groupToBoard d    
    
    main = play $ flattenBoard $ readBoard initialBoardStr
    
    rightMove :: [Square] -> PColor -> IO [Square]
    rightMove board c = do
        putStrLn "enter value of your piece: "
        s1 <- getLine
        let a = calculateIndex (s1 !! 0)(s1 !! 1)
        if a>63 || a<0 then rightMove board c else do
            putStrLn "enter value for destination: " 
            d1<- getLine
            let b = calculateIndex (d1 !! 0)(d1 !! 1)
            let board1 = makeMovewithColor c a b board
            if b>63 || b<0 then 
                rightMove board c 
                else do
                if isCheck c 0 board then rightMove board c
                else return board1 
                
    play board = do
        putStrLn  (showBoard . groupToBoard $ board)
        board1 <- rightMove board White
        putStr $ showBoard $ groupToBoard board1 
        if (isMat Black 0 board1) then return True
        else do
            board2 <- rightMove board1 Black
            if isMat White 0 board2 then return False
            else play board2
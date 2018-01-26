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
    
    main = do{
        play $ readBoard initialBoardStr    
        } 
    
    
    play board = do
        putStr $ showBoard board 
        putStrLn "enter value of your piece: "
        s1 <- getLine
        let a = calculateIndex (s1 !! 0)(s1 !! 1)
        putStrLn "enter value for destination: " 
        d1<- getLine
        let b = calculateIndex (d1 !! 0)(d1 !! 1)
        let board1 = makeMovewithColor White a b $ flattenBoard board
        putStr $ showBoard $ groupToBoard board1 
        putStrLn "enter value of your piece: "
        s2 <- getLine
        let a = calculateIndex (s2 !! 0)(s2 !! 1)
        putStrLn "enter value for destination: " 
        d2<- getLine
        let b = calculateIndex (d2 !! 0)(d2 !! 1)
        let board2 = makeMovewithColor Black a b board1
        play $ groupToBoard board2


        -- makeMove s b (flattenBoard $ readBoard initialBoardStr)
        
       
    
        
    --play :: Either Bool Board->Either Bool Board

    --data Player = AI | Human
    

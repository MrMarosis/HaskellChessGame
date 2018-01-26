module Chess where
    import Moves
    import ChessBoard
    import BoardState



    


    play :: IO ()
    play = do 
        putStrLn "enter value of your piece: "
        s <- getLine
        let a = calculateIndex (s !! 0)(s !! 1)
        putStrLn "enter value for destination: " 
        d<- getLine
        let b = calculateIndex (d !! 0)(d !! 1)
        --makeMove s b (flattenBoard $ readBoard initialBoardStr)
        
       
    
        
    --play :: Either Bool Board->Either Bool Board
    --play (Left a) = (Left a) 
    

    x = readBoard initialBoardStr
    y = flattenBoard x 
    z = (Piece White Queen)
    z1 = Just z
    d = addSquare z1 12 y 
    e = groupToBoard d

    --data Player = AI | Human
    
    



    rookCheck :: PColor->Int->[Square]->Int->(Int->Int->Int)->Int->[Int]                        
    rookCheck c x b dx f limit= if (f x 8)==limit then 
                        [x]
                    else
                        if not $ isPeace ( getSquare (x+dx) b) then 
                            [x]++rookDirection c (x+dx) b dx f limit
                        else
                            if isKingOrFriend  c (x+dx) b  then 
                                 [x]
                            else
                                [x,x+dx]

    

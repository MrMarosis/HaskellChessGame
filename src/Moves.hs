module Moves where 
    import BoardState
    import ChessBoard
    
    kingDirection :: PColor->Int->[Square]->[Int]
    kingDirection c x b= filter (isNotKingOrFriend c b)  (map (validate c x ) tab)
        where 
            tab =[-9, -8, -7, -1, 0, 1, 7, 8, 9]
            validate c x y = if abs((mod x 8) - (mod (x+y) 8)) > 1 || abs((div x 8) - (div (x+y) 8)) > 1  then x
                                        else 
                                            if x+y > -1 && x+y<66 then
                                                x+y
                                            else
                                                x
    
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
            

    
    --w pierwszym kroku sprawdzam zmianę w zakresie kolumn
    --[-9,-8,7-1,0,1,7,8,9]

    knightDirection :: PColor->Int->[Square]->[Int]
    knightDirection c x b = filter (isNotKingOrFriend c b) (map (validate c x) tab)
                                where 
                                    tab = [-17,-15, -10,-6, 6,10, 15,17]
                                    validate c x y = if x+y > -1 && x+y<66 then 
                                        if (abs((mod x 8) - (mod (x+y) 8)) == 1 && abs((div x 8) - (div (x+y) 8)) == 2) 
                                        || (abs((mod x 8) - (mod (x+y) 8)) == 2 && abs((div x 8) - (div (x+y) 8)) == 1) then x+y
                                            else 
                                                x
                                        else
                                            x

    pawnDirection :: PColor->Int->[Square]->[Int]
    pawnDirection c x b = if c==White then
                                (validateforwardW (x-8) b)++(validateslantW x (-7) b)++(validateslantW x (-9) b)
                            else if c==Black then
                                (validateforwardB (x+8) b)++(validateslantB x 7 b)++(validateslantB x 9 b)
                                else if (getColor $ getSquare x b)==White then [x+j|j<-[-7,-9], diff x j]
                                    else [x+j|j<-[7,9], diff x j]
                                where 
                                    validateforwardW x b = if x>=0 && isPeace (getSquare x b) == False then [x] else []
                                    validateslantW x y b = if x>=0 && False==(isKingOrFriend c (x+y) b) && isPeace (getSquare (x+y) b)
                                        && diff x y then [x+y] else []
                                    validateforwardB x b = if x<=65 && isPeace (getSquare x b) == False then [x] else []
                                    validateslantB x y b = if x<65 && False==(isKingOrFriend c (x+y) b) && isPeace (getSquare (x+y) b) 
                                        && diff x y then [x+y] else []
                                    diff x y = abs((mod x 8) - (mod (x+y) 8))==1
    
    --rook left dx=-1 f = mod x 8 limit = 0
    --rook right dx=1 f=mod x 8 limit = 7
    --rook down dx=9 f = div x 8 limit = 7
    --rook up dx=-9 f = dix x 8 limit=0
    -- np w l dół z pola 23 białą wieżą: rookDirection White 23 y 8 (\x y-> mod x y) 0
    
    rookMoves :: PColor->Int->[Square]->[Int]
    rookMoves c x b =   rookDirection c x b (-1) (\x y-> mod x y) 0 ++ 
                        rookDirection c x b (1) (\x y-> mod x y) 7 ++
                        rookDirection c x b (8) (\x y-> div x y) 7 ++
                        rookDirection c x b (-8) (\x y-> div x y) 0 ++
                        []
                    

    rookDirection :: PColor->Int->[Square]->Int->(Int->Int->Int)->Int->[Int]                        
    rookDirection c x b dx f limit= if (f x 8)==limit then 
                        [x]
                    else
                        if not $ isPeace ( getSquare (x+dx) b) then 
                            [x]++rookDirection c (x+dx) b dx f limit
                        else
                            if isKingOrFriend  c (x+dx) b  then 
                                 [x]
                            else
                                [x,x+dx]

    --bishop down left dx = 7 lim1 = 7 lim2 = 0
    --bishop down right dx = 9 lim1 = 7 lim2 = 7
    --bishop up right dx = -7 lim1 = 0 lim2 = 7
    --bishop up left dx = -9 lim1 = 0 lim2 = 0
    
    bishopMoves :: PColor->Int->[Square]->[Int]
    bishopMoves c x b = bishopDirection c x b 7 7 0 ++ 
                        bishopDirection c x b 9 7 7 ++
                        bishopDirection c x b (-7) 0 7 ++
                        bishopDirection c x b (-9) 0 0 ++
                        []

    bishopDirection :: PColor->Int->[Square]->Int->Int->Int->[Int]
    bishopDirection c x b dx lim1 lim2 = if div x 8==lim1 || mod x 8 ==lim2 then
                                        [x]
                                    else
                                        if not $ isPeace ( getSquare (x+dx) b) then 
                                            [x]++bishopDirection c (x+dx) b dx lim1 lim2
                                    else
                                        if isKingOrFriend  c (x+dx) b  then 
                                            [x]
                                        else
                                        [x,x+dx]


    queenMoves :: PColor->Int->[Square]->[Int]
    queenMoves c x b = bishopMoves c x b ++ rookMoves c x b
    
    
    possibleMovesForFigure:: Maybe Piece->Int->[Square]-> [Int]
    possibleMovesForFigure Nothing _ _ = []
    possibleMovesForFigure (Just (Piece c Pawn)) x b = pawnDirection c x b
    possibleMovesForFigure (Just (Piece c Rook)) x b = rookMoves c x b
    possibleMovesForFigure (Just (Piece c Knight)) x b = knightDirection c x b
    possibleMovesForFigure (Just (Piece c Bishop)) x b = bishopMoves c x b
    possibleMovesForFigure (Just (Piece c Queen)) x b = queenMoves c x b
    possibleMovesForFigure (Just (Piece c King)) x b = kingDirection c x b

    possibleChecksForFigure:: Maybe Piece->Int->[Square]-> [Int]
    possibleChecksForFigure Nothing _ _ = []
    possibleChecksForFigure (Just (Piece _ Pawn)) x b = pawnDirection Check x b
    possibleChecksForFigure (Just (Piece _ Rook)) x b = rookMoves Check x b
    possibleChecksForFigure (Just (Piece _ Knight)) x b = knightDirection Check x b
    possibleChecksForFigure (Just (Piece _ Bishop)) x b = bishopMoves Check x b
    possibleChecksForFigure (Just (Piece _ Queen)) x b = queenMoves Check x b
    possibleChecksForFigure (Just (Piece _ King)) x b = kingDirection Check x b
    
    checkMove::[Square]->Int->Int->Bool
    checkMove b s d =  inList d possibleMoves 
        where possibleMoves = possibleMovesForFigure  (readPiece $ showSquare $  getSquare s b) s b 

    makeMove :: Int -> Int ->[Square]-> [Square]
    makeMove s d b=
            if checkMove b s d then 
                addSquare Nothing s $ addSquare (getSquare s b) d b 
                else b
    
    makeMovewithColor::PColor->Int->Int->[Square]->[Square]
    makeMovewithColor c s d b=
        if (getColor $ getSquare s b) /= c then b
            else makeMove s d b

    concatMoves:: PColor->[Square]->Int->[Int]
    concatMoves _ _ 64 = []
    concatMoves c b x = if  getColor (getSquare x b) == c then
                (possibleChecksForFigure (getSquare x b) x b)++concatMoves c b (x+1)
                else 
                    concatMoves c b (x+1)

    isCheck::PColor->Int->[Square]->Bool
    isCheck _ 64 _ = False                   
    isCheck c x b = if (isKing (getSquare x b)) && getColor(getSquare x b) == c then 
                        if c==Black then 
                            inList x $ concatMoves White b 0
                        else 
                            inList x $ concatMoves Black b 0
                    else isCheck c (x+1) b

    anyIsNotInList::[Int]->[Int]->Bool
    anyIsNotInList [] y = False
    anyIsNotInList (x:xs) (y) = if inList x y 
                                    then anyIsNotInList xs y 
                                else
                                    True

    isMat:: PColor->Int->[Square]->Bool
    isMat _ 64 _ = False
    isMat c x b = if (isKing (getSquare x b)) && getColor(getSquare x b) == c then 
                        if c==Black then 
                            anyIsNotInList (possibleMovesForFigure (getSquare x b) x b) (concatMoves White b 0)
                        else 
                            anyIsNotInList (possibleMovesForFigure (getSquare x b) x b) (concatMoves Black b 0)
                    else isMat c (x+1) b    
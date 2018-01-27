module BoardState where
    import ChessBoard
    import Data.Char

    calculateIndex :: Char->Char-> Int
    calculateIndex x y = (ord x - ord 'A') + abs(ord y - ord '8')*8

    inList::Int->[Int]->Bool
    inList x [] = False
    inList x (y:ys) = if x==y then True
                        else inList x ys  
    
    isRepeating::Int->[Int]->Bool
    isRepeating x [] = False
    isRepeating x (y:ys) = if x==y then inList x ys else isRepeating x ys 

    isKing::Square->Bool
    isKing (Just (Piece _ King))=True
    isKing _ = False
    
    isKingOrFriend:: PColor->Int->[Square]->Bool
    isKingOrFriend c x b= if  (getColor $ getSquare x b) == Check  then False else
                                   if not $ isPeace $ getSquare x b then False
                                    else
                                        if isKing $ getSquare x b  then True
                                        else 
                                            if c == ( getColor $ getSquare x b ) then True
                                            else False
    
    isNotKingOrFriend:: PColor->[Square]->Int->Bool
    isNotKingOrFriend c b x= not $ isKingOrFriend c x b
    
    getColor::Square->PColor
    getColor (Just (Piece White _)) = White
    getColor (Just (Piece Black _)) = Black
    getColor _ = Check
    
    isPeace::Square->Bool
    isPeace (Nothing) = False
    isPeace (Just (Piece _ _)) = True

    addSquare :: Square -> Int -> [Square] -> [Square]
    addSquare sq 0 (xs) = sq:tail xs
    addSquare sq i (x:xs) = x:addSquare sq (i-1) xs  

    getSquare :: Int ->[Square]-> Square
    getSquare 0 (x:xs) = x
    getSquare i (x:xs) = getSquare (i-1) (xs) 


    --isCheck x c b = x inList list
    --       where 
                --list = foldl (:) [] tab
                

    
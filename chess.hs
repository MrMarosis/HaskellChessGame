module Chess where
    import Data.Char
    --import ChessBoard

    type Board = [[Square]]
    
    initialBoardStr::String
    initialBoardStr = unlines ["rnbqkbnr"
                                ,"pppppppp" --Black
                                ,"        "
                                ,"        "
                                ,"        "
                                ,"        "
                                ,"PPPPPPPP"
                                ,"RNBQKBNR"--White
                                ]
    
    readBoard :: String -> Board
    readBoard = map readRow . lines
            where readRow = map readSquare
    
    showBoard :: Board -> String
    showBoard = unlines . map showRow
            where showRow = map showSquare
    
    type Square = Maybe Piece
    
    data Piece = Piece PColor PType deriving(Show)
    data PColor = White | Black deriving(Show,Eq)
    data PType = Pawn | Knight | Bishop | Rook | Queen | King deriving(Show)
    
    showSquare :: Square->Char
    showSquare = maybe ' ' showPiece
    
    readSquare :: Char->Square
    readSquare = readPiece
    
    showPiece :: Piece -> Char
    showPiece (Piece White Pawn) = 'P'
    showPiece (Piece White Knight) = 'N'
    showPiece (Piece White Bishop) = 'B'
    showPiece (Piece White Rook) = 'R'
    showPiece (Piece White Queen) = 'Q'
    showPiece (Piece White King) = 'K'
    showPiece (Piece Black Pawn) = 'p'
    showPiece (Piece Black Knight) = 'n'
    showPiece (Piece Black Bishop) = 'b'
    showPiece (Piece Black Rook) = 'r'
    showPiece (Piece Black Queen) = 'q'
    showPiece (Piece Black King) = 'k'
    
    typeList :: [(Char, PType)]
    typeList = [('p',Pawn),
                ('k',Knight),
                ('b',Bishop),
                ('r',Rook),
                ('q',Queen),
                ('k',King)]
    
    readPiece :: Char -> Maybe Piece
    readPiece 'P' = Just (Piece White Pawn) 
    readPiece 'N' = Just (Piece White Knight)
    readPiece 'B' = Just (Piece White Bishop)
    readPiece 'R' = Just (Piece White Rook)
    readPiece 'Q' = Just (Piece White Queen)
    readPiece 'K' = Just (Piece White King) 
    readPiece 'p' = Just (Piece Black Pawn) 
    readPiece 'n' = Just (Piece Black Knight)
    readPiece 'b' = Just (Piece Black Bishop)
    readPiece 'r' = Just (Piece Black Rook)
    readPiece 'q' = Just (Piece Black Queen)
    readPiece 'k' = Just (Piece Black King) 
    readPiece _ = Nothing
        
    flattenBoard :: [[Square]] -> [Square]
    flattenBoard [] = []
    flattenBoard (x:xs) = x++(flattenBoard xs)

    group :: Int -> [a] -> [[a]]
    group _ [] = []
    group n l = (take n l) : (group n (drop n l))

    groupToBoard :: [Square]->[[Square]]
    groupToBoard = group 8

    calculateMove :: String -> String
    calculateMove = undefined
    
    addSquare :: Square -> Int -> [Square] -> [Square]
    addSquare sq 0 (xs) = sq:tail xs
    addSquare sq i (x:xs) = x:addSquare sq (i-1) xs  

    getSquare :: Int ->[Square]-> Square
    getSquare 0 (x:xs) = x
    getSquare i (x:xs) = getSquare (i-1) (xs) 

    makeMove :: Int -> Int ->[Square]-> [Square]
    makeMove x y b=do
        addSquare Nothing x $ addSquare (getSquare x b) y b

    calculateIndex :: Char->Char-> Int
    calculateIndex x y = (ord x - ord 'A') + abs(ord y - ord '8')*8

    main :: IO ()
    main = do 
        writeFile "Board.txt" initialBoardStr
        
    play :: Either Bool Board->Either Bool Board
    play (Left a) = (Left a) 
    

    x = readBoard initialBoardStr
    y = flattenBoard x
    z = (Piece White Queen)
    z1 = Just z
    d = addSquare z1 12 y 
    e = groupToBoard d

    data Player = AI | Human

    validateMove :: Player->Int->Int->[Square]->Bool
    validateMove x y b = undefined
    

    



    --filter (\x2 x3  isKingOrFriend c x2 x3)  (map (validate c x ) tab) b
    -- \x t -> [x+dx|dx<-t,(isKingOrFriend c x+dx b)==False]
    
    -- \x tab -> [x+dx|dx<-tab,(isColorOrKing dx b)==false]
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
            

    
    --w pierwszym kroku sprawdzam zmianę w zakresie kolumn
    --[-9,-8,7-1,0,1,7,8,9]

{-
    kingDirection :: PColor->Int->[Square]->[Int]
    kingDirection c x b= map (validate c x b) tab
        where 
            tab = [-9, -8, -7, -1, 0, 1, 7, 8, 9]
            validate c x b dx = if abs((mod x 8) - (mod (x+dx) 8)) > 1 || abs((div x 8) - (div (x+dx) 8)) > 1  then x
                                        else 
                                            if x+dx > -1 && x+dx<66 then
                                                if isKing $ getSquare (x+dx) b then 
                                                    x
                                                else 
                                                    if (getColor $ getSquare (x+dx) b) == c then 
                                                       x
                                                    else 
                                                        x+dx
                                            else
                                                x
-}  --filter (isNotKingOrFriend c b)
    knightDirection :: PColor->Int->[Square]->[Int]
    knightDirection c x b =  (map (validate c x) tab)
                                where 
                                    tab = [-17,-15, -10,-6, 6,10, 15,17]
                                    validate c x y = if abs((mod x 8) - (mod (x+y) 8)) > 1 || abs((div x 8) - (div (x+y) 8)) > 1  then x
                                    else 
                                        if x+y > -1 && x+y<66 then
                                            x+y
                                        else
                                            x

    --knightDirection = commonDirection c x  [-17,-15, -10,-6, 6,10, 15,17]

    pawnDirection :: PColor->Int->[Square]->[Int]
    pawnDirection c x b = if c== White then
                                (validateforwardW (x-8) b)++(validateslantW (x-7) b)++(validateslantW (x-9) b)
                            else
                                (validateforwardW (x+8) b)++(validateslantW (x+7) b)++(validateslantW (x+9) b)
                                where 
                                    validateforwardW x b = if x>=0 && isPeace (getSquare x b) == False then [x] else []
                                    validateslantW x b = if x>=0 && (isKingOrFriend c x b)==False && isPeace (getSquare x b) then [x] else []
                                    validateforwardB x b = if x<=65 && isPeace (getSquare x b) == False then [x] else []
                                    validateslantB x b = if x<65 && (isKingOrFriend c x b)==False && isPeace (getSquare x b) == False then [x] else []

    isKing::Square->Bool
    isKing (Just (Piece _ King))=True
    isKing _ = False

    isKingOrFriend:: PColor->Int->[Square]->Bool
    isKingOrFriend c x b= if not $ isPeace $ getSquare x b then False
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
    getColor _ = White

    isPeace::Square->Bool
    isPeace (Nothing) = False
    isPeace (Just (Piece _ _)) = True

    --rook left dx=-1 f = mod x 8 limit = 0
    --rook right dx=1 f=mod x 8 limit = 7
    --rook down dx=9 f = div x 8 limit = 7
    --rook up dx=-9 f = dix x 8 limit=0
    -- np w l dół z pola 23 białą wieżą: rookDirection White 23 y 8 (\x y-> mod x y) 0
    
    rookMoves :: PColor->Int->[Square]->[Int]
    rookMoves c x b =   rookDirection c x b (-1) (\x y-> mod x y) 0 ++ 
                        rookDirection c x b (1) (\x y-> mod x y) 7 ++
                        rookDirection c x b (8) (\x y-> mod x y) 7 ++
                        rookDirection c x b (-8) (\x y-> mod x y) 0 ++
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

    bishopDownLeft :: PColor->Int->[Square]->Int
    bishopDownLeft c x b= if div x 8==0 || mod x 8 ==0 then
                            x
                        else
                            if not $ isPeace ( getSquare (x+7) b) then 
                                bishopDownLeft c (x+7) b
                            else
                                if isKing $ getSquare (x+7) b then 
                                    x
                               else 
                                   if (getColor  $ getSquare (x+7) b) == c then 
                                        x
                                   else
                                      x+9
    
    inList::Int->[Int]->Bool
    inList x [] = False
    inList x (y:ys) = if x==y then True
                        else inList x ys  
    
    isRepeating::Int->[Int]->Bool
    isRepeating x [] = False
    isRepeating x (y:ys) = if x==y then inList x ys else isRepeating x ys 

    --filter isRepeating

    queenMoves :: PColor->Int->[Square]->[Int]
    queenMoves c x b = bishopMoves c x b ++ rookMoves c x b
    
    valiadateKingCheck :: Player->[Square]->Bool
    valiadateKingCheck = undefined --
    validateMoveKing :: Player->Int->Int->[Square]->Bool
    validateMoveKing p x y sx = undefined


    validateMoveRook :: Player->Int->Int->[Square]->Bool
    validateMoveRook = undefined -- [i/8,i/8+1,...,i/8+7][i%8,i%8+1,..,i%8+7] Trochę inaczej

    validateMoveKnight :: Player->Int->Int->[Square]->Bool
    validateMoveKnight = undefined --(+) currIndex -17,-14,-10,-6,+6,+10,+14,+17
    validateMoveBishop :: Player->Int->Int->[Square]->Bool
    validateMoveBishop = undefined -- (+) currIndex [-63-54,...,+54+63] ,[...]
    validateMoveQueen :: Player->Int->Int->[Square]->Bool
    validateMoveQueen p x y b = validateMoveRook p x y b  && validateMoveBishop p x y b
    validateMovePawn :: Player->Int->Int->[Square]->Bool --(+) currindex [+1, i/8+8+-1] 
    validateMovePawn = undefined
    
    
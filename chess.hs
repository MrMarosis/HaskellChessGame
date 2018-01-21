module Chess where
    import Data.Char

    type Board = [[Square]]
    
    initialBoardStr::String
    initialBoardStr = unlines ["rnbqkbnr"
                                ,"pppppppp"
                                ,"        "
                                ,"        "
                                ,"        "
                                ,"        "
                                ,"PPPPPPPP"
                                ,"RNBQKBNR"
                                ]
    
    readBoard :: String -> Board
    readBoard = map readRow . lines
            where readRow = map readSquare
    
    showBoard :: Board -> String
    showBoard = unlines . map showRow
            where showRow = map showSquare
    
    type Square = Maybe Piece
    
    data Piece = Piece PColor PType deriving(Show)
    data PColor = White | Black deriving(Show)
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
    readPiece 'p' = Just (Piece White Pawn) 
    readPiece 'n' = Just (Piece White Knight)
    readPiece 'b' = Just (Piece White Bishop)
    readPiece 'r' = Just (Piece White Rook)
    readPiece 'q' = Just (Piece White Queen)
    readPiece 'k' = Just (Piece White King) 
    readPiece _ = Nothing
    
    type Move = String 
    
    makeMove :: String -> String
    makeMove = undefined
    
    calculateMove :: String -> String
    calculateMove = undefined
    
    --changeBoard :: Square -> Int -> Board -> Board


    changeRow :: Square->Int->[Square]->[Square]
    changeRow sq 0 xs = sq: tail xs 
    changeRow sq i (x:xs) = x:changeRow sq (i-1) xs

    main :: IO ()
    main = do 
        writeFile "Board.txt" initialBoardStr
        
    calculateIndex :: String -> Int
    calculateIndex (x:y:xs) = ord x - ord 'A' + ord y - ord '0'
    
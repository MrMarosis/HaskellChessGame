module Chess where

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
type Square = Maybe Piece

data Piece = Piece PColor PType deriving(Show)
data PColor = White | Black deriving(Show)
data PType = Pawn | Knight | Bishop | Rook | Queen | King deriving(Show)
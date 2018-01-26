import Test.QuickCheck
import ChessBoard


testt::String->Bool
testt s = showBoard(readBoard s) == s


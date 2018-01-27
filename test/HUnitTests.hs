module HUnitTests where 
    import Test.HUnit
    import ChessBoard
    import BoardState
    
    --testshowPiece::Test
    --testshowPiece= TestCase(assertEqual ('P') (showPiece(Piece White Pawn)))

    testcalculateIndex = TestCase(assertEqual " " (0) (calculateIndex 'A' '8'))

    tests = TestList [testcalculateIndex]
module HUnitTests where 
    import Test.HUnit
    import ChessBoard
    import BoardState
    import Moves
    
    testBoardStr = unlines ["rnbq bnr"
                                ,"pppppppp" -- black
                                ,"        "
                                ,"        "
                                ,"  QkR   "
                                ,"        "
                                ,"PPPPPPPP" -- white
                                ," NB KBNR"
                                ]

    x = readBoard initialBoardStr
    y = flattenBoard x 

    c = readBoard testBoardStr
    t = flattenBoard c 



    --testcalculateIndex = TestCase(assertEqual " " (0) (calculateIndex 'A' '8'))

    tests_calculateIndex = test [ "test1" ~: "(Calculate Index A8)" ~: (0) ~=?(calculateIndex 'A' '8'),
                                  "test2" ~: "(Calculate Index H1)" ~: (63) ~=?(calculateIndex 'H' '1'),
                                  "test3" ~: "(Calculate Index E4)" ~: (36) ~=?(calculateIndex 'E' '4')]
    
    tests_inList = test [ "test1" ~: "(inList 2 [2,3]:)" ~: (True) ~=?(inList 2 [2,3]),
                          "test2" ~: "(inList 0 [2,3])" ~: (False) ~=?(inList 0 [2,3]),
                          "test3" ~: "(inList 100 [])" ~: (False) ~=?(inList 100 [])]

    tests_isRepeating = test [ "test1" ~: "(isRepeating 2 [2,2]:)" ~: (True) ~=?(isRepeating 2 [2,2]),
                          "test2" ~: "(isRepeating 2 [2,3])" ~: (False) ~=?(isRepeating 2 [2,3]),
                          "test3" ~: "(isRepeating 100 [])" ~: (False) ~=?(isRepeating 100 [])]

    tests_isKingorFriend = test [ "test1" ~: "(isKingorFriend Friend)" ~: (True) ~=?(isKingOrFriend White 56 y),
                                  "test2" ~: "(isKingorFriend King)" ~: (True) ~=?(isKingOrFriend Black 60 y),
                                  "test3" ~: "(isKingorFriend smth else)" ~: (False) ~=?(isKingOrFriend Black 59 y)]

    tests_isMat = test [ "test1" ~: "(isMat Mat)" ~: (True) ~=?(isMat White 0 t),
                                  "test2" ~: "(isMat notMat)" ~: (False) ~=?(isMat Black 0 y)]
                                  
    testkingDirection = TestCase(assertEqual "Checking King Moves" ([26,27,28,34,36,42,43,44]) (kingDirection Black 35 t))
                          
    testqueenMoves = TestCase(assertEqual "Checking queen Moves" ([3,3,3,3,3,3,4,3,3]) (queenMoves Black 3 t))

    tests =TestList [tests_calculateIndex,
                     tests_inList,
                     tests_isRepeating,
                     tests_isKingorFriend,
                     tests_isMat,
                     testkingDirection,
                     testqueenMoves]
    
    
    
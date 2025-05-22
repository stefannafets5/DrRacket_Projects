module TestFormula where

import Formula
import TestPP
import Data.List ( partition )
import qualified Data.Set as Set

testLiteralVariable :: TestData
testLiteralVariable =
    tests
        1
        2
        [ 
            testVal "literalVariable negative literal" 1 $ literalVariable (-1),
            testVal "literalVariable positive literal" 1 $ literalVariable 1,
            testVal "literalVariable list of positive literals" [1..100] $ map literalVariable [1..100],
            testVal "literalVariable list of negative literals" [1..100] $ map literalVariable [-1, -2..(-100)],
            testVal "literalVariable list of mixed literals" ([1..100] ++ [1..100]) $ map literalVariable $ [-1, -2..(-100)] ++ [1..100]
        ]

testIsPositive :: TestData
testIsPositive =
    tests
        2
        2
        [ 
            testVal "isPositive positive literal" True $ isPositive 1,
            testVal "isPositive negative literal" False $ isPositive (-1),
            testVal "isPositive list of positive literals" True $ all isPositive [1..100],
            testVal "isPositive list of negative literals" False $ any isPositive [-1, -2..(-100)],
            testVal "isPositive list of mixed literals" ([1..100], [-100, -99..(-1)]) $ partition isPositive $ [1..50] ++ [-100..(-51)] ++ [51..100] ++ [-50..(-1)]
        ]

testIsNegative :: TestData
testIsNegative =
    tests
        3
        2
        [ 
            testVal "isNegative positive literal" False $ isNegative 1,
            testVal "isNegative negative literal" True $ isNegative (-1),
            testVal "isNegative list of positive literals" False $ any isNegative [1..100],
            testVal "isNegative list of negative literals" True $ all isNegative [-1, -2..(-100)],
            testVal "isNegative list of mixed literals" ([-100, -99..(-1)], [1..100]) $ partition isNegative $ [1..50] ++ [-100..(-51)] ++ [51..100] ++ [-50..(-1)]
        ]

testComplement :: TestData
testComplement =
    tests
        4
        2
        [ 
            testVal "complement positive literal" (-1) $ complement 1,
            testVal "complement negative literal" 1 $ complement (-1),
            testVal "complement list of positive literals" [-1, -2..(-100)] $ map complement [1..100],
            testVal "complement list of negative literals" [1..100] $ map complement [-1, -2..(-100)],
            testVal "complement list of mixed literals" ([1..100] ++ [-1, -2..(-100)]) $ map complement $ [-1, -2..(-100)] ++ [1..100]
        ]

testFormulaLiterals :: TestData
testFormulaLiterals =
    tests
        5
        10
        [ testVal "formulaLiterals example" (Set.fromList [-2, -1, 1, 2, 3]) $ formulaLiterals $ toFormula [[1, -2, 3], [-1, 2, 3]]
        , testVal "formulaLiterals empty"   Set.empty                        $ formulaLiterals $ toFormula []
        , testVal "formulaLiterals unique"  (Set.fromList [1, 2, 3, 4])      $ formulaLiterals $ toFormula [[1, 2], [4], [3]]
        , testVal "formulaLiterals many"    (Set.fromList [-10, -8, -7, -6, -5, -2, 2, 3, 4, 9, 10])
                    $ formulaLiterals $ toFormula [[-7, -5, 3], [4, 3, -2], [-10, -6], [-7, 10, -10], [-8], [-7], [9], [9, 2]]
        ]

testClauseVariables :: TestData
testClauseVariables =
    tests
        6
        10
        [ testVal "clauseVariables example" (Set.fromList [1, 2])             $ clauseVariables $ toClause [1, -2, -1]
        , testVal "clauseVariables empty"   Set.empty                         $ clauseVariables $ toClause []
        , testVal "clauseVariables unique"  (Set.fromList [1, 2, 3, 4])       $ clauseVariables $ toClause [4, -2, 3, -1]
        , testVal "clauseVariables many"    (Set.fromList [1, 2, 3, 4, 5, 6]) $ clauseVariables $ toClause [3, 5, -3, -6, 2, 1, 4, -5, -2]
        ]

testFormulaVariables :: TestData
testFormulaVariables =
    tests
        7
        10
        [ testVal "formulaVariables example" (Set.fromList [1, 2, 3])       $ formulaVariables $ toFormula [[1, -2], [-1, 2, 3]]
        , testVal "formulaVariables empty"   Set.empty                      $ formulaVariables $ toFormula []
        , testVal "formulaVariables unique"  (Set.fromList [1, 3, 4, 5, 9]) $ formulaVariables $ toFormula [[5, -3], [1], [-4, 9]]
        , testVal "formulaVariables many"    (Set.fromList [1, 2, 4, 5, 6, 7, 8, 9])
                $ formulaVariables $ toFormula [[-9], [8, 9], [5, 4], [4, 6], [-9], [1], [7, -2], [-5], [8], [2, -2]]
        ]

testIsPureLiteral :: TestData
testIsPureLiteral =
    tests
        8
        15
        [ 
            testCond "isPureLiteral example 1" $ not $ isPureLiteral 1 $ toFormula [[1, -2, 3], [-1, 2, 3]],
            testCond "isPureLiteral example 2" $ not $ isPureLiteral 2 $ toFormula [[1, -2], [-1, 2, 3]],
            testCond "isPureLiteral example 3" $ isPureLiteral 3 $ toFormula [[1, -2, 3], [-1, 2, 3]],
            testCond "isPureLiteral empty formula" $ isPureLiteral 1 $ toFormula [],
            testCond "isPureLiteral formula with empty clause" $ isPureLiteral 1 $ toFormula [[]],
            testCond "isPureLiteral single literal" $ isPureLiteral 1 $ toFormula [[1]],
            testCond "isPureLiteral negative literal" $ isPureLiteral (-1) $ toFormula [[2, -1], [3, -1]],
            testCond "isPureLiteral with multiple appearances" $ isPureLiteral 1 $ toFormula [[1, 2], [1, 3], [1, 4]],
            testCond "isPureLiteral with multiple variables" $ isPureLiteral 5 $ toFormula [[1, 2], [-1, -2], [3, 4], [-3, -4], [5]],
            testCond "isPureLiteral large formula" $ isPureLiteral 10 $ toFormula [[1, 2, 3], [-1, -2, -3], [4, 5, 6], [-4, -5, -6], [7, 8, 9], [-7, -8, -9], [10]],
            testCond "isPureLiteral complementary literals in different clauses" $ not $ isPureLiteral 1 $ toFormula [[1, 2], [-1, 3], [4, 5]]
        ]

testIsUnitClause :: TestData
testIsUnitClause =
    tests
        9
        5
        [ 
            testCond "isUnitClause example 1" $ isUnitClause $ toClause [1],
            testCond "isUnitClause example 2" $ not $ isUnitClause $ toClause [1, 2],
            testCond "isUnitClause negative literal" $ isUnitClause $ toClause [-1],
            testCond "isUnitClause empty clause" $ not $ isUnitClause $ toClause [],
            testCond "isUnitClause multiple literals" $ not $ isUnitClause $ toClause [1, 2, 3, 4],
            testCond "isUnitClause duplicate literal" $ isUnitClause $ toClause [1, 1, 1, 1],
            testCond "isUnitClause with complementary literals" $ not $ isUnitClause $ toClause [1, -1],
            testCond "isUnitClause with large integer" $ isUnitClause $ toClause [1000000],
            testCond "isUnitClause with large negative integer" $ isUnitClause $ toClause [-1000000]
        ]

testIsValidClause :: TestData
testIsValidClause =
    tests
        10
        15
        [ 
            testCond "isValidClause example 1" $ not $ isValidClause $ toClause [],
            testCond "isValidClause example 2" $ isValidClause $ toClause [-1, 1, 2],
            testCond "isValidClause example 3" $ not $ isValidClause $ toClause [1, 2],
            testCond "isValidClause multiple complementary pairs" $ isValidClause $ toClause [1, -1, 2, -2, 3],
            testCond "isValidClause single literal" $ not $ isValidClause $ toClause [1],
            testCond "isValidClause only negative literals" $ not $ isValidClause $ toClause [-1, -2, -3],
            testCond "isValidClause mixed literals" $ isValidClause $ toClause [1, 2, 3, -3],
            testCond "isValidClause all complementary pairs" $ isValidClause $ toClause [1, -1, 2, -2, 3, -3],
            testCond "isValidClause one pair among many" $ isValidClause $ toClause [1, 2, 3, 4, 5, 6, 7, 8, 9, -9],
            testCond "isValidClause with large integers" $ isValidClause $ toClause [1000000, -1000000]
        ]

testIsValidFormula :: TestData
testIsValidFormula =
    tests
        11
        5
        [ 
            testCond "isValidFormula example 1" $ isValidFormula $ toFormula [],
            testCond "isValidFormula example 2" $ isValidFormula $ toFormula [[1, -1, 2], [-2, 2, 3]],
            testCond "isValidFormula example 3" $ not $ isValidFormula $ toFormula [[1, -1, 2], [-2, 3]],
            testCond "isValidFormula single invalid clause" $ not $ isValidFormula $ toFormula [[1, 2, 3]],
            testCond "isValidFormula single valid clause" $ isValidFormula $ toFormula [[1, -1]],
            testCond "isValidFormula multiple valid clauses" $ isValidFormula $ toFormula [[1, -1], [2, -2], [3, -3]],
            testCond "isValidFormula mixed valid and invalid" $ not $ isValidFormula $ toFormula [[1, -1], [2, 3], [4, -4]],
            testCond "isValidFormula empty clauses" $ not $ isValidFormula $ toFormula [[]],
            testCond "isValidFormula empty and valid clauses" $ not $ isValidFormula $ toFormula [[], [1, -1]],
            testCond "isValidFormula empty and invalid clauses" $ not $ isValidFormula $ toFormula [[], [1, 8]],
            testCond "isValidFormula large formula all invalid" $ not $ isValidFormula $ toFormula [[1, 8], [2, 9], [3, 6], [4, 5], [5, 6], [6, 7], [7, 4], [8, 2], [9, 7], [10, 1]],
            testCond "isValidFormula large formula all valid" $ isValidFormula $ toFormula [[1, -1], [2, -2], [3, -3], [4, -4], [5, -5], [6, -6], [7, -7], [8, -8], [9, -9], [10, -10]],
            testCond "isValidFormula large formula one invalid" $ not $ isValidFormula $ toFormula [[1, -1], [2, -2], [3, -3], [4, -4], [5, -5], [6, -6], [7, -7], [8, -8], [9, -9], [10]],
            testCond "isValidFormula large formula one valid" $ not $ isValidFormula $ toFormula [[1, -1], [2, 9], [3, 6], [4, 5], [5, 6], [6, 7], [7, 4], [8, 2], [9, 7], [10, 1]]
        ]

testSatisfiesFormula :: TestData
testSatisfiesFormula =
    tests
        12
        10
        [
            testCond "satisfiesFormula example 1" $ satisfiesFormula (Set.fromList [1, -2]) $ toFormula [[1, 2]],
            testCond "satisfiesFormula example 2" $ not $ satisfiesFormula (Set.fromList [1, -2]) $ toFormula [[1, 2], [2]],
            testCond "satisfiesFormula example 3" $ satisfiesFormula (Set.fromList [1, -2]) $ toFormula [[1, 2], [-2]],
            testCond "satisfiesFormula easy test" $ satisfiesFormula (Set.fromList [1, 2]) $ toFormula [[1, -2], [-1, 2]],
            testCond "satisfiesFormula easy test not satisfiable" $ not $ satisfiesFormula (Set.fromList [1, 2]) $ toFormula [[1, 2], [-1, -2]],
            testCond "satisfiesFormula only unit clauses" $ satisfiesFormula (Set.fromList [1, 2]) $ toFormula [[1], [2]],
            testCond "satisfiesFormula only unit clauses not satisfiable" $ not $ satisfiesFormula (Set.fromList [1, 2]) $ toFormula [[1], [-2]],
            testCond "satisfiesFormula medium test" $ satisfiesFormula (Set.fromList [1, -2, 3, -4]) $ toFormula [[1, -3], [-2, 4], [1, 2], [2, 3], [3, 4], [1, 4], [1, 3, 4], [1, -2, -3]],
            testCond "satisfiesFormula medium test not satisfiable" $ not $ satisfiesFormula (Set.fromList [1, 2, 3, -4]) $ toFormula [[1, -3], [-2, 4], [1, 2], [2, 3], [3, 4], [1, 4], [1, 3, 4], [1, -2, -3]],
            testCond "satisfiesFormula big test" $ satisfiesFormula (Set.fromList [1, 2, -3, 4, 5, -6, 7, -8]) $ toFormula [[1, 2], [1, 3], [2, 3], [2, 4], [1, 3, 6], [3, -6, 8], [1, 3, 8], [-2, -5, 7], [3, 6, 7, 8], [1, 3, 4, 6, 8], [-1, 4, 6, 7], [-1, -2, 3, -4, -5, 6, -7, -8], [7, 8], [2, -3, 4]]
        ]

testInterpretations :: TestData
testInterpretations =
    tests
        13
        20
        [
            testSet "interpretations example 1" [Set.empty] $ interpretations Set.empty,
            testSet "interpretations example 2" [Set.fromList [1], Set.fromList [-1]] $ interpretations $ Set.fromList [1],
            testSet "interpretations example 3" (map Set.fromList [[1,2],[1,-2],[-1,2],[-1,-2]]) $ interpretations $ Set.fromList [1, 2],
            testSet "interpretations three vars" (map Set.fromList [[1,2,3],[1,2,-3],[1,-2,3],[1,-2,-3],[-1,2,3],[-1,2,-3],[-1,-2,3],[-1,-2,-3]]) $ interpretations $ Set.fromList [1, 2, 3],
            testSet "interpretations four vars" (map Set.fromList [[1,2,3,4],[1,2,3,-4],[1,2,-3,4],[1,2,-3,-4],[1,-2,3,4],[1,-2,3,-4],[1,-2,-3,4],[1,-2,-3,-4],[-1,2,3,4],[-1,2,3,-4],[-1,2,-3,4],[-1,2,-3,-4],[-1,-2,3,4],[-1,-2,3,-4],[-1,-2,-3,4],[-1,-2,-3,-4]]) $ interpretations $ Set.fromList [1, 2, 3, 4]
        ]

testIsSatisfiable :: TestData
testIsSatisfiable =
    tests
        14
        12
        [
            testCond "isSatisfiable example 1" $ isSatisfiable $ toFormula [[1, 2], [-2]],
            testCond "isSatisfiable example 2" $ not $ isSatisfiable $ toFormula [[1], [-1]],
            testCond "isSatisfiable formula with empty clause" $ not $ isSatisfiable $ toFormula [[]],
            testCond "isSatisfiable empty formula" $ isSatisfiable $ toFormula [],
            testCond "isSatisfiable base case satisfiable" $ isSatisfiable $ toFormula [[1]],
            testCond "isSatisfiable base case not satisfiable" $ not $ isSatisfiable $ toFormula [[1], [2], [-1], [-2]],
            testCond "isSatisfiable easy test" $ isSatisfiable $ toFormula [[1, 2], [1, -2], [1], [-2]],
            testCond "isSatisfiable easy test not satisfiable" $ not $ isSatisfiable $ toFormula [[1, 2], [1, -2], [-1]],
            testCond "isSatisfiable medium test" $ isSatisfiable $ toFormula [[1, 2], [2, -3], [1, 2, 4], [-5, -6], [2, 6], [-3, -4], [-5, 6], [5, -6], [-1, -6]],
            testCond "isSatisfiable medium test not satisfiable" $ not $ isSatisfiable $ toFormula [[1, 2], [2, -3], [-1, 3], [-1, 4], [1, 4], [-4, -5], [-4, 5], [-5, 6], [-5, 3], [-5, 3], [5, 3], [5, -3]],
            testCond "isSatisfiable large special test" $ isSatisfiable $ toFormula [[1,2],[3,4],[5,6],[7,8],[-1,-2],[-3,-4],[-5,-6],[-7,-8],[-2,-1],[-4,-3],[-6,-5],[-8,-7],[-1,-3],[-3,-5],[-5,-7],[-2,-4],[-4,-6],[-6,-8]],
            testCond "isSatisfiable large special test unsatisfiable" $ not $ isSatisfiable $ toFormula [[1,2],[3,4],[5,6],[7,8],[-1,-2],[-3,-4],[-5,-6],[-7,-8],[-2,-1],[-4,-3],[-6,-5],[-8,-7],[-1,-3],[-3,-5],[-5,-7],[-1,-5],[-2,-4],[-4,-6],[-6,-8],[-2,-6]]
        ]

main :: IO ()
main =
    vmCheck
        [ testLiteralVariable
        , testIsPositive
        , testIsNegative
        , testComplement
        , testFormulaLiterals
        , testClauseVariables
        , testFormulaVariables
        , testIsPureLiteral
        , testIsUnitClause
        , testIsValidClause
        , testIsValidFormula
        , testSatisfiesFormula
        , testInterpretations
        , testIsSatisfiable
        ]

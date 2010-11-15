-- @+leo-ver=4-thin
-- @+node:gcross.20091202203048.6990:@thin runtests.hs
-- @@language Haskell

-- @<< Import needed modules >>
-- @+node:gcross.20091202203048.6992:<< Import needed modules >>
import Control.Applicative
import Control.Monad

import Data.Either.Unwrap
import Data.ErrorMessage
import Data.Monoid

import Test.Framework
import Test.Framework.Providers.HUnit
import Test.HUnit

import Text.PrettyPrint.ANSI.Leijen
-- @-node:gcross.20091202203048.6992:<< Import needed modules >>
-- @nl

-- @+others
-- @+node:gcross.20091202203048.7047:Values
-- @+node:gcross.20091202203048.7049:dictionary_mapping_words_to_lengths
dictionary_mapping_words_to_lengths :: [(String,Int)]
dictionary_mapping_words_to_lengths =
    [("foo",3)
    ,("bar",3)
    ]
-- @-node:gcross.20091202203048.7049:dictionary_mapping_words_to_lengths
-- @-node:gcross.20091202203048.7047:Values
-- @+node:gcross.20091202203048.6995:Functions
-- @+node:gcross.20091202203048.7002:sqrtWithError
sqrtWithError :: Float -> Either ErrorMessage Float
sqrtWithError x
    | x < 0
        = leftErrorMessageText
            ("Error computing the square root of " ++ (show x) ++ ":")
            "Square roots cannot be taken of negative numbers."
    | otherwise
        = Right (sqrt x)
-- @nonl
-- @-node:gcross.20091202203048.7002:sqrtWithError
-- @+node:gcross.20091202203048.7013:sumWithError
-- @+node:gcross.20091202203048.7001:sumWithError
sumWithError :: Either ErrorMessage Float -> Either ErrorMessage Float -> Either ErrorMessage Float
sumWithError (Left error1) (Left error2) = Left (error1 `mappend` error2)
sumWithError (Left error) _ = Left error
sumWithError _ (Left error) = Left error
sumWithError (Right value1) (Right value2) = Right (value1 + value2)

-- @-node:gcross.20091202203048.7001:sumWithError
-- @+node:gcross.20091202203048.7006:sumWithError_2
sumWithError_2 argument1 argument2 = do
    value1 <- argument1
    value2 <- argument2
    return (value1 + value2)
-- @-node:gcross.20091202203048.7006:sumWithError_2
-- @+node:gcross.20091202203048.7012:sumWithError_3
sumWithError_3 = liftM2 (+)
-- @-node:gcross.20091202203048.7012:sumWithError_3
-- @+node:gcross.20091202203048.7020:sumWithError_4
sumWithError_4 :: Either ErrorMessage Float -> Either ErrorMessage Float -> Either ErrorMessage Float
sumWithError_4 = liftA2 (+)
-- @-node:gcross.20091202203048.7020:sumWithError_4
-- @-node:gcross.20091202203048.7013:sumWithError
-- @+node:gcross.20091202203048.7014:showSumOrErrorOf
-- @+node:gcross.20091202203048.6997:showSumOrErrorOf
showSumOrErrorOf :: Float -> Float -> String
showSumOrErrorOf x y =
    case sumWithError (sqrtWithError x) (sqrtWithError y) of
        Right value -> "The value is " ++ show value
        Left error -> show . formatErrorMessage $ error
-- @nonl
-- @-node:gcross.20091202203048.6997:showSumOrErrorOf
-- @+node:gcross.20091202203048.7016:showSumOrErrorOf_2
showSumOrErrorOf_2 :: Float -> Float -> String
showSumOrErrorOf_2 x y =
    case sumWithError_2 (sqrtWithError x) (sqrtWithError y) of
        Right value -> "The value is " ++ show value
        Left error -> show . formatErrorMessage $ error
-- @-node:gcross.20091202203048.7016:showSumOrErrorOf_2
-- @+node:gcross.20091202203048.7018:showSumOrErrorOf_3
showSumOrErrorOf_3 :: Float -> Float -> String
showSumOrErrorOf_3 x y =
    case sumWithError_3 (sqrtWithError x) (sqrtWithError y) of
        Right value -> "The value is " ++ show value
        Left error -> show . formatErrorMessage $ error
-- @-node:gcross.20091202203048.7018:showSumOrErrorOf_3
-- @+node:gcross.20091202203048.7024:showSumOrErrorOf_4
showSumOrErrorOf_4 :: Float -> Float -> String
showSumOrErrorOf_4 x y =
    case sumWithError_4 (sqrtWithError x) (sqrtWithError y) of
        Right value -> "The value is " ++ show value
        Left error -> show . formatErrorMessage $ error
-- @-node:gcross.20091202203048.7024:showSumOrErrorOf_4
-- @-node:gcross.20091202203048.7014:showSumOrErrorOf
-- @-node:gcross.20091202203048.6995:Functions
-- @-others

main = defaultMain $
    -- @    << Tests >>
    -- @+node:gcross.20091202203048.6996:<< Tests >>
    -- @+others
    -- @+node:gcross.20091202203048.6993:Documentation examples
    [testGroup "Documentation examples"
        -- @    @+others
        -- @+node:gcross.20091202203048.6994:showSumOrErrorOf
        [testGroup "showSumOrErrorOf" $
            [testCase "(-1) (-2)" $
                assertEqual
                    "Was the expected message produced?"
                    (show . vcat . map text $
                        ["Error computing the square root of -1.0:"
                        ,"    Square roots cannot be taken of negative numbers."
                        ,"Error computing the square root of -2.0:"
                        ,"    Square roots cannot be taken of negative numbers."
                        ]
                    )
                    $
                    showSumOrErrorOf (-1) (-2)
            ,testCase "(-1) (-1)" $
                assertEqual
                    "Was the expected message produced?"
                    (show . vcat . map text $
                        ["Error computing the square root of -1.0:"
                        ,"    Square roots cannot be taken of negative numbers."
                        ]
                    )
                    $
                    showSumOrErrorOf (-1) (-1)
            ]
        -- @-node:gcross.20091202203048.6994:showSumOrErrorOf
        -- @+node:gcross.20091202203048.7008:showSumOrErrorOf_2
        ,testGroup "showSumOrErrorOf_2" $
            [testCase "(-1) (-2)" $
                assertEqual
                    "Was the expected message produced?"
                    (show . vcat . map text $
                        ["Error computing the square root of -1.0:"
                        ,"    Square roots cannot be taken of negative numbers."
                        ]
                    )
                    $
                    showSumOrErrorOf_2 (-1) (-2)
            ]
        -- @-node:gcross.20091202203048.7008:showSumOrErrorOf_2
        -- @+node:gcross.20091202203048.7010:showSumOrErrorOf_3
        ,testGroup "showSumOrErrorOf_3" $
            [testCase "(-1) (-2)" $
                assertEqual
                    "Was the expected message produced?"
                    (show . vcat . map text $
                        ["Error computing the square root of -1.0:"
                        ,"    Square roots cannot be taken of negative numbers."
                        ]
                    )
                    $
                    showSumOrErrorOf_3 (-1) (-2)
            ]
        -- @-node:gcross.20091202203048.7010:showSumOrErrorOf_3
        -- @+node:gcross.20091202203048.7022:showSumOrErrorOf_4
        ,testGroup "showSumOrErrorOf_3" $
            [testCase "(-1) (-2)" $
                assertEqual
                    "Was the expected message produced?"
                    (show . vcat . map text $
                        ["Error computing the square root of -1.0:"
                        ,"    Square roots cannot be taken of negative numbers."
                        ,"Error computing the square root of -2.0:"
                        ,"    Square roots cannot be taken of negative numbers."
                        ]
                    )
                    $
                    showSumOrErrorOf_4 (-1) (-2)
            ]
        -- @-node:gcross.20091202203048.7022:showSumOrErrorOf_4
        -- @+node:gcross.20091203120301.1651:Multiline strings
        ,testGroup "Multiline strings" $
            [testCase "errorMessageText" $
                assertEqual
                    "Was the expected message produced?"
                    (show . vcat . map text $
                        ["A poem:"
                        ,"    Roses are red."
                        ,"Violets are blue."
                        ]
                    )
                    .
                    show
                    .
                    formatErrorMessage
                    $
                    errorMessageText "A poem:" "Roses are red.\nViolets are blue."
            ,testCase "errorMessageTextFromMultilineString" $
                assertEqual
                    "Was the expected message produced?"
                    (show . vcat . map text $
                        ["A poem:"
                        ,"    Roses are red."
                        ,"    Violets are blue."
                        ]
                    )
                    .
                    show
                    .
                    formatErrorMessage
                    $
                    errorMessageTextFromMultilineString "A poem:" "Roses are red.\nViolets are blue."
            ]
        -- @-node:gcross.20091203120301.1651:Multiline strings
        -- @-others
        ]
    -- @-node:gcross.20091202203048.6993:Documentation examples
    -- @-others
    -- @nonl
    -- @-node:gcross.20091202203048.6996:<< Tests >>
    -- @nl
    ]
-- @-node:gcross.20091202203048.6990:@thin runtests.hs
-- @-leo

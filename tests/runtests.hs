import Control.Applicative
import Control.Monad

import Data.Either.Unwrap
import Data.ErrorMessage
import Data.Monoid

import Test.Framework
import Test.Framework.Providers.HUnit
import Test.HUnit

import Text.PrettyPrint.ANSI.Leijen

dictionary_mapping_words_to_lengths :: [(String,Int)]
dictionary_mapping_words_to_lengths =
    [("foo",3)
    ,("bar",3)
    ]
sqrtWithError :: Float -> Either ErrorMessage Float
sqrtWithError x
    | x < 0
        = leftErrorMessageText
            ("Error computing the square root of " ++ (show x) ++ ":")
            "Square roots cannot be taken of negative numbers."
    | otherwise
        = Right (sqrt x)
sumWithError :: Either ErrorMessage Float -> Either ErrorMessage Float -> Either ErrorMessage Float
sumWithError (Left error1) (Left error2) = Left (error1 `mappend` error2)
sumWithError (Left error) _ = Left error
sumWithError _ (Left error) = Left error
sumWithError (Right value1) (Right value2) = Right (value1 + value2)

sumWithError_2 argument1 argument2 = do
    value1 <- argument1
    value2 <- argument2
    return (value1 + value2)
sumWithError_3 = liftM2 (+)
sumWithError_4 :: Either ErrorMessage Float -> Either ErrorMessage Float -> Either ErrorMessage Float
sumWithError_4 = liftA2 (+)
showSumOrErrorOf :: Float -> Float -> String
showSumOrErrorOf x y =
    case sumWithError (sqrtWithError x) (sqrtWithError y) of
        Right value -> "The value is " ++ show value
        Left error -> show . formatErrorMessage $ error
showSumOrErrorOf_2 :: Float -> Float -> String
showSumOrErrorOf_2 x y =
    case sumWithError_2 (sqrtWithError x) (sqrtWithError y) of
        Right value -> "The value is " ++ show value
        Left error -> show . formatErrorMessage $ error
showSumOrErrorOf_3 :: Float -> Float -> String
showSumOrErrorOf_3 x y =
    case sumWithError_3 (sqrtWithError x) (sqrtWithError y) of
        Right value -> "The value is " ++ show value
        Left error -> show . formatErrorMessage $ error
showSumOrErrorOf_4 :: Float -> Float -> String
showSumOrErrorOf_4 x y =
    case sumWithError_4 (sqrtWithError x) (sqrtWithError y) of
        Right value -> "The value is " ++ show value
        Left error -> show . formatErrorMessage $ error
lookupAndReturnResultOrError :: String -> Either Doc Int
lookupAndReturnResultOrError word =
    case lookup word dictionary_mapping_words_to_lengths of
        Nothing -> Left (text word)
        Just word_length -> Right word_length
getWordLengthsOrError :: [String] -> Either ErrorMessage [Int]
getWordLengthsOrError =
    mapLeft
        (errorMessage
            "Error looking up the following words in the dictionary:"
        )
    .
    gatherResultsOrError
    .
    map lookupAndReturnResultOrError


main = defaultMain $
    [testGroup "Documentation examples"
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
        ,testCase "getWordLengthsOrError" $
            assertEqual
                "Was the expected message produced?"
                (Left . show . vcat . map text $
                    ["Error looking up the following words in the dictionary:"
                    ,"    apple"
                    ,"    cat"
                    ]
                )
                .
                mapLeft (show . formatErrorMessage)
                $
                getWordLengthsOrError ["foo","apple","cat","bar"]

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
        ]
    ]

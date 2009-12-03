-- @+leo-ver=4-thin
-- @+node:gcross.20091202203048.1603:@shadow ErrorMessage.hs
-- @@language Haskell
-- @@raw
------------------------------------------------------------------------------
-- |
-- @@end_raw
-- @+at
--  Module      :  Data.ErrorMessage
--  Copyright   :  (c) Gregory Crosswhite
--  License     :  BSD-style
--  Maintainer  :  gcross@phys.washington.edu
--  Stability   :  provisional
--  Portability :  portable
--  
--  This philosophy behind this package is that it is often better to find out 
--  all of the errors that have occured in a computation and report them 
--  simultaneously, rather than aborting as soon as the first error is 
--  encountered.  Towards this end, this module supplies a type of
--  /combinable error messages/ so that all of the errors from subcomputations 
--  can be gathered and presented together.
--  
--  The following provides an example of how these can be used:
--  
-- @-at
-- @@c
-- @@raw
-- > sqrtWithError :: Float -> Either ErrorMessage Float
-- > sqrtWithError x
-- >     | x < 0
-- >         = leftErrorMessageText
-- >             ("Error computing the square root of " ++ (show x) ++ ":")
-- >             "Square roots cannot be taken of negative numbers."
-- >     | otherwise
-- >         = Right (sqrt x)
-- > 
-- > sumWithError :: Either ErrorMessage Float -> Either ErrorMessage Float -> Either ErrorMessage Float
-- > sumWithError (Left error1) (Left error2) = Left (error1 `mappend` error2)
-- > sumWithError (Left error) _ = Left error
-- > sumWithError _ (Left error) = Left error
-- > sumWithError (Right value1) (Right value2) = Right (value1 + value2)
-- > 
-- > showSumOrErrorOf :: Float -> Float -> String
-- > showSumOrErrorOf x y =
-- >     case sumWithError (sqrtWithError x) (sqrtWithError y) of
-- >         Right value -> "The value is " ++ show value
-- >         Left error -> show . formatErrorMessage $ error
-- @@end_raw
-- @+at
--  
--  The result of @showSumOrErrorOf (-1) (-2)@ is the string,
--  
--  > Error computing the square root of -1:
--  >     Square roots cannot be taken of negative numbers.
--  > Error computing the square root of -2:
--  >     Square roots cannot be taken of negative numbers.
--  
--  whereas the result of @showSumOrErrorOf (-1) (-1)@ is the string,
--  
--  > Error computing the square root of -1:
--  >     Square roots cannot be taken of negative numbers.
--  
--  Note how the error message only appears once;  this is because the process 
--  of combining the error messages automatically eliminates all identical 
--  headings under the assumption that they came from the same original 
--  computation, as was the case here.
--  
--  Currently, the definition of @sumWithError@ is largely boilerplate.  
--  Happily, the Haskell community has done a lot of work to identify patterns 
--  such as these and to write libraries that allow us to express them 
--  concisely.  In particular, a standard trick when working with errors like 
--  this is to express the calculation as a 'Monad', such as by using the 
--  following definition:
--  
--  > sumWithError_2 argument1 argument2 = do
--  >     value1 <- argument1
--  >     value2 <- argument2
--  >     return (value1 + value2)
--  
--  Or, even more concisely:
--  
--  > sumWithError_3 = liftM2 (+)
--  
--  Unfortunately though, neither of these definitions have the same semantics 
--  as the original @sumWithError@, as using both we get the following error 
--  message for @showSumOrErrorOf (-1) (-2)@:
--  
--  > Error computing the square root of -1:
--  >     Square roots cannot be taken of negative numbers.
--  
--  That is, we have lost the second of the two error messages.  The reason 
--  for this is that 'Monad'-style error processing expresses the computation 
--  as a sequence, and gives up as soon as it sees any error.  In this case of 
--  @sumWithError@, however, the evaluation of the second argument can proceed 
--  even if there was an error in the first argument.  Thus, rather than using 
--  a 'Monad' pattern, we use an 'Applicative' pattern:
--  
--  > sumWithError_4 = liftA2 (+)
--  
--  Now both error messages are displayed.
--  
-- @-at
-- @@c
-- @@raw
------------------------------------------------------------------------------
-- @@end_raw

-- @<< Language extensions >>
-- @+node:gcross.20091202203048.7000:<< Language extensions >>
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
-- @-node:gcross.20091202203048.7000:<< Language extensions >>
-- @nl

module Data.ErrorMessage
  (
    -- * The ErrorMessage Type
     ErrorMessage
    -- $error_message_type

    -- ** Instances for ErrorMessage

    -- $error_message_instances

    -- ** Instances for Doc

    -- $doc_instances

    -- ** Applicative Instances

    -- $applicative_instances

    -- * Creation of Error Messages

    -- $error_message_creation

    ,errorMessage
    ,errorMessageText
    ,errorMessageTextFromMultilineString
    ,leftErrorMessage
    ,leftErrorMessageText
    ,leftErrorMessageTextFromMultilineString

    -- * Formatting of Error Messages

    -- $error_message_formatting

    ,formatErrorMessage
    ,formatMessageWithHeading

    -- * Gathering Results with Errors

    -- $gathering_results
    ,gatherResultsOrErrors
    ,gatherResultsOrError
  ) where

-- @<< Import needed modules >>
-- @+node:gcross.20091202203048.1605:<< Import needed modules >>
import Control.Arrow
import Control.Applicative hiding (empty)
import Control.Applicative.Infix
import Control.Monad
import Control.Monad.Error

import Data.Either
import Data.Either.Unwrap
import Data.Function
import Data.Monoid

import Data.Map (Map)
import qualified Data.Map as Map

import Text.PrettyPrint.ANSI.Leijen
-- @-node:gcross.20091202203048.1605:<< Import needed modules >>
-- @nl

-- @+others
-- @+node:gcross.20091202203048.1606:Types
-- @+node:gcross.20091202203048.1607:ErrorMessage
newtype ErrorMessage = ErrorMessage { unwrapErrorMessage :: Map String Doc }
-- @-node:gcross.20091202203048.1607:ErrorMessage
-- @-node:gcross.20091202203048.1606:Types
-- @+node:gcross.20091202203048.1608:Instances
-- @+node:gcross.20091202203048.1609:Applicative (Either e a)
-- @@raw
-- @@end_raw

instance (Monoid e) => Applicative (Either e) where
    pure = Right
    (<*>) (Left error2) (Left error1) = Left (error1 `mappend` error2)
    (<*>) (Left error) _ = Left error
    (<*>) _ (Left error) = Left error
    (<*>) (Right function) (Right argument) = Right (function argument)
-- @-node:gcross.20091202203048.1609:Applicative (Either e a)
-- @+node:gcross.20091202203048.1610:Applicative (ErrorT e m a)
instance (Monoid e, Error e, Monad m) => Applicative (ErrorT e m) where
    pure = return
    e_fn <*> e_arg = ErrorT $ liftM2 (<*>) (runErrorT e_fn) (runErrorT e_arg)
-- @-node:gcross.20091202203048.1610:Applicative (ErrorT e m a)
-- @+node:gcross.20091202203048.1611:Error ErrorMessage
instance Error ErrorMessage where
    noMsg = strMsg "(and he did not even bother to include an error message!  :-/)"
    strMsg = errorMessage "Error caused by the programmer:" . text
-- @-node:gcross.20091202203048.1611:Error ErrorMessage
-- @+node:gcross.20091202203048.7025:Monoid ErrorMessage
instance Monoid ErrorMessage where
    mempty = ErrorMessage Map.empty
    mappend (ErrorMessage a) (ErrorMessage b) = ErrorMessage (mappend a b)
    mconcat = ErrorMessage . mconcat . map unwrapErrorMessage
-- @-node:gcross.20091202203048.7025:Monoid ErrorMessage
-- @+node:gcross.20091202203048.1613:Error Doc
instance Error Doc where
    noMsg = empty
    strMsg = text
-- @-node:gcross.20091202203048.1613:Error Doc
-- @+node:gcross.20091202203048.1612:Monoid Doc
instance Monoid Doc where
    mempty = empty
    mappend = (<$$>)
    mconcat = vcat
-- @-node:gcross.20091202203048.1612:Monoid Doc
-- @-node:gcross.20091202203048.1608:Instances
-- @+node:gcross.20091202203048.1614:Functions
-- @+node:gcross.20091202203048.7038:Error Message Creation
-- @+node:gcross.20091202203048.1619:errorMessage / leftErrorMessage
-- @@raw
-- |
-- @@end_raw
-- @+at
--  The function 'errorMessage' takes a heading and a body and produce an 
--  ErrorMessage object from them;  this can be considered to be a thin 
--  wrapper around 'Data.Map.singleton'.
-- @-at
-- @@c

errorMessage :: String -> Doc -> ErrorMessage
errorMessage heading = ErrorMessage . Map.singleton heading

-- @@raw
-- |
-- @@end_raw
-- @+at
--  Since one usually wants to return not just an ErrorMessage, but a value of 
--  the form @Left error_message@, the function 'leftErrorMessage' is provided 
--  as a convenience;  it creates the error message, and then wraps it inside 
--  of 'Left'.
-- @-at
-- @@c

leftErrorMessage :: String -> Doc -> Either ErrorMessage a
leftErrorMessage heading = Left . errorMessage heading
-- @-node:gcross.20091202203048.1619:errorMessage / leftErrorMessage
-- @+node:gcross.20091202203048.1620:errorMessageText / leftErrorMessageText
-- @@raw
-- |
-- @@end_raw
-- @+at
--  The function 'errorMessageText' is similar to the function 'errorMessage', 
--  but for the body it takes a 'String' instead of a 'Doc'.  It is provided 
--  for convenience.
-- @-at
-- @@c

errorMessageText :: String -> String -> ErrorMessage
errorMessageText heading = errorMessage heading . text


-- @@raw
-- |
-- @@end_raw
-- @+at
--  The function 'leftErrorMessageText' is 'errorMessageText' composed with 
--  the 'Left' constructor for convenience.
-- @-at
-- @@c
leftErrorMessageText :: String -> String -> Either ErrorMessage a
leftErrorMessageText heading = Left . errorMessageText heading
-- @-node:gcross.20091202203048.1620:errorMessageText / leftErrorMessageText
-- @+node:gcross.20091202203048.1621:errorMessageTextFromMultilineString / leftErrorMessageTextFromMultilineString
-- @@raw
-- |
-- @@end_raw
-- @+at
--  Use this function when you want to create an error message from a 
--  multi-line string.
--  
--  Although one could alternatively use 'errorMessageText', if one were to do 
--  this then one would only see only the first line of be indented when the 
--  error message is formatted for output.  For example,
--  
--  > errorMessageText "A poem:" "Roses are red.\nViolets are blue."
--  
--  produces the following (formatted) error message:
--  
--  > A poem:
--  >     Roses are red.
--  > Violets are blue.
--  
--  The reason for this is because the line breaks are not known to the 'Doc' 
--  combinators, and so the indentation is not handled properly.  The function 
--  'errorMessageTextFromMultilineString' takes care of this for you.  For 
--  example,
--  
-- @-at
-- @@c
-- @@raw
-- > errorMessageTextFromMultilineString "A poem:" "Roses are red.\nViolets are blue."
-- @@end_raw
-- @+at
--  
--  produces the following (formatted) error message:
--  
--  > A poem:
--  >     Roses are red.
--  >     Violets are blue.
--  
-- @-at
-- @@c

errorMessageTextFromMultilineString :: String -> String -> ErrorMessage
errorMessageTextFromMultilineString heading = errorMessage heading . vcat . map text . lines

-- @@raw
-- |
-- @@end_raw
-- @+at
--  The function 'leftErrorMessageTextFromMultilineString' is 
--  'errorMessageTextFromMultilineString' composed with the 'Left' constructor 
--  for convenience.
-- @-at
-- @@c

leftErrorMessageTextFromMultilineString :: String -> String -> Either ErrorMessage a
leftErrorMessageTextFromMultilineString heading = Left . errorMessageTextFromMultilineString heading
-- @-node:gcross.20091202203048.1621:errorMessageTextFromMultilineString / leftErrorMessageTextFromMultilineString
-- @-node:gcross.20091202203048.7038:Error Message Creation
-- @+node:gcross.20091202203048.7039:Formatting
-- @+node:gcross.20091202203048.1617:formatErrorMessage
-- @@raw
-- |
-- @@end_raw
-- @+at
--  This function takes an 'ErrorMessage' and formats it into a 'Doc'.  It 
--  does this by converting the headings into 'text' objects, merging them 
--  with their respective bodies (the latter having been indented by four 
--  spaces), and then concatenating the result.
-- @-at
-- @@c

formatErrorMessage :: ErrorMessage -> Doc
formatErrorMessage =
    vcat
    .
    map (uncurry formatMessageWithHeading)
    .
    Map.assocs
    .
    unwrapErrorMessage
-- @-node:gcross.20091202203048.1617:formatErrorMessage
-- @+node:gcross.20091202203048.1618:formatMessageWithHeading
-- @@raw
-- |
-- @@end_raw
-- @+at
--  This is the utility function used by 'formatErrorMessage' to format a 
--  'Doc' given a heading and a body;  it indents the body by four spaces and 
--  then appends it after the heading.
-- @-at
-- @@c

formatMessageWithHeading :: String -> Doc -> Doc
formatMessageWithHeading heading body =
    text heading
    <$$>
    indent 4 body
-- @-node:gcross.20091202203048.1618:formatMessageWithHeading
-- @-node:gcross.20091202203048.7039:Formatting
-- @+node:gcross.20091202203048.7040:Extracting results from a list
-- @+node:gcross.20091202203048.1616:gatherResultsOrErrors
-- @@raw
-- |
-- @@end_raw
-- @+at
--  This function takes a list of values which might contain errors and 
--  returns either a list of the errors found in the values or the full list 
--  of results.  Note that there is no restriction on the type of the errors.
-- @-at
-- @@c
gatherResultsOrErrors :: [Either e a] -> Either [e] [a]
gatherResultsOrErrors eithers =
    case partitionEithers (eithers) of
        ([],results) -> Right results
        (errors,_) -> Left errors
-- @-node:gcross.20091202203048.1616:gatherResultsOrErrors
-- @+node:gcross.20091202203048.1615:gatherResultsOrError
-- @@raw
-- |
-- @@end_raw
-- @+at
--  This function is similar to 'gatherResultsOrErrors', but instead of 
--  returning a list of errors it combines them into a single error.  Note 
--  that only restriction on the type of the error is that it be an instance 
--  of 'Monoid', so this operation is not limited to 'ErrorMessage's but could 
--  also be used for, say, 'Doc's, as in the following example:
--  
--  > dictionary_mapping_words_to_lengths :: [(String,Int)]
--  > dictionary_mapping_words_to_lengths =
--  >     [("foo",3)
--  >     ,("bar",3)
--  >     ]
--  >
--  > getWordLengthsOrError :: [String] -> Either ErrorMessage [Int]
--  > getWordLengthsOrError =
--  >    mapLeft
--  >         (errorMessage
--  >             "Error looking up the following words in the dictionary:"
--  >         )
--  >     .
--  >     gatherResultsOrError
--  >     .
--  >     map lookupAndReturnResultOrError
--  
--  The function call
--  
--  > getWordLengthsOrError ["foo","apple","cat","bar"]
--  
--  results in the following error message:
--  
--  > Error looking up the following words in the dictionary:
--  >     apple
--  >     cat
--  
-- @-at
-- @@c
gatherResultsOrError :: Monoid e => [Either e a] -> Either e [a]
gatherResultsOrError = mapLeft mconcat . gatherResultsOrErrors
-- @-node:gcross.20091202203048.1615:gatherResultsOrError
-- @-node:gcross.20091202203048.7040:Extracting results from a list
-- @-node:gcross.20091202203048.1614:Functions
-- @-others

-- @<< Documentation sections >>
-- @+node:gcross.20091202203048.7003:<< Documentation sections >>
-- @+others
-- @+node:gcross.20091202203048.7004:ErrorMessage type
-- @@raw
-- $error_message_type
-- @@end_raw
-- @+at
--  The 'ErrorMessage' type is simply a map from 'String' to 'Doc'; the reason 
--  why the values are 'Doc' is because this allows us to compose them using 
--  the combinators in Leijen's pretty-printing library.
-- @-at
-- @@c



-- @+node:gcross.20091202203048.7027:ErrorMessage instances
-- @@raw
-- $error_message_instances
-- @@end_raw
-- @+at
--  In some respects, the most important part of the 'ErrorMessage' type are 
--  its instances:
--  
--  * The 'Monoid' instance says that we can take any two error messages and 
--  combine them using 'mappend' and/or 'mconcat';  the implementation for 
--  this is just that of the underlying Map type.
--  
--  * The 'Error' instance allows us to work inside the 'ErrorT' monad using 
--  'ErrorMessage' as the error type.  Although it was mentioned earlier that 
--  using 'Applicative' is generally preferable since it finds as many errors 
--  as possible before halting, there are times when a later computation 
--  really does need the result of an earlier computation, and in this case 
--  the sequential structure of 'Monad's exactly fits the bill.
--  
--  Note that in order for 'ErrorMessage' to be an instance of 'Error', I 
--  needed to define how to create an 'ErrorMessage' without a heading 
--  ('strMsg') and possibly without even a body ('noMsg');  however, if this 
--  ever happens, it means that the error was not handled properly --- e.g., 
--  when there is a pattern match failure.  Thus, the heading of errors 
--  created by 'noMsg' and 'strMsg' is
--  /Error caused by the programmer:/
-- @-at
-- @@c


-- @-node:gcross.20091202203048.7027:ErrorMessage instances
-- @+node:gcross.20091202203048.7029:Doc instances
-- @@raw
-- $doc_instances
-- @@end_raw
-- @+at
--  Unfortunately, it does not show up in the API documentation that this 
--  module also defines the following two instances for 'Doc':
--  
--  * The 'Monoid' instance says that we can combine any two 'Doc's by 
--  concatenating them vertically.
--  
--  * The 'Error' instance defines a 'noMsg' error to be the 'empty' 'Doc' and 
--  the 'strMsg' error to be a 'text' 'Doc'.
--  
-- @-at
-- @@c


-- @-node:gcross.20091202203048.7029:Doc instances
-- @+node:gcross.20091202203048.7031:Applicative instances
-- @@raw
-- $applicative_instances
-- @@end_raw
-- @+at
--  Unfortunately, it does not show up in the API documentation that this 
--  module also defines the following two 'Applicative' instances:
--  
-- @-at
-- @@c
-- @@raw
-- > instance (Monoid e) => Applicative (Either e) where ...
-- @@end_raw
-- @+at
--  
--  This instance declaration allows you to lift pure functions into functions 
--  that work with values that might have errors, since both 'Doc' and 
--  'ErrorMessage' are instances of 'Monoid'.  For example, we can use
--  @liftA2 (+)@ to lift the @(+)@ function into a function that checks both 
--  of its arguments for errors before computing the sum.  As was described 
--  earlier, the advantage of @liftA2 (+)@ over @liftM2 (+)@ is that the 
--  former checks for errors in both arguments and will combine them if 
--  present, whereas the latter will ignore errors in the second argument if 
--  there is an error in the first argument.
--  
-- @-at
-- @@c
-- @@raw
-- > instance (Monoid e, Error e, Monad m) => Applicative (ErrorT e m) where
-- >     pure = return
-- >     e_fn <*> e_arg = ErrorT $ liftM2 (<*>) (runErrorT e_fn) (runErrorT e_arg)
-- @@end_raw
-- @+at
--  
--  This instance definition lifts the @Applicative (Either e)@ so that it 
--  works for values obtained from monadic computations.  Note that the 
--  definition first executes the monad @e_fn@ and then the monad @e_arg@, and 
--  only after both monads have been executed in this sequence does it apply 
--  the operator @(\<*\>)@ to the values in order to possibly combine their 
--  error messages.
--  
--  These instances allow you to write code like the following:
--  
-- @-at
-- @@c
-- @@raw
-- > data Point = Point { x :: Int, y :: Int, z :: Int }
-- >
-- > pOrError = Point <$> xOrError <*> yOrError <*> zOrError
-- @@end_raw
-- @+at
--  
--  The value @pOrError@ is either a @Point@, or a combination of the error 
--  messages in @xOrError@, @yOrError@, and @zOrError@.
--  
-- @-at
-- @@c


-- @-node:gcross.20091202203048.7031:Applicative instances
-- @-node:gcross.20091202203048.7004:ErrorMessage type
-- @+node:gcross.20091202203048.7032:Creating Error Messages
-- @@raw
-- $error_message_creation
-- @@end_raw
-- @+at
--  Up to now we have spent a lot time discussing how to combine 
--  'ErrorMessage's, but little time discussing how to produce them.  The 
--  provided functions for doing this are as follows:
-- @-at
-- @@c

-- @-node:gcross.20091202203048.7032:Creating Error Messages
-- @+node:gcross.20091202203048.7042:Formatting Error Messages
-- @@raw
-- $error_message_formatting
-- @@end_raw
-- @+at
--  The end purpose of 'ErrorMessage' \'s existence is to be displayed to the 
--  user.  Towards this end, the following functions format an 'ErrorMessage' 
--  into a 'Doc'.
-- @-at
-- @@c

-- @-node:gcross.20091202203048.7042:Formatting Error Messages
-- @+node:gcross.20091202203048.7044:Gathering Results with Errors
-- @@raw
-- $gathering_results_with_errors
-- @@end_raw
-- @+at
--  Although there are many combinators available (such as 'liftA' and \<$\>) 
--  for lifting pure functions to functions that handle errors, there are 
--  times when one wants to gather together a list of results which might 
--  possibly contain some errors.  The following functions assist in doing 
--  this.
-- @-at
-- @@c

-- @-node:gcross.20091202203048.7044:Gathering Results with Errors
-- @-others
-- @-node:gcross.20091202203048.7003:<< Documentation sections >>
-- @nl
-- @-node:gcross.20091202203048.1603:@shadow ErrorMessage.hs
-- @-leo

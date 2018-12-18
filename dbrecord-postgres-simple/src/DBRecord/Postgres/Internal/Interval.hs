{-# LANGUAGE TemplateHaskell   #-}
{-# LANGUAGE OverloadedStrings #-}


-- pull req : https://github.com/lpsmith/postgresql-simple/pull/219
module DBRecord.Postgres.Internal.Interval where

import Data.Int
import Database.PostgreSQL.Simple.FromField
import Database.PostgreSQL.Simple.ToField
import Control.Applicative ((<$>), (<*), (*>))
import qualified Prelude as P
import Data.Attoparsec.ByteString.Char8 as A (Parser, space, option, char, many1, anyChar, string, decimal, parseOnly, signed, peekChar')
import qualified Data.Attoparsec.ByteString.Char8 as A
-- import Database.PostgreSQL.Simple.Compat (toPico)
import Data.ByteString.Builder (Builder, integerDec)
import Data.ByteString.Builder.Prim (primBounded, (>*<), primFixed, int64Dec, int32Dec, FixedPrim, BoundedPrim, char8, liftFixedToBounded)
import qualified Data.ByteString as B
import Data.Bits ((.&.))
import Data.Char (ord)
import Prelude
import qualified Database.PostgreSQL.Simple.TypeInfo.Static as TI
import Database.PostgreSQL.Simple.TypeInfo.Macro (inlineTypoid)
import Data.Monoid ((<>))
import qualified Database.PostgreSQL.LibPQ as PQ
import qualified Data.ByteString.Char8 as B8
import Control.Exception

data Interval = Interval { intervalMonths :: Int32
                         , intervalDays :: Int32
                         , intervalMicroseconds :: Integer }
                         deriving (Show, Read, Eq)

zeroInterval :: Interval
zeroInterval = Interval 0 0 0

twoDigits :: Parser Int
twoDigits = do
  a <- A.digit
  b <- A.digit
  let c2d c = ord c .&. 15
  return $! c2d a * 10 + c2d b

liftB :: FixedPrim a -> BoundedPrim a
liftB = liftFixedToBounded

instance ToField Interval where
    toField = Plain . inQuotes . intervalBuilder
    {-# INLINE toField #-}

intervalB :: Interval -> Builder
intervalB x = boundedPrefix <> integerDec afterSeconds <> fixedSuffix
  where
    (hours, afterHours) = intervalMicroseconds x `quotRem` 3600000000
    (minutes, afterMinutes) = afterHours `quotRem` 60000000
    (seconds, afterSeconds) = afterMinutes `quotRem` 1000000

    boundedPrefix = primBounded
      (int32Dec >*<
        liftB char8 >*< liftB char8 >*< liftB char8 >*< liftB char8 >*< liftB char8 >*< liftB char8 >*<
        int32Dec >*<
        liftB char8 >*< liftB char8 >*< liftB char8 >*< liftB char8 >*< liftB char8 >*< liftB char8 >*<
        int64Dec >*<
        liftB char8 >*< liftB char8 >*< liftB char8 >*< liftB char8 >*< liftB char8 >*< liftB char8 >*< liftB char8 >*<
        int64Dec >*<
        liftB char8 >*< liftB char8 >*< liftB char8 >*< liftB char8 >*< liftB char8 >*< liftB char8 >*<
        int64Dec >*<
        liftB char8 >*< liftB char8 >*< liftB char8 >*< liftB char8 >*< liftB char8 >*< liftB char8)
      (intervalMonths x,
        (' ', ('m', ('o', ('n', ('s', (' ',
        (intervalDays x,
        (' ', ('d', ('a', ('y', ('s', (' ',
        (fromIntegral hours,
        (' ', ('h', ('o', ('u', ('r', ('s', (' ',
        (fromIntegral minutes,
        (' ', ('m', ('i', ('n', ('s', (' ',
        (fromIntegral seconds,
        (' ', ('s', ('e', ('c', ('s', ' ')))))))))))))))))))))))))))))))))))

    fixedSuffix = primFixed (char8 >*< char8 >*< char8 >*< char8 >*< char8 >*< char8 >*< char8 >*< char8 >*<
                             char8 >*< char8 >*< char8 >*< char8 >*< char8)
                            (' ', ('m', ('i', ('c', ('r', ('o', ('s', ('e', ('c', ('o', ('n', ('d', 's')))))))))))) 

interval :: Parser Interval
interval = do
  parsedYears <- option 0 $ signed decimal <* string " year" <* optionalS <* optionalSpace
  parsedMonths <- option 0 $ signed decimal <* string " mon" <* optionalS <* optionalSpace
  parsedDays <- option 0 $ signed decimal <* string " day" <* optionalS <* optionalSpace
  parsedMicroseconds <- option 0 $ do
    possibleNegativeSign <- peekChar'
    normalizeSign <- case possibleNegativeSign of '-' -> anyChar *> return negate
                                                  _   -> return id
    parsedHours <- decimal <* char ':'
    parsedMinutes <- twoDigits <* char ':'
    microsecondsOfSeconds <- (*microsecondScale) <$> twoDigits
    maybePartialSeconds <- option Nothing $ Just <$> do
      partialSecondStr <- char '.' *> many1 A.digit
      let partialSeconds = read $ P.take 6 $ partialSecondStr ++ repeat '0'
      return partialSeconds

    let minutesMicros = microsecondScale * 60 * fromIntegral parsedMinutes
    let hoursMicros = microsecondScale * 3600 * parsedHours
    let parsedMicroseconds = case maybePartialSeconds of Nothing ->
                                                          microsecondsOfSeconds +
                                                          minutesMicros +
                                                          hoursMicros
                                                         Just parsedPartialSecond ->
                                                          microsecondsOfSeconds + parsedPartialSecond +
                                                          minutesMicros +
                                                          hoursMicros

    return $ normalizeSign parsedMicroseconds

  let allMonths = 12 * parsedYears + parsedMonths
  return Interval { intervalMonths = allMonths,
                    intervalDays = parsedDays,
                    intervalMicroseconds = fromIntegral parsedMicroseconds}
    where optionalS = option 's' (char 's')
          optionalSpace = option ' ' space
          microsecondScale = 1000000

intervalBuilder :: Interval -> Builder
intervalBuilder = intervalB

parseInterval :: B.ByteString -> Either String Interval
parseInterval = A.parseOnly interval

instance FromField Interval where
  fromField = ff $(inlineTypoid TI.interval) "Interval" parseInterval

ff :: PQ.Oid -> String -> (B8.ByteString -> Either String a)
   -> Field -> Maybe B8.ByteString -> Conversion a
ff compatOid hsType parse f mstr =
  if typeOid f /= compatOid
  then err Incompatible ""
  else case mstr of
         Nothing -> err UnexpectedNull ""
         Just str -> case parse str of
                       Left msg -> err ConversionFailed msg
                       Right val -> return val
 where
   err errC msg = do
     typnam <- typename f
     left $ errC (B8.unpack typnam)
                 (tableOid f)
                 (maybe "" B8.unpack (name f))
                 hsType
                 msg
{-# INLINE ff #-}

left :: Exception a => a -> Conversion b
left = conversionError

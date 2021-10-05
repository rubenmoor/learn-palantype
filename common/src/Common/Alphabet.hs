{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}

module Common.Alphabet where

import           Data.Aeson   (FromJSON, FromJSONKey, ToJSON, ToJSONKey)
import           Data.List    (sort)
import           Data.Set     (Set)
import qualified Data.Set     as Set
import           Data.Text    (Text)
import qualified Data.Text    as Text
import           GHC.Generics (Generic)

-- SCPTH+MFRNLYOEAUI^NLCMFRPT+SH
data PTChar =
    LeftS
  | LeftC
  | LeftP
  | LeftT
  | LeftH
  | LeftCross
  | LeftM
  | LeftF
  | LeftR
  | LeftN
  | LeftL
  | LeftY
  | LeftO
  | LeftE
  | LeftPipe
  | RightPipe
  | RightA
  | RightU
  | MiddleI
  | RightPoint
  | RightN
  | RightL
  | RightC
  | RightM
  | RightF
  | RightR
  | RightP
  | RightT
  | RightCross
  | RightS
  | RightH
  | RightE
  deriving (Eq, Ord, Generic)

instance FromJSON PTChar
instance ToJSON PTChar
instance FromJSONKey PTChar
instance ToJSONKey PTChar

instance Show PTChar where
  show LeftS      = "S-"
  show LeftC      = "C-"
  show LeftP      = "P-"
  show LeftT      = "T-"
  show LeftH      = "H-"
  show LeftCross  = "+-"
  show LeftM      = "M-"
  show LeftF      = "F-"
  show LeftR      = "R-"
  show LeftN      = "N-"
  show LeftL      = "L-"
  show LeftY      = "Y-"
  show LeftO      = "O-"
  show LeftE      = "E-"
  show LeftPipe   = "|-"
  show RightPipe  = "-|"
  show RightA     = "-A"
  show RightU     = "-U"
  show MiddleI    = "I"
  show RightPoint = "-^"
  show RightN     ="-N"
  show RightL     = "-L"
  show RightC     = "-C"
  show RightM     = "-M"
  show RightF     = "-F"
  show RightR     = "-R"
  show RightP     = "-P"
  show RightT     = "-T"
  show RightCross = "-+"
  show RightS     = "-S"
  show RightH     = "-H"
  show RightE     = "-e"

instance Read PTChar where
  readsPrec _ [] = []
  readsPrec _ [first] = case first of
    'I' -> [(MiddleI, "")]
    _   -> []
  readsPrec _ (first:rest1@(second:rest2)) = case first of
    'S' | second == '-' -> [(LeftS, rest2)]
    'C' | second == '-' -> [(LeftC, rest2)]
    'P' | second == '-' -> [(LeftP, rest2)]
    'T' | second == '-' -> [(LeftT, rest2)]
    'H' | second == '-' -> [(LeftH, rest2)]
    '+' | second == '-' -> [(LeftCross, rest2)]
    'M' | second == '-' -> [(LeftM, rest2)]
    'F' | second == '-' -> [(LeftF, rest2)]
    'R' | second == '-' -> [(LeftR, rest2)]
    'N' | second == '-' -> [(LeftN, rest2)]
    'L' | second == '-' -> [(LeftL, rest2)]
    'Y' | second == '-' -> [(LeftY, rest2)]
    'O' | second == '-' -> [(LeftO, rest2)]
    'E' | second == '-' -> [(LeftE, rest2)]
    '|' | second == '-' -> [(LeftPipe, rest2)]
    'I' -> [(MiddleI, rest1)]
    '-' -> case second of
      '|' -> [(RightPipe, rest2)]
      'A' -> [(RightA, rest2)]
      'U' -> [(RightU, rest2)]
      '^' -> [(RightPoint, rest2)]
      'N' -> [(RightN, rest2)]
      'L' -> [(RightL, rest2)]
      'C' -> [(RightC, rest2)]
      'M' -> [(RightM, rest2)]
      'F' -> [(RightF, rest2)]
      'R' -> [(RightR, rest2)]
      'P' -> [(RightP, rest2)]
      'T' -> [(RightT, rest2)]
      '+' -> [(RightCross, rest2)]
      'S' -> [(RightS, rest2)]
      'H' -> [(RightH, rest2)]
      'e' -> [(RightE, rest2)]
      _   -> []
    _ -> []

isLeftHand :: PTChar -> Bool
isLeftHand = \case
  LeftS      -> True
  LeftC      -> True
  LeftP      -> True
  LeftT      -> True
  LeftH      -> True
  LeftCross  -> True
  LeftM      -> True
  LeftF      -> True
  LeftR      -> True
  LeftN      -> True
  LeftL      -> True
  LeftY      -> True
  LeftO      -> True
  LeftE      -> True
  LeftPipe   -> True
  RightPipe  -> False
  RightA     -> False
  RightU     -> False
  MiddleI    -> True
  RightPoint -> False
  RightN     -> False
  RightL     -> False
  RightC     -> False
  RightM     -> False
  RightF     -> False
  RightR     -> False
  RightP     -> False
  RightT     -> False
  RightCross -> False
  RightS     -> False
  RightH     -> False
  RightE     -> False

isRightHand :: PTChar -> Bool
isRightHand = \case
  LeftS      -> False
  LeftC      -> False
  LeftP      -> False
  LeftT      -> False
  LeftH      -> False
  LeftCross  -> False
  LeftM      -> False
  LeftF      -> False
  LeftR      -> False
  LeftN      -> False
  LeftL      -> False
  LeftY      -> False
  LeftO      -> False
  LeftE      -> False
  LeftPipe   -> False
  RightPipe  -> True
  RightA     -> True
  RightU     -> True
  MiddleI    -> True
  RightPoint -> True
  RightN     -> True
  RightL     -> True
  RightC     -> True
  RightM     -> True
  RightF     -> True
  RightR     -> True
  RightP     -> True
  RightT     -> True
  RightCross -> True
  RightS     -> True
  RightH     -> True
  RightE     -> True

newtype PTChord = PTChord { unPTChord :: [PTChar] }
  deriving (Eq, Ord)

-- make sure the chord have the letters sorted
mkPTChord :: Set PTChar -> PTChord
mkPTChord = PTChord . sort . Set.toList

showChord :: PTChord -> Text
showChord = Text.unwords . fmap showLetter . unPTChord

showChord' :: PTChord -> Text
showChord' = Text.unwords . fmap showKey . unPTChord

showKey :: PTChar -> Text
showKey = \case
  LeftS -> "S-"
  LeftC -> "C-"
  LeftP -> "P-"
  LeftT -> "T-"
  LeftH -> "H-"
  LeftCross -> "+-"
  LeftM -> "M-"
  LeftF -> "F-"
  LeftR -> "R-"
  LeftN -> "N-"
  LeftL -> "L-"
  LeftY -> "Y"
  LeftO -> "O"
  LeftE -> "E-"
  LeftPipe -> "|-"
  RightPipe -> "-|"
  RightA -> "A"
  RightU -> "U"
  MiddleI -> "I"
  RightPoint -> "^"
  RightN -> "-N"
  RightL -> "-L"
  RightC -> "-C"
  RightM -> "-M"
  RightF -> "-F"
  RightR -> "-R"
  RightP -> "-P"
  RightT -> "-T"
  RightCross -> "-+"
  RightS -> "-S"
  RightH -> "-H"
  RightE -> "-e"

showLetter :: PTChar -> Text
showLetter = \case
  LeftS -> "S"
  LeftC -> "C"
  LeftP -> "P"
  LeftT -> "T"
  LeftH -> "H"
  LeftCross -> "+"
  LeftM -> "M"
  LeftF -> "F"
  LeftR -> "R"
  LeftN -> "N"
  LeftL -> "L"
  LeftY -> "Y"
  LeftO -> "O"
  LeftE -> "E"
  LeftPipe -> "|"
  RightPipe -> "|"
  RightA -> "A"
  RightU -> "U"
  MiddleI -> "I"
  RightPoint -> "^"
  RightN -> "N"
  RightL -> "L"
  RightC -> "C"
  RightM -> "M"
  RightF -> "F"
  RightR -> "R"
  RightP -> "P"
  RightT -> "T"
  RightCross -> "+"
  RightS -> "S"
  RightH -> "H"
  RightE -> "e"

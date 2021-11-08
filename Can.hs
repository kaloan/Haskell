
{-# LANGUAGE EmptyDataDeriving #-}
module Can where

import           Binary (Binary (..), Bit (..))

data LeadingOne
  = CanOne
  | LeadingOne :| Bit
  deriving Show


canOne :: LeadingOne
canOne = CanOne

data Can
  = CanZero
  | LO LeadingOne
  deriving Show

canZero :: Can
canZero = CanZero

snoc :: Can -> Bit -> Can
snoc CanZero Zero        = CanZero
snoc CanZero One         = LO CanOne
snoc (LO leadingOne) bit = LO $ leadingOne :| bit

forgetLeadingOne :: LeadingOne -> Binary
forgetLeadingOne CanOne           = End :. One
forgetLeadingOne (leading :| bit) = forgetLeadingOne leading :. bit

forget :: Can -> Binary
forget CanZero         = End
forget (LO leadingOne) = forgetLeadingOne leadingOne

canonicalise :: Binary -> Can
canonicalise End                  = CanZero
canonicalise (End :. Zero)        = CanZero
canonicalise (End :. Zero :. bin) = canonicalise $ End :. bin
canonicalise (End :. One :. bin)  = LO $ CanOne :. bin
canonicalise (bin :. Zero)        = LO $ canonicalise bin :| Zero
canonicalise (bin :. One)         = LO $ canonicalise bin :| One

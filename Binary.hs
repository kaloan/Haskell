module Binary where

data Binary
  = End
  | Binary :. Bit
  deriving Show

data Bit = Zero | One
  deriving Show

infixl 6 :.

succBinary :: Binary -> Binary
succBinary End           = End :. One
succBinary (bin :. Zero) = bin :. One
succBinary (bin :. One)  = succBinary bin :. Zero

integerToBinary :: Integer -> Binary
integerToBinary 0 = End
integerToBinary n = succBinary $ integerToBinary $ n - 1

binaryToInteger :: Binary -> Integer
binaryToInteger End           = 0
binaryToInteger (bin :. Zero) = 2 * binaryToInteger bin
binaryToInteger (bin :. One)  = 1 + 2 * binaryToInteger bin

hasLeadingZero :: Binary -> Bool
hasLeadingZero End           = False
hasLeadingZero (bin :. Zero) = True
hasLeadingZero (bin :. One)  = False

isEnd :: Binary -> Bool
isEnd End = True
isEnd _   = False

canonicalise :: Binary -> Binary
canonicalise End                  = End
canonicalise (End :. Zero)        = End
canonicalise (End :. Zero :. bin) = canonicalise $ End :. bin
canonicalise (End :. One :. bin)  = End :. One :. bin
canonicalise (bin :. Zero)        = canonicalise bin :. Zero
canonicalise (bin :. One)         = canonicalise bin :. One

addBinary :: Binary -> Binary -> Binary
addBinary End bin2                      = bin2
addBinary bin1 End                      = bin1
addBinary (bin1 :. Zero) (bin2 :. Zero) = addBinary bin1 bin2 :. Zero
addBinary (bin1 :. One) (bin2 :. Zero)  = addBinary bin1 bin2 :. One
addBinary (bin1 :. Zero) (bin2 :. One)  = addBinary bin1 bin2 :. One
addBinary (bin1 :. One) (bin2 :. One)   = addBinary bin1 (succBinary bin2) :. Zero

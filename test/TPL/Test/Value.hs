module TPL.Test.Value where

import Control.Applicative

import Test.QuickCheck

import TPL.Parse
import TPL.Value

instance Arbitrary TPLValue where
  arbitrary = oneof [
    nullValue
                    ]
              
permutationsOf :: [a] -> Gen [a]
permutationsOf = listOf1 . oneof . map return
              
idString = do start <- oneof . map return $ idChar
              rest  <- permutationsOf $ idChar ++ ['0'..'9']
              return $ start:rest
  where idChar = '_' : ['a'..'z'] ++ ['A'..'Z']

nullValue = return Null
idValue   = Id <$> idString
numValue  = Number <$> arbitrary
strValue  = String <$> arbitrary
boolValue = Boolean <$> arbitrary
opValue   = Operator <$> permutationsOf operatorCharacters
natValue  = Native <$> idString

patternList = List <$> listOf (frequency [(124, idValue), (1, patternList)])
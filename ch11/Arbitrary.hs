-- file: ch11/Arbitrary.hs

module Arbitrary where

import Test.QuickCheck

{-
class Arbitrary a where
    arbitrary   :: Gen a
    elements    :: [a] -> Gen a
    choose      :: Random a => (a, a) -> Gen a
    oneof       :: [Gen a] -> Gen a
-}

data Ternary
    = Yes
    | No
    | Unknown
    deriving (Eq, Show)

{-
instance Arbitrary Ternary where
    arbitrary   = elements [Yes, No, Unknown]
-}

instance Arbitrary Ternary where
    arbitrary   = do
        n <- choose (0, 2) :: Gen Int
        return $ case n of
                      0 -> Yes
                      1 -> No
                      2 -> Unknown

instance Arbitrary Char where
    arbitrary = elements (['A'..'Z'] ++ ['a'..'z'] ++ " ~!@#$%^&*()")

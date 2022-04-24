module Visitor where

data Pastry = Beignet
            | Cruller
            deriving (Eq, Show)

class Operations p where
    print :: p -> String

instance Operations Pastry where
    print Beignet = "beignet"
    print Cruller = "cruller"

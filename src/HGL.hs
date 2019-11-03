-- |
module HGL where

-- |
data Event
  = Char
    { char   :: !Char
    , isDown :: !Bool
    }
  | Button
    { pt     :: !Point
    , isLeft :: !Bool
    , isDown :: !Bool
    }
  | MouseMove
    { pt :: !Point
    }
  deriving Show

-- |
newtype Point = Point (Int, Int)
  deriving Show

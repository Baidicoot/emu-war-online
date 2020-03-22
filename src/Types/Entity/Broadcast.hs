module Types.Entity.Broadcast where
-- gameplay-related types

data Broadcast = Broadcast String String deriving(Eq) -- entity -> entity communication

instance Show Broadcast where
    show (Broadcast a b) = a ++ ':':b

data Action -- entity -> game communication
    = Attack Int Int
    | Summon Int (Int, Int)
    deriving(Eq, Show)

data Event -- game -> entity communication
    -- to be added as neccesary
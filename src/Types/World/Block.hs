module Types.World.Block where

data Block = Block {
    text :: Char,
    wall :: Bool
}

type BlockDataTable = [Block]
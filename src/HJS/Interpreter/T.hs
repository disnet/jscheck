module T where



class Fraggle a where
    frag :: a  -> (Int -> Int)

instance Fraggle Bool where
    frag _ = const 0

instance Fraggle a => Fraggle (a -> Bool) where
    frag x y = 1

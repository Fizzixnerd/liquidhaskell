module Client0 where

    import Client1 
    
    a :: Eq a => B a -> Bool 
    a (B x y) = x == y 

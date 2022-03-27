module Parse1 where


{-@ test :: v:a -> (r:a, l:a) @-}
test x = (x, x)

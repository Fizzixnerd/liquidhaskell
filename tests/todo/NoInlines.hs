module NoInlines where

foo :: IO ()
foo =
  if True 
    then return ()
    else return ()

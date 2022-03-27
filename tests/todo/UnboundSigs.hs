module UnboundSigs where



data MI s
  = Small { mi_input :: String  }


{-@ Small :: forall s. {v:String | s == v } -> MI s @-}

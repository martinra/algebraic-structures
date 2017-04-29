{-# LANGUAGE
    FlexibleContexts
  #-}

module Math.Structure.Tasty.Intertwine
where


intertwiningMorphisms
  :: Eq a
  => (b -> c)
  -> (b -> a)
  -> (c -> a)
  -> b
  -> Bool
intertwiningMorphisms bToC f g x
  = f x == g (bToC x)

intertwiningInnerPairing
  :: Eq a
  => (b -> c)
  -> (b -> b -> a)
  -> (c -> c -> a)
  -> b -> b
  -> Bool
intertwiningInnerPairing bToC
  = intertwiningPairing bToC bToC

intertwiningPairing
  :: Eq a
  => (b -> c) -> (b' -> c')
  -> (b -> b' -> a)
  -> (c -> c' -> a)
  -> b -> b'
  -> Bool
intertwiningPairing bToC b'ToC' f g x y
  = f x y == g (bToC x) (b'ToC' y)

intertwiningEndomorphisms
  ::  Eq c 
  => (b -> c)
  -> (b -> b)
  -> (c -> c)
  -> b
  -> Bool
intertwiningEndomorphisms bToC f g x
  = bToC (f x) == g (bToC x)

intertwiningBinaryOperators
  :: Eq c
  => (b -> c)
  -> (b -> b -> b)
  -> (c -> c -> c)
  -> b -> b
  -> Bool
intertwiningBinaryOperators bToC = intertwiningLeftActions bToC bToC

intertwiningLeftActions
  :: Eq c
  => (b -> c) -> (b' -> c')
  -> (b -> b' -> b)
  -> (c -> c' -> c)
  -> b -> b'
  -> Bool
intertwiningLeftActions bToC b'ToC' f g x y
  = bToC (f x y) == g (bToC x) (b'ToC' y)

intertwiningRightActions
  :: Eq c
  => (b -> c) -> (b' -> c')
  -> (b' -> b -> b)
  -> (c' -> c -> c)
  -> b' -> b
  -> Bool
intertwiningRightActions bToC b'ToC' f g x y
  = bToC (f x y) == g (b'ToC' x) (bToC y)

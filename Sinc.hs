module Sinc ( sinc ) where

epsilon :: RealFloat a => a
epsilon = encodeFloat 1 (fromIntegral $ 1-floatDigits epsilon)

sinc :: (RealFloat a) => a -> a
sinc x =
  if abs x >= taylor_n_bound
    then sin (pi * x) / (pi * x)
    else 1 - x^2/6 + x^4/120
  where
    taylor_n_bound = sqrt $ sqrt epsilon

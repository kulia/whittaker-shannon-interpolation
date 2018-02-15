module SincInterpolate ( resample ) where

import Sinc

resample :: (Float, [Float]) -> (Float -> Float)
resample (ts, signal) =
  signalFunc
   where
     signalFunc t = sum $ zipWith (*) signal (bases t)
     bases t = [sinc $ (t - n*ts)/ts | n <- [0..]]

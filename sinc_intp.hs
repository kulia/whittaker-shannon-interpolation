import Graphics.Rendering.Chart.Easy
import Graphics.Rendering.Chart.Backend.Diagrams

type Sample = Float
type SamplingPeriod = Float
type SignalFunc = Time -> Value
type Time = Float
type Value = Float

data DiscreteSignal = DiscreteSignal SamplingPeriod [Sample]


signal :: SignalFunc
signal x = (sin (x*pi/45) + 1) / 2 * (sin (x*pi/5))


resample :: DiscreteSignal -> SignalFunc
resample signal =
  signalFunc
   where
     DiscreteSignal ts samples = signal
     signalFunc t = sum $ zipWith (*) samples (bases t)
     bases t = [sinc $ (t - n*ts)/ts | n <- [0..]]


sampledSignal :: DiscreteSignal
sampledSignal = DiscreteSignal 7 $ map signal [0,7..400]

resampledSignal :: SignalFunc
resampledSignal = resample sampledSignal

main = toFile def "example_resample.svg" $ do
  layout_title .= "Resample Signal"
  setColors [opaque blue, opaque red]
  plot (line "Signal" [(map (\x -> (x,resampledSignal x)) [0,(0.5)..400])])
  plot (points "Discrete Time" (zip [0,7..400] samples))
  where
    DiscreteSignal _ samples = sampledSignal


-------------
epsilon :: RealFloat a => a
epsilon = encodeFloat 1 (fromIntegral $ 1-floatDigits epsilon)

sinc :: (RealFloat a) => a -> a
sinc x =
  if abs x >= taylor_n_bound
    then sin x / x
    else 1 - x^2/6 + x^4/120
 where
  taylor_n_bound = sqrt $ sqrt epsilon

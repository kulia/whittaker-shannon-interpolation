import Graphics.Rendering.Chart.Easy
import Graphics.Rendering.Chart.Backend.Diagrams

import SincInterpolate

signal :: Float -> Float
signal x = (sin (x*pi/45) + 1) / 2 * (sin (x*pi/5))

ts = 2.3

sampledSignal :: (Float, [Float])
sampledSignal = (ts, map signal [0,ts..400])

resampledSignal :: Float -> Float
resampledSignal = resample sampledSignal


main = toFile def "fig/example_resample.svg" $ do
  layout_title .= "Resample Signal"
  setColors [opaque green ,opaque blue, opaque red]
  -- plot (line "Sinc" [])
  plot (line "Signal" [(map (\x -> (x, signal x)) [0,(0.5)..400])])
  plot (line "Reconstructed Signal" [(map (\x -> (x, resampledSignal x)) [0,(0.5)..400])])
  plot (points "Discrete Time" (zip [0,ts..400] samples))
  where
    (ts, samples) = sampledSignal

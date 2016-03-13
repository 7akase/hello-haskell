import Data.Complex
import Numeric.FFT
import Graphics.Gnuplot.Simple
import Control.Applicative

sample = do
  let xs  = linearScale 1024 (0, 2*(pi :: Double))
  let ys  = (:+ 0.0) <$> sin <$> xs
  plotPath [] $ zip xs $ magnitude <$> fft ys

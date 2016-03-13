import Graphics.Gnuplot.Simple

plotData xs ys = plotPath [(Title "untitled")] $ zip xs ys

sample = do
  let xs = linearScale 1000 (0, 2*(pi :: Float))
  let ys = fmap sin xs
  let points = zip xs ys
  plotPath [(Title "untitled")] points

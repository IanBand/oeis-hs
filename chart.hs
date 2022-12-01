import Graphics.Rendering.Chart.Easy
import Graphics.Rendering.Chart.Backend.Diagrams(toFile)
import Sequence

main = toFile def "chart.svg" $ do
    layout_title .= "oeis"
    setColors [opaque blue, opaque red, opaque green]
    len <- return 400
    plot (points "est sqrt"    (zip [1..len]  (map (floor . sqrt . fromIntegral) [1..len]) ))
    plot (points "est log"     (zip [1..len]  (map (floor . log . fromIntegral) [1..len]) ))
    plot (points "doubleCountSeq points" (zip [1..len]  (rseq doubleCountSeq 0 len)))
    --plot (points "lessMost" (zip [1..len]  (rseq lessMostSeq 0 len)))
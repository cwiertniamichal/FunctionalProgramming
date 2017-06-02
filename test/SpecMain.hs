import SpecHU(runHU)
import SpecQC(runQC)

main:: IO ()
main = do
    runHU
    runQC
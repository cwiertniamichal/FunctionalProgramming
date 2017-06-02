import SpecHU(runHU)
import SpecPrintHU(runPrintHU)
import SpecQC(runQC)

main:: IO ()
main = do
    runPrintHU 
    runHU
    runQC
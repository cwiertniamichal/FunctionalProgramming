@ECHO OFF

stack build --haddock

REM .stack-work\install\ff4c9dd0\bin\Parser-exe.exe

stack path --local-install-root > f
set /p var=<f
del /f f
set var=%var%\bin\Parser-exe.exe

ECHO.
ECHO EXECUTING MAIN  
ECHO ==================================================================================================== & ECHO. 
%var%
ECHO. & ECHO ==================================================================================================== & ECHO. 

stack clean
stack test --coverage
stack hpc report --all --open

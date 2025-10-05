@echo off
echo COBOL Program Runner
echo ===================

set COBOL_HOME=%CD%
set PATH=%COBOL_HOME%\bin;%PATH%

if not exist bin (
    echo Error: bin directory not found
    echo Please run build.bat first to compile the programs
    pause
    exit /b 1
)

echo Available programs:
echo ==================
dir /b bin\*.exe

echo.
echo Enter program name (without .exe) to run:
set /p PROGRAM_NAME=

if not exist bin\%PROGRAM_NAME%.exe (
    echo Error: Program %PROGRAM_NAME%.exe not found
    pause
    exit /b 1
)

echo.
echo Running %PROGRAM_NAME%.exe...
echo =============================
bin\%PROGRAM_NAME%.exe

echo.
echo Program execution completed.
pause

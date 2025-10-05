@echo off
echo COBOL Test Script
echo =================

set COBOL_HOME=%CD%
set PATH=%COBOL_HOME%\bin;%PATH%

echo Running all sample programs...
echo =============================

if not exist bin (
    echo Error: bin directory not found
    echo Please run build.bat first
    pause
    exit /b 1
)

echo.
echo Testing Hello World program...
echo ==============================
if exist bin\hello.exe (
    bin\hello.exe
    echo.
) else (
    echo hello.exe not found
)

echo.
echo Testing Calculator program...
echo =============================
if exist bin\calculator.exe (
    echo Note: Calculator requires interactive input
    echo You can run it manually with: bin\calculator.exe
    echo.
) else (
    echo calculator.exe not found
)

echo.
echo Testing File I/O program...
echo ==========================
if exist bin\fileio.exe (
    echo Running file I/O test...
    cd samples
    bin\..\bin\fileio.exe
    cd ..
    echo.
) else (
    echo fileio.exe not found
)

echo.
echo Testing Employee Management program...
echo =====================================
if exist bin\employee.exe (
    echo Note: Employee program requires interactive input
    echo You can run it manually with: bin\employee.exe
    echo.
) else (
    echo employee.exe not found
)

echo.
echo Test completed!
pause

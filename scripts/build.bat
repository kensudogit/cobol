@echo off
echo COBOL Build Script
echo ==================

set COBOL_HOME=%CD%
set PATH=%COBOL_HOME%\bin;%PATH%

echo Checking for GnuCOBOL...
where cobc >nul 2>&1
if %errorlevel% neq 0 (
    echo Error: GnuCOBOL not found in PATH
    echo Please install GnuCOBOL and add it to your PATH
    echo Run setup.bat for installation instructions
    pause
    exit /b 1
)

echo GnuCOBOL found!
cobc --version

echo.
echo Building COBOL programs...

if not exist bin mkdir bin

echo Compiling sample programs...
for %%f in (samples\*.cob) do (
    echo Compiling %%f...
    cobc -x -free -Wall -o bin\%%~nf.exe %%f
    if %errorlevel% neq 0 (
        echo Error compiling %%f
        pause
        exit /b 1
    )
)

echo Compiling source programs...
for %%f in (src\*.cob) do (
    echo Compiling %%f...
    cobc -x -free -Wall -o bin\%%~nf.exe %%f
    if %errorlevel% neq 0 (
        echo Error compiling %%f
        pause
        exit /b 1
    )
)

echo.
echo Build completed successfully!
echo Executables are in the bin directory.
echo.
pause

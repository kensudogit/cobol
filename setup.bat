@echo off
echo COBOL Development Environment Setup
echo ===================================

echo.
echo Checking for GnuCOBOL installation...

where cobc >nul 2>&1
if %errorlevel% neq 0 (
    echo GnuCOBOL is not installed or not in PATH.
    echo.
    echo Please install GnuCOBOL:
    echo 1. Download from: https://sourceforge.net/projects/gnucobol/
    echo 2. Or use package manager: choco install gnucobol
    echo 3. Or use MSYS2: pacman -S mingw-w64-x86_64-gnucobol
    echo.
    echo After installation, add GnuCOBOL to your PATH.
    echo.
    pause
    exit /b 1
)

echo GnuCOBOL found!
cobc --version

echo.
echo Setting up environment...
set COBOL_HOME=%CD%
set PATH=%COBOL_HOME%\bin;%PATH%

echo.
echo Environment variables set:
echo COBOL_HOME=%COBOL_HOME%
echo PATH updated to include %COBOL_HOME%\bin

echo.
echo COBOL development environment is ready!
echo.
echo To compile a COBOL program:
echo   cobc -x -o program.exe program.cob
echo.
echo To run a program:
echo   program.exe
echo.
pause

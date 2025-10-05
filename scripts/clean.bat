@echo off
echo COBOL Clean Script
echo ==================

echo Cleaning build artifacts...

if exist bin (
    echo Removing bin directory...
    rmdir /s /q bin
    echo bin directory removed.
) else (
    echo bin directory does not exist.
)

if exist samples\output.txt (
    echo Removing output.txt...
    del samples\output.txt
    echo output.txt removed.
)

echo.
echo Clean completed successfully!
pause

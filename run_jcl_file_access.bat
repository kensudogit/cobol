@echo off
REM JCL File Access Control Sample Runner
REM This batch file compiles and runs the JCL file access control sample programs

setlocal enabledelayedexpansion

echo ============================================
echo JCL File Access Control Sample Runner
echo ============================================
echo.

REM Set up directories
set SRC_DIR=java\com\example\cobol
set BIN_DIR=bin

REM Create bin directory if it doesn't exist
if not exist %BIN_DIR% (
    mkdir %BIN_DIR%
    echo Created bin directory.
)

REM Compile all Java files
echo Compiling Java files...
javac -d %BIN_DIR% %SRC_DIR%\JCLDCBInfo.java
if errorlevel 1 (
    echo Failed to compile JCLDCBInfo.java
    exit /b 1
)

javac -d %BIN_DIR% -cp %BIN_DIR% %SRC_DIR%\JCLFileAccessController.java
if errorlevel 1 (
    echo Failed to compile JCLFileAccessController.java
    exit /b 1
)

javac -d %BIN_DIR% -cp %BIN_DIR% %SRC_DIR%\JCLFileAccessSample.java
if errorlevel 1 (
    echo Failed to compile JCLFileAccessSample.java
    exit /b 1
)

javac -d %BIN_DIR% -cp %BIN_DIR% %SRC_DIR%\JCLSampleAndTestCases.java
if errorlevel 1 (
    echo Failed to compile JCLSampleAndTestCases.java
    exit /b 1
)

echo Compilation successful!
echo.

REM Run the sample programs
echo Running JCL File Access Sample...
echo ============================================
java -cp %BIN_DIR% com.example.cobol.JCLFileAccessSample
echo.
echo.

echo Running JCL Sample and Test Cases Generator...
echo ============================================
java -cp %BIN_DIR% com.example.cobol.JCLSampleAndTestCases
echo.
echo.

echo ============================================
echo All samples executed successfully!
echo ============================================
echo.

pause


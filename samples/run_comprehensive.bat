@echo off
REM ================================================================
REM COBOL Comprehensive Sample Program - Windows Batch Runner
REM COBOL言語の全貌デモンストレーションスクリプト
REM ================================================================

SET PROGRAM_NAME=comprehensive_sample
SET SOURCE_FILE=comprehensive_sample.cob
SET CURRENT_DIR=%~dp0
SET DATA_DIR=%CURRENT_DIR%data
SET OUTPUT_DIR=%CURRENT_DIR%output

:MAIN
echo.
echo ================================================================
echo COBOL Comprehensive Sample Program
echo COBOL言語の全貌デモンストレーション
echo ================================================================
echo.

:CHECK_ENVIRONMENT
REM 環境チェック
echo Checking environment...
where cobc >nul 2>&1
if %errorlevel% neq 0 (
    echo ERROR: GNU COBOL (cobc) not found in PATH
    echo Please install GNU COBOL or add cobc to your PATH
    echo Download from: https://sourceforge.net/projects/gnucobol/
    pause
    exit /b 1
)
echo GNU COBOL compiler found ✓

:CREATE_DIRECTORIES
REM ディレクトリ作成
echo Creating directories...
if not exist "%DATA_DIR%" mkdir "%DATA_DIR%"
if not exist "%OUTPUT_DIR%" mkdir "%OUTPUT_DIR%"
echo Directories created ✓

:COMPILE_PROGRAM
REM プログラムのコンパイル
echo.
echo Compiling %PROGRAM_NAME%...
cobc -free -frelax-syntax-check -g -o "%PROGRAM_NAME%.exe" "%SOURCE_FILE%"
if %errorlevel% neq 0 (
    echo ERROR: Compilation failed
    echo Please check the COBOL source code syntax
    pause
    exit /b 1
)
echo Compilation completed successfully ✓

:CREATE_SAMPLE_DATA
REM サンプルデータファイルの作成
echo.
echo Creating sample data files...

REM 顧客マスタファイルの作成
echo Creating customer master file...
echo 1234567John Smith           123 Main St           AnytownCA9021012345678901000A20250128                    > "%DATA_DIR%\CUSTOMER.MAST"
echo 1234568Jane Doe             456 Oak Ave           SeattleWA9810123456789010001A20250128                    >> "%DATA_DIR%\CUSTOMER.MAST"
echo 1234569Bob Johnson          789 Pine St           BostonMA0210123456789010002I20250128                    >> "%DATA_DIR%\CUSTOMER.MAST"
echo Customer master file created ✓

REM 売上トランザクションファイルの作成
echo Creating sales transaction file...
echo 000000011234567PROD-00110000001001000.0000002025012800010000000121234568PROD-0022000000050005000.000000502501280002000000021234568PROD-0031500000150007500.000000113501280003 > "%DATA_DIR%\SALES.TRAN"
echo Sales transaction file created ✓

:COPY_DATA_TO_WORKING_DIR
REM データファイルを実行ディレクトリにコピー
copy "%DATA_DIR%\*.*" . >nul 2>&1
echo Sample data copied ✓

:RUN_PROGRAM
REM プログラムの実行
echo.
echo Running %PROGRAM_NAME%...
echo.
echo ================================================================
echo PROGRAM EXECUTION OUTPUT
echo ================================================================
"%PROGRAM_NAME%.exe"
set RUN_STATUS=%errorlevel%

echo.
echo ================================================================
echo PROGRAM EXECUTION COMPLETED
echo ================================================================

if %RUN_STATUS% equ 0 (
    echo Program executed successfully ✓
) else (
    echo Program execution ended with status code: %RUN_STATUS%
)

:SHOW_OUTPUT_FILES
REM 出力ファイルの表示
echo.
echo Generated output files:
if exist "DAILY-REPORT.TXT" (
    echo ✓ Report file created: DAILY-REPORT.TXT
    echo.
    echo Report content:
    echo ================================================================
    type "DAILY-REPORT.TXT"
    echo ================================================================
) else (
    echo ✗ Report file not created
)

if exist "lambda.log" (
    echo ✓ Log file created: lambda.log
    echo.
    echo Log content:
    echo ================================================================
    type "lambda.log"
    echo ================================================================
) else (
    echo ✗ Log file not created

:SUMMARY
REM 実行サマリー
echo.
echo ================================================================
echo EXECUTION SUMMARY
echo ================================================================
echo.
echo Program: %PROGRAM_NAME%
echo Source: %SOURCE_FILE%
echo Data files: %DATA_DIR%
echo Output files: Current directory
echo.
echo This demonstration tested the following COBOL features:
echo.
echo DIVISIONs:
echo   - IDENTIFICATION DIVISION (Program identification)
echo   - ENVIRONMENT DIVISION (File definitions)
echo   - DATA DIVISION (Data structures and variables)
echo   - PROCEDURE DIVISION (Program logic)
echo.
echo Data Types:
echo   - Numeric data (PIC 9, 9V99)
echo   - Alphanumeric data (PIC X, A)
echo   - Edited data (PIC Z, edit mask)
echo   - Computational data (COMP, COMP-3)
echo.
echo File Operations:
echo   - Sequential file access (READ/WRITE)
echo   - File status checking
echo   - Multi-file processing
echo.
echo Structured Programming:
echo   - Conditional logic (IF/ELSE, EVALUATE)
echo   - Loop constructs (PERFORM VARYING)
echo   - Subroutines (PERFORM)
echo   - Array processing (OCCURS)
echo.
echo Advanced Features:
echo   - String manipulation (STRING/UNSTRING/INSPECT)
echo   - Mathematical calculations (COMPUTE)
echo   - Report generation
echo   - Error handling
echo.
echo COBOL remains a powerful language for business applications,
echo particularly in enterprise systems requiring high precision
echo numerical calculations and data processing.
echo.
echo ================================================================

:CLEANUP
REM 一時ファイルのクリーンアップ
echo.
echo Cleaning up temporary files...
del "%PROGRAM_NAME%.exe" 2>nul
echo Cleanup completed ✓

:PAUSE_AND_EXIT
echo.
echo Press any key to exit...
pause >nul
exit /b %RUN_STATUS%

REM ================================================================
REM END OF SCRIPT
REM ================================================================

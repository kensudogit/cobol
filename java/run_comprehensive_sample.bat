@echo off
REM ================================================================
REM COBOL Comprehensive Sample Program - Java Runner for Windows
REM COBOL言語の全貌デモンストレーション（Java実装版）
REM ================================================================

SET PROGRAM_NAME=ComprehensiveSample
SET RUNNER_NAME=ComprehensiveSampleRunner
SET CURRENT_DIR=%~dp0
SET JAVA_CLASS_PATH=%CURRENT_DIR%
SET OUTPUT_FILE=DAILY-REPORT.TXT
SET LOG_FILE=lambda.log

:MAIN
echo.
echo ================================================================
echo COBOL Comprehensive Sample Program (Java Implementation)
echo COBOL言語の全貌デモンストレーション - Java移植版
echo ================================================================
echo.

:CHECK_JAVA_ENVIRONMENT
REM Java環境チェック
echo Checking Java environment...
java -version >nul 2>&1
if %errorlevel% neq 0 (
    echo ERROR: Java not found in PATH　
    echo Please install Java JDK 8 or later and add to PATH
    echo Download from: https://www.oracle.com/java/technologies/downloads/
    pause
    exit /b 1
)

REM Javaバージョン表示
java -version
echo Java environment found ✓

:SETUP_ENVIRONMENT
REM 環境設定
cd /d "%CURRENT_DIR%"
echo.
echo Setting up Java environment...
echo Class path: %JAVA_CLASS_PATH%
echo Working directory: %CD%

:CREATE_DATA_DIRECTORIES
REM ディレクトリ作成（必要に応じて）
if not exist "data" echo Creating data directory...
if not exist "output" echo Creating output directory...

:CLEAN_PREVIOUS_OUTPUTS
REM 前回の出力ファイルをクリア（コメントアウト可能）
REM echo Cleaning previous output files...
REM if exist "%OUTPUT_FILE%" del "%OUTPUT_FILE%"
REM if exist "%LOG_FILE%" del "%LOG_FILE%"

:COMPILE_JAVA_PROGRAMS
REM Javaプログラムのコンパイル
echo.
echo Compiling Java programs...
echo Compiling %PROGRAM_NAME%...
javac -cp . "%PROGRAM_NAME%.java"
if %errorlevel% neq 0 (
    echo ERROR: Failed to compile %PROGRAM_NAME%.java
    echo Please check Java syntax errors
    pause
    exit /b 1
)

echo Compiling %RUNNER_NAME%...
javac -cp . "%RUNNER_NAME%.java"
if %errorlevel% neq 0 (
    echo ERROR: Failed to compile %RUNNER_NAME%.java
    echo Please check Java syntax errors
    pause
    exit /b 1
)
echo Java compilation completed successfully ✓

:EXECUTE_PROGRAM_WITH_RUNNER
REM ランナー経由でプログラムを実行（サンプルデータ作成→実行）
echo.
echo Executing comprehensive sample program with runner...
echo ================================================================
echo PROGRAM EXECUTION OUTPUT
echo ================================================================

java -cp . "%RUNNER_NAME%"

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

:SHOW_OUTPUT_RESULTS
REM 出力結果の確認と表示
echo.
echo Checking generated output files...

if exist "%OUTPUT_FILE%" (
    echo ✓ Report file generated: %OUTPUT_FILE%
    echo.
    echo ================================================================
    echo REPORT FILE CONTENT
    echo ================================================================
    type "%OUTPUT_FILE%"
    echo ================================================================
) else (
    echo ✗ Report file not generated: %OUTPUT_FILE%
)

if exist "%LOG_FILE%" (
    echo.
    echo ✓ Log file generated: %LOG_FILE%
    echo.
    echo ================================================================
    echo LOG FILE CONTENT (Last 20 lines)
    echo ================================================================
    powershell "Get-Content '%LOG_FILE%' | Select-Object -Last 20"
    echo ================================================================
) else (
    echo.
    echo ✗ Log file not generated: %LOG_FILE%
)

:SHOW_DATA_FILES
REM データファイルの一覧表示
echo.
echo ================================================================
echo GENERATED DATA FILES
echo ================================================================
echo.
echo Customer Master File:
if exist "CUSTOMER.MAST" (
    echo ✓ CUSTOMER.MAST exists (%~zCUSTOMER.MAST% bytes)
) else (
    echo ✗ CUSTOMER.MAST not found
)

echo.
echo Sales Transaction File:
if exist "SALES.TRAN" (
    echo ✓ SALES.TRAN exists (%~zSALES.TRAN% bytes)
) else (
    echo ✗ SALES.TRAN not found
)

:EXECUTE_DIRECT_PROGRAM_TEST
REM 直接プログラム実行テスト（ランナーなし）
echo.
echo ================================================================
echo DIRECT PROGRAM EXECUTION TEST
echo ================================================================
echo.
echo Testing direct program execution...
echo (This skips data file creation and runs program directly)
echo.

REM 顧客マスタファイルが存在する場合のみテスト
if exist "CUSTOMER.MAST" (
    java -cp . "%PROGRAM_NAME%"
    set DIRECT_RUN_STATUS=%errorlevel%
    
    echo Direct execution completed with status: %DIRECT_RUN_STATUS%
) else (
    echo Skipping direct execution - no sample data files found
    echo (Run the runner first to create sample data)
)

:PERFORMANCE_TEST
REM 簡単なパフォーマンステスト
echo.
echo ================================================================
echo SIMPLE PERFORMANCE TEST
echo ================================================================
echo.
echo Running multiple iterations to test stability...

set ITERATION_COUNT=5
echo Running %ITERATION_COUNT% iterations...

for /L %%i in (1,1,%ITERATION_COUNT%) do (
    echo Iteration %%i:
    java -cp . "%RUNNER_NAME%" >nul 2>&1
    if %errorlevel% equ 0 (
        echo   ✓ Success
    ) else (
        echo   ✗ Failed with status %errorlevel%
    )
)

:COBOL_TO_JAVA_COMPARISON
REM COBOL to Java対応機能の説明
echo.
echo ================================================================
echo COBOL TO JAVA MIGRATION FEATURES DEMONSTRATED
echo ================================================================
echo.
echo This Java program demonstrates the complete migration of a
echo COBOL comprehensive sample program:
echo.
echo DIVISIONs (COBOL):               Java Implementation:
echo ================================================================
echo IDENTIFICATION DIVISION    →    Class definition and documentation
echo ENVIRONMENT DIVISION       →    File paths, constants, configuration
echo DATA DIVISION             →    Class fields, data structures
echo PROCEDURE DIVISION        →    Method implementations
echo.
echo Data Types (COBOL):                Java Types:
echo ================================================================
echo PIC 9(n)                    →    int, long (numeric)
echo PIC 9(n)V99                 →    BigDecimal (decimal with precision)
echo PIC X(n)                    →    String (text/alphanumeric)
echo PIC A(n)                    →    String (alphabetic)
echo COMP                        →    int/long binary (computed)
echo COMP-3                      →    BigDecimal (packed decimal)
echo.
echo Control Structures (COBOL):       Java Statements:
echo ================================================================
echo PERFORM paragraph            →    method invocation
echo PERFORM VARYING...UNTIL...  →    for/while loop
echo IF...ELSE...END-IF         →    if/else statements
echo EVALUATE...WHEN...END-EVALUATE → switch/case statements
echo DO-WHILE/UNTIL              →    while/do-while loops
echo.
echo File Operations (COBOL):          Java Operations:
echo ================================================================
echo READ file                    →    BufferedReader.readLine()
echo WRITE record                 →    PrintWriter.println()
echo OPEN file                    →    new FileReader/FileWriter()
echo CLOSE file                   →    close() method
echo FILE-STATUS                 →    IOException handling
echo.
echo Array Operations (COBOL):          Java Collections:
echo ================================================================
echo OCCURS n TIMES              →    List<E> collections
echo OCCURS 1 TO n DEPENDING     →    ArrayList with dynamic sizing
echo SEARCH array                →    Stream.filter() operations
echo SET index                   →    Collection iterator operations
echo.
echo String Operations (COBOL):          Java Methods:
echo ================================================================
echo STRING...INTO               →    StringBuilder.append()
echo UNSTRING...INTO             →    String.split() operations
echo INSPECT...REPLACING         →    String.replace() operations
echo INSPECT...TALLYING          →    String.indexOf/regex counting
echo.
echo Mathematical Operations (COBOL):     Java BigDecimal:
echo ================================================================
echo ADD/SUBTRACT/MULTIPLY/DIVIDE →    BigDecimal arithmetic methods
echo COMPUTE                      →    assignment with calculation
echo ROUNDED                      →    setScale(roundingMode)
echo ON-SIZE-ERROR                →    ArithmeticException handling
echo.
echo Error Handling (COBOL):             Java Exception Handling:
echo ================================================================
echo FILE-STATUS checking         →    try/catch IOException blocks
echo ON-OVERFLOW                 →    boolean flag checking
echo ON-SIZE-ERROR               →    ArithmeticException catching
echo AT-END                      →    null return from read operations
echo.
echo This migration maintains the business logic and data processing
echo capabilities of the original COBOL program while leveraging
echo Java's modern language features, collections, and exception
echo handling for better maintainability and performance.
echo.
echo ================================================================

:CLEANUP_COMPILED_CLASSES
REM コンパイル済みクラスファイルのクリーンアップ
echo.
echo Cleaning up compiled class files...
if exist "%PROGRAM_NAME%.class" del "%PROGRAM_NAME%.class"
if exist "%RUNNER_NAME%.class" del "%RUNNER_NAME%.class"
echo Cleanup completed ✓

:FINAL_SUMMARY
REM 最終サマリー
echo.
echo ================================================================
echo FIN AL EXECUTION SUMMARY
echo ================================================================
echo.
echo Program: %PROGRAM_NAME% (Java Implementation)
echo Original: comprehensive_sample.cob (COBOL Program)
echo Migrated: Java with modern collections and exception handling
echo Data Files: CUSTOMER.MAST, SALES.TRAN (Sample Data)
echo Output Files: %OUTPUT_FILE%, %LOG_FILE%
echo.
echo Features demonstrated:
echo ✓ Complete COBOL to Java migration
echo ✓ Business data processing logic
echo ✓ File I/O operations
echo ✓ Mathematical calculations with precision
echo ✓ String manipulation operations
echo ✓ Collection and array processing
echo ✓ Exception handling and error management
echo ✓ Report generation capabilities
echo ✓ Object-oriented programming principles
echo.
echo The Java implementation provides equivalent functionality
echo to the original COBOL program while offering:
echo • Better type safety with compiled-time checking
echo • Modern exception handling mechanisms  
echo • Powerful collection frameworks for data management
echo • Improved performance through JVM optimizations
echo • Enhanced maintainability with OOP principles
echo • Cross-platform compatibility
echo • Integration with modern development tools
echo.
echo COBOL skills remain valuable for legacy system maintenance,
echo while Java migration enables future enhancements and
echo integration with modern enterprise systems.
echo.
echo ================================================================

:PAUSE_AND_EXIT
echo.
echo Press any key to exit...
pause >nul
exit /b %RUN_STATUS%

REM ================================================================
REM END OF SCRIPT
REM ================================================================

@echo off
REM ================================================================
REM COBOL非インタラクティブ処理 - Java版実行スクリプト
REM ================================================================

echo =====================================
echo COBOL Non-Interactive Java Samples
echo =====================================

REM 設定ファイルの準備
echo Preparing configuration files...

REM 設定ファイルのコピー
if not exist "CONFIG.DAT" (
    echo TAX_RATE 0.08 > CONFIG.DAT
    echo COMMISSION_RATE 0.05 >> CONFIG.DAT
    echo MINIMUM_AMOUNT 100.00 >> CONFIG.DAT
    echo MAXIMUM_AMOUNT 999999.99 >> CONFIG.DAT
    echo PROCESSING_LIMIT 1000 >> CONFIG.DAT
    echo Configuration file created: CONFIG.DAT
)

REM パラメータファイルのコピー
if not exist "PARAMS.DAT" (
    echo MODE,A > PARAMS.DAT
    echo RETRY_COUNT,3 >> PARAMS.DAT
    echo Parameter file created: PARAMS.DAT
)

REM 入力データファイルのコピー
if not exist "INPUT.DAT" (
    echo CUSTOMER001,John Smith,1500.50,20250106,Y > INPUT.DAT
    echo CUSTOMER002,Jane Doe,2300.75,20250106,A >> INPUT.DAT
    echo CUSTOMER003,Bob Johnson,850.25,20250106,Y >> INPUT.DAT
    echo CUSTOMER004,Alice Brown,3200.00,20250106,A >> INPUT.DAT
    echo CUSTOMER005,Charlie Wilson,1200.00,20250106,Y >> INPUT.DAT
    echo Input data file created: INPUT.DAT
)

REM バッチ制御ファイルのコピー
if not exist "BATCH.CTL" (
    echo MODE,A > BATCH.CTL
    echo MAX_RETRIES,3 >> BATCH.CTL
    echo NOTIFICATION,Y >> BATCH.CTL
    echo RECOVERY,Y >> BATCH.CTL
    echo Batch control file created: BATCH.CTL
)

REM 処理ファイルのコピー
if not exist "PROCESS.DAT" (
    echo 00000001,Test Data,Y > PROCESS.DAT
    echo 00000002,Sample Data,A >> PROCESS.DAT
    echo 00000003,Example Data,Y >> PROCESS.DAT
    echo 00000004,Demo Data,A >> PROCESS.DAT
    echo 00000005,Trial Data,Y >> PROCESS.DAT
    echo Process file created: PROCESS.DAT
)

REM 生成設定ファイルのコピー
if not exist "GENCONFIG.DAT" (
    echo TOTAL_RECORDS,1000 > GENCONFIG.DAT
    echo GENERATION_MODE,A >> GENCONFIG.DAT
    echo DATA_PATTERN,RANDOM >> GENCONFIG.DAT
    echo RANDOM_SEED,12345 >> GENCONFIG.DAT
    echo Generation config file created: GENCONFIG.DAT
)

REM データテンプレートファイルのコピー
if not exist "TEMPLATE.DAT" (
    echo John Smith,NAME > TEMPLATE.DAT
    echo Jane Doe,NAME >> TEMPLATE.DAT
    echo Bob Johnson,NAME >> TEMPLATE.DAT
    echo Alice Brown,NAME >> TEMPLATE.DAT
    echo Charlie Wilson,NAME >> TEMPLATE.DAT
    echo Premium,CATEGORY >> TEMPLATE.DAT
    echo Standard,CATEGORY >> TEMPLATE.DAT
    echo Basic,CATEGORY >> TEMPLATE.DAT
    echo Economy,CATEGORY >> TEMPLATE.DAT
    echo Luxury,CATEGORY >> TEMPLATE.DAT
    echo A,STATUS >> TEMPLATE.DAT
    echo I,STATUS >> TEMPLATE.DAT
    echo S,STATUS >> TEMPLATE.DAT
    echo P,STATUS >> TEMPLATE.DAT
    echo C,STATUS >> TEMPLATE.DAT
    echo Template file created: TEMPLATE.DAT
)

echo.
echo Configuration files prepared successfully!
echo.

REM プログラムの実行
echo Select program to run:
echo 1. Non-Interactive Sample
echo 2. Batch Processing Sample
echo 3. Automated Data Generation
echo 4. Run All Samples
echo.

set /p choice="Enter your choice (1-4): "

if "%choice%"=="1" goto run_non_interactive
if "%choice%"=="2" goto run_batch
if "%choice%"=="3" goto run_automated
if "%choice%"=="4" goto run_all
goto invalid_choice

:run_non_interactive
echo.
echo Running Non-Interactive Sample...
mvn exec:java -Dexec.mainClass="com.example.cobol.NonInteractiveSample"
goto end

:run_batch
echo.
echo Running Batch Processing Sample...
mvn exec:java -Dexec.mainClass="com.example.cobol.BatchProcessingSample"
goto end

:run_automated
echo.
echo Running Automated Data Generation...
mvn exec:java -Dexec.mainClass="com.example.cobol.AutomatedDataGeneration"
goto end

:run_all
echo.
echo Running All Samples...
echo.
echo 1. Non-Interactive Sample:
mvn exec:java -Dexec.mainClass="com.example.cobol.NonInteractiveSample"
echo.
echo 2. Batch Processing Sample:
mvn exec:java -Dexec.mainClass="com.example.cobol.BatchProcessingSample"
echo.
echo 3. Automated Data Generation:
mvn exec:java -Dexec.mainClass="com.example.cobol.AutomatedDataGeneration"
goto end

:invalid_choice
echo Invalid choice. Please run the script again.
goto end

:end
echo.
echo =====================================
echo Execution completed!
echo =====================================
pause

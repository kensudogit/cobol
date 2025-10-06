#!/bin/bash
# ================================================================
# COBOL非インタラクティブ処理 - Java版実行スクリプト (Linux/Mac)
# ================================================================

echo "====================================="
echo "COBOL Non-Interactive Java Samples"
echo "====================================="

# 設定ファイルの準備
echo "Preparing configuration files..."

# 設定ファイルの作成
if [ ! -f "CONFIG.DAT" ]; then
    cat > CONFIG.DAT << EOF
TAX_RATE 0.08
COMMISSION_RATE 0.05
MINIMUM_AMOUNT 100.00
MAXIMUM_AMOUNT 999999.99
PROCESSING_LIMIT 1000
EOF
    echo "Configuration file created: CONFIG.DAT"
fi

# パラメータファイルの作成
if [ ! -f "PARAMS.DAT" ]; then
    cat > PARAMS.DAT << EOF
MODE,A
RETRY_COUNT,3
EOF
    echo "Parameter file created: PARAMS.DAT"
fi

# 入力データファイルの作成
if [ ! -f "INPUT.DAT" ]; then
    cat > INPUT.DAT << EOF
CUSTOMER001,John Smith,1500.50,20250106,Y
CUSTOMER002,Jane Doe,2300.75,20250106,A
CUSTOMER003,Bob Johnson,850.25,20250106,Y
CUSTOMER004,Alice Brown,3200.00,20250106,A
CUSTOMER005,Charlie Wilson,1200.00,20250106,Y
EOF
    echo "Input data file created: INPUT.DAT"
fi

# バッチ制御ファイルの作成
if [ ! -f "BATCH.CTL" ]; then
    cat > BATCH.CTL << EOF
MODE,A
MAX_RETRIES,3
NOTIFICATION,Y
RECOVERY,Y
EOF
    echo "Batch control file created: BATCH.CTL"
fi

# 処理ファイルの作成
if [ ! -f "PROCESS.DAT" ]; then
    cat > PROCESS.DAT << EOF
00000001,Test Data,Y
00000002,Sample Data,A
00000003,Example Data,Y
00000004,Demo Data,A
00000005,Trial Data,Y
EOF
    echo "Process file created: PROCESS.DAT"
fi

# 生成設定ファイルの作成
if [ ! -f "GENCONFIG.DAT" ]; then
    cat > GENCONFIG.DAT << EOF
TOTAL_RECORDS,1000
GENERATION_MODE,A
DATA_PATTERN,RANDOM
RANDOM_SEED,12345
EOF
    echo "Generation config file created: GENCONFIG.DAT"
fi

# データテンプレートファイルの作成
if [ ! -f "TEMPLATE.DAT" ]; then
    cat > TEMPLATE.DAT << EOF
John Smith,NAME
Jane Doe,NAME
Bob Johnson,NAME
Alice Brown,NAME
Charlie Wilson,NAME
Premium,CATEGORY
Standard,CATEGORY
Basic,CATEGORY
Economy,CATEGORY
Luxury,CATEGORY
A,STATUS
I,STATUS
S,STATUS
P,STATUS
C,STATUS
EOF
    echo "Template file created: TEMPLATE.DAT"
fi

echo ""
echo "Configuration files prepared successfully!"
echo ""

# プログラムの実行
echo "Select program to run:"
echo "1. Non-Interactive Sample"
echo "2. Batch Processing Sample"
echo "3. Automated Data Generation"
echo "4. Run All Samples"
echo ""

read -p "Enter your choice (1-4): " choice

case $choice in
    1)
        echo ""
        echo "Running Non-Interactive Sample..."
        mvn exec:java -Dexec.mainClass="com.example.cobol.NonInteractiveSample"
        ;;
    2)
        echo ""
        echo "Running Batch Processing Sample..."
        mvn exec:java -Dexec.mainClass="com.example.cobol.BatchProcessingSample"
        ;;
    3)
        echo ""
        echo "Running Automated Data Generation..."
        mvn exec:java -Dexec.mainClass="com.example.cobol.AutomatedDataGeneration"
        ;;
    4)
        echo ""
        echo "Running All Samples..."
        echo ""
        echo "1. Non-Interactive Sample:"
        mvn exec:java -Dexec.mainClass="com.example.cobol.NonInteractiveSample"
        echo ""
        echo "2. Batch Processing Sample:"
        mvn exec:java -Dexec.mainClass="com.example.cobol.BatchProcessingSample"
        echo ""
        echo "3. Automated Data Generation:"
        mvn exec:java -Dexec.mainClass="com.example.cobol.AutomatedDataGeneration"
        ;;
    *)
        echo "Invalid choice. Please run the script again."
        exit 1
        ;;
esac

echo ""
echo "====================================="
echo "Execution completed!"
echo "====================================="

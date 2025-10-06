# COBOL非インタラクティブ処理サンプル集

## 概要

このディレクトリには、COBOLでACCEPT文を使わずにインターラクティブな処理を中断せずに進めるためのサンプルプログラムが含まれています。

## サンプルファイル一覧

### 1. ファイルベース入力処理
- **ファイル**: `non_interactive_sample.cob`
- **説明**: ファイルからの入力データ読み込みによる非インタラクティブ処理
- **特徴**:
  - 設定ファイルからのパラメータ読み込み
  - 入力データファイルの自動処理
  - ログファイルへの自動出力
  - エラーハンドリング

### 2. バッチ処理
- **ファイル**: `batch_processing_sample.cob`
- **説明**: バッチ処理による自動化
- **特徴**:
  - コマンドライン引数の処理
  - 環境変数の読み込み
  - エラーハンドリングとリトライ機能
  - 処理結果の自動通知

### 3. 自動データ生成
- **ファイル**: `automated_data_generation.cob`
- **説明**: テストデータの自動生成
- **特徴**:
  - ランダムデータの生成
  - データパターンの自動適用
  - 大量データの一括生成
  - データ検証の自動化

### 4. JCLベース処理
- **ファイル**: `non_interactive_sample.jcl`
- **説明**: JCLを使用した完全自動化
- **特徴**:
  - ファイル準備の自動化
  - コンパイル・リンク・実行の自動化
  - エラーハンドリング
  - 結果検証の自動化

## 非インタラクティブ処理の手法

### 1. ファイルベース入力

```cobol
*> 設定ファイルからの読み込み
OPEN INPUT CONFIG-FILE
PERFORM UNTIL EOF-REACHED
    READ CONFIG-FILE
        AT END
            MOVE 'Y' TO EOF-FLAG
        NOT AT END
            PERFORM PROCESS-CONFIG-RECORD
    END-READ
END-PERFORM
CLOSE CONFIG-FILE
```

**利点**:
- オペレータの介入が不要
- 設定の変更が容易
- 処理の再現性が高い

### 2. パラメータファイル使用

```cobol
*> パラメータファイルの処理
EVALUATE PARAM-TYPE
    WHEN 'TAX_RATE'
        UNSTRING PARAM-VALUE DELIMITED BY SPACE
            INTO TAX-RATE
    WHEN 'COMMISSION_RATE'
        UNSTRING PARAM-VALUE DELIMITED BY SPACE
            INTO COMMISSION-RATE
END-EVALUATE
```

**利点**:
- 処理パラメータの外部化
- 複数環境での設定管理
- バッチ処理での柔軟性

### 3. 環境変数・システム変数使用

```cobol
*> 現在日時の取得（ACCEPT文を使用しない）
MOVE FUNCTION CURRENT-DATE TO TIMESTAMP-WORK
MOVE TIMESTAMP-WORK(1:8) TO CURRENT-DATE
MOVE TIMESTAMP-WORK(9:6) TO CURRENT-TIME
```

**利点**:
- システム情報の自動取得
- タイムスタンプの自動生成
- 外部依存の削減

### 4. 自動データ生成

```cobol
*> ランダムデータの生成
PERFORM GENERATE-RANDOM-NUMBER
COMPUTE GENERATED-ID = FUNCTION MOD(RANDOM-NUMBER, 9999999) + 1
COMPUTE GENERATED-AMOUNT = FUNCTION MOD(RANDOM-NUMBER, 999999) + 100
```

**利点**:
- テストデータの自動生成
- 大量データの一括処理
- データパターンの多様化

## 実行方法

### 1. ファイルベース処理の実行

```bash
# 設定ファイルの準備
echo "TAX_RATE 0.08" > CONFIG.DAT
echo "COMMISSION_RATE 0.05" >> CONFIG.DAT

# 入力データの準備
echo "CUSTOMER001,John Smith,1500.50,20250106,Y" > INPUT.DAT

# プログラムの実行
cobc -x non_interactive_sample.cob
./non_interactive_sample
```

### 2. バッチ処理の実行

```bash
# バッチ制御ファイルの準備
echo "MODE,A" > BATCH.CTL
echo "MAX_RETRIES,3" >> BATCH.CTL

# 処理ファイルの準備
echo "00000001,Test Data,Y" > PROCESS.DAT

# プログラムの実行
cobc -x batch_processing_sample.cob
./batch_processing_sample
```

### 3. 自動データ生成の実行

```bash
# 生成設定ファイルの準備
echo "TOTAL_RECORDS,1000" > GENCONFIG.DAT
echo "GENERATION_MODE,A" >> GENCONFIG.DAT

# テンプレートファイルの準備
echo "John Smith,NAME" > TEMPLATE.DAT
echo "Premium,CATEGORY" >> TEMPLATE.DAT

# プログラムの実行
cobc -x automated_data_generation.cob
./automated_data_generation
```

### 4. JCL処理の実行

```jcl
//NONINT JOB (ACCT),'NON-INTERACTIVE COBOL',
//         CLASS=A,MSGCLASS=X,MSGLEVEL=(1,1)
//RUN EXEC PGM=NONINT
//INPUT    DD DSN=&SYSUID..INPUT.DATA,DISP=SHR
//PARAMS   DD DSN=&SYSUID..PARAMS.DATA,DISP=SHR
//OUTPUT   DD DSN=&SYSUID..OUTPUT.DATA,DISP=(NEW,CATLG)
//LOG      DD DSN=&SYSUID..PROCESS.LOG,DISP=(NEW,CATLG)
```

## 設定ファイルの例

### 設定ファイル (CONFIG.DAT)
```
TAX_RATE 0.08
COMMISSION_RATE 0.05
MINIMUM_AMOUNT 100.00
MAXIMUM_AMOUNT 999999.99
PROCESSING_LIMIT 1000
```

### パラメータファイル (PARAMS.DAT)
```
TAX_RATE,0.08
COMMISSION_RATE,0.05
MINIMUM_AMOUNT,100.00
MAXIMUM_AMOUNT,999999.99
PROCESSING_LIMIT,1000
RETRY_COUNT,3
```

### 入力データファイル (INPUT.DAT)
```
CUSTOMER001,John Smith,1500.50,20250106,Y
CUSTOMER002,Jane Doe,2300.75,20250106,A
CUSTOMER003,Bob Johnson,850.25,20250106,Y
CUSTOMER004,Alice Brown,3200.00,20250106,A
CUSTOMER005,Charlie Wilson,1200.00,20250106,Y
```

## エラーハンドリング

### 1. ファイル存在チェック
```cobol
OPEN INPUT CONFIG-FILE
IF FILE-STATUS NOT = '00'
    DISPLAY 'Warning: Configuration file not found'
    PERFORM WRITE-LOG-MESSAGE
    CLOSE CONFIG-FILE
    EXIT PARAGRAPH
END-IF
```

### 2. データ妥当性チェック
```cobol
IF CUSTOMER-ID = 0 OR CUSTOMER-ID = SPACES
    MOVE 'E' TO LOG-LEVEL
    MOVE 'Invalid customer ID' TO LOG-MESSAGE
    PERFORM WRITE-LOG-MESSAGE
    MOVE 'N' TO PROCESSING-FLAG
END-IF
```

### 3. リトライ機能
```cobol
PERFORM WITH TEST AFTER
    UNTIL RETRY-COUNT >= MAX-RETRIES OR PROCESSING-STATUS = 'C'
    
    PERFORM PROCESS-RECORD-DATA
    
    IF PROCESSING-STATUS = 'E'
        ADD 1 TO RETRY-COUNT
        DISPLAY 'Retry attempt: ' RETRY-COUNT
        PERFORM DELAY-PROCESSING
    END-IF
END-PERFORM
```

## ログ機能

### 1. ログメッセージの書き込み
```cobol
WRITE-LOG-MESSAGE.
    MOVE FUNCTION CURRENT-DATE TO LOG-TIMESTAMP
    
    STRING LOG-TIMESTAMP DELIMITED BY SIZE
           ' [' DELIMITED BY SIZE
           LOG-LEVEL DELIMITED BY SIZE
           '] ' DELIMITED BY SIZE
           LOG-MESSAGE DELIMITED BY SIZE
              INTO LOG-RECORD
    END-STRING
    
    WRITE LOG-RECORD.
```

### 2. ログレベルの管理
```cobol
01  LOG-CONTROL.
    05  LOG-LEVEL              PIC X(1)     VALUE 'I'.
        88  LOG-INFO           VALUE 'I'.
        88  LOG-WARNING        VALUE 'W'.
        88  LOG-ERROR           VALUE 'E'.
```

## 統計情報の収集

### 1. 処理統計
```cobol
01  PROCESSING-STATISTICS.
    05  TOTAL-RECORDS          PIC 9(6)     VALUE 0.
    05  PROCESSED-RECORDS      PIC 9(6)     VALUE 0.
    05  ERROR-RECORDS          PIC 9(6)     VALUE 0.
    05  SKIPPED-RECORDS        PIC 9(6)     VALUE 0.
    05  PROCESSING-TIME        PIC 9(4)     VALUE 0.
```

### 2. サマリーレポート生成
```cobol
DISPLAY '====================================='
DISPLAY 'PROCESSING SUMMARY REPORT'
DISPLAY '====================================='
DISPLAY 'Total Records Processed: ' RECORD-COUNTER
DISPLAY 'Successful Records:      ' SUCCESS-COUNT
DISPLAY 'Error Records:          ' ERROR-COUNT
DISPLAY 'Total Amount Processed:  ' TOTAL-PROCESSED
DISPLAY 'Average Amount:         ' AVERAGE-AMOUNT
DISPLAY '====================================='
```

## 注意事項

1. **ファイルパス**: 実行環境に応じてファイルパスを調整してください
2. **権限**: ファイルの読み書き権限を確認してください
3. **エンコーディング**: 文字エンコーディングに注意してください
4. **リソース**: 大量データ処理時はメモリ使用量に注意してください

## トラブルシューティング

### よくある問題と解決方法

1. **ファイルが見つからない**
   - ファイルパスの確認
   - ファイルの存在確認
   - 権限の確認

2. **データ形式エラー**
   - 入力データの形式確認
   - 区切り文字の確認
   - データ型の確認

3. **メモリ不足**
   - 処理レコード数の調整
   - バッファサイズの調整
   - メモリ使用量の監視

## 関連ファイル

- `comprehensive_sample.cob`: 包括的COBOLサンプル
- `README.md`: プロジェクト全体の説明
- `docs/cobol-reference.md`: COBOLリファレンス

## ライセンス

このプロジェクトは教育目的で作成されています。

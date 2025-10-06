       IDENTIFICATION DIVISION.
       PROGRAM-ID. NON-INTERACTIVE-SAMPLE.
       AUTHOR. COBOL Education Project.
       DATE-WRITTEN. 2025.
       SECURITY. Confidential.
       *> ================================================================
       *> 非インタラクティブ処理のサンプルプログラム
       *> 
       *> このプログラムでは以下の非インタラクティブ処理を実装します：
       *> - ファイルからの入力データ読み込み
       *> - パラメータファイルの使用
       *> - バッチ処理での自動化
       *> - 設定ファイルによる処理制御
       *> - ログファイルへの出力
       *> ================================================================
       
       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       
       FILE-CONTROL.
           *> 入力データファイル
           SELECT INPUT-DATA-FILE
               ASSIGN TO 'INPUT.DAT'
               ORGANIZATION IS LINE SEQUENTIAL
               STATUS IS FILE-STATUS.
           
           *> パラメータファイル
           SELECT PARAMETER-FILE
               ASSIGN TO 'PARAMS.DAT'
               ORGANIZATION IS LINE SEQUENTIAL
               STATUS IS FILE-STATUS.
           
           *> 設定ファイル
           SELECT CONFIG-FILE
               ASSIGN TO 'CONFIG.DAT'
               ORGANIZATION IS LINE SEQUENTIAL
               STATUS IS FILE-STATUS.
           
           *> ログ出力ファイル
           SELECT LOG-FILE
               ASSIGN TO 'PROCESS.LOG'
               ORGANIZATION IS LINE SEQUENTIAL
               STATUS IS FILE-STATUS.
           
           *> 結果出力ファイル
           SELECT OUTPUT-FILE
               ASSIGN TO 'OUTPUT.DAT'
               ORGANIZATION IS LINE SEQUENTIAL
               STATUS IS FILE-STATUS.
       
       DATA DIVISION.
       
       FILE SECTION.
       
       FD  INPUT-DATA-FILE
           LABEL RECORDS ARE STANDARD
           RECORDING MODE IS F.
       01  INPUT-RECORD.
           05  CUSTOMER-ID             PIC 9(7).
           05  CUSTOMER-NAME           PIC X(30).
           05  TRANSACTION-AMOUNT      PIC 9(8)V99.
           05  TRANSACTION-DATE        PIC X(8).
           05  PROCESSING-FLAG         PIC X(1).
       
       FD  PARAMETER-FILE
           LABEL RECORDS ARE STANDARD
           RECORDING MODE IS F.
       01  PARAMETER-RECORD.
           05  PARAM-TYPE              PIC X(10).
           05  PARAM-VALUE             PIC X(50).
       
       FD  CONFIG-FILE
           LABEL RECORDS ARE STANDARD
           RECORDING MODE IS F.
       01  CONFIG-RECORD.
           05  CONFIG-KEY              PIC X(20).
           05  CONFIG-VALUE            PIC X(30).
       
       FD  LOG-FILE
           LABEL RECORDS ARE STANDARD
           RECORDING MODE IS F.
       01  LOG-RECORD                  PIC X(132).
       
       FD  OUTPUT-FILE
           LABEL RECORDS ARE STANDARD
           RECORDING MODE IS F.
       01  OUTPUT-RECORD.
           05  PROCESSED-CUSTOMER-ID   PIC 9(7).
           05  PROCESSED-AMOUNT         PIC 9(8)V99.
           05  PROCESSING-RESULT       PIC X(20).
           05  PROCESSING-TIMESTAMP    PIC X(14).
       
       WORKING-STORAGE SECTION.
       
       *> ================================================================
       *> ファイル制御変数
       *> ================================================================
       
       01  FILE-CONTROL-VARIABLES.
           05  FILE-STATUS             PIC X(2).
               88  FILE-STATUS-OK      VALUE '00'.
               88  FILE-STATUS-EOF    VALUE '10'.
               88  FILE-STATUS-ERROR  VALUE '23'.
           05  RECORD-COUNTER          PIC 9(6)     VALUE 0.
           05  EOF-FLAG               PIC X(1)      VALUE 'N'.
               88  EOF-REACHED        VALUE 'Y'.
               88  EOF-NOT-REACHED    VALUE 'N'.
       
       *> ================================================================
       *> 処理制御変数
       *> ================================================================
       
       01  PROCESSING-CONTROL.
           05  PROCESSING-MODE          PIC X(10)    VALUE SPACES.
           05  BATCH-MODE              PIC X(1)     VALUE 'Y'.
               88  BATCH-PROCESSING    VALUE 'Y'.
               88  INTERACTIVE-MODE     VALUE 'N'.
           05  AUTO-PROCESSING         PIC X(1)     VALUE 'Y'.
               88  AUTO-MODE           VALUE 'Y'.
               88  MANUAL-MODE          VALUE 'N'.
           05  ERROR-COUNT             PIC 9(3)     VALUE 0.
           05  SUCCESS-COUNT          PIC 9(3)     VALUE 0.
       
       *> ================================================================
       *> パラメータ変数
       *> ================================================================
       
       01  PROCESSING-PARAMETERS.
           05  TAX-RATE               PIC 9V9999    VALUE 0.08.
           05  COMMISSION-RATE       PIC 9V9999    VALUE 0.05.
           05  MINIMUM-AMOUNT         PIC 9(6)V99   VALUE 100.00.
           05  MAXIMUM-AMOUNT         PIC 9(8)V99   VALUE 999999.99.
           05  PROCESSING-LIMIT       PIC 9(5)     VALUE 1000.
           05  RETRY-COUNT           PIC 9(2)     VALUE 3.
       
       *> ================================================================
       *> 計算領域
       *> ================================================================
       
       01  CALCULATION-FIELDS.
           05  CALCULATED-TAX         PIC 9(7)V99   VALUE 0.
           05  CALCULATED-COMMISSION PIC 9(7)V99   VALUE 0.
           05  NET-AMOUNT            PIC 9(8)V99   VALUE 0.
           05  TOTAL-PROCESSED       PIC 9(9)V99   VALUE 0.
           05  AVERAGE-AMOUNT        PIC 9(6)V99   VALUE 0.
       
       *> ================================================================
       *> 日付・時刻管理
       *> ================================================================
       
       01  DATE-TIME-FIELDS.
           05  CURRENT-DATE           PIC X(8).
           05  CURRENT-TIME           PIC X(8).
           05  PROCESSING-START-TIME PIC X(8).
           05  PROCESSING-END-TIME    PIC X(8).
           05  TIMESTAMP-WORK         PIC X(14).
       
       *> ================================================================
       *> ログ管理
       *> ================================================================
       
       01  LOG-CONTROL.
           05  LOG-LEVEL              PIC X(1)     VALUE 'I'.
               88  LOG-INFO           VALUE 'I'.
               88  LOG-WARNING        VALUE 'W'.
               88  LOG-ERROR          VALUE 'E'.
           05  LOG-MESSAGE            PIC X(100).
           05  LOG-TIMESTAMP          PIC X(14).
       
       *> ================================================================
       *> プロシージャ部
       *> ================================================================
       
       PROCEDURE DIVISION.
       
       MAIN-PROCEDURE.
           DISPLAY '====================================='
           DISPLAY 'Non-Interactive COBOL Sample Program'
           DISPLAY '====================================='
           
           PERFORM INITIALIZE-PROGRAM
           
           PERFORM LOAD-CONFIGURATION
           
           PERFORM LOAD-PARAMETERS
           
           PERFORM PROCESS-INPUT-DATA
           
           PERFORM GENERATE-SUMMARY-REPORT
           
           PERFORM FINALIZE-PROGRAM
           
           STOP RUN.
       
       *> ================================================================
       *> 初期化処理
       *> ================================================================
       
       INITIALIZE-PROGRAM.
           DISPLAY 'Initializing non-interactive program...'
           
           *> 変数の初期化
           INITIALIZE CALCULATION-FIELDS
           INITIALIZE PROCESSING-CONTROL ALL TO SPACES
           MOVE 'Y' TO BATCH-MODE
           MOVE 'Y' TO AUTO-PROCESSING
           
           *> 現在の日付と時刻を取得（ACCEPT文を使用しない方法）
           MOVE FUNCTION CURRENT-DATE TO TIMESTAMP-WORK
           MOVE TIMESTAMP-WORK(1:8) TO CURRENT-DATE
           MOVE TIMESTAMP-WORK(9:6) TO CURRENT-TIME
           MOVE CURRENT-TIME TO PROCESSING-START-TIME
           
           *> ログファイルを開く
           OPEN OUTPUT LOG-FILE
           PERFORM WRITE-LOG-MESSAGE
           
           DISPLAY 'Program initialized successfully'.
       
       *> ================================================================
       *> 設定ファイル読み込み
       *> ================================================================
       
       LOAD-CONFIGURATION.
           DISPLAY 'Loading configuration from file...'
           
           OPEN INPUT CONFIG-FILE
           
           IF FILE-STATUS NOT = '00'
               DISPLAY 'Warning: Configuration file not found'
               PERFORM WRITE-LOG-MESSAGE
               CLOSE CONFIG-FILE
               EXIT PARAGRAPH
           END-IF
           
           PERFORM UNTIL EOF-REACHED
               READ CONFIG-FILE
                   AT END
                       MOVE 'Y' TO EOF-FLAG
                   NOT AT END
                       PERFORM PROCESS-CONFIG-RECORD
               END-READ
           END-PERFORM
           
           CLOSE CONFIG-FILE
           MOVE 'N' TO EOF-FLAG
           
           DISPLAY 'Configuration loaded successfully'.
       
       PROCESS-CONFIG-RECORD.
           *> 設定値の処理
           EVALUATE CONFIG-KEY
               WHEN 'TAX_RATE'
                   UNSTRING CONFIG-VALUE DELIMITED BY SPACE
                       INTO TAX-RATE
               WHEN 'COMMISSION_RATE'
                   UNSTRING CONFIG-VALUE DELIMITED BY SPACE
                       INTO COMMISSION-RATE
               WHEN 'MINIMUM_AMOUNT'
                   UNSTRING CONFIG-VALUE DELIMITED BY SPACE
                       INTO MINIMUM-AMOUNT
               WHEN 'MAXIMUM_AMOUNT'
                   UNSTRING CONFIG-VALUE DELIMITED BY SPACE
                       INTO MAXIMUM-AMOUNT
               WHEN 'PROCESSING_LIMIT'
                   UNSTRING CONFIG-VALUE DELIMITED BY SPACE
                       INTO PROCESSING-LIMIT
               WHEN OTHER
                   DISPLAY 'Unknown config key: ' CONFIG-KEY
           END-EVALUATE.
       
       *> ================================================================
       *> パラメータファイル読み込み
       *> ================================================================
       
       LOAD-PARAMETERS.
           DISPLAY 'Loading parameters from file...'
           
           OPEN INPUT PARAMETER-FILE
           
           IF FILE-STATUS NOT = '00'
               DISPLAY 'Warning: Parameter file not found'
               PERFORM WRITE-LOG-MESSAGE
               CLOSE PARAMETER-FILE
               EXIT PARAGRAPH
           END-IF
           
           PERFORM UNTIL EOF-REACHED
               READ PARAMETER-FILE
                   AT END
                       MOVE 'Y' TO EOF-FLAG
                   NOT AT END
                       PERFORM PROCESS-PARAMETER-RECORD
               END-READ
           END-PERFORM
           
           CLOSE PARAMETER-FILE
           MOVE 'N' TO EOF-FLAG
           
           DISPLAY 'Parameters loaded successfully'.
       
       PROCESS-PARAMETER-RECORD.
           *> パラメータの処理
           EVALUATE PARAM-TYPE
               WHEN 'MODE'
                   MOVE PARAM-VALUE TO PROCESSING-MODE
               WHEN 'RETRY_COUNT'
                   UNSTRING PARAM-VALUE DELIMITED BY SPACE
                       INTO RETRY-COUNT
               WHEN OTHER
                   DISPLAY 'Unknown parameter type: ' PARAM-TYPE
           END-EVALUATE.
       
       *> ================================================================
       *> 入力データ処理
       *> ================================================================
       
       PROCESS-INPUT-DATA.
           DISPLAY 'Processing input data...'
           
           OPEN INPUT INPUT-DATA-FILE
           OPEN OUTPUT OUTPUT-FILE
           
           IF FILE-STATUS NOT = '00'
               DISPLAY 'Error: Input data file not found'
               PERFORM WRITE-LOG-MESSAGE
               CLOSE INPUT-DATA-FILE
               CLOSE OUTPUT-FILE
               EXIT PARAGRAPH
           END-IF
           
           MOVE 'N' TO EOF-FLAG
           
           PERFORM UNTIL EOF-REACHED OR RECORD-COUNTER >= PROCESSING-LIMIT
               READ INPUT-DATA-FILE
                   AT END
                       MOVE 'Y' TO EOF-FLAG
                   NOT AT END
                       ADD 1 TO RECORD-COUNTER
                       PERFORM PROCESS-SINGLE-RECORD
               END-READ
           END-PERFORM
           
           CLOSE INPUT-DATA-FILE
           CLOSE OUTPUT-FILE
           
           DISPLAY 'Input data processing completed'
           DISPLAY 'Records processed: ' RECORD-COUNTER.
       
       PROCESS-SINGLE-RECORD.
           *> 個別レコードの処理
           PERFORM VALIDATE-RECORD
           
           IF PROCESSING-FLAG = 'Y'
               PERFORM CALCULATE-AMOUNTS
               PERFORM WRITE-OUTPUT-RECORD
               ADD 1 TO SUCCESS-COUNT
           ELSE
               ADD 1 TO ERROR-COUNT
               PERFORM WRITE-LOG-MESSAGE
           END-IF.
       
       VALIDATE-RECORD.
           *> レコードの妥当性チェック
           IF CUSTOMER-ID = 0 OR CUSTOMER-ID = SPACES
               MOVE 'E' TO LOG-LEVEL
               MOVE 'Invalid customer ID' TO LOG-MESSAGE
               PERFORM WRITE-LOG-MESSAGE
               MOVE 'N' TO PROCESSING-FLAG
           ELSE
               IF TRANSACTION-AMOUNT < MINIMUM-AMOUNT OR 
                  TRANSACTION-AMOUNT > MAXIMUM-AMOUNT
                   MOVE 'W' TO LOG-LEVEL
                   MOVE 'Amount out of range' TO LOG-MESSAGE
                   PERFORM WRITE-LOG-MESSAGE
                   MOVE 'Y' TO PROCESSING-FLAG
               ELSE
                   MOVE 'Y' TO PROCESSING-FLAG
               END-IF
           END-IF.
       
       CALCULATE-AMOUNTS.
           *> 金額計算
           COMPUTE CALCULATED-TAX = TRANSACTION-AMOUNT * TAX-RATE
                    ROUNDED MODE ROUND-HALF-EVEN
           
           COMPUTE CALCULATED-COMMISSION = TRANSACTION-AMOUNT * COMMISSION-RATE
                    ROUNDED MODE ROUND-HALF-EVEN
           
           COMPUTE NET-AMOUNT = TRANSACTION-AMOUNT - CALCULATED-TAX - CALCULATED-COMMISSION
           
           ADD TRANSACTION-AMOUNT TO TOTAL-PROCESSED.
       
       WRITE-OUTPUT-RECORD.
           *> 出力レコードの書き込み
           MOVE CUSTOMER-ID TO PROCESSED-CUSTOMER-ID
           MOVE NET-AMOUNT TO PROCESSED-AMOUNT
           MOVE 'SUCCESS' TO PROCESSING-RESULT
           
           MOVE FUNCTION CURRENT-DATE TO TIMESTAMP-WORK
           MOVE TIMESTAMP-WORK TO PROCESSING-TIMESTAMP
           
           WRITE OUTPUT-RECORD.
       
       *> ================================================================
       *> ログ機能
       *> ================================================================
       
       WRITE-LOG-MESSAGE.
           *> ログメッセージの書き込み
           MOVE FUNCTION CURRENT-DATE TO TIMESTAMP-WORK
           MOVE TIMESTAMP-WORK TO LOG-TIMESTAMP
           
           STRING LOG-TIMESTAMP DELIMITED BY SIZE
                  ' [' DELIMITED BY SIZE
                  LOG-LEVEL DELIMITED BY SIZE
                  '] ' DELIMITED BY SIZE
                  LOG-MESSAGE DELIMITED BY SIZE
                     INTO LOG-RECORD
           END-STRING
           
           WRITE LOG-RECORD.
       
       *> ================================================================
       *> サマリーレポート生成
       *> ================================================================
       
       GENERATE-SUMMARY-REPORT.
           DISPLAY 'Generating summary report...'
           
           *> 平均金額の計算
           IF RECORD-COUNTER > 0
               COMPUTE AVERAGE-AMOUNT = TOTAL-PROCESSED / RECORD-COUNTER
                       ROUNDED MODE ROUND-HALF-EVEN
           END-IF
           
           *> サマリー情報の表示
           DISPLAY '====================================='
           DISPLAY 'PROCESSING SUMMARY REPORT'
           DISPLAY '====================================='
           DISPLAY 'Total Records Processed: ' RECORD-COUNTER
           DISPLAY 'Successful Records:      ' SUCCESS-COUNT
           DISPLAY 'Error Records:          ' ERROR-COUNT
           DISPLAY 'Total Amount Processed:  ' TOTAL-PROCESSED
           DISPLAY 'Average Amount:         ' AVERAGE-AMOUNT
           DISPLAY 'Tax Rate Applied:       ' TAX-RATE
           DISPLAY 'Commission Rate:        ' COMMISSION-RATE
           DISPLAY '====================================='
           
           *> ログファイルにサマリーを書き込み
           MOVE 'I' TO LOG-LEVEL
           MOVE 'Processing completed successfully' TO LOG-MESSAGE
           PERFORM WRITE-LOG-MESSAGE.
       
       *> ================================================================
       *> 終了処理
       *> ================================================================
       
       FINALIZE-PROGRAM.
           DISPLAY 'Finalizing program...'
           
           MOVE FUNCTION CURRENT-DATE TO TIMESTAMP-WORK
           MOVE TIMESTAMP-WORK(9:6) TO PROCESSING-END-TIME
           
           DISPLAY 'Program execution completed successfully'
           DISPLAY 'Start time: ' PROCESSING-START-TIME
           DISPLAY 'End time:   ' PROCESSING-END-TIME
           DISPLAY '====================================='
           
           CLOSE LOG-FILE.
       
       END PROGRAM NON-INTERACTIVE-SAMPLE.

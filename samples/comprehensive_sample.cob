       IDENTIFICATION DIVISION.
       PROGRAM-ID. COMPREHENSIVE-SAMPLE.
       AUTHOR. COBOL Education Project.
       DATE-WRITTEN. 2025.
       SECURITY. Confidential.
       *> ================================================================
       *> COBOL言語の全貌を理解するための包括的サンプルプログラム
       *> 
       *> このプログラムでは以下のCOBOL機能を実装・説明します：
       *> - 4つのDIVISION構成
       *> - データ型と変数定義
       *> - ファイル入出力
       *> - 配列とテーブル（オキュア）
       *> - 文字列操作
       *> - 数学演算
       *> - 条件分岐

       *> - ループ処理
       *> - サブルーチン（パフォーム）
       *> - 表組み制御
       *> - セグメンテーション
       *> - レポート機能
       *> ================================================================
       
       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       
       *> ================================================================
       *> ファイル制御部 - COBOLのファイル入出力機能
       *> ================================================================
       
       FILE-CONTROL.
           *> 顧客マスタファイル（順次編成）
           SELECT CUSTOMER-MASTER 
               ASSIGN TO 'CUSTOMER.MAST'
               ORGANIZATION IS SEQUENTIAL
               STATUS IS FILE-STATUS.
           
           *> 売上トランザクションファイル（相対編成）
           SELECT SALES-TRANSACTIONS
               ASSIGN TO 'SALES.TRAN'
               ORGANIZATION IS RELATIVE
               ACCESS MODE IS SEQUENTIAL
               RELATIVE KEY IS REL-KEY-INDEX
               STATUS IS FILE-STATUS.
           
           *> レポート出力ファイル（行順次編成）
           SELECT REPORT-OUTPUT
               ASSIGN TO 'DAILY-REPORT.TXT'
               ORGANIZATION IS LINE SEQUENTIAL
               STATUS IS FILE-STATUS.
           
           *> ソートファイル
           SELECT SORT-INFILE
               ASSIGN TO 'SORTINPUT.DAT'.
           SELECT SORT-OUTFILE
               ASSIGN TO 'SORTOUTPUT.DAT'.
           SELECT SORT-WORKFILE
               ASSIGN TO 'SORTWORK.DAT'.
       
       *> ================================================================
       *> データ部 - COBOLのデータ構造と変数定義
       *> ================================================================
       
       DATA DIVISION.
       
       FILE SECTION.
       
       *> 顧客マスタレコード構造
       FD  CUSTOMER-MASTER
           LABEL RECORDS ARE STANDARD
           RECORDING MODE IS F
           BLOCK CONTAINS 0 RECORDS.
       01  CUSTOMER-RECORD.
           05  CUSTOMER-ID             PIC 9(7).
           05  CUSTOMER-NAME           PIC X(30).
           05  CUSTOMER-ADDRESS. 
               10  STREET-ADDRESS      PIC X(40).
               10  CITY               PIC X(20).
               10  STATE              PIC X(2).
               10  ZIP-CODE           PIC X(10).
           05  CUSTOMER-PHONE          PIC X(15).
           05  CREDIT-LIMIT           PIC 9(8)V99.
           05  CUSTOMER-STATUS        PIC X(1).
               *> A=Active, I=Inactive, S=Suspended
           05  DATE-CREATED           PIC X(8).
           05  FILLER                PIC X(20).
       
       *> 売上トランザクションレコード構造
       FD  SALES-TRANSACTIONS
           LABEL RECORDS ARE STANDARD
           RECORDING MODE IS F.
       01  SALES-RECORD.
           05  TRANSACTION-ID         PIC 9(8).
           05  CUSTOMER-ID           PIC 9(7).
           05  PRODUCT-CODE          PIC X(10).
           05  QUANTITY             PIC 9(5).
           05  UNIT-PRICE           PIC 9(5)V99.
           05  TOTAL-AMOUNT         PIC 9(7)V99.
           05  TRANSACTION-DATE     PIC X(8).
           05  SALESPERSON-ID       PIC 9(4).
       
       *> レポート出力ファイル
       FD  REPORT-OUTPUT
           LABEL RECORDS ARE STANDARD
           RECORDING MODE IS F.
       01  REPORT-LINE               PIC X(132).
       
       *> ソートファイル定義
       SD  SORT-WORKFILE.
       01  SORT-RECORD.
           05  SORT-CUSTOMER-ID      PIC 9(7).
           05  SORT-NAME             PIC X(30).
           05  SORT-SALES-TOTAL      PIC 9(8)V99.
       
       WORKING-STORAGE SECTION.
       
       *> ================================================================
       *> COBOLの基本的なデータ型とPIC句の例
       *> ================================================================
       
       *> 数値型データ
       01  NUMERIC-DATA.
           05  INTEGER-NUMBER        PIC 9(5)         VALUE 12345.
           05  DECIMAL-NUMBER        PIC 9(7)V99      VALUE 12345.67.
           05  SIGNED-NUMBER         PIC S9(5)         VALUE -12345.
           05  LEADING-ZEROS         PIC 09(5)        VALUE ZEROES.
           05  TRAILING-ZEROS       PIC 9(5)0         VALUE ZEROES.
           05  EDIT-MASK            PIC ZZZ,ZZZ.99   VALUE ZEROES.
       
       *> 英数字型データ
       01  ALPHANUMERIC-DATA.
           05  TEXT-FIELD           PIC X(50)         VALUE SPACES.
           05  ALPHABETIC-FIELD     PIC A(20)         VALUE SPACES.
           05  NATIONAL-FIELD       PIC N(20)         VALUE SPACES.
           05  FIXED-LENGTH-STRING  PIC X(10)         VALUE 'HELLO'.
       
       *> バイナリ・パック形式データ
       01  COMPUTATIONAL-DATA.
           05  BINARY-NUMBER        PIC 9(5) COMP     VALUE 100.
           05  PACKED-DECIMAL       PIC 9(7)V99 COMP-3 VALUE 12345.67.
           05  FLOATING-POINT        PIC 9(7)V99 COMP-1 VALUE 12345.67.
           05  DOUBLE-PRECISION      PIC 9(7)V99 COMP-2 VALUE 12345.67.
       
       *> ================================================================
       *> 配列とテーブル（OCCURS句）の例
       *> ================================================================
       
       01  SALES-TABLE.
           05  TABLE-STATISTICS.
               10  TOTAL-RECORDS        PIC 9(5)     VALUE 0.
               10  RECORDS-PROCESSED    PIC 9(5)     VALUE 0.
               10  RECORDS-ERROR        PIC 9(5)     VALUE 0.
           05  MONTHLY-SALES OCCURS 12 TIMES
                    INDEXED BY MONTH-INDEX.
               10  MONTH-NAME           PIC X(10).
               10  MONTH-SALES          PIC 9(8)V99 VALUE 0.
               10  MONTH-CUSTOMERS      PIC 9(4) VALUE 0.
       
       01  PRODUCT-TABLE.
           05  PRODUCT COUNT           PIC 9(3)     VALUE 5.
           05  PRODUCT-DATA OCCURS 1 TO 100 TIMES DEPENDING ON PRODUCT-COUNT
                    INDEXED BY PRODUCT-INDEX.
               10  PRODUCT-ID           PIC X(10).
               10  PRODUCT-NAME        PIC X(30).
               10  PRODUCT-PRICE       PIC 9(5)V99.
               10  PRODUCT-DESCRIPTION PIC X(100).
               10  WARRANTY-MONTHS     PIC 9(3).
               10  PRODUCT-CATEGORY    PIC X(20).
       
       01  CUSTOMER-SEARCH-TABLE.
           05  SEARCH-KEY              PIC 9(7).
           05  SEARCH-RESULT OCCURS 50 TIMES
                    INDEXED BY SEARCH-INDEX.
               10  FOUND-CUSTOMER-ID   PIC 9(7).
               10  FOUND-CUSTOMER-NAME PIC X(30).
               10  SCORE               PIC 9(3)V9 VALUE 0.
       
       *> ================================================================
       *> ファイル制御変数
       *> ================================================================
       
       01  FILE-CONTROL-VARIABLES.
           05  FILE-STATUS             PIC X(2).
               88  FILE-STATUS-OK     VALUE '00'.
               88  FILE-STATUS-EOF    VALUE '10'.
               88  FILE-STATUS-ERROR  VALUE '23'.
           05  RECORD-COUNTER          PIC 9(6)     VALUE 0.
           05  EOF-FLAG               PIC X(1)      VALUE 'N'.
               88  EOF-REACHED        VALUE 'Y'.
               88  EOF-NOT-REACHED    VALUE 'N'.
           05  REL-KEY-INDEX          PIC 9(5)     VALUE 1.
       
       *> ================================================================
       *> 処理制御変数
       *> ================================================================
       
       01  PROCESSING-CONTROL.
           05  MAIN-LOOP-COUNTER       PIC 9(3)     VALUE 0.
           05  PROCESSING-STATUS       PIC X(1)     VALUE 'C'.
               *> C=Continue, S=Stop, E=Error
           05  ERROR-MESSAGES          PIC X(200).
           05  RETRY-COUNT            PIC 9(2)     VALUE 0.
        
       *> ================================================================
       *> 計算・計算結果領域
       *> ================================================================
       
       01  CALCULATION-FIELDS.
           05  GROSS-SALES             PIC 9(9)V99 VALUE 0.
           05  NET-SALES              PIC 9(9)V99 VALUE 0.
           05  TAX-RATE               PIC 9V9999 VALUE 0.08.
           05  COMMISSION-RATE        PIC 9V9999 VALUE 0.05.
           05  CALCULATED-TAX         PIC 9(7)V99 VALUE 0.
           05  CALCULATED-COMMISSION  PIC 9(7)V99 VALUE 0.
           05  WEIGHTED-AVERAGE       PIC 9(5)V999 VALUE 0.
           05  PERCENTAGE-VALUE       PIC 9(3)V99 VALUE 0.
       
       *> ================================================================
       *> 日付・時刻管理
       *> ================================================================
       
       01  DATE-TIME-FIELDS.
           05  CURRENT-DATE           PIC X(8).
           05  CURRENT-TIME           PIC X(8).
           05  PROCESSING-START-TIME PIC X(8).
           05  PROCESSING-END-TIME    PIC X(8).
           05  ELAPSED-TIME           PIC 9(4).
           05  DATE-WORK.
               10  WORK-YEAR          PIC 9(4).
               10  WORK-MONTH         PIC 99.
               10  WORK-DAY           PIC 99.
           05  YEAR-PART             PIC X(4).
           05  MONTH-PART            PIC X(2).
           05  DAY-PART              PIC X(2).
           05  COUNT-OF-A            PIC 9(3).
       
       *> ================================================================
       *> レポート関連変数
       *> ================================================================
       
       01  REPORT-CONTROL.
           05  PAGE-NUMBER            PIC 9(4)     VALUE 1.
           05  LINE-COUNT             PIC 99       VALUE 0.
           05  LINES-PER-PAGE         PIC 99       VALUE 55.
           05  REPORT-HEADER-PRINTED  PIC X(1)     VALUE 'N'.
               88  HEADER-PRINTED     VALUE 'Y'.
               88  HEADER-NOT-PRINTED VALUE 'N'.
        
       01  REPORT-DATA.
           05  REPORT-TITLE           PIC X(50) VALUE 
               'COBOL COMPREHENSIVE SAMPLE PROGRAM REPORT'.
           05  REPORT-SUBTITLE        PIC X(50) VALUE 
               'Sales Analysis and Customer Statistics'.
           05  REPORT-DATE            PIC X(20) VALUE SPACES.
       
       *> ================================================================
       *> プロシージャ部 - COBOLの処理ロジック実行部
       *> ================================================================
       
       PROCEDURE DIVISION.
       
       MAIN-PROCEDURE.
           DISPLAY '====================================='
           DISPLAY 'COBOL Comprehensive Sample Program'
           DISPLAY '====================================='
           
           PERFORM INITIALIZE-PROGRAM
           
           PERFORM PROCESS-CUSTOMER-FILE
           
           PERFORM PROCESS-SALES-TRANSACTIONS
           
           PERFORM GENERATE-STATISTICS
           
           PERFORM GENERATE-REPORT-HEADING
           
           PERFORM PROCESS-PRODUCT-TABLE
           
           PERFORM DEMONSTRATE-COMPUTATIONAL-DATA
           
           PERFORM PERFORM-MATHEMATICAL-CALCULATIONS
           
           PERFORM DEMONSTRATE-STRING-OPERATIONS
           
           PERFORM DEMONSTRATE-CONDITIONAL-LOGIC
           
           PERFORM DEMONSTRATE-LOOP-CONSTRUCTS
           
           PERFORM DEMONSTRATE-SEARCH-OPERATIONS
           
           PERFORM DEMONSTRATE-SORT-OPERATIONS
           
           PERFORM GENERATE-DETAIL-REPORT
           
           PERFORM FINALIZE-PROGRAM
           
           STOP RUN.
       
       *> ================================================================
       *> 初期化処理
       *> ================================================================
       
       INITIALIZE-PROGRAM.
           DISPLAY 'Initializing program...'
           
           *> 変数の初期化
           INITIALIZE CALCULATION-FIELDS
           INITIALIZE PROCESSING-CONTROL ALL TO SPACES
           INITIALIZE REPORT-CONTROL ALL TO ZEROS
           
           *> 現在の日付と時刻を取得
           ACCEPT CURRENT-DATE FROM DATE
           ACCEPT CURRENT-TIME FROM TIME
           ACCEPT PROCESSING-START-TIME FROM TIME
           
           *> 月名テーブルの初期化
           PERFORM INITIALIZE-MONTH-TABLE
           
           DISPLAY 'Program initialized successfully'.
       
       *> ================================================================
       *> ファイル処理の例
       *> ================================================================
       
       PROCESS-CUSTOMER-FILE.
           DISPLAY 'Processing Customer Master File...'
           
           OPEN INPUT CUSTOMER-MASTER
           
           PERFORM UNTIL EOF-REACHED
               READ CUSTOMER-MASTER
                   AT END
                       MOVE 'Y' TO EOF-FLAG
                   NOT AT END
                       ADD 1 TO RECORD-COUNTER
                       PERFORM PROCESS-SINGLE-CUSTOMER
               END-READ
           END-PERFORM
           
           CLOSE CUSTOMER-MASTER
           
           DISPLAY 'Customer file processing completed'
           DISPLAY 'Records processed: ' RECORD-COUNTER.
       
       PROCESS-SINGLE-CUSTOMER.
           *> 顧客データの個別処理ロジック
           ADD CUSTOMER-ID TO GROSS-SALES
           
           *> 条件分岐の例
           EVALUATE CUSTOMER-STATUS
               WHEN 'A'
                   PERFORM PROCESS-ACTIVE-CUSTOMER
               WHEN 'I'
                   PERFORM PROCESS-INACTIVE-CUSTOMER
               WHEN 'S'
                   PERFORM PROCESS-SUSPENDED-CUSTOMER
               WHEN OTHER
                   PERFORM PROCESS-UNKNOWN-CUSTOMER
           END-EVALUATE.
       
       PROCESS-SALES-TRANSACTIONS.
           DISPLAY 'Processing Sales Transaction File...'
           
           OPEN INPUT SALES-TRANSACTIONS
           
           MOVE 'N' TO EOF-FLAG
           MOVE 0 TO RECORD-COUNTER
           
           PERFORM WITH TEST AFTER UNTIL EOF-REACHED
               READ SALES-TRANSACTIONS
                   AT END
                       MOVE 'Y' TO EOF-FLAG
                       DISPLAY 'End of sales transactions reached'
                   NOT AT END
                       ADD 1 TO RECORD-COUNTER
                       PERFORM PROCESS-TRANSACTION-RECORD
               END-READ
           END-PERFORM
           
           CLOSE SALES-TRANSACTIONS.
       
       PROCESS-TRANSACTION-RECORD.
           *> 売上トランザクションの処理
           COMPUTE CALCULATED-TAX = TOTAL-AMOUNT * TAX-RATE
                    ROUNDED MODE ROUND-HALF-EVEN
           
           COMPUTE CALCULATED-COMMISSION = TOTAL-AMOUNT * COMMISSION-RATE
                    ROUNDED MODE ROUND-HALF-EVEN
           
           ADD TOTAL-AMOUNT TO GROSS-SALES
           
           *> 月別売上に加算
           MOVE TRANSACTION-DATE TO DATE-WORK
           PERFORM ADD-TO-MONTHLY-SALES
           
           *> 商品別統計への追加
           PERFORM UPDATE-PRODUCT-STATISTICS.
       
       *> ================================================================
       *> 統計処理と計算機能
       *> ================================================================
       
       GENERATE-STATISTICS.
           DISPLAY 'Generating Statistics...'
           
           *> 重み付き平均の計算
           PERFORM CALCULATE-WEIGHTED-AVERAGE
           
           *> パーセント計算
           PERFORM CALCULATE-PERCENTAGES
           
           *> 総合計の表示
           PERFORM DISPLAY-FINANCIAL-SUMMARY
           
           DISPLAY 'Statistics generation completed'.
       
       CALCULATE-WEIGHTED-AVERAGE.
           *> COBOLの基本演算（減算、乗算、除算）
           COMPUTE GROSS-SALES = GROSS-SALES - CALCULATED-TAX
           
           IF GROSS-SALES NOT = 0
               DIVIDE GROSS-SALES BY 2 GIVING WEIGHTED-AVERAGE
                       ROUNDED MODE ROUND-HALF-EVEN
           END-IF.
       
       CALCULATE-PERCENTAGES.
           IF GROSS-SALES > 0
               COMPUTE PERCENTAGE-VALUE = 
                   (CALCULATED-COMMISSION / GROSS-SALES) * 100
                       ROUNDED MODE ROUND-HALF-EVEN
           END-IF.
       
       *> ================================================================
       *> 文字列操作の例
       *> ================================================================
       
       DEMONSTRATE-STRING-OPERATIONS.
           DISPLAY 'Demonstrating String Operations...'
           
           *> 文字列の結合（STRING文）
           STRING REPORT-TITLE DELIMITED BY SIZE
                  ' - ' DELIMITED BY SIZE
                  CURRENT-DATE DELIMITED BY SIZE
                     INTO REPORT-SUBTITLE
                     ON OVERFLOW
                         DISPLAY 'String operation overflow'
           END-STRING
           
           *> 文字列の分解（UNSTRING文）
           UNSTRING TRANSACTION-DATE DELIMITED BY ALL SPACES
                    INTO YEAR-PART, MONTH-PART, DAY-PART
                    ON OVERFLOW
                        DISPLAY 'Unstring operation overflow'
           END-UNSTRING
           
           *> 文字列の検索（INSPECT文）
           INSPECT CUSTOMER-NAME TALLYING
                   COUNT-OF-A FOR CHARACTERS BEFORE INITIAL '-'
           
           *> 文字列の置換
           INSPECT CUSTOMER-NAME REPLACING ALL SPACES BY '-'
               
           *> 文字列の先頭に文字を追加
           INSPECT REPORT-SUBTITLE REPLACING LEADING SPACES BY '*'
           
           DISPLAY 'String operations completed'.
       
       *> ================================================================
       *> 条件分岐の例
       *> ================================================================
       
       DEMONSTRATE-CONDITIONAL-LOGIC.
           DISPLAY 'Demonstrating Conditional Logic...'
           
           *> IF文の例
           IF CUSTOMER-STATUS = 'A'
               DISPLAY 'Customer is active'
               PERFORM PROCESS-ACTIVE-CUSTOMER
           ELSE
               IF CUSTOMER-STATUS = 'I'
                   DISPLAY 'Customer is inactive'
               ELSE
                   DISPLAY 'Customer status unknown'
               END-IF
           END-IF
           
           *> SWITCH-CASE的な処理（EVALUATE文）
           EVALUATE TRUE
               WHEN CREDIT-LIMIT GREATER THAN OR EQUAL TO 50000
                   DISPLAY 'Premium customer'
                   PERFORM PREMIUM-CUSTOMER-PROCESSING
               WHEN CREDIT-LIMIT GREATER THAN OR EQUAL TO 10000
                   DISPLAY 'Standard customer'
                   PERFORM PERFORM-STANDARD-CUSTOMER-PROCESSING
               WHEN CREDIT-LIMIT LESS THAN 10000
                   DISPLAY 'Basic customer'
                   PERFORM BASIC-CUSTOMER-PROCESSING
               WHEN OTHER
                   DISPLAY 'Credit limit not set'
           END-EVALUATE
           
           *> 複合条件の例
           IF (CUSTOMER-STATUS = 'A') AND (CREDIT-LIMIT > 0)
               DISPLAY 'Valid active customer with credit'
           ELSE
               IF (CUSTOMER-STATUS = 'I') OR (CREDIT-LIMIT = 0)
                   DISPLAY 'Invalid or inactive customer'
               END-IF
           END-IF
          
           DISPLAY 'Conditional logic demonstration completed'.
       
       *> ================================================================
       *> ループ処理の例
       *> ================================================================
       
       DEMONSTRATE-LOOP-CONSTRUCTS.
           DISPLAY 'Demonstrating Loop Constructs...'
           
           *> 基本のPERFORM文（固定回数ループ）
           PERFORM VARYING MAIN-LOOP-COUNTER FROM 1 BY 1
               UNTIL MAIN-LOOP-COUNTER > 10
               DISPLAY 'Loop iteration: ' MAIN-LOOP-COUNTER
               PERFORM PROCESS-LOOP-ITERATION
           END-PERFORM
           
           *> 配列を使ったループ（INDEX付き）
           PERFORM VARYING MONTH-INDEX FROM 1 BY 1
               UNTIL MONTH-INDEX > 12
               DISPLAY 'Processing month: ' MONTH-NAME(MONTH-INDEX)
           END-PERFORM
           
           *> ネストしたループ（二重ループ）
           PERFORM VARYING MONTH-INDEX FROM 1 BY 1
               UNTIL MONTH-INDEX > 12
               PERFORM VARYING PRODUCT-INDEX FROM 1 BY 1
                   UNTIL PRODUCT-INDEX > 5
                   PERFORM CROSS-MONTH-PRODUCT-CALCULATION
               END-PERFORM
           END-PERFORM
           
           *> 条件付きループ
           PERFORM WITH TEST BEFORE
               UNTIL RETRY-COUNT >= 3 OR (PROCESSING-STATUS = 'S')
               DISPLAY 'Retry attempt: ' RETRY-COUNT
               ADD 1 TO RETRY-COUNT
               PERFORM VALIDATE-DATA
           END-PERFORM
           
           DISPLAY 'Loop constructs demonstration completed'.
       
       *> ================================================================
       *> サーチとソート操作の例
       *> ================================================================
       
       DEMONSTRATE-SEARCH-OPERATIONS.
           DISPLAY 'Demonstrating Search Operations...'
           
           *> SEARCH文（配列の検索）
           SET MONTH-INDEX TO 1
           SEARCH MONTHLY-SALES
               WHEN MONTH-NAME(MONTH-INDEX) = 'December'
                   DISPLAY 'Found December sales data'
                   PERFORM PROCESS-DECEMBER-SALES
           END-SEARCH
           
           *> SET文とINDEX操作
           SET SEARCH-INDEX TO 1
           PERFORM SEARCH-CUSTOMER-BY-STATUS
           
           *> 手動サーチ例
           PERFORM SEARCH-TABLE-MANUALLY
           
           DISPLAY 'Search operations demonstration completed'.
       
       DEMONSTRATE-SORT-OPERATIONS.
           DISPLAY 'Demonstrating Sort Operations...'
           
           *> COBOLの内部ソート機能（SORT文）
           SORT SORT-WORKFILE ON ASCENDING KEY SORT-SALES-TOTAL
               INPUT PROCEDURE IS PREPARE-SORT-DATA
               OUTPUT PROCEDURE IS PROCESS-SORTED-DATA
           
           DISPLAY 'Sort operations demonstration completed'.
       
       *> ================================================================
       *> レポート生成機能
       *> ================================================================
       
       GENERATE-DETAIL-REPORT.
           DISPLAY 'Generating Detailed Report...'
           
           OPEN OUTPUT REPORT-OUTPUT
           
           PERFORM WRITE-REPORT-HEADING
           
           PERFORM VARYING MONTH-INDEX FROM 1 BY 1
               UNTIL MONTH-INDEX > 12
               PERFORM WRITE-MONTH-REPORT-LINE
               PERFORM CHECK-PAGE-BREAK
           END-PERFORM
           
           PERFORM WRITE-REPORT-FOOTING
           
           CLOSE REPORT-OUTPUT
           
           DISPLAY 'Detailed report generated'.
       
       *> ================================================================
       *> サブルーチン（PERFORM文）の例
       *> ================================================================
       
       INITIALIZE-MONTH-TABLE.
           *> 月名テーブルの初期化
           MOVE 'January   ' TO MONTH-NAME(1)
           MOVE 'February  ' TO MONTH-NAME(2)
           MOVE 'March     ' TO MONTH-NAME(3)
           MOVE 'April     ' TO MONTH-NAME(4)
           MOVE 'May       ' TO MONTH-NAME(5)
           MOVE 'June      ' TO MONTH-NAME(6)
           MOVE 'July      ' TO MONTH-NAME(7)
           MOVE 'August    ' TO MONTH-NAME(8)
           MOVE 'September ' TO MONTH-NAME(9)
           MOVE 'October   ' TO MONTH-NAME(10)
           MOVE 'November  ' TO MONTH-NAME(11)
           MOVE 'December  ' TO MONTH-NAME(12).
       
       ADD-TO-MONTHLY-SALES.
           *> 月別売上への加算処理
           IF WORK-MONTH >= 1 AND WORK-MONTH <= 12
               SET MONTH-INDEX TO WORK-MONTH
               ADD TOTAL-AMOUNT TO MONTH-SALES(MONTH-INDEX)
               ADD 1 TO MONTH-CUSTOMERS(MONTH-INDEX)
           END-IF.
       
       DISPLAY-FINANCIAL-SUMMARY.
           DISPLAY 'Financial Summary:'
           DISPLAY '================='
           DISPLAY 'Gross Sales:     ' GROSS-SALES
           DISPLAY 'Calculated Tax:   ' CALCULATED-TAX
           DISPLAY 'Commission:       ' CALCULATED-COMMISSION
           DISPLAY 'Weighted Average: ' WEIGHTED-AVERAGE
           DISPLAY 'Percentage:       ' PERCENTAGE-VALUE '%'.
       
       FINALIZE-PROGRAM.
           DISPLAY 'Finalizing program...'
           
           ACCEPT PROCESSING-END-TIME FROM TIME
           
           DISPLAY 'Program execution completed successfully'
           DISPLAY 'Start time: ' PROCESSING-START-TIME
           DISPLAY 'End time:   ' PROCESSING-END-TIME
           DISPLAY '====================================='.
       
       *> ================================================================
       *> 顧客ステータス別処理ルーチン
       *> ================================================================
       
       PROCESS-ACTIVE-CUSTOMER.
           DISPLAY 'Processing Active Customer: ' CUSTOMER-NAME
           *> アクティブ顧客の特別処理
           IF CREDIT-LIMIT > 0
               COMPUTE CALCULATED-COMMISSION = CREDIT-LIMIT * 0.02
               DISPLAY 'Active customer commission calculated: ' CALCULATED-COMMISSION
           END-IF
           *> アクティブ顧客の売上にボーナス加算
           ADD 100 TO GROSS-SALES
           DISPLAY 'Bonus added for active customer'.
       
       PROCESS-INACTIVE-CUSTOMER.
           DISPLAY 'Processing Inactive Customer: ' CUSTOMER-NAME
           *> 非アクティブ顧客の処理
           MOVE 0 TO CALCULATED-COMMISSION
           DISPLAY 'Commission set to zero for inactive customer'
           *> 非アクティブ顧客の売上からペナルティ減算
           IF GROSS-SALES > 50
               SUBTRACT 50 FROM GROSS-SALES
               DISPLAY 'Penalty applied to inactive customer'
           END-IF.
       
       PROCESS-SUSPENDED-CUSTOMER.
           DISPLAY 'Processing Suspended Customer: ' CUSTOMER-NAME
           *> 停止顧客の処理
           MOVE 0 TO CALCULATED-COMMISSION
           DISPLAY 'Commission set to zero for suspended customer'
           *> 停止顧客の売上から大きなペナルティ減算
           IF GROSS-SALES > 100
               SUBTRACT 100 FROM GROSS-SALES
               DISPLAY 'Large penalty applied to suspended customer'
           END-IF
           *> 停止理由の記録
           MOVE 'SUSPENDED' TO ERROR-MESSAGES.
       
       PROCESS-UNKNOWN-CUSTOMER.
           DISPLAY 'Processing Unknown Status Customer: ' CUSTOMER-NAME
           *> 不明ステータス顧客の処理
           DISPLAY 'Warning: Customer status is unknown'
           MOVE 'UNKNOWN_STATUS' TO ERROR-MESSAGES
           *> デフォルト処理
           PERFORM PROCESS-ACTIVE-CUSTOMER.
       
       *> ================================================================
       *> 商品統計更新処理
       *> ================================================================
       
       UPDATE-PRODUCT-STATISTICS.
           DISPLAY 'Updating Product Statistics for: ' PRODUCT-CODE
           *> 商品別統計の更新
           SET PRODUCT-INDEX TO 1
           SEARCH PRODUCT-DATA
               WHEN PRODUCT-ID(PRODUCT-INDEX) = PRODUCT-CODE
                   DISPLAY 'Found existing product: ' PRODUCT-NAME(PRODUCT-INDEX)
                   ADD QUANTITY TO PRODUCT-COUNT
                   COMPUTE PRODUCT-PRICE(PRODUCT-INDEX) = 
                       PRODUCT-PRICE(PRODUCT-INDEX) + UNIT-PRICE
               WHEN PRODUCT-ID(PRODUCT-INDEX) = SPACES
                   DISPLAY 'Adding new product to table'
                   MOVE PRODUCT-CODE TO PRODUCT-ID(PRODUCT-INDEX)
                   MOVE 'New Product' TO PRODUCT-NAME(PRODUCT-INDEX)
                   MOVE UNIT-PRICE TO PRODUCT-PRICE(PRODUCT-INDEX)
                   MOVE 'General' TO PRODUCT-CATEGORY(PRODUCT-INDEX)
                   MOVE 12 TO WARRANTY-MONTHS(PRODUCT-INDEX)
           END-SEARCH.
       
       *> ================================================================
       *> ループ処理と計算ルーチン
       *> ================================================================
       
       PROCESS-LOOP-ITERATION.
           DISPLAY 'Processing loop iteration: ' MAIN-LOOP-COUNTER
           *> ループ内での処理
           COMPUTE WEIGHTED-AVERAGE = WEIGHTED-AVERAGE + MAIN-LOOP-COUNTER
           *> 条件に応じた処理
           IF MAIN-LOOP-COUNTER MOD 2 = 0
               DISPLAY 'Even iteration - special processing'
               ADD 10 TO GROSS-SALES
           ELSE
               DISPLAY 'Odd iteration - standard processing'
               ADD 5 TO GROSS-SALES
           END-IF.
       
       CROSS-MONTH-PRODUCT-CALCULATION.
           DISPLAY 'Cross calculation for month: ' MONTH-INDEX ' product: ' PRODUCT-INDEX
           *> 月と商品のクロス計算
           COMPUTE MONTH-SALES(MONTH-INDEX) = 
               MONTH-SALES(MONTH-INDEX) + (PRODUCT-INDEX * 100)
           *> 商品カテゴリ別の処理
           EVALUATE PRODUCT-INDEX
               WHEN 1
                   ADD 50 TO MONTH-SALES(MONTH-INDEX)
               WHEN 2
                   ADD 75 TO MONTH-SALES(MONTH-INDEX)
               WHEN 3
                   ADD 100 TO MONTH-SALES(MONTH-INDEX)
               WHEN OTHER
                   ADD 25 TO MONTH-SALES(MONTH-INDEX)
           END-EVALUATE.
       
       *> ================================================================
       *> データ検証とリトライ処理
       *> ================================================================
       
       VALIDATE-DATA.
           DISPLAY 'Validating data...'
           *> データ検証ロジック
           IF CUSTOMER-ID = 0 OR CUSTOMER-ID = SPACES
               DISPLAY 'Invalid customer ID'
               MOVE 'E' TO PROCESSING-STATUS
               MOVE 'INVALID_CUSTOMER_ID' TO ERROR-MESSAGES
           ELSE
               IF TOTAL-AMOUNT < 0
                   DISPLAY 'Invalid total amount'
                   MOVE 'E' TO PROCESSING-STATUS
                   MOVE 'NEGATIVE_AMOUNT' TO ERROR-MESSAGES
               ELSE
                   IF QUANTITY = 0
                       DISPLAY 'Invalid quantity'
                       MOVE 'E' TO PROCESSING-STATUS
                       MOVE 'ZERO_QUANTITY' TO ERROR-MESSAGES
                   ELSE
                       DISPLAY 'Data validation passed'
                       MOVE 'C' TO PROCESSING-STATUS
                   END-IF
               END-IF
           END-IF.
       
       *> ================================================================
       *> 検索操作ルーチン
       *> ================================================================
       
       SEARCH-CUSTOMER-BY-STATUS.
           DISPLAY 'Searching customers by status...'
           *> ステータス別顧客検索
           SET SEARCH-INDEX TO 1
           PERFORM VARYING SEARCH-INDEX FROM 1 BY 1
               UNTIL SEARCH-INDEX > 50
               IF FOUND-CUSTOMER-ID(SEARCH-INDEX) = CUSTOMER-ID
                   DISPLAY 'Customer found in search table'
                   MOVE CUSTOMER-ID TO FOUND-CUSTOMER-ID(SEARCH-INDEX)
                   MOVE CUSTOMER-NAME TO FOUND-CUSTOMER-NAME(SEARCH-INDEX)
                   MOVE 100 TO SCORE(SEARCH-INDEX)
                   EXIT PERFORM
               END-IF
           END-PERFORM.
       
       SEARCH-TABLE-MANUALLY.
           DISPLAY 'Manual table search...'
           *> 手動テーブル検索
           SET MONTH-INDEX TO 1
           PERFORM VARYING MONTH-INDEX FROM 1 BY 1
               UNTIL MONTH-INDEX > 12
               IF MONTH-SALES(MONTH-INDEX) > 1000
                   DISPLAY 'High sales month found: ' MONTH-NAME(MONTH-INDEX)
                   DISPLAY 'Sales amount: ' MONTH-SALES(MONTH-INDEX)
               END-IF
           END-PERFORM.
       
       *> ================================================================
       *> ソート処理ルーチン
       *> ================================================================
       
       PREPARE-SORT-DATA.
           DISPLAY 'Preparing data for sort...'
           *> ソート用データの準備
           OPEN OUTPUT SORT-INFILE
           PERFORM VARYING MONTH-INDEX FROM 1 BY 1
               UNTIL MONTH-INDEX > 12
               MOVE CUSTOMER-ID TO SORT-CUSTOMER-ID
               MOVE MONTH-NAME(MONTH-INDEX) TO SORT-NAME
               MOVE MONTH-SALES(MONTH-INDEX) TO SORT-SALES-TOTAL
               WRITE SORT-RECORD
           END-PERFORM
           CLOSE SORT-INFILE.
       
       PROCESS-SORTED-DATA.
           DISPLAY 'Processing sorted data...'
           *> ソート済みデータの処理
           OPEN INPUT SORT-OUTFILE
           PERFORM UNTIL EOF-REACHED
               READ SORT-OUTFILE
                   AT END
                       MOVE 'Y' TO EOF-FLAG
                   NOT AT END
                       DISPLAY 'Sorted record: ' SORT-CUSTOMER-ID ' ' SORT-NAME ' ' SORT-SALES-TOTAL
                       ADD SORT-SALES-TOTAL TO GROSS-SALES
               END-READ
           END-PERFORM
           CLOSE SORT-OUTFILE
           MOVE 'N' TO EOF-FLAG.
       
       *> ================================================================
       *> レポート生成ルーチン
       *> ================================================================
       
       WRITE-REPORT-HEADING.
           DISPLAY 'Writing report heading...'
           *> レポートヘッダーの書き込み
           MOVE SPACES TO REPORT-LINE
           STRING REPORT-TITLE DELIMITED BY SIZE
                  ' - Page ' DELIMITED BY SIZE
                  PAGE-NUMBER DELIMITED BY SIZE
                     INTO REPORT-LINE
           END-STRING
           WRITE REPORT-LINE
           MOVE SPACES TO REPORT-LINE
           STRING REPORT-SUBTITLE DELIMITED BY SIZE
                  ' - Generated on ' DELIMITED BY SIZE
                  CURRENT-DATE DELIMITED BY SIZE
                     INTO REPORT-LINE
           END-STRING
           WRITE REPORT-LINE
           MOVE SPACES TO REPORT-LINE
           WRITE REPORT-LINE
           MOVE 3 TO LINE-COUNT.
       
       WRITE-MONTH-REPORT-LINE.
           DISPLAY 'Writing month report line...'
           *> 月別レポート行の書き込み
           MOVE SPACES TO REPORT-LINE
           STRING MONTH-NAME(MONTH-INDEX) DELIMITED BY SIZE
                  ' Sales: ' DELIMITED BY SIZE
                  MONTH-SALES(MONTH-INDEX) DELIMITED BY SIZE
                  ' Customers: ' DELIMITED BY SIZE
                  MONTH-CUSTOMERS(MONTH-INDEX) DELIMITED BY SIZE
                     INTO REPORT-LINE
           END-STRING
           WRITE REPORT-LINE
           ADD 1 TO LINE-COUNT.
       
       CHECK-PAGE-BREAK.
           DISPLAY 'Checking page break...'
           *> ページブレークのチェック
           IF LINE-COUNT >= LINES-PER-PAGE
               PERFORM WRITE-REPORT-HEADING
               ADD 1 TO PAGE-NUMBER
           END-IF.
       
       WRITE-REPORT-FOOTING.
           DISPLAY 'Writing report footing...'
           *> レポートフッターの書き込み
           MOVE SPACES TO REPORT-LINE
           WRITE REPORT-LINE
           MOVE SPACES TO REPORT-LINE
           STRING 'Report generated at ' DELIMITED BY SIZE
                  PROCESSING-END-TIME DELIMITED BY SIZE
                  ' - Total Pages: ' DELIMITED BY SIZE
                  PAGE-NUMBER DELIMITED BY SIZE
                     INTO REPORT-LINE
           END-STRING
           WRITE REPORT-LINE
           MOVE SPACES TO REPORT-LINE
           STRING 'Total Gross Sales: ' DELIMITED BY SIZE
                  GROSS-SALES DELIMITED BY SIZE
                     INTO REPORT-LINE
           END-STRING
           WRITE REPORT-LINE.
       
       GENERATE-REPORT-HEADING.
           DISPLAY 'Generating report heading...'
           *> レポートヘッダーの生成
           MOVE SPACES TO REPORT-DATE
           STRING CURRENT-DATE(1:4) DELIMITED BY SIZE
                  '/' DELIMITED BY SIZE
                  CURRENT-DATE(5:2) DELIMITED BY SIZE
                  '/' DELIMITED BY SIZE
                  CURRENT-DATE(7:2) DELIMITED BY SIZE
                     INTO REPORT-DATE
           END-STRING
           DISPLAY 'Report date set to: ' REPORT-DATE.
       
       PROCESS-PRODUCT-TABLE.
           DISPLAY 'Processing product table...'
           *> 商品テーブルの処理
           PERFORM VARYING PRODUCT-INDEX FROM 1 BY 1
               UNTIL PRODUCT-INDEX > PRODUCT-COUNT
               DISPLAY 'Processing product: ' PRODUCT-ID(PRODUCT-INDEX)
               DISPLAY 'Product name: ' PRODUCT-NAME(PRODUCT-INDEX)
               DISPLAY 'Product price: ' PRODUCT-PRICE(PRODUCT-INDEX)
               DISPLAY 'Product category: ' PRODUCT-CATEGORY(PRODUCT-INDEX)
               DISPLAY 'Warranty months: ' WARRANTY-MONTHS(PRODUCT-INDEX)
           END-PERFORM.
       
       *> ================================================================
       *> 計算処理と数学演算デモンストレーション
       *> ================================================================
       
       DEMONSTRATE-COMPUTATIONAL-DATA.
           DISPLAY 'Demonstrating Computational Data...'
           *> 計算用データのデモンストレーション
           DISPLAY 'Binary number: ' BINARY-NUMBER
           DISPLAY 'Packed decimal: ' PACKED-DECIMAL
           DISPLAY 'Floating point: ' FLOATING-POINT
           DISPLAY 'Double precision: ' DOUBLE-PRECISION
           
           *> 計算用データの演算
           COMPUTE BINARY-NUMBER = BINARY-NUMBER * 2
           COMPUTE PACKED-DECIMAL = PACKED-DECIMAL + 100.50
           COMPUTE FLOATING-POINT = FLOATING-POINT / 2
           COMPUTE DOUBLE-PRECISION = DOUBLE-PRECISION * 1.5
           
           DISPLAY 'After calculations:'
           DISPLAY 'Binary number: ' BINARY-NUMBER
           DISPLAY 'Packed decimal: ' PACKED-DECIMAL
           DISPLAY 'Floating point: ' FLOATING-POINT
           DISPLAY 'Double precision: ' DOUBLE-PRECISION.
       
       PERFORM-MATHEMATICAL-CALCULATIONS.
           DISPLAY 'Performing Mathematical Calculations...'
           *> 数学的計算の実行
           COMPUTE GROSS-SALES = GROSS-SALES + (GROSS-SALES * 0.1)
           COMPUTE CALCULATED-TAX = GROSS-SALES * TAX-RATE
           COMPUTE CALCULATED-COMMISSION = GROSS-SALES * COMMISSION-RATE
           COMPUTE NET-SALES = GROSS-SALES - CALCULATED-TAX - CALCULATED-COMMISSION
           
           *> 統計計算
           IF RECORD-COUNTER > 0
               COMPUTE WEIGHTED-AVERAGE = GROSS-SALES / RECORD-COUNTER
           END-IF
           
           DISPLAY 'Mathematical calculations completed'
           DISPLAY 'Gross Sales: ' GROSS-SALES
           DISPLAY 'Net Sales: ' NET-SALES
           DISPLAY 'Weighted Average: ' WEIGHTED-AVERAGE.
       
       *> ================================================================
       *> 顧客階層別処理ルーチン
       *> ================================================================
       
       PREMIUM-CUSTOMER-PROCESSING.
           DISPLAY 'Processing Premium Customer...'
           *> プレミアム顧客の特別処理
           COMPUTE CALCULATED-COMMISSION = CREDIT-LIMIT * 0.03
           ADD 200 TO GROSS-SALES
           DISPLAY 'Premium customer bonus applied'
           DISPLAY 'Premium commission: ' CALCULATED-COMMISSION
           *> プレミアム顧客の特別サービス
           MOVE 'PREMIUM_SERVICE' TO ERROR-MESSAGES.
       
       PERFORM-STANDARD-CUSTOMER-PROCESSING.
           DISPLAY 'Processing Standard Customer...'
           *> 標準顧客の処理
           COMPUTE CALCULATED-COMMISSION = CREDIT-LIMIT * 0.02
           ADD 100 TO GROSS-SALES
           DISPLAY 'Standard customer processing completed'
           DISPLAY 'Standard commission: ' CALCULATED-COMMISSION
           *> 標準サービス
           MOVE 'STANDARD_SERVICE' TO ERROR-MESSAGES.
       
       BASIC-CUSTOMER-PROCESSING.
           DISPLAY 'Processing Basic Customer...'
           *> ベーシック顧客の処理
           COMPUTE CALCULATED-COMMISSION = CREDIT-LIMIT * 0.01
           ADD 50 TO GROSS-SALES
           DISPLAY 'Basic customer processing completed'
           DISPLAY 'Basic commission: ' CALCULATED-COMMISSION
           *> ベーシックサービス
           MOVE 'BASIC_SERVICE' TO ERROR-MESSAGES.
       
       END PROGRAM COMPREHENSIVE-SAMPLE.

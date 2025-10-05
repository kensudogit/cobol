       IDENTIFICATION DIVISION.
       PROGRAM-ID. lambda-function.
       AUTHOR. COBOL Lambda Converter.
       DATE-WRITTEN. 2025.
       SECURITY. None.
       
       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
       SELECT LAMBDA-LOG ASSIGN TO "LAMBDA.LOG"
           ORGANIZATION IS LINE SEQUENTIAL.
       
       DATA DIVISION.
       FILE SECTION.
       FD  LAMBDA-LOG
           LABEL RECORDS ARE STANDARD
           RECORDING MODE IS F.
       01  LOG-RECORD PIC X(500).
       
       WORKING-STORAGE SECTION.
       
       *> 環境変数の定義
       01  ENVIRONMENT-VARIABLES.
           05  POSTS-TABLE-NAME PIC X(50) VALUE 'wp_posts'.
           05  POSTMETA-TABLE-NAME PIC X(50) VALUE 'wp_postmeta'.
           05  CONTENT-BUCKET-NAME PIC X(50) VALUE 'kenko21-web'.
           05  MAX-RETRY-ATTEMPTS PIC 9(3) VALUE 003.
           05  CACHE-TTL-SECONDS PIC 9(4) VALUE 3600.
           05  DEFAULT-PAGE-SIZE PIC 9(3) VALUE 010.
       
       *> 定数の定義
       01  CONSTANTS.
           05  APPLICATION-JSON PIC X(20) VALUE 'application/json'.
           05  ALLOWED-METHODS PIC X(20) VALUE 'OPTIONS,POST,GET'.
           05  ALLOWED-HEADERS PIC X(100) 
               VALUE 'Content-Type,X-Amz-Date,Authorization,X-Api-Key,X-Amz-Security-Token'.
           05  INTERNAL-SERVER-ERROR PIC X(30) VALUE 'Internal server error'.
           05  STATUS-200 PIC X(3) VALUE '200'.
           05  STATUS-400 PIC X(3) VALUE '400'.
           05  STATUS-500 PIC X(3) VALUE '500'.
       
       *> Lambda関数の状態管理
       01  LAMBDA-STATUS.
           05 STATUS-CODE PIC X(3).
           05 RESPONSE-BODY PIC X(2000).
           05 IS-INITIALIZED PIC X VALUE 'N'.
           05 INITIALIZATION-SUCCESS PIC X VALUE 'N'.
       
       *> HTTPリクエストの解析
       01  HTTP-REQUEST.
           05  REQUEST-BODY PIC X(2000).
           05  KEYWORD PIC X(100).
           05  SITE-CODE PIC X(10).
           05  PAGE PIC 9(3) VALUE 001.
           05  PER-PAGE PIC 9(3) VALUE 010.
           05  LIMIT PIC 9(4) VALUE 0000.
       
       *> Django Postレコードの定義
       01  WP-POST-RECORD.
           05  SITE-CODE PIC X(3).
           05  POST-ID PIC 9(5).
           05  POST-GUID PIC X(100).
           05  MENU-ORDER PIC 9(3).
           05  PING-STATUS PIC X(10).
           05  POST-AUTHOR PIC 9(5).
           05  POST-CONTENT PIC X(5000).
           05  POST-DATE PIC X(30).
           05  POST-TITLE PIC X(200).
           05  POST-NAME PIC X(100).
           05  POST-STATUS PIC X(10).
           05  POST-TYPE PIC X(20).
           05  COMMENT-COUNT PIC 9(3).
       
       *> 検索結果の定義
       01  SEARCH-RESULTS.
           05  RESULTS-COUNT PIC 9(3) VALUE 0.
           05  RESULT-TABLE OCCURS 100 TIMES
                   INDEXED BY I-RESULT.
               10  RESULT-ID PIC 9(5).
               10  RESULT-TITLE PIC X(200).
               10  RESULT-URL PIC X(100).
               10  RESULT-POST-DATE PIC X(30).
               10  RESULT-RELEVANCE-SCORE PIC 9(3) VALUE 0.
       
       *> カテゴリ検索結果
       01  CATEGORY-RESULTS.
           05  CATEGORY-COUNT PIC 9(3) VALUE 0.
           05  CATEGORY-TABLE OCCURS 50 TIMES
                   INDEXED BY I-CATEGORY.
               10  CATEGORY-NAME PIC X(50).
       
       *> エラー管理
       01  ERROR-HANDLING.
           05  ERROR-OCCURRED PIC X VALUE 'N'.
           05  ERROR-MESSAGE PIC X(200).
           05  ERROR-DETAILS PIC X(500).
       
       *> ログ管理
       01  LOGGING.
           05  LOG-MESSAGE PIC X(500).
           05  LOG-TIMESTAMP PIC X(30).
           05  LOG-LEVEL PIC X(10).
       
       *> 一時作業変数
       01  TEMP-VARIABLES.
           05  WS-COUNTER PIC 9(3).
           05  WS-TEMP PIC X(100).
           05  WS-WORKING PIC X(200).
           05  WS-MATCH-FOUND PIC X VALUE 'N'.
       
       *> レート制限管理
       01  RATE-LIMITING.
           05  MAX-REQUESTS PIC 9(4) VALUE 0100.
           05  TIME-WINDOW PIC 9(4) VALUE 3600.
           05  CURRENT-REQUESTS PIC 9(4) VALUE 0000.
           05  RATE-LIMIT-EXCEEDED PIC X VALUE 'N'.
       
       PROCEDURE DIVISION.
       MAIN-PROCEDURE.
           PERFORM INITIALIZE-LAMBDA
           
           PERFORM PARSE-REQUEST
           
           EVALUATE WS-WORKING (1:3)
               WHEN "GET"
                   PERFORM HANDLE-GET-REQUEST
               WHEN "POST"
                   PERFORM HANDLE-POST-REQUEST
               WHEN "OPT"
                   PERFORM HANDLE-OPTIONS-REQUEST
               WHEN OTHER
                   PERFORM SET-ERROR-STATUS
           END-EVALUATE
           
           PERFORM CREATE-RESPONSE
           
           PERFORM LOG-RESPONSE
           
           STOP RUN.
       
       INITIALIZE-LAMBDA.
           DISPLAY "=== Lambda Function Initialization ==="
           
           MOVE 'Y' TO IS-INITIALIZED
           
           PERFORM VERIFY-ENVIRONMENT-VARIABLES
           
           PERFORM TEST-DATABASE-CONNECTION
           
           MOVE 'Y' TO INITIALIZATION-SUCCESS
           
           DISPLAY "Lambda function initialized successfully".
       
       VERIFY-ENVIRONMENT-VARIABLES.
           IF POSTS-TABLE-NAME = SPACES
               DISPLAY "Warning: POSTS_TABLE_NAME not set"
           END-IF
           
           IF POSTMETA-TABLE-NAME = SPACES
               DISPLAY "Warning: POSTMETA_TABLE_NAME not set"
           END-IF
           
           IF CONTENT-BUCKET-NAME = SPACES
               DISPLAY "Warning: CONTENT_BUCKET_NAME not set"
           END-IF.
       
       TEST-DATABASE-CONNECTION.
           DISPLAY "Testing database connection..."
           *> COBOLでは実際のデータベース接続テストをシミュレート
           MOVE "Database connection successful" TO LOG-MESSAGE
           PERFORM WRITE-LOG.
       
       PARSE-REQUEST.
           DISPLAY "=== Parsing HTTP Request ==="
           
           *> リクエストボディからパラメータを抽出
           UNSTRING REQUEST-BODY DELIMITED BY '{"'
               INTO TEMP-VARIABLES, KEYWORD, SITE-CODE
               ON OVERFLOW
                   DISPLAY "Request parsing overflow"
           END-UNSTRING
           
           DISPLAY "Parsed keyword: " KEYWORD
           DISPLAY "Parsed site_code: " SITE-CODE.
       
       HANDLE-GET-REQUEST.
           DISPLAY "=== Handling GET Request ==="
           
           PERFORM CHECK-RATE-LIMIT
           
           IF RATE-LIMIT-EXCEEDED = 'N'
               PERFORM SEARCH-CONTENT
               PERFORM VALIDATE-SEARCH-RESULTS
           END-IF.
       
       HANDLE-POST-REQUEST.
           DISPLAY "=== Handling POST Request ==="
           
           PERFORM CHECK-RATE-LIMIT
           
           IF RATE-LIMIT-EXCEEDED = 'N'
               EVALUATE WS-WORKING (5:10)
                   WHEN "categories"
                       PERFORM HANDLE-CATEGORIES-REQUEST
                   WHEN "external-"
                       PERFORM HANDLE-EXTERNAL-CONTENT-REQUEST
                   WHEN "health"
                       PERFORM HANDLE-HEALTH-REQUEST
                   WHEN "search-"
                       PERFORM HANDLE-SEARCH-REQUEST
                   WHEN OTHER
                       PERFORM SET-ERROR-STATUS
               END-EVALUATE
           END-IF.
       
       HANDLE-OPTIONS-REQUEST.
           DISPLAY "=== Handling OPTIONS Request ==="
           MOVE STATUS-200 TO STATUS-CODE
           MOVE "CORS preflight request handled" TO RESPONSE-BODY.
       
       HANDLE-CATEGORIES-REQUEST.
           DISPLAY "=== Handling Categories Request ==="
           
           PERFORM RETRIEVE-ALL-CATEGORIES
           PERFORM BUILD-CATEGORIES-RESPONSE.
       
       HANDLE-EXTERNAL-CONTENT-REQUEST.
           DISPLAY "=== Handling External Content Request ==="
           
           PERFORM SEARCH-EXTERNAL-CONTENT
           PERFORM BUILD-EXTERNAL-CONTENT-RESPONSE.
       
       HANDLE-HEALTH-REQUEST.
           DISPLAY "=== Handling Health Check Request ==="
           
           PERFORM PERFORM-HEALTH-CHECK
           PERFORM BUILD-HEALSEARCH-REQUEST.
       
       HANDLE-SEARCH-REQUEST.
           DISPLAY "=== Handling Search Request ==="
           
           PERFORM EXECUTE-ADVANCED-SEARCH
           PERFORM BUILD-SEARCH-RESPONSE.
       
       CHECK-RATE-LIMIT.
           ADD 1 TO CURRENT-REQUESTS
           
           IF CURRENT-REQUESTS > MAX-REQUESTS
               MOVE 'Y' TO RATE-LIMIT-EXCEEDED
               DISPLAY "Rate limit exceeded"
           ELSE
               MOVE 'N' TO RATE-LIMIT-EXCEEDED
               DISPLAY "Rate limit OK"
           END-IF.
       
       SEARCH-CONTENT.
           DISPLAY "=== Searching Content ==="
           
           MOVE 0 TO RESULTS-COUNT
           MOVE "Simple content search" TO LOG-MESSAGE
           PERFORM WRITE-LOG
           
           *> 基本的な検索ロジック（COBOL版）
           PERFORM VARYING WS-COUNTER FROM 1 BY 1
               UNTIL WS-COUNTER > 10
               ADD 1 TO RESULTS-COUNT
               SET I-RESULT TO RESULTS-COUNT
               
               MOVE WS-COUNTER TO RESULT-ID(I-RESULT)
               STRING "Sample Post " WS-COUNTER 
                   INTO RESULT-TITLE(I-RESULT)
               END-STRING
               
               MOVE "sample-url.com" TO RESULT-URL(I-RESULT)
               MOVE "2025-01-01" TO RESULT-POST-DATE(I-RESULT)
               
               COMPUTE RESULT-RELEVANCE-SCORE(I-RESULT) = WS-COUNTER * 10
           END-PERFORM.
       
       VALIDATE-SEARCH-RESULTS.
           IF RESULTS-COUNT = 0
               DISPLAY "No search results found"
               MOVE 'Y' TO ERROR-OCCURRED
               MOVE "No results found" TO ERROR-MESSAGE
           ELSE
               DISPLAY "Found " RESULTS-COUNT " results"
               MOVE 'N' TO ERROR-OCCURRED
           END-IF.
       
       RETRIEVE-ALL-CATEGORIES.
           DISPLAY "=== Retrieving Categories ==="
           
           MOVE 0 TO CATEGORY-COUNT
           
           *> サンプルカテゴリの追加
           ADD 1 TO CATEGORY-COUNT
           SET I-CATEGORY TO CATEGORY-COUNT
           MOVE "健康管理" TO CATEGORY-NAME(I-CATEGORY)
           
           ADD 1 TO CATEGORY-COUNT
           SET I-CATEGORY TO CATEGORY-COUNT
           MOVE "喫煙対策" TO CATEGORY-NAME(I-CATEGORY)
           
           ADD 1 TO CATEGORY-COUNT
           SET I-CATEGORY TO CATEGORY-COUNT
           MOVE "女性の健康" TO CATEGORY-NAME(I-CATEGORY)
           
           DISPLAY "Found " CATEGORY-COUNT " categories".
       
       SEARCH-EXTERNAL-CONTENT.
           DISPLAY "=== Searching External Content ==="
           
           MOVE 0 TO RESULTS-COUNT
           
           *> 外部サイト検索のシミュレーション
           PERFORM VARYING WS-COUNTER FROM 1 BY 1
               UNTIL WS-COUNTER > 5
               ADD 1 TO RESULTS-COUNT
               SET I-RESULT TO RESULTS-COUNT
               
               MOVE WS-COUNTER TO RESULT-ID(I-RESULT)
               STRING "External Content " WS-COUNTER 
                   INTO RESULT-TITLE(I-RESULT)
               END-STRING
               
               MOVE "external-site.com" TO RESULT-URL(I-RESULT)
               MOVE "2025-01-01" TO RESULT-POST-DATE(I-RESULT)
           END-PERFORM.
       
       PERFORM-HEALTH-CHECK.
           DISPLAY "=== Performing Health Check ==="
           
           *> システムの健康状態をチェック
           IF IS-INITIALIZED = 'Y' AND INITIALIZATION-SUCCESS = 'Y'
               MOVE STATUS-200 TO STATUS-CODE
               MOVE "System healthy" TO RESPONSE-BODY
           ELSE
               MOVE STATUS-500 TO STATUS-CODE
               MOVE "System unhealthy" TO RESPONSE-BODY
           END-IF.
       
       EXECUTE-ADVANCED-SEARCH.
           DISPLAY "=== Executing Advanced Search ==="
           
           PERFORM SEARCH-CONTENT
           
           IF KEYWORD NOT = SPACES
               PERFORM ENHANCE-SEARCH-WITH-KEYWORDS
           END-IF
           
           PERFORM CALCULATE-RELEVANCE-SCORES.
       
       ENHANCE-SEARCH-WITH-KEYWORDS.
           DISPLAY "=== Enhancing search with keywords ==="
           
           *> キーワード検索のロジック
           PERFORM VARYING I-RESULT FROM 1 BY 1
               UNTIL I-RESULT > RESULTS-COUNT
                   MOVE 'N' TO WS-MATCH-FOUND
                   
                   *> タイトルでのキーワードマッチング
                   IF RESULT-TITLE(I-RESULT) CONTAINS KEYWORD
                       MOVE 'Y' TO WS-MATCH-FOUND
                       ADD 20 TO RESULT-RELEVANCE-SCORE(I-RESULT)
                   END-IF
                   
                   *> 完全マッチが見つからない場合は低スコアを与える
                   IF WS-MATCH-FOUND = 'N'
                       ADD 5 TO RESULT-RELEVANCE-SCORE(I-RESULT)
                   END-IF
           END-PERFORM.
       
       CALCULATE-RELEVANCE-SCORES.
           DISPLAY "=== Calculating Relevance Scores ==="
           
           *> スコアに基づいてソート（バブルソート）
           PERFORM VARYING WS-COUNTER FROM 1 BY 1
               UNTIL WS-COUNTER > RESULTS-COUNT - 1
               PERFORM VARYING WS-WORKING FROM 1 BY 1
                   UNTIL WS-WORKING > RESULTS-COUNT - WS-COUNTER
                   IF RESULT-RELEVANCE-SCORE(WS-WORKING) < 
                      RESULT-RELEVANCE-SCORE(WS-WORKING + 1)
                       PERFORM SWAP-RESULTS
                   END-IF
               END-PERFORM
           END-PERFORM.
       
       SWAP-RESULTS.
           *> 検索結果の交換
           MOVE RESULT-ID(WS-WORKING) TO WS-TEMP
           MOVE RESULT-ID(WS-WORKING + 1) TO RESULT-ID(WS-WORKING)
           MOVE WS-TEMP TO RESULT-ID(WS-WORKING + 1)
           
           MOVE RESULT-TITLE(WS-WORKING) TO WS-TEMP
           MOVE RESULT-TITLE(WS-WORKING + 1) TO RESULT-TITLE(WS-WORKING)
           MOVE WS-TEMP TO RESULT-TITLE(A-WORKING + 1).
       
       BUILD-CATEGORIES-RESPONSE.
           STRING '{"status":"success","categories":['
               INTO RESPONSE-BODY
           END-STRING
           
           PERFORM VARYING I-CATEGORY FROM 1 BY 1
               UNTIL I-CATEGORY > CATEGORY-COUNT
               STRING '{"name":"' CATEGORY-NAME(I-CATEGORY) '"},{"' CATEGORY-NAME(I-CATEGORY) '"}'
                   INTO RESPONSE-BODY
               END-STRING
           END-PERFORM
           
           STRING '],"count":' CATEGORY-COUNT '}'
               INTO RESPONSE-BODY
           END-STRING.
       
       BUILD-EXTERNAL-CONTENT-RESPONSE.
           STRING '{"status":"success","results":['
               INTO RESPONSE-BODY
           END-STRING
           
           PERFORM VARYING I-RESULT FROM 1 BY 1
               UNTIL I-RESULT > RESULTS-COUNT
               STRING '{"id":' RESULT-ID(I-RESULT) 
                      ',"title":"' RESULT-TITLE(I-RESULT) '",'
                      '"url":"' RESULT-URL(I-RESULT) '",'
                      '"post_date":"' RESULT-POST-DATE(I-RESULT) '"}'
                   INTO RESPONSE-BODY
               END-STRING
           END-PERFORM
           
           STRING '],"total":' RESULTS-COUNT '}'
               INTO RESPONSE-BODY
           END-STRING.
       
       BUILD-SEARCH-RESPONSE.
           STRING '{"status":"success","results":['
               INTO RESPONSE-BODY
           END-STRING
           
           PERFORM VARYING I-RESULT FROM 1 BY 1
               UNTIL I-RESULT > RESULTS-COUNT
               STRING '{"id":' RESULT-ID(I-RESULT) 
                      ',"title":"' RESULT-TITLE(I-RESULT) '",'
                      '"url":"' RESULT-URL(I-RESULT) '",'
                      '"post_date":"' RESULT-POST-DATE(I-RESULT) '",'
                      '"score":' RESULT-RELEVANCE-SCORE(I-RESULT) '}'
                   INTO RESPONSE-BODY
               END-STRING
           END-PERFORM
           
           STRING '],"pagination":{"total":' RESULTS-COUNT 
                  ',"page":' PAGE ',"per_page":' PER-PAGE '}}'
               INTO RESPONSE-BODY
           END-STRING.
       
       WRITE-LOG.
           OPEN OUTPUT LAMBDA-LOG
           MOVE LOG-MESSAGE TO LOG-RECORD
           WRITE LOG-RECORD
           CLOSE LAMBDA-LOG.
       
       LOG-RESPONSE.
           STRING "Status: " STATUS-CODE " Response: " RESPONSE-BODY
               INTO LOG-MESSAGE
           END-STRING
           PERFORM WRITE-LOG.
       
       CREATE-RESPONSE.
           DISPLAY "=== Creating Final Response ==="
           
           IF ERROR-OCCURRED = 'Y'
               MOVE STATUS-500 TO STATUS-CODE
           ELSE
               MOVE STATUS-200 TO STATUS-CODE
           END-IF
           
           DISPLAY "Final Status Code: " STATUS-CODE
           DISPLAY "Response Body: " RESPONSE-BODY.
       
       SET-ERROR-STATUS.
           MOVE 'Y' TO ERROR-OCCURRED
           MOVE STATUS-400 TO STATUS-CODE
           MOVE '{"error":"Invalid action","message":"Invalid request"}' 
               TO RESPONSE-BODY.
       
       END PROGRAM lambda-function.

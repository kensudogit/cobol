# COBOL言語の全貌 - 包括的サンプルプログラム解説

## 概要

`comprehensive_sample.cob`は、COBOL言語の全機能を網羅した包括的なサンプルプログラムです。このプログラムを通じて、COBOLの構造、文法、機能を総合的に学習できます。

## COBOLプログラムの基本構造

### 1. IDENTIFICATION DIVISION（識別部）
```cobol
IDENTIFICATION DIVISION.
PROGRAM-ID. COMPREHENSIVE-SAMPLE.
AUTHOR. COBOL Education Project.
DATE-WRITTEN. 2025.
SECURITY. Confidential.
```
**機能**：
- プログラムの識別情報を定義
- プログラム名、作成者、作成日などを記録

### 2. ENVIRONMENT DIVISION（環境部）
**INPUT-OUTPUT SECTION / FILE-CONTROL**：
```cobol
SELECT CUSTOMER-MASTER 
    ASSIGN TO 'CUSTOMER.MAST'
    ORGANIZATION IS SEQUENTIAL
```
**機能**：
- 外部ファイルの定義
- ファイル組織の指定（順次、相対、索引）
- 各ファイルのアクセス方法の設定

## データ型とPIC句

### 基本データ型

#### 1. 数値型（9型）
```cobol
05  INTEGER-NUMBER        PIC 9(5)         VALUE 12345.
05  DECIMAL-NUMBER        PIC 9(7)V99      VALUE 12345.67.
05  SIGNED-NUMBER         PIC S9(5)         VALUE -12345.
```
- `9(5)`：5桁の整数
- `V99`：小数点以下2桁
- `S`：符号付き

#### 2. 英数字型（X型/A型）
```cobol
05  TEXT-FIELD           PIC X(50)         VALUE SPACES.
05  ALPHABETIC-FIELD     PIC A(20)         VALUE SPACES.
05  NATIONAL-FIELD       PIC N(20)         VALUE SPACES.
```
- `X`：任意の文字（英数字記号）
- `A`：英字のみ
- `N`：国際文字

#### 3. 編集型（Z型）
```cobol
05  EDIT-MASK            PIC ZZZ,ZZZ.99   VALUE ZEROES.
05  LEADING-ZEROS         PIC 09(5)        VALUE ZEROES.
```
- `Z`：ゼロサプレス（先頭ゼロを空白に）
- カンマ、ピリオドでの表示形式指定

### 計算用データ型
```cobol
05  BINARY-NUMBER        PIC 9(5) COMP     VALUE 100.
05  PACKED-DECIMAL       PIC 9(7)V99 COMP-3 VALUE 12345.67.
05  FLOATING-POINT        PIC 9(7)V99 COMP-1 VALUE 12345.67.
```
- `COMP`：バイナリ形式
- `COMP-3`：パック10進数
- `COMP-1/2`：浮動小数点

## 配列とテーブル（OCCURS句）

### 固定サイズ配列
```cobol
05  MONTHLY-SALES OCCURS 12 TIMES
        INDEXED BY MONTH-INDEX.
    10  MONTH-NAME           PIC X(10).
    10  MONTH-SALES          PIC 9(8)V99 VALUE 0.
```
- `OCCURS 12 TIMES`：12個の要素
- `INDEXED BY`：インデックス変数の定義

### 可変サイズ配列（DEPENDING ON）
```cobol
05  PRODUCT-DATA OCCURS 1 TO 100 TIMES DEPENDING ON PRODUCT-COUNT
        INDEXED BY PRODUCT-INDEX.
```
- 実行時にサイズが決まる配列

## ファイル入出力

### ファイル種類と組織

#### 1. 順次ファイル（SEQUENTIAL）
```cobol
SELECT CUSTOMER-DATABASE 
    ORGANIZATION IS SEQUENTIAL
    ACCESS MODE IS SEQUENTIAL
```
- レコードを順番に読む

#### 2. 相対ファイル（RELATIVE）
```cobol
SELECT SALES-TRANSACTIONS
    ORGANIZATION IS RELATIVE
    ACCESS MODE IS SEQUENTIAL
    RELATIVE KEY IS REL-KEY-INDEX
```
- 相対レコード番号でアクセス

#### 3. 索引ファイル（INDEXED）
```cobol
SELECT MASTER-FILE
    ORGANIZATION IS INDEXED
    ACCESS MODE IS RANDOM
    RECORD KEY IS CUSTOMER-ID
```
- キーで高速検索

### ファイル操作コマンド
```cobol
OPEN INPUT    CUSTOMER-FILE
READ          CUSTOMER-FILE
WRITE         OUTPUT-LINE
CLOSE         CUSTOMER-FILE
REWRITE       CUSTOMER-RECORD
DELETE        CUSTOMER-RECORD
```

## 計算と演算

### 基本演算
```cobol
ADD AMOUNT-1 TO TOTAL-AMOUNT
SUBTRACT DISCOUNT FROM GROSS-AMOUNT GIVING NET-AMOUNT
MULTIPLY HOURS BY RATE GIVING PAY
DIVIDE AMOUNT BY QUANTITY GIVING UNIT-PRICE
```

### COMPUTE文による計算
```cobol
COMPUTE TOTAL-SALES = BASIC-SALES + COMMISSION - TAX
COMPUTE TAX-AMOUNT = GROSS * TAX-RATE
          ROUNDED MODE ROUND-HALF-EVEN
```

### 丸めモード
- `ROUND-HALF-EVEN`：最近偶数丸め
- `ROUND-HALF-UP`：四捨五入
- `ROUND-HALF-DOWN`：切捨て

## 文字列操作

### STRING文（文字列結合）
```cobol
STRING FIRST-NAME DELIMITED BY SPACE
       ' ' DELIMITED BY SIZE
       LAST-NAME DELIMITED BY SIZE
          INTO FULL-NAME
          ON OVERFLOW
              DISPLAY 'String overflow occurred'
END-STRING
```

### UNSTRING文（文字列分解）
```cobol
UNSTRING DATE-FIELD DELIMITED BY '-'
         INTO YEAR-PART, MONTH-PART, DAY-PART
         TALLYING IN ITEM-COUNT
         ON OVERFLOW
             DISPLAY 'Parsing failed'
END-UNSTRING
```

### INSPECT文（文字列検査・置換）
```cobol
INSPECT TEXT-LINE TALLYING
        SPACE-COUNT FOR CHARACTERS BEFORE INITIAL '-'

INSPECT TEXT-LINE REPLACING ALL SPACES BY '-'

INSPECT TEXT-LINE REPLACING LEADING SPACES BY '*'
```

## 条件分岐

### IF文
```cobol
IF CUSTOMER-STATUS = 'A'
    PERFORM PROCESS-ACTION-CUSTOMER
ELSE
    IF CUSTOMER-STATUS = 'I'
        PERFORM PROCESS-INACTIVE-CUSTOMER
    ELSE
        PERFORM PROCESS-OTHER-STATUS
    END-IF
END-IF
```

### EVALUATE文（多分岐）
```cobol
EVALUATE CUSTOMER-STATUS
    WHEN 'A'
        DISPLAY 'Customer is active'
        PERFORM PROCESS-ACTIVE-CUSTOMER
    WHEN 'I'
        DISPLAY 'Customer is inactive'
    WHEN 'S'
        DISPLAY 'Customer status suspended'
    WHEN OTHER
        DISPLAY 'Unknown customer status'
END-EVALUATE
```

### 複合条件
```cobol
IF (STATUS = 'A') AND (AMOUNT > 1000) AND (NOT END-OF-FILE)
    PERFORM SPECIAL-PROCESSING
END-IF

IF (ERROR-FLAG = 'Y') OR (RETRY-COUNT > 5)
    PERFORM ERROR-HANDLING
END-IF
```

## ループ処理

### PERFORM文によるループ

#### 1. 条件ループ
```cobol
PERFORM UNTIL EOF-REACHED
    READ CUSTOMER-FILE
        AT END
            MOVE 'Y' TO EOF-FLAG
        NOT AT END
            PERFORM PROCESS-CUSTOMER-RECORD
    END-READ
END-PERFORM
```

#### 2. カウンタループ
```cobol
PERFORM VARYING COUNTER FROM 1 BY 1
    UNTIL COUNTER > 10
    DISPLAY 'Counter: ' COUNTER
    PERFORM PROCESS-ITERATION
END-PERFORM
```

#### 3. インデックス付きループ
```cobol
PERFORM VARYING MONTH-INDEX FROM 1 BY 1
    UNTIL MONTH-INDEX > 12
    DISPLAY MONTH-NAME(MONTH-INDEX)
    PERFORM PROCESS-MONTH-DATA
END-PERFORM
```

#### 4. ネストしたループ
```cobol
PERFORM VARYING ROW-INDEX FROM 1 BY 1
    UNTIL ROW-INDEX > MAX-ROWS
    PERFORM VARYING COL-INDEX FROM 1 BY 1
        UNTIL COL-INDEX > MAX-COLS
        PERFORM CALCULATE-CELL-VALUE
    END-PERFORM
END-PERFORM
```

## 検索とソート

### SEARCH文（配列検索）
```cobol
SET MONTH-INDEX TO 1
SEARCH MONTHLY-SALES
    WHEN MONTH-NAME(MONTH-INDEX) = 'December'
        DISPLAY 'Found December'
        PERFORM PROCESS-DECEMBER-DATA
END-SEARCH
```

### 手動検索
```cobol
PERFORM VARYING SEARCH-INDEX FROM 1 BY 1
    UNTIL SEARCH-INDEX > MAX-ENTREIS
    IF CUSTOMER-ID(SEARCH-INDEX) = TARGET-ID
        MOVE CUSTOMER-NAME(SEARCH-INDEX) TO FOUND-NAME
        MOVE 'Y' TO SEARCH-FOUND-FLAG
    END-IF
END-PERFORM
```

### SORT文（ソート操作）
```cobol
SORT SORT-WORKFILE ON ASCENDING KEY SORT-SALES-TOTAL
    INPUT PROCEDURE IS PREPARE-SORT-DATA
    OUTPUT PROCEDURE IS PROCESS-SORTED-DATA
```

## COPY句とセグメンテーション

### COPY句（共通定義取り込み）
```cobol
COPY 'CUSTOMER-COPYBOOK'.
COPY 'DFHAID'.
```

### セグメンテーション（メモリ効率）
```cobol
PROGRAM-ID. COMPREHENSIVE-SAMPLE.
    [Common] IS SECTION-A
    [Use] IDENTIFIED BY SECTION-A.
```

## フラグと条件変数

### 88レベルの条件変数
```cobol
05 FILE-STATUS PIC X(2).
   88 FILE-STATUS-OK    VALUE '00'.
   88 FILE-STATUS-EOF   VALUE '10'.
   88 FILE-STATUS-ERROR VALUE '23'.

05 CUSTOMER-TYPE PIC X(1).
   88 PREMIUM-CUSTOMER VALUE 'P'.
   88 STANDARD-CUSTOMER VALUE 'S'.
   88 BASIC-CUSTOMER VALUE 'B'.
```

### 使用例
```cobol
IF FILE-STATUS-OK
    PERFORM READ-NEXT-RECORD
END-IF

IF PREMIUM-CUSTOMER
    PERFORM PREMIUM-DISCOUNT
END-IF
```

## レポート機能

### ページ制御
```cobol
PERFORM WRITE-PAGE-HEADING
PERFORM WRITE-DETAIL-LINE
PERFORM CHECK-PAGE-BREAK
PERFORM WRITE-PAGE-FOOTING
```

### 改行制御
```cobol
IF LINE-COUNT >= LINES-PER-PAGE
    PERFORM WRITE-PAGE-HEADING
    MOVE 1 TO LINE-COUNT
END-IF
```

## エラーハンドリング

### ファイルステータス検査
```cobol
READ CUSTOMER-FILE
    AT END
        MOVE 'Y' TO EOF-FLAG
    NOT AT END
        PERFORM PROCESS-RECORD
END-READ

IF FILE-STATUS NOT = '00'
    DISPLAY 'File error: ' FILE-STATUS
    PERFORM ERROR-HANDLING
END-IF
```

### INPUT/OUTPUT例外処理
```cobol
STRING TEXT-FIELD DELIMITED BY SIZE
       INTO OUTPUT-FIELD
       ON OVERFLOW
           DISPLAY 'OVERFLOW ERROR'
           PERFORM HANDLE-OVERFLOW
END-STRING
```

## レポート例

### 出力例
```
=====================================
COBOL Comprehensive Sample Program Report - 2025-01-28
=====================================
Monthly Sales Summary:
January:   $45,230.00 (45 customers)
February:  $52,890.50 (52 customers)
...
December:  $78,450.75 (78 customers)

Financial Summary:
=================
Gross Sales:      $456,230.00
Calculated Tax:    $36,498.40
Commission:        $22,811.50
Weighted Average: $228,115.00
Percentage:        5.00%
```

## 実行とデバッグ

### コンパイル
```bash
cobol comprehensive_sample.cob
```

### 実行
```bash
./comprehensive_sample
```

### デバッグオプション
```cobol
DEBUG-ITEM.
    05 DEBUG-FLAG PIC X(1) VALUE 'N'.
        88 DEBUG-ON VALUE 'Y'.
        88 DEBUG-OFF VALUE 'N'.

IF DEBUG-ON
    DISPLAY 'Processing customer: ' CUSTOMER-ID
    DISPLAY 'Status: ' CUSTOMER-STATUS
END-IF
```

## COBOLの特徴まとめ

### 長所
1. **可読性**：英語に近い記述で理解しやすい
2. **保守性**：長期間の保守に適している
3. **数値計算精度**：金融・会計処理に強い
4. **大容量データ処理**：メインフレームに適している
5. **バッチ処理**：大量データの一括処理に優秀

### 適用分野
- 銀行システム
- 保険システム
- 政府機関の基幹システム
- 大企業の経理システム
- レガシーシステム管理

### 学習ポイント
1. **DIVISION構造**：4つの部分を理解する
2. **データ型**：PIC句の使い分け
3. **ファイル処理**：入出力の仕組み
4. **制御構造**：条件分岐とループ
5. **配列操作**：OCCURS句の活用

この包括的サンプルプログラムを通じて、COBOL言語の全貌を理解し、実践的なプログラミングスキルを身につけることができます。

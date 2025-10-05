# COBOL言語リファレンス

## 概要

COBOL（Common Business Oriented Language）は、ビジネスアプリケーション開発に特化したプログラミング言語です。

## プログラム構造

### 4つのDIVISION

1. **IDENTIFICATION DIVISION** - プログラム識別情報
2. **ENVIRONMENT DIVISION** - 環境設定
3. **DATA DIVISION** - データ定義
4. **PROCEDURE DIVISION** - 処理ロジック

## IDENTIFICATION DIVISION

```cobol
IDENTIFICATION DIVISION.
PROGRAM-ID. PROGRAM-NAME.
AUTHOR. AUTHOR-NAME.
DATE-WRITTEN. DATE.
DATE-COMPILED. DATE.
SECURITY. SECURITY-LEVEL.
```

## ENVIRONMENT DIVISION

```cobol
ENVIRONMENT DIVISION.
CONFIGURATION SECTION.
SOURCE-COMPUTER. COMPUTER-NAME.
OBJECT-COMPUTER. COMPUTER-NAME.

INPUT-OUTPUT SECTION.
FILE-CONTROL.
    SELECT FILE-NAME ASSIGN TO 'filename'
    ORGANIZATION IS SEQUENTIAL.
```

## DATA DIVISION

### WORKING-STORAGE SECTION
```cobol
DATA DIVISION.
WORKING-STORAGE SECTION.
01 WS-VARIABLE PIC X(10) VALUE 'INITIAL'.
01 WS-NUMBER PIC 9(5) VALUE 12345.
01 WS-DECIMAL PIC 9(5)V99 VALUE 123.45.
```

### FILE SECTION
```cobol
FILE SECTION.
FD FILE-NAME.
01 FILE-RECORD PIC X(80).
```

## PROCEDURE DIVISION

### 基本構造
```cobol
PROCEDURE DIVISION.
MAIN-PROCEDURE.
    DISPLAY 'Hello, COBOL!'
    STOP RUN.
```

## データ型（PICTURE句）

### 文字型
- `PIC X(n)` - 文字列（n文字）
- `PIC A(n)` - 英字（n文字）

### 数値型
- `PIC 9(n)` - 数値（n桁）
- `PIC 9(n)V99` - 小数点付き数値
- `PIC S9(n)` - 符号付き数値

### 編集済み数値型
- `PIC ZZZ9.99` - ゼロサプレス
- `PIC $ZZZ9.99` - 通貨記号付き
- `PIC 9999-99-99` - 日付形式

## 制御構造

### IF文
```cobol
IF condition THEN
    statement-1
ELSE
    statement-2
END-IF
```

### EVALUATE文
```cobol
EVALUATE variable
    WHEN value-1
        statement-1
    WHEN value-2
        statement-2
    WHEN OTHER
        statement-3
END-EVALUATE
```

### PERFORM文
```cobol
# 基本PERFORM
PERFORM procedure-name

# INLINE PERFORM
PERFORM
    statement-1
    statement-2
END-PERFORM

# PERFORM UNTIL
PERFORM UNTIL condition
    statement
END-PERFORM

# PERFORM VARYING
PERFORM VARYING counter FROM 1 BY 1
UNTIL counter > 10
    statement
END-PERFORM
```

## 入出力操作

### DISPLAY文
```cobol
DISPLAY 'Message'
DISPLAY variable
DISPLAY 'Value: ' variable
```

### ACCEPT文
```cobol
ACCEPT variable
ACCEPT variable FROM DATE
ACCEPT variable FROM TIME
```

### ファイル操作
```cobol
OPEN INPUT input-file
OPEN OUTPUT output-file
OPEN I-O update-file

READ file-name
    AT END
        statement
    NOT AT END
        statement
END-READ

WRITE record-name

CLOSE file-name
```

## 算術演算

### COMPUTE文
```cobol
COMPUTE result = operand1 + operand2
COMPUTE result = operand1 - operand2
COMPUTE result = operand1 * operand2
COMPUTE result = operand1 / operand2
```

### 算術演算子
- `+` - 加算
- `-` - 減算
- `*` - 乗算
- `/` - 除算
- `**` - べき乗

## 文字列操作

### STRING文
```cobol
STRING source-1 DELIMITED BY SIZE
       source-2 DELIMITED BY SIZE
       INTO destination
END-STRING
```

### UNSTRING文
```cobol
UNSTRING source DELIMITED BY delimiter
         INTO destination-1 destination-2
END-UNSTRING
```

## 内部関数

### 数値関数
- `FUNCTION ABS(value)` - 絶対値
- `FUNCTION MAX(value1, value2)` - 最大値
- `FUNCTION MIN(value1, value2)` - 最小値

### 文字列関数
- `FUNCTION LENGTH(string)` - 文字列長
- `FUNCTION UPPER-CASE(string)` - 大文字変換
- `FUNCTION LOWER-CASE(string)` - 小文字変換

### 日付関数
- `FUNCTION CURRENT-DATE` - 現在日時
- `FUNCTION DATE-OF-INTEGER(integer)` - 日付変換

## テーブル（配列）

### 定義
```cobol
01 WS-TABLE.
    05 WS-ITEM OCCURS 10 TIMES PIC X(20).
```

### 使用
```cobol
MOVE 'VALUE' TO WS-ITEM(1)
DISPLAY WS-ITEM(INDEX)
```

## サブルーチン

### 定義
```cobol
PROCEDURE DIVISION.
MAIN-PROCEDURE.
    PERFORM SUBROUTINE-NAME
    STOP RUN.

SUBROUTINE-NAME.
    DISPLAY 'Subroutine called'
    EXIT.
```

### パラメータ付きサブルーチン
```cobol
PROCEDURE DIVISION USING parameter-1 parameter-2.
SUBROUTINE-NAME.
    DISPLAY parameter-1 parameter-2
    EXIT.
```

## エラーハンドリング

### ファイルエラー
```cobol
READ file-name
    AT END
        DISPLAY 'End of file reached'
    NOT AT END
        DISPLAY 'Record read successfully'
END-READ
```

### 数値エラー
```cobol
ON SIZE ERROR
    DISPLAY 'Numeric overflow'
NOT ON SIZE ERROR
    DISPLAY 'Operation successful'
END-COMPUTE
```

## ベストプラクティス

1. **命名規則**
   - 変数名は説明的で分かりやすく
   - WS-（Working Storage）、FD-（File Description）などの接頭辞を使用

2. **コメント**
   - 複雑なロジックにはコメントを追加
   - プログラムの目的を明確に記述

3. **エラーハンドリング**
   - ファイル操作には適切なエラーハンドリングを実装
   - 数値計算にはSIZE ERRORを考慮

4. **パフォーマンス**
   - 不要なファイル操作を避ける
   - 効率的なデータ構造を使用

## デバッグ

### デバッグ出力
```cobol
DISPLAY 'DEBUG: Variable value = ' variable
```

### 条件付きデバッグ
```cobol
IF DEBUG-MODE = 'Y' THEN
    DISPLAY 'Debug information'
END-IF
```

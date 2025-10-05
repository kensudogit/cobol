       IDENTIFICATION DIVISION.
       PROGRAM-ID. CALCULATOR.
       AUTHOR. COBOL Developer.
       DATE-WRITTEN. 2024.
       *> プログラム名: Calculator
       *> 目的: 四則演算を行う電卓プログラム
       *> 機能: ユーザー入力による加算・減算・乗算・除算の実行
       
       ENVIRONMENT DIVISION.
       *> 環境設定（このプログラムでは特別な設定は不要）
       
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       *> 作業領域の変数定義
       01 WS-NUM1 PIC 9(5)V99 VALUE 0.
       *> 第1オペランド（5桁整数+2桁小数）
       01 WS-NUM2 PIC 9(5)V99 VALUE 0.
       *> 第2オペランド（5桁整数+2桁小数）
       01 WS-RESULT PIC 9(8)V99 VALUE 0.
       *> 計算結果（8桁整数+2桁小数）
       01 WS-OPERATION PIC X(1).
       *> 演算子（+, -, *, /）
       01 WS-CONTINUE PIC X(1) VALUE 'Y'.
       *> 継続フラグ（Y/N）
       01 WS-DISPLAY-NUM1 PIC ZZZZ9.99.
       *> 表示用第1オペランド（ゼロサプレス形式）
       01 WS-DISPLAY-NUM2 PIC ZZZZ9.99.
       *> 表示用第2オペランド（ゼロサプレス形式）
       01 WS-DISPLAY-RESULT PIC ZZZZZZ9.99.
       *> 表示用計算結果（ゼロサプレス形式）
       
       PROCEDURE DIVISION.
       MAIN-PROCEDURE.
       *> メイン処理開始
           DISPLAY '=========================================='
           DISPLAY 'COBOL Calculator Program'
           DISPLAY '=========================================='
           DISPLAY ' '
           *> プログラムヘッダー表示
           
           PERFORM UNTIL WS-CONTINUE = 'N' OR WS-CONTINUE = 'n'
           *> ユーザーが'N'または'n'を入力するまで繰り返し
               DISPLAY 'Enter first number: '
               ACCEPT WS-NUM1
               *> 第1オペランドの入力
               
               DISPLAY 'Enter second number: '
               ACCEPT WS-NUM2
               *> 第2オペランドの入力
               
               DISPLAY 'Enter operation (+, -, *, /): '
               ACCEPT WS-OPERATION
               *> 演算子の入力
               
               EVALUATE WS-OPERATION
               *> 演算子による分岐処理
                   WHEN '+'
                       COMPUTE WS-RESULT = WS-NUM1 + WS-NUM2
                       DISPLAY 'Addition: '
                       *> 加算処理
                   WHEN '-'
                       COMPUTE WS-RESULT = WS-NUM1 - WS-NUM2
                       DISPLAY 'Subtraction: '
                       *> 減算処理
                   WHEN '*'
                       COMPUTE WS-RESULT = WS-NUM1 * WS-NUM2
                       DISPLAY 'Multiplication: '
                       *> 乗算処理
                   WHEN '/'
                       IF WS-NUM2 = 0 THEN
                           DISPLAY 'Error: Division by zero!'
                           *> ゼロ除算エラー
                       ELSE
                           COMPUTE WS-RESULT = WS-NUM1 / WS-NUM2
                           DISPLAY 'Division: '
                           *> 除算処理
                       END-IF
                   WHEN OTHER
                       DISPLAY 'Invalid operation!'
                       *> 無効な演算子
               END-EVALUATE
               
               IF WS-OPERATION = '+' OR WS-OPERATION = '-' OR 
                  WS-OPERATION = '*' OR (WS-OPERATION = '/' AND WS-NUM2 NOT = 0)
               *> 有効な演算の場合のみ結果表示
                   MOVE WS-NUM1 TO WS-DISPLAY-NUM1
                   MOVE WS-NUM2 TO WS-DISPLAY-NUM2
                   MOVE WS-RESULT TO WS-DISPLAY-RESULT
                   *> 表示用変数に値をコピー
                   DISPLAY WS-DISPLAY-NUM1 ' ' WS-OPERATION ' ' WS-DISPLAY-NUM2 
                           ' = ' WS-DISPLAY-RESULT
                   *> 計算式と結果を表示
               END-IF
               
               DISPLAY ' '
               DISPLAY 'Continue? (Y/N): '
               ACCEPT WS-CONTINUE
               DISPLAY ' '
               *> 継続確認
           END-PERFORM
           
           DISPLAY 'Thank you for using COBOL Calculator!'
           *> 終了メッセージ
           STOP RUN.
           *> プログラム終了

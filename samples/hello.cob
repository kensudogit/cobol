       IDENTIFICATION DIVISION.
       PROGRAM-ID. HELLO.
       AUTHOR. COBOL Developer.
       DATE-WRITTEN. 2024.
       *> プログラム名: Hello World
       *> 目的: COBOLの基本構文を学習するためのサンプルプログラム
       *> 機能: メッセージ表示とループ処理のデモンストレーション
       
       ENVIRONMENT DIVISION.
       *> 環境設定（このプログラムでは特別な設定は不要）
       
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       *> 作業領域の変数定義
       01 WS-MESSAGE PIC X(50) VALUE 'Hello, COBOL World!'.
       *> 表示メッセージ（50文字の文字列）
       01 WS-COUNTER PIC 9(3) VALUE 0.
       *> カウンター変数（3桁の数値、初期値0）
       
       PROCEDURE DIVISION.
       MAIN-PROCEDURE.
       *> メイン処理開始
           DISPLAY '=========================================='
           DISPLAY 'Welcome to COBOL Development Environment!'
           DISPLAY '=========================================='
           DISPLAY ' '
           *> ヘッダー表示
           
           DISPLAY WS-MESSAGE
           DISPLAY ' '
           *> メッセージ表示
           
           PERFORM VARYING WS-COUNTER FROM 1 BY 1
           UNTIL WS-COUNTER > 5
               DISPLAY 'Counter: ' WS-COUNTER
           END-PERFORM
           *> カウンターを1から5まで繰り返し表示
           
           DISPLAY ' '
           DISPLAY 'Program completed successfully!'
           *> 完了メッセージ表示
           STOP RUN.
           *> プログラム終了

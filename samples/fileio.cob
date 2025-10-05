       IDENTIFICATION DIVISION.
       PROGRAM-ID. FILEIO.
       AUTHOR. COBOL Developer.
       DATE-WRITTEN. 2024.
       *> プログラム名: File I/O
       *> 目的: ファイルの読み書き操作を学習するサンプルプログラム
       *> 機能: input.txtを読み込んでoutput.txtに書き出す
       
       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
       *> ファイル制御部の定義
           SELECT INPUT-FILE ASSIGN TO 'input.txt'
           ORGANIZATION IS LINE SEQUENTIAL.
           *> 入力ファイル（行順次編成）
           SELECT OUTPUT-FILE ASSIGN TO 'output.txt'
           ORGANIZATION IS LINE SEQUENTIAL.
           *> 出力ファイル（行順次編成）
       
       DATA DIVISION.
       FILE SECTION.
       *> ファイル定義部
       FD INPUT-FILE.
       01 INPUT-RECORD PIC X(80).
       *> 入力レコード（80文字の文字列）
       
       FD OUTPUT-FILE.
       01 OUTPUT-RECORD PIC X(80).
       *> 出力レコード（80文字の文字列）
       
       WORKING-STORAGE SECTION.
       *> 作業領域の変数定義
       01 WS-EOF-FLAG PIC X(1) VALUE 'N'.
       *> ファイル終端フラグ（N=継続、Y=終端）
       01 WS-LINE-COUNT PIC 9(3) VALUE 0.
       *> 処理行数カウンター（3桁の数値）
       01 WS-MESSAGE PIC X(80).
       *> メッセージ用変数（80文字の文字列）
       
       PROCEDURE DIVISION.
       MAIN-PROCEDURE.
       *> メイン処理開始
           DISPLAY '=========================================='
           DISPLAY 'COBOL File I/O Example'
           DISPLAY '=========================================='
           DISPLAY ' '
           *> プログラムヘッダー表示
           
           OPEN INPUT INPUT-FILE
           OPEN OUTPUT OUTPUT-FILE
           *> 入力ファイルと出力ファイルをオープン
           
           PERFORM UNTIL WS-EOF-FLAG = 'Y'
           *> ファイル終端まで繰り返し処理
               READ INPUT-FILE
               *> 入力ファイルから1行読み込み
                   AT END
                       MOVE 'Y' TO WS-EOF-FLAG
                       *> ファイル終端に達した場合
                   NOT AT END
                       ADD 1 TO WS-LINE-COUNT
                       *> 処理行数をカウント
                       MOVE INPUT-RECORD TO OUTPUT-RECORD
                       *> 入力レコードを出力レコードにコピー
                       WRITE OUTPUT-RECORD
                       *> 出力ファイルに書き込み
                       DISPLAY 'Processed: ' INPUT-RECORD
                       *> 処理した内容を画面表示
               END-READ
           END-PERFORM
           
           CLOSE INPUT-FILE
           CLOSE OUTPUT-FILE
           *> ファイルをクローズ
           
           DISPLAY ' '
           DISPLAY 'File processing complete!'
           DISPLAY 'Total lines processed: ' WS-LINE-COUNT
           DISPLAY 'Output written to output.txt'
           *> 処理完了メッセージと統計情報表示
           
           STOP RUN.
           *> プログラム終了

       IDENTIFICATION DIVISION.
       PROGRAM-ID. EMPLOYEE.
       AUTHOR. COBOL Developer.
       DATE-WRITTEN. 2024.
       *> プログラム名: Employee Management System
       *> 目的: 従業員情報管理システム
       *> 機能: 従業員情報の登録・表示・管理
       
       ENVIRONMENT DIVISION.
       *> 環境設定（このプログラムでは特別な設定は不要）
       
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       *> 作業領域の変数定義
       01 WS-EMPLOYEE-RECORD.
       *> 従業員レコード構造体
           05 WS-EMP-ID PIC 9(5).
           *> 従業員ID（5桁の数値）
           05 WS-EMP-NAME PIC X(30).
           *> 従業員名（30文字の文字列）
           05 WS-EMP-DEPT PIC X(20).
           *> 部署名（20文字の文字列）
           05 WS-EMP-SALARY PIC 9(7)V99.
           *> 給与（7桁整数+2桁小数）
           05 WS-EMP-HIRE-DATE PIC X(10).
           *> 入社日（10文字の文字列）
       
       01 WS-DISPLAY-SALARY PIC ZZZ,ZZZ.99.
       *> 表示用給与（カンマ区切り形式）
       01 WS-CONTINUE PIC X(1) VALUE 'Y'.
       *> 継続フラグ（Y/N）
       01 WS-CHOICE PIC 9(1).
       *> メニュー選択（1桁の数値）
       
       PROCEDURE DIVISION.
       MAIN-PROCEDURE.
       *> メイン処理開始
           DISPLAY '=========================================='
           DISPLAY 'Employee Management System'
           DISPLAY '=========================================='
           DISPLAY ' '
           *> プログラムヘッダー表示
           
           PERFORM UNTIL WS-CONTINUE = 'N' OR WS-CONTINUE = 'n'
           *> ユーザーが'N'または'n'を入力するまで繰り返し
               DISPLAY '1. Add Employee'
               DISPLAY '2. Display Employee'
               DISPLAY '3. Exit'
               DISPLAY 'Enter your choice (1-3): '
               ACCEPT WS-CHOICE
               *> メニュー表示と選択入力
               
               EVALUATE WS-CHOICE
               *> 選択による分岐処理
                   WHEN 1
                       PERFORM ADD-EMPLOYEE
                       *> 従業員追加処理
                   WHEN 2
                       PERFORM DISPLAY-EMPLOYEE
                       *> 従業員表示処理
                   WHEN 3
                       MOVE 'N' TO WS-CONTINUE
                       *> 終了フラグ設定
                   WHEN OTHER
                       DISPLAY 'Invalid choice! Please try again.'
                       *> 無効な選択
               END-EVALUATE
               
               DISPLAY ' '
               *> 空行表示
           END-PERFORM
           
           DISPLAY 'Thank you for using Employee Management System!'
           *> 終了メッセージ
           STOP RUN.
           *> プログラム終了
       
       ADD-EMPLOYEE.
       *> 従業員追加サブルーチン
           DISPLAY 'Enter Employee ID: '
           ACCEPT WS-EMP-ID
           *> 従業員IDの入力
           
           DISPLAY 'Enter Employee Name: '
           ACCEPT WS-EMP-NAME
           *> 従業員名の入力
           
           DISPLAY 'Enter Department: '
           ACCEPT WS-EMP-DEPT
           *> 部署名の入力
           
           DISPLAY 'Enter Salary: '
           ACCEPT WS-EMP-SALARY
           *> 給与の入力
           
           DISPLAY 'Enter Hire Date (YYYY-MM-DD): '
           ACCEPT WS-EMP-HIRE-DATE
           *> 入社日の入力
           
           DISPLAY 'Employee added successfully!'
           DISPLAY ' '
           *> 追加完了メッセージ
       
       DISPLAY-EMPLOYEE.
       *> 従業員表示サブルーチン
           DISPLAY 'Employee Information:'
           DISPLAY '===================='
           *> ヘッダー表示
           DISPLAY 'ID: ' WS-EMP-ID
           *> 従業員ID表示
           DISPLAY 'Name: ' WS-EMP-NAME
           *> 従業員名表示
           DISPLAY 'Department: ' WS-EMP-DEPT
           *> 部署名表示
           MOVE WS-EMP-SALARY TO WS-DISPLAY-SALARY
           *> 給与を表示用形式に変換
           DISPLAY 'Salary: $' WS-DISPLAY-SALARY
           *> 給与表示（カンマ区切り）
           DISPLAY 'Hire Date: ' WS-EMP-HIRE-DATE
           *> 入社日表示
           DISPLAY ' '
           *> 空行表示

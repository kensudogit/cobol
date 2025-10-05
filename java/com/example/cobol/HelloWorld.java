/**
 * Hello World プログラム
 * COBOLプログラム hello.cob をJavaに移植
 * 
 * 目的: COBOLの基本構文を学習するためのサンプルプログラム
 * 機能: メッセージ表示とループ処理のデモンストレーション
 */
public class HelloWorld {

    /**
     * メイン処理
     * COBOLのMAIN-PROCEDUREに対応
     */
    public static void main(String[] args) {
        // プログラムヘッダー表示
        // COBOLのDISPLAY文に対応
        System.out.println("==========================================");
        System.out.println("Welcome to COBOL Development Environment!");
        System.out.println("==========================================");
        System.out.println();

        // メッセージ表示（COBOLのWS-MESSAGEに対応）
        String message = "Hello, COBOL World!"; // WS-MESSAGE PIC X(50)
        System.out.println(message);
        System.out.println();

        // カウンターを1から5まで繰り返し表示
        // COBOLのPERFORM VARYING文に対応:
        // PERFORM VARYING WS-COUNTER FROM 1 BY 1 UNTIL WS-COUNTER > 5
        for (int counter = 1; counter <= 5; counter++) {
            // WS-COUNTER PIC 9(3) - 3桁の数値
            System.out.println("Counter: " + counter);
        }

        System.out.println();
        System.out.println("Program completed successfully!");
    }
}

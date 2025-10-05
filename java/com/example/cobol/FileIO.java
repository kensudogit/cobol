import java.io.BufferedReader;
import java.io.BufferedWriter;
import java.io.FileReader;
import java.io.FileWriter;
import java.io.IOException;

/**
 * ファイル入出力プログラム
 * COBOLプログラム fileio.cob をJavaに移植
 * 
 * 目的: ファイルの読み書き操作を学習するサンプルプログラム
 * 機能: input.txtを読み込んでoutput.txtに書き出す
 */
public class FileIO {

    private static final String INPUT_FILE = "input.txt";
    private static final String OUTPUT_FILE = "output.txt";

    // COBOLの作業領域変数に対応
    private static boolean eofFlag = false; // WS-EOF-FLAG PIC X(1) VALUE 'N'
    private static int lineCount = 0; // WS-LINE-COUNT PIC 9(3) VALUE 0

    /**
     * メイン処理
     * COBOLのMAIN-PROCEDUREに対応
     */
    public static void main(String[] args) {
        // プログラムヘッダー表示
        // COBOLのDISPLAY文に対応
        System.out.println("===========================================");
        System.out.println("COBOL File I/O Example");
        System.out.println("===========================================");
        System.out.println();

        BufferedReader inputBuffer = null;
        BufferedWriter outputBuffer = null;

        try {
            // 入力ファイルと出力ファイルをオープン
            // COBOLのOPEN INPUT/OUTPUT文に対応
            inputBuffer = new BufferedReader(new FileReader(INPUT_FILE));
            outputBuffer = new BufferedWriter(new FileWriter(OUTPUT_FILE));

            String line;
            // ファイル終端まで繰り返し処理
            // COBOLのPERFORM UNTIL WS-EOF-FLAG = 'Y'に対応
            while ((line = inputBuffer.readLine()) != null) {
                // 処理行数をカウント（COBOLのADD 1 TO WS-LINE-COUNTに対応）
                lineCount++;

                // 出力ファイルに書き込み
                // COBOLのWRITE OUTPUT-RECORDに対応
                outputBuffer.write(line);
                outputBuffer.newLine();

                // 処理した内容を画面表示
                // COBOLのDISPLAY 'Processed: ' INPUT-RECORDに対応
                System.out.println("Processed: " + line);
            }

            // ファイルをクローズ
            // COBOLのCLOSE INPUT-FILE/OUTPUT-FILEに対応
            inputBuffer.close();
            outputBuffer.close();

            // 処理完了メッセージと統計情報表示
            System.out.println();
            System.out.println("File processing complete!");
            System.out.println("Total lines processed: " + lineCount);
            System.out.println("Output written to " + OUTPUT_FILE);

        } catch (IOException e) {
            System.err.println("File processing error: " + e.getMessage());
            e.printStackTrace();
        } finally {
            // リソースのクリーンアップ
            try {
                if (inputBuffer != null)
                    inputBuffer.close();
                if (outputBuffer != null)
                    outputBuffer.close();
            } catch (IOException e) {
                System.err.println("Error closing files: " + e.getMessage());
            }
        }
    }

    /**
     * ファイル処理の実行
     * COBOLのファイル処理ロジックをJava風に実装
     */
    private static void processFile() {
        try (BufferedReader reader = new BufferedReader(new FileReader(INPUT_FILE));
                BufferedWriter writer = new BufferedWriter(new FileWriter(OUTPUT_FILE))) {

            String line;
            while ((line = reader.readLine()) != null) {
                lineCount++;
                writer.write(line);
                writer.newLine();
                System.out.println("Processed: " + line);
            }

        } catch (IOException e) {
            System.err.println("File processing error: " + e.getMessage());
            e.printStackTrace();
        }
    }
}

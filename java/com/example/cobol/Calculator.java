import java.util.Scanner;
import java.math.BigDecimal;

/**
 * 四則演算を行う電卓プログラム
 * COBOLプログラム calculator.cob をJavaに移植
 * 
 * 機能: ユーザー入力による加算・減算・乗算・除算の実行
 */
public class Calculator {

    private static Scanner scanner = new Scanner(System.in);

    /**
     * メイン処理
     * COBOLのMAIN-PROCEDUREに対応
     */
    public static void main(String[] args) {
        displayHeader();

        boolean continueFlag = true;

        while (continueFlag) {
            // 第1オペランドの入力
            System.out.print("Enter first number: ");
            BigDecimal num1 = getDecimalInput();

            // 第2オペランドの入力
            System.out.print("Enter second number: ");
            BigDecimal num2 = getDecimalInput();

            // 演算子の入力
            System.out.print("Enter operation (+, -, *, /): ");
            String operation = scanner.nextLine().trim();

            // 演算実行
            BigDecimal result = performCalculation(num1, num2, operation);

            // 結果表示（有効な演算の場合のみ）
            displayResult(num1, num2, operation, result);

            // 継続確認
            System.out.println();
            System.out.print("Continue? (Y/N): ");
            String continueInput = scanner.nextLine().trim().toLowerCase();
            continueFlag = continueInput.equals("y") || continueInput.equals("yes");

            System.out.println();
        }

        System.out.println("Thank you for using COBOL Calculator!");
    }

    /**
     * プログラムヘッダー表示
     * COBOLのDISPLAY文に対応
     */
    private static void displayHeader() {
        System.out.println("===========================================");
        System.out.println("COBOL Calculator Program");
        System.out.println("/===========================================");
        System.out.println();
    }

    /**
     * 数値入力処理
     * COBOLのACCEPT文に対応
     */
    private static BigDecimal getDecimalInput() {
        while (true) {
            try {
                String input = scanner.nextLine().trim();
                return new BigDecimal(input);
            } catch (NumberFormatException e) {
                System.out.print("Invalid number format. Please enter again: ");
            }
        }
    }

    /**
     * 演算実行処理
     * COBOLのEVALUATE文に対応
     */
    private static BigDecimal performCalculation(BigDecimal num1, BigDecimal num2, String operation) {
        switch (operation) {
            case "+":
                System.out.println("Addition: ");
                return num1.add(num2);

            case "-":
                System.out.println("Subtraction: ");
                return num1.subtract(num2);

            case "*":
                System.out.println("Multiplication: ");
                return num1.multiply(num2);

            case "/":
                if (num2.compareTo(BigDecimal.ZERO) == 0) {
                    System.out.println("Error: Division by zero!");
                    return null; // エラーを示す
                } else {
                    System.out.println("Division: ");
                    return num1.divide(num2, 2, BigDecimal.ROUND_HALF_UP); // 小数点第2位まで
                }

            default:
                System.out.println("Invalid operation!");
                return null; // エラーを示す
        }
    }

    /**
     * 計算結果表示
     * COBOLの表示用変数処理に対応
     */
    private static void displayResult(BigDecimal num1, BigDecimal num2, String operation, BigDecimal result) {
        // 有効な演算の場合のみ結果表示
        // COBOLのIF文条件に対応: WS-OPERATION = '+' OR WS-OPERATION = '-' OR
        // WS-OPERATION = '*' OR (WS-OPERATION = '/' AND WS-NUM2 NOT = 0)
        boolean isValidOperation = ("+".equals(operation) || "-".equals(operation) || "*".equals(operation) ||
                ("/".equals(operation) && num2.compareTo(BigDecimal.ZERO) != 0));

        if (isValidOperation && result != null) {
            // 表示用数値形式（COBOLのWS-DISPLAY-NUM1, WS-DISPLAY-NUM2, WS-DISPLAY-RESULTに対応）
            String displayNum1 = formatNumber(num1);
            String displayNum2 = formatNumber(num2);
            String displayResult = formatNumber(result);

            System.out.println(displayNum1 + " " + operation + " " + displayNum2 + " = " + displayResult);
        }
    }

    /**
     * 数値の表示用フォーマット
     * COBOLのZ形式（ゼロサプレス）に対応
     */
    private static String formatNumber(BigDecimal number) {
        // ゼロサプレス形式（12345.00 -> 12345.00, 00000.50 -> 0.50）
        return number.toPlainString();
    }
}

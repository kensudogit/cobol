package com.example.cobol;

import java.io.File;
import java.io.IOException;
import java.nio.file.Files;
import java.nio.file.Paths;

/**
 * COBOL Comprehensive Sample Program Java Runner
 * ランナークラス：サンプルデータ作成とプログラム実行
 */
public class ComprehensiveSampleRunner {

    public static void main(String[] args) {
        System.out.println("=====================================");
        System.out.println("COBOL Comprehensive Sample Runner");
        System.out.println("Java Implementation Startup");
        System.out.println("=====================================");
        System.out.println();

        ComprehensiveSampleRunner runner = new ComprehensiveSampleRunner();

        try {
            runner.createSampleData();
            runner.runProgram();
        } catch (Exception e) {
            System.err.println("Runner execution failed: " + e.getMessage());
            e.printStackTrace();
        }
    }

    private void createSampleData() {
        System.out.println("Creating sample data files...");

        // 顧客マスタファイルの作成
        createCustomerMasterFile();

        // 売上トランザクションファイルの作成
        createSalesTransactionFile();

        System.out.println("Sample data files created successfully");
        System.out.println();
    }

    private void createCustomerMasterFile() {
        System.out.println("Creating customer master file...");

        String customerData = String.join("\n",
                // 顧客1（固定幅形式：COBOLレコード形式）
                String.format("%-7d%-30s%-40s%-20s%-2s%-10s%-15s%-9s%-1s%-8s%-20s",
                        1234567, "John Smith", "123 Main Street", "Anytown", "CA", "90210",
                        "12345678901", "00100000", "A", "20250128", ""),

                // 顧客2
                String.format("%-7d%-30s%-40s%-20s%-2s%-10s%-15s%-9s%-1s%-8s%-20s",
                        1234568, "Jane Doe", "456 Oak Avenue", "Seattle", "WA", "98101",
                        "12345678902", "00200000", "A", "20250128", ""),

                // 顧客3
                String.format("%-7d%-30s%-40s%-20s%-2s%-10s%-15s%-9s%-1s%-8s%-20s",
                        1234569, "Bob Johnson", "789 Pine Street", "Boston", "MA", "02101",
                        "12345678903", "00050000", "I", "20250128", ""));

        writeToFile("CUSTOMER.MAST", customerData);
        System.out.println("✓ Customer master file created");
    }

    private void createSalesTransactionFile() {
        System.out.println("Creating sales transaction file...");

        String salesData = String.join("",
                // トランザクション1（固定幅なし：連続形式）
                String.format("%-8d%-7d%-10s%-5d%-8s%-9s%-8s%-4d",
                        1, 1234567, "PROD-001", 100, "10.00", "1000.00", "20250128", 1001),

                // トランザクション2
                String.format("%-8d%-7d%-10s%-5d%-8s%-9s%-8s%-4d",
                        2, 1234568, "PROD-002", 50, "50.00", "2500.00", "20250128", 1002),

                // トランザクション3
                String.format("%-8d%-7d%-10s%-5d%-8s%-9s%-8s%-4d",
                        3, 1234568, "PROD-003", 15, "15.00", "225.00", "20250128", 1003),

                // トランザクション4
                String.format("%-8d%-7d%-10s%-5d%-8s%-9s%-8s%-4d",
                        4, 1234567, "PROD-004", 75, "25.00", "1875.00", "20250128", 1004),

                // トランザクション5
                String.format("%-8d%-7d%-10s%-5d%-8s%-9s%-8s%-4d",
                        5, 1234569, "PROD-005", 200, "5.50", "1100.00", "20250128", 1005));

        writeToFile("SALES.TRAN", salesData);
        System.out.println("✓ Sales transaction file created");
    }

    private void writeToFile(String filename, String content) {
        try {
            Files.write(Paths.get(filename), content.getBytes());
        } catch (IOException e) {
            System.err.println("Error creating file " + filename + ": " + e.getMessage());
        }
    }

    private void runProgram() {
        System.out.println("Running Comprehensive Sample Program...");
        System.out.println();

        // ComprehensiveSampleプログラムを実行
        ComprehensiveSample.main(new String[0]);

        System.out.println();
        System.out.println("Program execution completed!");
        System.out.println("Check generated files:");
        System.out.println("  - DAILY-REPORT.TXT (Report output)");
        System.out.println("  - lambda.log (Program log)");

        showGeneratedReport();
    }

    private void showGeneratedReport() {
        System.out.println();
        System.out.println("=====================================");
        System.out.println("GENERATED REPORT PREVIEW");
        System.out.println("=====================================");

        try {
            String reportContent = readFile("DAILY-REPORT.TXT");
            System.out.println(reportContent);
        } catch (IOException e) {
            System.out.println("Report file not found or could not be read");
        }

        System.out.println("=====================================");
    }

    private String readFile(String filename) throws IOException {
        return new String(Files.readAllBytes(Paths.get(filename)));
    }

    /**
     * ファイル一覧を表示するユーティリティメソッド
     */
    public static void listProjectFiles() {
        System.out.println("Project Files:");
        System.out.println("==============");

        File currentDir = new File(".");

        // Javaソースファイル
        System.out.println("Java Source Files:");
        File[] javaFiles = currentDir.listFiles((dir, name) -> name.endsWith(".java"));
        if (javaFiles != null) {
            for (File file : javaFiles) {
                System.out.println("  " + file.getName());
            }
        }

        // COBOLソースファイル
        System.out.println("\nCOBOL Source Files:");
        File[] cobFiles = currentDir.listFiles((dir, name) -> name.endsWith(".cob"));
        if (cobFiles != null) {
            for (File file : cobFiles) {
                System.out.println("  " + file.getName());
            }
        }

        // データファイル
        System.out.println("\nData Files:");
        File[] dataFiles = currentDir
                .listFiles((dir, name) -> name.equals("CUSTOMER.MAST") || name.equals("SALES.TRAN"));
        if (dataFiles != null) {
            for (File file : dataFiles) {
                System.out.println("  " + file.getName() + " (" + file.length() + " bytes)");
            }
        }

        // 出力ファイル
        System.out.println("\nOutput Files:");
        File[] outputFiles = currentDir
                .listFiles((dir, name) -> name.equals("DAILY-REPORT.TXT") || name.equals("lambda.log"));
        if (outputFiles != null) {
            for (File file : outputFiles) {
                System.out.println("  " + file.getName() + " (" + file.length() + " bytes)");
            }
        } else {
            System.out.println("  No output files found");
        }
    }
}

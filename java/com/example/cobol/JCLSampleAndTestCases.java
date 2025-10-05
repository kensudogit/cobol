package com.example.cobol;

import java.io.File;
import java.io.FileWriter;
import java.io.IOException;
import java.nio.charset.StandardCharsets;
import java.util.ArrayList;
import java.util.List;

/**
 * JCLサンプルとテストケースの管理クラス
 * 
 * このクラスは、様々なJCLステートメントのサンプルと
 * それに対応するテストケースを提供します。
 */
public class JCLSampleAndTestCases {

    private static final String JCL_SAMPLES_DIR = "jcl_samples";
    private static final String TEST_CASES_DIR = "test_cases";

    /**
     * メイン処理
     */
    public static void main(String[] args) {
        System.out.println("===========================================");
        System.out.println("JCL Samples and Test Cases Generator");
        System.out.println("===========================================");
        System.out.println();

        try {
            // ディレクトリを作成
            createDirectories();

            // JCLサンプルを生成
            generateJCLSamples();

            // テストケースを生成
            generateTestCases();

            // サンプルを実行
            runSamples();

        } catch (Exception e) {
            System.err.println("Error: " + e.getMessage());
            e.printStackTrace();
        }
    }

    /**
     * ディレクトリを作成
     */
    private static void createDirectories() {
        File jclDir = new File(JCL_SAMPLES_DIR);
        File testDir = new File(TEST_CASES_DIR);

        if (!jclDir.exists()) {
            jclDir.mkdirs();
        }

        if (!testDir.exists()) {
            testDir.mkdirs();
        }
    }

    /**
     * JCLサンプルを生成
     */
    private static void generateJCLSamples() {
        System.out.println("=== Generating JCL Samples ===");

        // 固定長レコードのJCLサンプル
        generateFixedLengthJCLSample();

        // 可変長レコードのJCLサンプル
        generateVariableLengthJCLSample();

        // ブロック化レコードのJCLサンプル
        generateBlockedJCLSample();

        // 複合レコード形式のJCLサンプル
        generateComplexJCLSample();

        // キー付きレコードのJCLサンプル
        generateKeyedJCLSample();

        System.out.println("JCL samples generated successfully!");
        System.out.println();
    }

    /**
     * 固定長レコードのJCLサンプルを生成
     */
    private static void generateFixedLengthJCLSample() {
        String jclContent = "//FIXEDLEN JOB (ACCT),'FIXED LENGTH RECORDS',CLASS=A,MSGCLASS=X\n" +
                "//STEP1    EXEC PGM=YOURPROG\n" +
                "//INPUT    DD DSN=YOUR.DATASET.INPUT,DISP=SHR,\n" +
                "//            DCB=(RECFM=F,LRECL=80,DSORG=PS)\n" +
                "//OUTPUT   DD DSN=YOUR.DATASET.OUTPUT,DISP=(NEW,CATLG),\n" +
                "//            DCB=(RECFM=F,LRECL=80,DSORG=PS),\n" +
                "//            SPACE=(TRK,(10,5))\n" +
                "//SYSPRINT DD SYSOUT=*\n" +
                "//SYSUDUMP DD SYSOUT=*\n" +
                "/*\n";

        writeToFile(JCL_SAMPLES_DIR + File.separator + "fixed_length.jcl", jclContent);
        System.out.println("Generated: fixed_length.jcl");
    }

    /**
     * 可変長レコードのJCLサンプルを生成
     */
    private static void generateVariableLengthJCLSample() {
        String jclContent = "//VARLEN   JOB (ACCT),'VARIABLE LENGTH RECORDS',CLASS=A,MSGCLASS=X\n" +
                "//STEP1    EXEC PGM=YOURPROG\n" +
                "//INPUT    DD DSN=YOUR.DATASET.INPUT,DISP=SHR,\n" +
                "//            DCB=(RECFM=V,LRECL=255,DSORG=PS)\n" +
                "//OUTPUT   DD DSN=YOUR.DATASET.OUTPUT,DISP=(NEW,CATLG),\n" +
                "//            DCB=(RECFM=V,LRECL=255,DSORG=PS),\n" +
                "//            SPACE=(TRK,(10,5))\n" +
                "//SYSPRINT DD SYSOUT=*\n" +
                "//SYSUDUMP DD SYSOUT=*\n" +
                "/*\n";

        writeToFile(JCL_SAMPLES_DIR + File.separator + "variable_length.jcl", jclContent);
        System.out.println("Generated: variable_length.jcl");
    }

    /**
     * ブロック化レコードのJCLサンプルを生成
     */
    private static void generateBlockedJCLSample() {
        String jclContent = "//BLOCKED  JOB (ACCT),'BLOCKED RECORDS',CLASS=A,MSGCLASS=X\n" +
                "//STEP1    EXEC PGM=YOURPROG\n" +
                "//INPUT    DD DSN=YOUR.DATASET.INPUT,DISP=SHR,\n" +
                "//            DCB=(RECFM=FB,LRECL=80,BLKSIZE=800,DSORG=PS)\n" +
                "//OUTPUT   DD DSN=YOUR.DATASET.OUTPUT,DISP=(NEW,CATLG),\n" +
                "//            DCB=(RECFM=FB,LRECL=80,BLKSIZE=800,DSORG=PS),\n" +
                "//            SPACE=(TRK,(10,5))\n" +
                "//SYSPRINT DD SYSOUT=*\n" +
                "//SYSUDUMP DD SYSOUT=*\n" +
                "/*\n";

        writeToFile(JCL_SAMPLES_DIR + File.separator + "blocked_records.jcl", jclContent);
        System.out.println("Generated: blocked_records.jcl");
    }

    /**
     * 複合レコード形式のJCLサンプルを生成
     */
    private static void generateComplexJCLSample() {
        String jclContent = "//COMPLEX  JOB (ACCT),'COMPLEX RECORD FORMATS',CLASS=A,MSGCLASS=X\n" +
                "//STEP1    EXEC PGM=YOURPROG\n" +
                "//INPUT    DD DSN=YOUR.DATASET.INPUT,DISP=SHR,\n" +
                "//            DCB=(RECFM=FB,LRECL=100,BLKSIZE=1000,DSORG=PS,\n" +
                "//            OPTCD=Q,BUFNO=3)\n" +
                "//OUTPUT   DD DSN=YOUR.DATASET.OUTPUT,DISP=(NEW,CATLG),\n" +
                "//            DCB=(RECFM=FB,LRECL=100,BLKSIZE=1000,DSORG=PS,\n" +
                "//            OPTCD=Q,BUFNO=3),\n" +
                "//            SPACE=(TRK,(10,5))\n" +
                "//SYSPRINT DD SYSOUT=*\n" +
                "//SYSUDUMP DD SYSOUT=*\n" +
                "/*\n";

        writeToFile(JCL_SAMPLES_DIR + File.separator + "complex_format.jcl", jclContent);
        System.out.println("Generated: complex_format.jcl");
    }

    /**
     * キー付きレコードのJCLサンプルを生成
     */
    private static void generateKeyedJCLSample() {
        String jclContent = "//KEYED    JOB (ACCT),'KEYED RECORDS',CLASS=A,MSGCLASS=X\n" +
                "//STEP1    EXEC PGM=YOURPROG\n" +
                "//INPUT    DD DSN=YOUR.DATASET.INPUT,DISP=SHR,\n" +
                "//            DCB=(RECFM=FB,LRECL=100,BLKSIZE=1000,DSORG=PS,\n" +
                "//            KEYLEN=10,KEYOFF=0)\n" +
                "//OUTPUT   DD DSN=YOUR.DATASET.OUTPUT,DISP=(NEW,CATLG),\n" +
                "//            DCB=(RECFM=FB,LRECL=100,BLKSIZE=1000,DSORG=PS,\n" +
                "//            KEYLEN=10,KEYOFF=0),\n" +
                "//            SPACE=(TRK,(10,5))\n" +
                "//SYSPRINT DD SYSOUT=*\n" +
                "//SYSUDUMP DD SYSOUT=*\n" +
                "/*\n";

        writeToFile(JCL_SAMPLES_DIR + File.separator + "keyed_records.jcl", jclContent);
        System.out.println("Generated: keyed_records.jcl");
    }

    /**
     * テストケースを生成
     */
    private static void generateTestCases() {
        System.out.println("=== Generating Test Cases ===");

        // 固定長レコードのテストケース
        generateFixedLengthTestCases();

        // 可変長レコードのテストケース
        generateVariableLengthTestCases();

        // ブロック化レコードのテストケース
        generateBlockedTestCases();

        // エラーハンドリングのテストケース
        generateErrorHandlingTestCases();

        System.out.println("Test cases generated successfully!");
        System.out.println();
    }

    /**
     * 固定長レコードのテストケースを生成
     */
    private static void generateFixedLengthTestCases() {
        String testContent = "=== Fixed Length Records Test Cases ===\n\n" +
                "Test Case 1: Basic Fixed Length Record\n" +
                "Description: Test basic fixed length record reading and writing\n" +
                "Input: 80-character fixed length records\n" +
                "Expected: All records should be read and written correctly\n\n" +
                "Test Case 2: Short Record Padding\n" +
                "Description: Test padding of short records with spaces\n" +
                "Input: Records shorter than 80 characters\n" +
                "Expected: Records should be padded with spaces to 80 characters\n\n" +
                "Test Case 3: Long Record Truncation\n" +
                "Description: Test truncation of records longer than 80 characters\n" +
                "Input: Records longer than 80 characters\n" +
                "Expected: Records should be truncated to 80 characters\n\n";

        writeToFile(TEST_CASES_DIR + File.separator + "fixed_length_test_cases.txt", testContent);
        System.out.println("Generated: fixed_length_test_cases.txt");
    }

    /**
     * 可変長レコードのテストケースを生成
     */
    private static void generateVariableLengthTestCases() {
        String testContent = "=== Variable Length Records Test Cases ===\n\n" +
                "Test Case 1: Basic Variable Length Record\n" +
                "Description: Test basic variable length record reading and writing\n" +
                "Input: Variable length records with length prefix\n" +
                "Expected: All records should be read and written correctly\n\n" +
                "Test Case 2: Maximum Length Record\n" +
                "Description: Test maximum length variable record\n" +
                "Input: Record with maximum allowed length (255 characters)\n" +
                "Expected: Record should be handled correctly\n\n" +
                "Test Case 3: Empty Record\n" +
                "Description: Test empty variable length record\n" +
                "Input: Record with zero length\n" +
                "Expected: Empty record should be handled correctly\n\n";

        writeToFile(TEST_CASES_DIR + File.separator + "variable_length_test_cases.txt", testContent);
        System.out.println("Generated: variable_length_test_cases.txt");
    }

    /**
     * ブロック化レコードのテストケースを生成
     */
    private static void generateBlockedTestCases() {
        String testContent = "=== Blocked Records Test Cases ===\n\n" +
                "Test Case 1: Basic Blocked Record\n" +
                "Description: Test basic blocked record reading and writing\n" +
                "Input: Multiple records in blocks\n" +
                "Expected: All records should be read and written correctly\n\n" +
                "Test Case 2: Partial Block\n" +
                "Description: Test partial block handling\n" +
                "Input: Block that doesn't contain full number of records\n" +
                "Expected: Partial block should be handled correctly\n\n" +
                "Test Case 3: Block Flushing\n" +
                "Description: Test block flushing when closing file\n" +
                "Input: Records that don't fill complete block\n" +
                "Expected: Partial block should be flushed to file\n\n";

        writeToFile(TEST_CASES_DIR + File.separator + "blocked_test_cases.txt", testContent);
        System.out.println("Generated: blocked_test_cases.txt");
    }

    /**
     * エラーハンドリングのテストケースを生成
     */
    private static void generateErrorHandlingTestCases() {
        String testContent = "=== Error Handling Test Cases ===\n\n" +
                "Test Case 1: File Not Found\n" +
                "Description: Test handling of non-existent files\n" +
                "Input: Non-existent file path\n" +
                "Expected: Appropriate error code should be returned\n\n" +
                "Test Case 2: Access Denied\n" +
                "Description: Test handling of access denied errors\n" +
                "Input: File with restricted access\n" +
                "Expected: Appropriate error code should be returned\n\n" +
                "Test Case 3: Invalid Record Format\n" +
                "Description: Test handling of invalid record formats\n" +
                "Input: Invalid RECFM parameter\n" +
                "Expected: Appropriate error code should be returned\n\n" +
                "Test Case 4: Invalid DCB Parameters\n" +
                "Description: Test handling of invalid DCB parameters\n" +
                "Input: Invalid DCB parameter values\n" +
                "Expected: Appropriate error code should be returned\n\n";

        writeToFile(TEST_CASES_DIR + File.separator + "error_handling_test_cases.txt", testContent);
        System.out.println("Generated: error_handling_test_cases.txt");
    }

    /**
     * サンプルを実行
     */
    private static void runSamples() {
        System.out.println("=== Running Samples ===");

        // JCLサンプルを解析してテスト
        testJCLParsing();

        // ファイルアクセス制御をテスト
        testFileAccessControl();

        System.out.println("Samples executed successfully!");
        System.out.println();
    }

    /**
     * JCL解析をテスト
     */
    private static void testJCLParsing() {
        System.out.println("Testing JCL parsing...");

        String[] jclStatements = {
                "//INPUT DD DSN=TEST.FIXED,DISP=SHR,DCB=(RECFM=F,LRECL=80,DSORG=PS)",
                "//OUTPUT DD DSN=TEST.VARIABLE,DISP=(NEW,CATLG),DCB=(RECFM=V,LRECL=255,DSORG=PS)",
                "//BLOCKED DD DSN=TEST.BLOCKED,DISP=SHR,DCB=(RECFM=FB,LRECL=80,BLKSIZE=800,DSORG=PS)",
                "//COMPLEX DD DSN=TEST.COMPLEX,DISP=SHR,DCB=(RECFM=FB,LRECL=100,BLKSIZE=1000,DSORG=PS,OPTCD=Q,BUFNO=3)",
                "//KEYED DD DSN=TEST.KEYED,DISP=SHR,DCB=(RECFM=FB,LRECL=100,BLKSIZE=1000,DSORG=PS,KEYLEN=10,KEYOFF=0)"
        };

        for (String jclStatement : jclStatements) {
            JCLDCBInfo dcbInfo = JCLDCBInfo.parseFromJCL(jclStatement);
            System.out.println("Parsed JCL: " + jclStatement);
            System.out.println("DD Name: " + dcbInfo.getDdName());
            System.out.println("Dataset Name: " + dcbInfo.getDatasetName());
            System.out.println("Record Format: " + dcbInfo.getRecordFormat());
            System.out.println("Logical Record Length: " + dcbInfo.getLogicalRecordLength());
            System.out.println("Block Size: " + dcbInfo.getBlockSize());
            System.out.println();
        }
    }

    /**
     * ファイルアクセス制御をテスト
     */
    private static void testFileAccessControl() {
        System.out.println("Testing file access control...");

        // 固定長レコードのテスト
        JCLDCBInfo fixedDcbInfo = new JCLDCBInfo("TEST", "TEST.FIXED");
        fixedDcbInfo.setDCBParameter("RECFM", "F");
        fixedDcbInfo.setDCBParameter("LRECL", "80");
        fixedDcbInfo.setDCBParameter("DSORG", "PS");

        JCLFileAccessController controller = new JCLFileAccessController(fixedDcbInfo);

        String testFile = TEST_CASES_DIR + File.separator + "test_fixed.txt";
        int result = controller.openFile(testFile, "rw");

        if (result == JCLFileAccessController.SUCCESS) {
            System.out.println("File opened successfully: " + testFile);

            // テストレコードを書き込み
            String[] testRecords = {
                    "This is a test record for fixed length format",
                    "Another test record to verify the functionality",
                    "Third test record to ensure proper handling"
            };

            for (String record : testRecords) {
                byte[] recordBytes = record.getBytes(StandardCharsets.UTF_8);
                result = controller.writeRecord(recordBytes);

                if (result == JCLFileAccessController.SUCCESS) {
                    System.out.println("Record written: " + record);
                } else {
                    System.err.println("Write error: " + JCLFileAccessController.getErrorMessage(result));
                }
            }

            controller.closeFile();

            // ファイルを読み取り
            result = controller.openFile(testFile, "r");

            if (result == JCLFileAccessController.SUCCESS) {
                System.out.println("Reading records from file:");

                byte[] recordBuffer = new byte[80];
                int recordCount = 0;

                while ((result = controller.readRecord(recordBuffer)) == JCLFileAccessController.SUCCESS) {
                    recordCount++;
                    String record = new String(recordBuffer, StandardCharsets.UTF_8).trim();
                    System.out.println("Record " + recordCount + ": " + record);
                }

                if (result == JCLFileAccessController.END_OF_FILE) {
                    System.out.println("End of file reached. Total records: " + recordCount);
                } else {
                    System.err.println("Read error: " + JCLFileAccessController.getErrorMessage(result));
                }

                controller.closeFile();
            }
        } else {
            System.err.println("Failed to open file: " + JCLFileAccessController.getErrorMessage(result));
        }

        System.out.println();
    }

    /**
     * ファイルに内容を書き込み
     * 
     * @param fileName ファイル名
     * @param content  内容
     */
    private static void writeToFile(String fileName, String content) {
        try (FileWriter writer = new FileWriter(fileName)) {
            writer.write(content);
        } catch (IOException e) {
            System.err.println("Error writing to file " + fileName + ": " + e.getMessage());
        }
    }
}

package com.example.cobol;

import java.io.File;
import java.io.IOException;
import java.nio.charset.StandardCharsets;
import java.util.List;

/**
 * JCLのDCB情報を使用したファイルアクセス制御のサンプルプログラム
 * 
 * このクラスは、様々なレコード形式（固定長、可変長、ブロック化）のファイルに
 * アクセスし、データの読み書き制御処理を実演します。
 */
public class JCLFileAccessSample {

    private static final String TEST_DIR = "test_data";
    private static final String FIXED_LENGTH_FILE = "fixed_length.txt";
    private static final String VARIABLE_LENGTH_FILE = "variable_length.txt";
    private static final String BLOCKED_FILE = "blocked_file.txt";

    // 定数定義
    private static final String DD_NAME_INPUT = "INPUT";
    private static final String PARAM_RECFM = "RECFM";
    private static final String PARAM_LRECL = "LRECL";
    private static final String PARAM_DSORG = "DSORG";
    private static final String MSG_FILE_OPENED = "File opened successfully: ";
    private static final String MSG_RECORD_WRITTEN = "Record written: ";
    private static final String MSG_WRITE_ERROR = "Write error: ";
    private static final String MSG_READING_RECORDS = "\nReading records from file:";
    private static final String MSG_RECORD_PREFIX = "Record ";
    private static final String MSG_END_OF_FILE = "End of file reached. Total records: ";
    private static final String MSG_READ_ERROR = "Read error: ";
    private static final String MSG_FAILED_TO_OPEN = "Failed to open file: ";

    /**
     * メイン処理
     */
    public static void main(String[] args) {
        System.out.println("===========================================");
        System.out.println("JCL DCB File Access Control Sample");
        System.out.println("===========================================");
        System.out.println();

        try {
            // テストディレクトリを作成
            createTestDirectory();

            // 固定長レコードのテスト
            testFixedLengthRecords();

            // 可変長レコードのテスト
            testVariableLengthRecords();

            // ブロック化レコードのテスト
            testBlockedRecords();

            // 複合レコード形式のテスト
            testComplexRecordFormats();

        } catch (Exception e) {
            System.err.println("Error: " + e.getMessage());
            e.printStackTrace();
        }
    }

    /**
     * テストディレクトリを作成
     */
    private static void createTestDirectory() {
        File testDir = new File(TEST_DIR);
        if (!testDir.exists()) {
            testDir.mkdirs();
        }
    }

    /**
     * 固定長レコードのテスト
     */
    private static void testFixedLengthRecords() {
        System.out.println("=== Fixed Length Records Test ===");

        // 固定長レコードのDCB情報を作成
        JCLDCBInfo dcbInfo = new JCLDCBInfo(DD_NAME_INPUT, "TEST.FIXED");
        dcbInfo.setDCBParameter(PARAM_RECFM, "F");
        dcbInfo.setDCBParameter(PARAM_LRECL, "80");
        dcbInfo.setDCBParameter(PARAM_DSORG, "PS");

        System.out.println(dcbInfo.getDetailedInfo());

        // ファイルアクセス制御オブジェクトを作成
        JCLFileAccessController controller = new JCLFileAccessController(dcbInfo);

        // ファイルをオープン（書き込みモード）
        String filePath = TEST_DIR + File.separator + FIXED_LENGTH_FILE;
        int result = controller.openFile(filePath, "rw");

        if (result == JCLFileAccessController.SUCCESS) {
            System.out.println(MSG_FILE_OPENED + filePath);

            // 固定長レコードを書き込み
            String[] testRecords = {
                    "RECORD001: This is a fixed length record with exactly 80 characters",
                    "RECORD002: Another fixed length record for testing purposes",
                    "RECORD003: Fixed length records are padded with spaces if needed"
            };

            for (String record : testRecords) {
                byte[] recordBytes = record.getBytes(StandardCharsets.UTF_8);
                result = controller.writeRecord(recordBytes);

                if (result == JCLFileAccessController.SUCCESS) {
                    System.out.println(MSG_RECORD_WRITTEN + record);
                } else {
                    System.err.println(MSG_WRITE_ERROR + JCLFileAccessController.getErrorMessage(result));
                }
            }

            // ファイルをクローズ
            controller.closeFile();

            // ファイルを読み取りモードでオープン
            result = controller.openFile(filePath, "r");

            if (result == JCLFileAccessController.SUCCESS) {
                System.out.println(MSG_READING_RECORDS);

                byte[] recordBuffer = new byte[80];
                int recordCount = 0;

                while ((result = controller.readRecord(recordBuffer)) == JCLFileAccessController.SUCCESS) {
                    recordCount++;
                    String record = new String(recordBuffer, StandardCharsets.UTF_8).trim();
                    System.out.println(MSG_RECORD_PREFIX + recordCount + ": " + record);
                }

                if (result == JCLFileAccessController.END_OF_FILE) {
                    System.out.println(MSG_END_OF_FILE + recordCount);
                } else {
                    System.err.println(MSG_READ_ERROR + JCLFileAccessController.getErrorMessage(result));
                }

                controller.closeFile();
            }
        } else {
            System.err.println(MSG_FAILED_TO_OPEN + JCLFileAccessController.getErrorMessage(result));
        }

        System.out.println();
    }

    /**
     * 可変長レコードのテスト
     */
    private static void testVariableLengthRecords() {
        System.out.println("=== Variable Length Records Test ===");

        // 可変長レコードのDCB情報を作成
        JCLDCBInfo dcbInfo = new JCLDCBInfo(DD_NAME_INPUT, "TEST.VARIABLE");
        dcbInfo.setDCBParameter(PARAM_RECFM, "V");
        dcbInfo.setDCBParameter(PARAM_LRECL, "255");
        dcbInfo.setDCBParameter(PARAM_DSORG, "PS");

        System.out.println(dcbInfo.getDetailedInfo());

        // ファイルアクセス制御オブジェクトを作成
        JCLFileAccessController controller = new JCLFileAccessController(dcbInfo);

        // ファイルをオープン（書き込みモード）
        String filePath = TEST_DIR + File.separator + VARIABLE_LENGTH_FILE;
        int result = controller.openFile(filePath, "rw");

        if (result == JCLFileAccessController.SUCCESS) {
            System.out.println(MSG_FILE_OPENED + filePath);

            // 可変長レコードを書き込み
            String[] testRecords = {
                    "Short record",
                    "This is a medium length record",
                    "This is a very long record that demonstrates variable length record handling in JCL DCB file access control system"
            };

            for (String record : testRecords) {
                byte[] recordBytes = record.getBytes(StandardCharsets.UTF_8);
                result = controller.writeRecord(recordBytes);

                if (result == JCLFileAccessController.SUCCESS) {
                    System.out.println(MSG_RECORD_WRITTEN + record);
                } else {
                    System.err.println(MSG_WRITE_ERROR + JCLFileAccessController.getErrorMessage(result));
                }
            }

            // ファイルをクローズ
            controller.closeFile();

            // ファイルを読み取りモードでオープン
            result = controller.openFile(filePath, "r");

            if (result == JCLFileAccessController.SUCCESS) {
                System.out.println(MSG_READING_RECORDS);

                byte[] recordBuffer = new byte[255];
                int recordCount = 0;

                while ((result = controller.readRecord(recordBuffer)) == JCLFileAccessController.SUCCESS) {
                    recordCount++;
                    String record = new String(recordBuffer, StandardCharsets.UTF_8).trim();
                    System.out.println(MSG_RECORD_PREFIX + recordCount + ": " + record);
                }

                if (result == JCLFileAccessController.END_OF_FILE) {
                    System.out.println(MSG_END_OF_FILE + recordCount);
                } else {
                    System.err.println(MSG_READ_ERROR + JCLFileAccessController.getErrorMessage(result));
                }

                controller.closeFile();
            }
        } else {
            System.err.println(MSG_FAILED_TO_OPEN + JCLFileAccessController.getErrorMessage(result));
        }

        System.out.println();
    }

    /**
     * ブロック化レコードのテスト
     */
    private static void testBlockedRecords() {
        System.out.println("=== Blocked Records Test ===");

        // ブロック化レコードのDCB情報を作成
        JCLDCBInfo dcbInfo = new JCLDCBInfo(DD_NAME_INPUT, "TEST.BLOCKED");
        dcbInfo.setDCBParameter(PARAM_RECFM, "FB");
        dcbInfo.setDCBParameter(PARAM_LRECL, "80");
        dcbInfo.setDCBParameter("BLKSIZE", "800");
        dcbInfo.setDCBParameter(PARAM_DSORG, "PS");

        System.out.println(dcbInfo.getDetailedInfo());

        // ファイルアクセス制御オブジェクトを作成
        JCLFileAccessController controller = new JCLFileAccessController(dcbInfo);

        // ファイルをオープン（書き込みモード）
        String filePath = TEST_DIR + File.separator + BLOCKED_FILE;
        int result = controller.openFile(filePath, "rw");

        if (result == JCLFileAccessController.SUCCESS) {
            System.out.println(MSG_FILE_OPENED + filePath);

            // ブロック化レコードを書き込み
            String[] testRecords = {
                    "BLOCK001: This is a blocked fixed length record",
                    "BLOCK002: Blocked records improve I/O efficiency",
                    "BLOCK003: Multiple records are stored in blocks",
                    "BLOCK004: Block size is typically larger than record size",
                    "BLOCK005: This demonstrates blocked record handling",
                    "BLOCK006: JCL DCB controls block size and record format",
                    "BLOCK007: Blocked records reduce I/O operations",
                    "BLOCK008: This is the eighth record in the block",
                    "BLOCK009: Blocking improves performance significantly",
                    "BLOCK010: This is the tenth record in the block"
            };

            for (String record : testRecords) {
                byte[] recordBytes = record.getBytes(StandardCharsets.UTF_8);
                result = controller.writeRecord(recordBytes);

                if (result == JCLFileAccessController.SUCCESS) {
                    System.out.println(MSG_RECORD_WRITTEN + record);
                } else {
                    System.err.println(MSG_WRITE_ERROR + JCLFileAccessController.getErrorMessage(result));
                }
            }

            // ブロックをフラッシュ
            controller.flushBlock();

            // ファイルをクローズ
            controller.closeFile();

            // ファイルを読み取りモードでオープン
            result = controller.openFile(filePath, "r");

            if (result == JCLFileAccessController.SUCCESS) {
                System.out.println(MSG_READING_RECORDS);

                byte[] recordBuffer = new byte[80];
                int recordCount = 0;

                while ((result = controller.readRecord(recordBuffer)) == JCLFileAccessController.SUCCESS) {
                    recordCount++;
                    String record = new String(recordBuffer, StandardCharsets.UTF_8).trim();
                    System.out.println(MSG_RECORD_PREFIX + recordCount + ": " + record);
                }

                if (result == JCLFileAccessController.END_OF_FILE) {
                    System.out.println(MSG_END_OF_FILE + recordCount);
                } else {
                    System.err.println(MSG_READ_ERROR + JCLFileAccessController.getErrorMessage(result));
                }

                controller.closeFile();
            }
        } else {
            System.err.println(MSG_FAILED_TO_OPEN + JCLFileAccessController.getErrorMessage(result));
        }

        System.out.println();
    }

    /**
     * 複合レコード形式のテスト
     */
    private static void testComplexRecordFormats() {
        System.out.println("=== Complex Record Formats Test ===");

        // JCLステートメントからDCB情報を解析
        String jclStatement = "//INPUT DD DSN=TEST.COMPLEX,DISP=SHR,DCB=(RECFM=FB,LRECL=100,BLKSIZE=1000,DSORG=PS)";
        JCLDCBInfo dcbInfo = JCLDCBInfo.parseFromJCL(jclStatement);

        System.out.println("Parsed JCL Statement:");
        System.out.println(jclStatement);
        System.out.println("\nParsed DCB Information:");
        System.out.println(dcbInfo.getDetailedInfo());

        // ファイルアクセス制御オブジェクトを作成
        JCLFileAccessController controller = new JCLFileAccessController(dcbInfo);

        // ファイルをオープン（書き込みモード）
        String filePath = TEST_DIR + File.separator + "complex_format.txt";
        int result = controller.openFile(filePath, "rw");

        if (result == JCLFileAccessController.SUCCESS) {
            System.out.println(MSG_FILE_OPENED + filePath);

            // 複合レコード形式のレコードを書き込み
            String[] testRecords = {
                    "COMPLEX001: This demonstrates complex record format handling",
                    "COMPLEX002: JCL DCB provides flexible file access control",
                    "COMPLEX003: Multiple record formats can be supported"
            };

            for (String record : testRecords) {
                byte[] recordBytes = record.getBytes(StandardCharsets.UTF_8);
                result = controller.writeRecord(recordBytes);

                if (result == JCLFileAccessController.SUCCESS) {
                    System.out.println(MSG_RECORD_WRITTEN + record);
                } else {
                    System.err.println(MSG_WRITE_ERROR + JCLFileAccessController.getErrorMessage(result));
                }
            }

            // ブロックをフラッシュ
            controller.flushBlock();

            // ファイルをクローズ
            controller.closeFile();

            // ファイルを読み取りモードでオープン
            result = controller.openFile(filePath, "r");

            if (result == JCLFileAccessController.SUCCESS) {
                System.out.println(MSG_READING_RECORDS);

                byte[] recordBuffer = new byte[100];
                int recordCount = 0;

                while ((result = controller.readRecord(recordBuffer)) == JCLFileAccessController.SUCCESS) {
                    recordCount++;
                    String record = new String(recordBuffer, StandardCharsets.UTF_8).trim();
                    System.out.println(MSG_RECORD_PREFIX + recordCount + ": " + record);
                }

                if (result == JCLFileAccessController.END_OF_FILE) {
                    System.out.println(MSG_END_OF_FILE + recordCount);
                } else {
                    System.err.println(MSG_READ_ERROR + JCLFileAccessController.getErrorMessage(result));
                }

                controller.closeFile();
            }
        } else {
            System.err.println(MSG_FAILED_TO_OPEN + JCLFileAccessController.getErrorMessage(result));
        }

        System.out.println();
    }
}

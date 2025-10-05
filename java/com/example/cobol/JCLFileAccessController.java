package com.example.cobol;

import java.io.*;
import java.nio.ByteBuffer;
import java.nio.channels.FileChannel;
import java.nio.charset.StandardCharsets;
import java.util.ArrayList;
import java.util.List;

/**
 * JCLのDCB情報に基づくファイルアクセス制御クラス
 * 
 * このクラスは、JCLで指定されたDCB情報を使用して、
 * 様々なレコード形式（固定長、可変長、ブロック化）のファイルに
 * アクセスし、データの読み書き制御を行います。
 */
public class JCLFileAccessController {

    private JCLDCBInfo dcbInfo;
    private RandomAccessFile file;
    private FileChannel fileChannel;
    private boolean isOpen;
    private long currentPosition;
    private int currentBlockSize;
    private ByteBuffer blockBuffer;

    // エラーコード定数
    public static final int SUCCESS = 0;
    public static final int FILE_NOT_FOUND = 1;
    public static final int ACCESS_DENIED = 2;
    public static final int INVALID_RECORD_FORMAT = 3;
    public static final int READ_ERROR = 4;
    public static final int WRITE_ERROR = 5;
    public static final int END_OF_FILE = 6;
    public static final int INVALID_DCB_INFO = 7;

    /**
     * コンストラクタ
     * 
     * @param dcbInfo DCB情報
     */
    public JCLFileAccessController(JCLDCBInfo dcbInfo) {
        this.dcbInfo = dcbInfo;
        this.isOpen = false;
        this.currentPosition = 0;
        this.currentBlockSize = 0;
    }

    /**
     * ファイルをオープン
     * 
     * @param fileName ファイル名
     * @param mode     アクセスモード（"r" = 読み取り、"rw" = 読み書き）
     * @return エラーコード
     */
    public int openFile(String fileName, String mode) {
        try {
            file = new RandomAccessFile(fileName, mode);
            fileChannel = file.getChannel();
            isOpen = true;
            currentPosition = 0;

            // ブロック化レコードの場合、ブロックバッファを初期化
            if (dcbInfo.isBlockedRecord()) {
                currentBlockSize = dcbInfo.getBlockSize();
                blockBuffer = ByteBuffer.allocate(currentBlockSize);
            }

            return SUCCESS;
        } catch (FileNotFoundException ex) {
            return FILE_NOT_FOUND;
        } catch (SecurityException ex) {
            return ACCESS_DENIED;
        } catch (Exception ex) {
            return READ_ERROR;
        }
    }

    /**
     * ファイルをクローズ
     * 
     * @return エラーコード
     */
    public int closeFile() {
        try {
            if (file != null) {
                file.close();
            }
            if (fileChannel != null) {
                fileChannel.close();
            }
            isOpen = false;
            return SUCCESS;
        } catch (IOException ex) {
            return READ_ERROR;
        }
    }

    /**
     * レコードを読み取り
     * 
     * @param record 読み取ったレコードを格納するバッファ
     * @return エラーコード
     */
    public int readRecord(byte[] record) {
        if (!isOpen) {
            return READ_ERROR;
        }

        try {
            if (dcbInfo.isFixedLengthRecord()) {
                return readFixedLengthRecord(record);
            } else if (dcbInfo.isVariableLengthRecord()) {
                return readVariableLengthRecord(record);
            } else {
                return INVALID_RECORD_FORMAT;
            }
        } catch (IOException ex) {
            return READ_ERROR;
        }
    }

    /**
     * 固定長レコードを読み取り
     * 
     * @param record 読み取ったレコードを格納するバッファ
     * @return エラーコード
     */
    private int readFixedLengthRecord(byte[] record) throws IOException {
        int recordLength = dcbInfo.getLogicalRecordLength();

        if (dcbInfo.isBlockedRecord()) {
            return readBlockedFixedLengthRecord(record, recordLength);
        } else {
            return readUnblockedFixedLengthRecord(record, recordLength);
        }
    }

    /**
     * ブロック化固定長レコードを読み取り
     * 
     * @param record       読み取ったレコードを格納するバッファ
     * @param recordLength レコード長
     * @return エラーコード
     */
    private int readBlockedFixedLengthRecord(byte[] record, int recordLength) throws IOException {
        // ブロックバッファが空の場合、新しいブロックを読み取り
        if (blockBuffer.remaining() < recordLength) {
            int bytesRead = fileChannel.read(blockBuffer);
            if (bytesRead == -1) {
                return END_OF_FILE;
            }
            blockBuffer.flip();
        }

        // レコードをブロックバッファから読み取り
        if (blockBuffer.remaining() >= recordLength) {
            blockBuffer.get(record, 0, recordLength);
            currentPosition += recordLength;
            return SUCCESS;
        } else {
            return END_OF_FILE;
        }
    }

    /**
     * 非ブロック化固定長レコードを読み取り
     * 
     * @param record       読み取ったレコードを格納するバッファ
     * @param recordLength レコード長
     * @return エラーコード
     */
    private int readUnblockedFixedLengthRecord(byte[] record, int recordLength) throws IOException {
        ByteBuffer buffer = ByteBuffer.allocate(recordLength);
        int bytesRead = fileChannel.read(buffer);

        if (bytesRead == -1) {
            return END_OF_FILE;
        }

        if (bytesRead < recordLength) {
            return READ_ERROR;
        }

        buffer.flip();
        buffer.get(record, 0, recordLength);
        currentPosition += recordLength;
        return SUCCESS;
    }

    /**
     * 可変長レコードを読み取り
     * 
     * @param record 読み取ったレコードを格納するバッファ
     * @return エラーコード
     */
    private int readVariableLengthRecord(byte[] record) throws IOException {
        // 可変長レコードの場合は、最初の4バイトでレコード長を取得
        ByteBuffer lengthBuffer = ByteBuffer.allocate(4);
        int bytesRead = fileChannel.read(lengthBuffer);

        if (bytesRead == -1) {
            return END_OF_FILE;
        }

        if (bytesRead < 4) {
            return READ_ERROR;
        }

        lengthBuffer.flip();
        int recordLength = lengthBuffer.getInt();

        if (recordLength > record.length) {
            return READ_ERROR;
        }

        // レコードデータを読み取り
        ByteBuffer dataBuffer = ByteBuffer.allocate(recordLength);
        bytesRead = fileChannel.read(dataBuffer);

        if (bytesRead < recordLength) {
            return READ_ERROR;
        }

        dataBuffer.flip();
        dataBuffer.get(record, 0, recordLength);
        currentPosition += 4 + recordLength;
        return SUCCESS;
    }

    /**
     * レコードを書き込み
     * 
     * @param record 書き込むレコード
     * @return エラーコード
     */
    public int writeRecord(byte[] record) {
        if (!isOpen) {
            return WRITE_ERROR;
        }

        try {
            if (dcbInfo.isFixedLengthRecord()) {
                return writeFixedLengthRecord(record);
            } else if (dcbInfo.isVariableLengthRecord()) {
                return writeVariableLengthRecord(record);
            } else {
                return INVALID_RECORD_FORMAT;
            }
        } catch (IOException ex) {
            return WRITE_ERROR;
        }
    }

    /**
     * 固定長レコードを書き込み
     * 
     * @param record 書き込むレコード
     * @return エラーコード
     */
    private int writeFixedLengthRecord(byte[] record) throws IOException {
        int recordLength = dcbInfo.getLogicalRecordLength();

        if (record.length > recordLength) {
            return WRITE_ERROR;
        }

        // レコード長に満たない場合は空白で埋める
        byte[] paddedRecord = new byte[recordLength];
        System.arraycopy(record, 0, paddedRecord, 0, record.length);
        for (int i = record.length; i < recordLength; i++) {
            paddedRecord[i] = ' ';
        }

        if (dcbInfo.isBlockedRecord()) {
            return writeBlockedFixedLengthRecord(paddedRecord);
        } else {
            return writeUnblockedFixedLengthRecord(paddedRecord);
        }
    }

    /**
     * ブロック化固定長レコードを書き込み
     * 
     * @param record 書き込むレコード
     * @return エラーコード
     */
    private int writeBlockedFixedLengthRecord(byte[] record) throws IOException {
        // ブロックバッファにレコードを追加
        if (blockBuffer.remaining() < record.length) {
            // ブロックバッファが満杯の場合、ファイルに書き込み
            blockBuffer.flip();
            fileChannel.write(blockBuffer);
            blockBuffer.clear();
        }

        blockBuffer.put(record);
        return SUCCESS;
    }

    /**
     * 非ブロック化固定長レコードを書き込み
     * 
     * @param record 書き込むレコード
     * @return エラーコード
     */
    private int writeUnblockedFixedLengthRecord(byte[] record) throws IOException {
        ByteBuffer buffer = ByteBuffer.wrap(record);
        fileChannel.write(buffer);
        currentPosition += record.length;
        return SUCCESS;
    }

    /**
     * 可変長レコードを書き込み
     * 
     * @param record 書き込むレコード
     * @return エラーコード
     */
    private int writeVariableLengthRecord(byte[] record) throws IOException {
        // レコード長を書き込み
        ByteBuffer lengthBuffer = ByteBuffer.allocate(4);
        lengthBuffer.putInt(record.length);
        lengthBuffer.flip();
        fileChannel.write(lengthBuffer);

        // レコードデータを書き込み
        ByteBuffer dataBuffer = ByteBuffer.wrap(record);
        fileChannel.write(dataBuffer);
        currentPosition += 4 + record.length;
        return SUCCESS;
    }

    /**
     * ファイル位置を設定
     * 
     * @param position ファイル位置
     * @return エラーコード
     */
    public int seek(long position) {
        if (!isOpen) {
            return READ_ERROR;
        }

        try {
            fileChannel.position(position);
            currentPosition = position;
            return SUCCESS;
        } catch (IOException ex) {
            return READ_ERROR;
        }
    }

    /**
     * 現在のファイル位置を取得
     * 
     * @return ファイル位置
     */
    public long getCurrentPosition() {
        return currentPosition;
    }

    /**
     * ファイルサイズを取得
     * 
     * @return ファイルサイズ
     */
    public long getFileSize() {
        if (!isOpen) {
            return -1;
        }

        try {
            return fileChannel.size();
        } catch (IOException ex) {
            return -1;
        }
    }

    /**
     * ブロック化レコードの場合、残りのブロックをフラッシュ
     * 
     * @return エラーコード
     */
    public int flushBlock() {
        if (!isOpen || !dcbInfo.isBlockedRecord()) {
            return SUCCESS;
        }

        try {
            if (blockBuffer.position() > 0) {
                blockBuffer.flip();
                fileChannel.write(blockBuffer);
                blockBuffer.clear();
            }
            return SUCCESS;
        } catch (IOException ex) {
            return WRITE_ERROR;
        }
    }

    /**
     * 全レコードを読み取り
     * 
     * @return レコードのリスト
     */
    public List<String> readAllRecords() {
        List<String> records = new ArrayList<>();
        byte[] recordBuffer = new byte[dcbInfo.getLogicalRecordLength()];

        int readResult;
        while ((readResult = readRecord(recordBuffer)) == SUCCESS) {
            String record = new String(recordBuffer, StandardCharsets.UTF_8).trim();
            records.add(record);
        }

        return records;
    }

    /**
     * レコード数を取得
     * 
     * @return レコード数
     */
    public long getRecordCount() {
        if (!isOpen) {
            return -1;
        }

        try {
            long fileSize = fileChannel.size();
            int recordLength = dcbInfo.getLogicalRecordLength();

            if (dcbInfo.isBlockedRecord()) {
                int blockSize = dcbInfo.getBlockSize();
                long blockCount = fileSize / blockSize;
                return blockCount * (blockSize / recordLength);
            } else {
                return fileSize / recordLength;
            }
        } catch (IOException ex) {
            return -1;
        }
    }

    /**
     * エラーコードを文字列に変換
     * 
     * @param errorCode エラーコード
     * @return エラーメッセージ
     */
    public static String getErrorMessage(int errorCode) {
        switch (errorCode) {
            case SUCCESS:
                return "Success";
            case FILE_NOT_FOUND:
                return "File not found";
            case ACCESS_DENIED:
                return "Access denied";
            case INVALID_RECORD_FORMAT:
                return "Invalid record format";
            case READ_ERROR:
                return "Read error";
            case WRITE_ERROR:
                return "Write error";
            case END_OF_FILE:
                return "End of file";
            case INVALID_DCB_INFO:
                return "Invalid DCB information";
            default:
                return "Unknown error";
        }
    }
}

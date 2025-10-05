package com.example.cobol;

import java.util.HashMap;
import java.util.Map;
import java.util.regex.Matcher;
import java.util.regex.Pattern;

/**
 * JCL（Job Control Language）のDCB情報を解析・管理するクラス
 * 
 * DCB（Data Control Block）は、ファイルの属性やアクセス方法を定義する情報です。
 * このクラスは、JCLで指定されたDCB情報を解析し、Javaでのファイル操作に必要な
 * 情報を提供します。
 */
public class JCLDCBInfo {

    // DCBパラメータの定数定義
    public static final String DSORG = "DSORG"; // データセット組織
    public static final String RECFM = "RECFM"; // レコード形式
    public static final String LRECL = "LRECL"; // 論理レコード長
    public static final String BLKSIZE = "BLKSIZE"; // ブロックサイズ
    public static final String KEYLEN = "KEYLEN"; // キー長
    public static final String KEYOFF = "KEYOFF"; // キーオフセット
    public static final String OPTCD = "OPTCD"; // オプションコード
    public static final String BUFNO = "BUFNO"; // バッファ数

    // レコード形式の定数
    public static final String RECFM_F = "F"; // 固定長レコード
    public static final String RECFM_V = "V"; // 可変長レコード
    public static final String RECFM_U = "U"; // 未定義レコード
    public static final String RECFM_B = "B"; // ブロック化
    public static final String RECFM_S = "S"; // スパンドレコード
    public static final String RECFM_A = "A"; // ASA制御文字
    public static final String RECFM_M = "M"; // マシン制御文字
    public static final String RECFM_D = "D"; // ダブルワード境界

    // データセット組織の定数
    public static final String DSORG_PS = "PS"; // 物理順次
    public static final String DSORG_PO = "PO"; // 分割データセット
    public static final String DSORG_VS = "VS"; // 仮想順次
    public static final String DSORG_DA = "DA"; // 直接アクセス
    public static final String DSORG_IS = "IS"; // インデックス順次
    public static final String DSORG_ES = "ES"; // 拡張順次

    private Map<String, String> dcbParameters;
    private String datasetName;
    private String ddName;

    /**
     * コンストラクタ
     * 
     * @param ddName      DD名
     * @param datasetName データセット名
     */
    public JCLDCBInfo(String ddName, String datasetName) {
        this.ddName = ddName;
        this.datasetName = datasetName;
        this.dcbParameters = new HashMap<>();
    }

    /**
     * DD名を取得
     * 
     * @return DD名
     */
    public String getDdName() {
        return ddName;
    }

    /**
     * データセット名を取得
     * 
     * @return データセット名
     */
    public String getDatasetName() {
        return datasetName;
    }

    /**
     * DCBパラメータを設定
     * 
     * @param parameter パラメータ名
     * @param value     値
     */
    public void setDCBParameter(String parameter, String value) {
        dcbParameters.put(parameter.toUpperCase(), value);
    }

    /**
     * DCBパラメータを取得
     * 
     * @param parameter パラメータ名
     * @return 値
     */
    public String getDCBParameter(String parameter) {
        return dcbParameters.get(parameter.toUpperCase());
    }

    /**
     * 論理レコード長を取得
     * 
     * @return 論理レコード長
     */
    public int getLogicalRecordLength() {
        String lrecl = getDCBParameter(LRECL);
        return lrecl != null ? Integer.parseInt(lrecl) : 80; // デフォルト80
    }

    /**
     * ブロックサイズを取得
     * 
     * @return ブロックサイズ
     */
    public int getBlockSize() {
        String blksize = getDCBParameter(BLKSIZE);
        return blksize != null ? Integer.parseInt(blksize) : getLogicalRecordLength();
    }

    /**
     * レコード形式を取得
     * 
     * @return レコード形式
     */
    public String getRecordFormat() {
        String recfm = getDCBParameter(RECFM);
        return recfm != null ? recfm : RECFM_F; // デフォルト固定長
    }

    /**
     * データセット組織を取得
     * 
     * @return データセット組織
     */
    public String getDatasetOrganization() {
        String dsorg = getDCBParameter(DSORG);
        return dsorg != null ? dsorg : DSORG_PS; // デフォルト物理順次
    }

    /**
     * キー長を取得
     * 
     * @return キー長
     */
    public int getKeyLength() {
        String keylen = getDCBParameter(KEYLEN);
        return keylen != null ? Integer.parseInt(keylen) : 0;
    }

    /**
     * キーオフセットを取得
     * 
     * @return キーオフセット
     */
    public int getKeyOffset() {
        String keyoff = getDCBParameter(KEYOFF);
        return keyoff != null ? Integer.parseInt(keyoff) : 0;
    }

    /**
     * バッファ数を取得
     * 
     * @return バッファ数
     */
    public int getBufferNumber() {
        String bufno = getDCBParameter(BUFNO);
        return bufno != null ? Integer.parseInt(bufno) : 1;
    }

    /**
     * 固定長レコードかどうかを判定
     * 
     * @return 固定長レコードの場合true
     */
    public boolean isFixedLengthRecord() {
        return getRecordFormat().contains(RECFM_F);
    }

    /**
     * 可変長レコードかどうかを判定
     * 
     * @return 可変長レコードの場合true
     */
    public boolean isVariableLengthRecord() {
        return getRecordFormat().contains(RECFM_V);
    }

    /**
     * ブロック化レコードかどうかを判定
     * 
     * @return ブロック化レコードの場合true
     */
    public boolean isBlockedRecord() {
        return getRecordFormat().contains(RECFM_B);
    }

    /**
     * ASA制御文字を持つレコードかどうかを判定
     * 
     * @return ASA制御文字を持つ場合true
     */
    public boolean hasASAControlCharacter() {
        return getRecordFormat().contains(RECFM_A);
    }

    /**
     * マシン制御文字を持つレコードかどうかを判定
     * 
     * @return マシン制御文字を持つ場合true
     */
    public boolean hasMachineControlCharacter() {
        return getRecordFormat().contains(RECFM_M);
    }

    /**
     * キー付きレコードかどうかを判定
     * 
     * @return キー付きレコードの場合true
     */
    public boolean hasKey() {
        return getKeyLength() > 0;
    }

    /**
     * JCLステートメントからDCB情報を解析
     * 
     * @param jclStatement JCLステートメント
     * @return 解析されたDCB情報
     */
    public static JCLDCBInfo parseFromJCL(String jclStatement) {
        // DD名を抽出
        Pattern ddPattern = Pattern.compile("//(\\w+)\\s+DD");
        Matcher ddMatcher = ddPattern.matcher(jclStatement);
        String ddName = ddMatcher.find() ? ddMatcher.group(1) : "UNKNOWN";

        // データセット名を抽出
        Pattern dsPattern = Pattern.compile("DSN=([^,]+)");
        Matcher dsMatcher = dsPattern.matcher(jclStatement);
        String datasetName = dsMatcher.find() ? dsMatcher.group(1) : "UNKNOWN";

        JCLDCBInfo dcbInfo = new JCLDCBInfo(ddName, datasetName);

        // DCBパラメータを抽出
        Pattern dcbPattern = Pattern.compile("DCB=\\(([^)]+)\\)");
        Matcher dcbMatcher = dcbPattern.matcher(jclStatement);

        if (dcbMatcher.find()) {
            String dcbParams = dcbMatcher.group(1);
            String[] params = dcbParams.split(",");

            for (String param : params) {
                String[] keyValue = param.trim().split("=");
                if (keyValue.length == 2) {
                    dcbInfo.setDCBParameter(keyValue[0].trim(), keyValue[1].trim());
                }
            }
        }

        return dcbInfo;
    }

    /**
     * DCB情報の文字列表現を取得
     * 
     * @return DCB情報の文字列
     */
    @Override
    public String toString() {
        StringBuilder sb = new StringBuilder();
        sb.append("DD Name: ").append(ddName).append("\n");
        sb.append("Dataset Name: ").append(datasetName).append("\n");
        sb.append("DCB Parameters:\n");

        for (Map.Entry<String, String> entry : dcbParameters.entrySet()) {
            sb.append("  ").append(entry.getKey()).append("=").append(entry.getValue()).append("\n");
        }

        return sb.toString();
    }

    /**
     * DCB情報の詳細を取得
     * 
     * @return DCB情報の詳細
     */
    public String getDetailedInfo() {
        StringBuilder sb = new StringBuilder();
        sb.append("=== DCB Information ===\n");
        sb.append("DD Name: ").append(ddName).append("\n");
        sb.append("Dataset Name: ").append(datasetName).append("\n");
        sb.append("Record Format: ").append(getRecordFormat()).append("\n");
        sb.append("Dataset Organization: ").append(getDatasetOrganization()).append("\n");
        sb.append("Logical Record Length: ").append(getLogicalRecordLength()).append("\n");
        sb.append("Block Size: ").append(getBlockSize()).append("\n");
        sb.append("Key Length: ").append(getKeyLength()).append("\n");
        sb.append("Key Offset: ").append(getKeyOffset()).append("\n");
        sb.append("Buffer Number: ").append(getBufferNumber()).append("\n");
        sb.append("Fixed Length: ").append(isFixedLengthRecord()).append("\n");
        sb.append("Variable Length: ").append(isVariableLengthRecord()).append("\n");
        sb.append("Blocked: ").append(isBlockedRecord()).append("\n");
        sb.append("Has Key: ").append(hasKey()).append("\n");
        sb.append("ASA Control: ").append(hasASAControlCharacter()).append("\n");
        sb.append("Machine Control: ").append(hasMachineControlCharacter()).append("\n");

        return sb.toString();
    }
}

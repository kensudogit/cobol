# JCL DCB ファイルアクセス制御システム

## 概要

このシステムは、JCL（Job Control Language）で指定されたDCB（Data Control Block）情報を使用して、様々なレコード形式のファイルにアクセスし、データの読み書き制御処理を行うJavaプログラムです。

## 機能

### サポートされるレコード形式

1. **固定長レコード（RECFM=F）**
   - 全てのレコードが同じ長さ
   - 短いレコードは空白でパディング
   - 最もシンプルなレコード形式

2. **可変長レコード（RECFM=V）**
   - レコードごとに長さが異なる
   - レコード長情報（4バイト）が各レコードの先頭に格納
   - ディスク容量を効率的に使用

3. **ブロック化レコード（RECFM=FB, VB）**
   - 複数のレコードをブロック単位でグループ化
   - I/O操作の回数を減らして性能を向上
   - ブロックサイズはレコード長の倍数

### サポートされるDCBパラメータ

- **RECFM（レコード形式）**
  - F: 固定長
  - V: 可変長
  - B: ブロック化
  - A: ASA制御文字
  - M: マシン制御文字

- **LRECL（論理レコード長）**
  - レコードの長さ（バイト単位）

- **BLKSIZE（ブロックサイズ）**
  - ブロックの長さ（バイト単位）
  - 通常はLRECLの倍数

- **DSORG（データセット組織）**
  - PS: 物理順次
  - PO: 分割データセット
  - IS: インデックス順次

- **KEYLEN（キー長）**
  - レコードキーの長さ

- **KEYOFF（キーオフセット）**
  - レコードキーの開始位置

- **BUFNO（バッファ数）**
  - バッファリング用のバッファ数

## クラス構成

### 1. JCLDCBInfo.java

JCLのDCB情報を解析・管理するクラス

**主な機能：**
- DCBパラメータの設定・取得
- JCLステートメントからDCB情報を解析
- レコード形式の判定（固定長、可変長、ブロック化など）
- DCB情報の詳細表示

**主なメソッド：**
```java
// DCBパラメータの設定
void setDCBParameter(String parameter, String value)

// DCBパラメータの取得
String getDCBParameter(String parameter)

// レコード形式の判定
boolean isFixedLengthRecord()
boolean isVariableLengthRecord()
boolean isBlockedRecord()

// JCLステートメントから解析
static JCLDCBInfo parseFromJCL(String jclStatement)
```

### 2. JCLFileAccessController.java

DCB情報に基づくファイルアクセス制御クラス

**主な機能：**
- ファイルのオープン・クローズ
- レコードの読み取り・書き込み
- 固定長・可変長・ブロック化レコードの処理
- エラーハンドリング

**主なメソッド：**
```java
// ファイル操作
int openFile(String fileName, String mode)
int closeFile()

// レコード操作
int readRecord(byte[] record)
int writeRecord(byte[] record)

// ブロック操作
int flushBlock()

// ファイル位置操作
int seek(long position)
long getCurrentPosition()
```

**エラーコード：**
- `SUCCESS`: 成功
- `FILE_NOT_FOUND`: ファイルが見つからない
- `ACCESS_DENIED`: アクセス拒否
- `INVALID_RECORD_FORMAT`: 無効なレコード形式
- `READ_ERROR`: 読み取りエラー
- `WRITE_ERROR`: 書き込みエラー
- `END_OF_FILE`: ファイル終端

### 3. JCLFileAccessSample.java

ファイルアクセス制御のサンプルプログラム

**主な機能：**
- 固定長レコードのテスト
- 可変長レコードのテスト
- ブロック化レコードのテスト
- 複合レコード形式のテスト

### 4. JCLSampleAndTestCases.java

JCLサンプルとテストケースの管理クラス

**主な機能：**
- JCLステートメントのサンプル生成
- テストケースの生成
- JCL解析のテスト
- ファイルアクセス制御のテスト

## 使用方法

### 1. コンパイル

```bash
# JCLDCBInfo.java をコンパイル
javac -d bin java\com\example\cobol\JCLDCBInfo.java

# JCLFileAccessController.java をコンパイル
javac -d bin -cp bin java\com\example\cobol\JCLFileAccessController.java

# JCLFileAccessSample.java をコンパイル
javac -d bin -cp bin java\com\example\cobol\JCLFileAccessSample.java

# JCLSampleAndTestCases.java をコンパイル
javac -d bin -cp bin java\com\example\cobol\JCLSampleAndTestCases.java
```

または、提供されているバッチファイルを使用：

```bash
run_jcl_file_access.bat
```

### 2. 実行

```bash
# サンプルプログラムの実行
java -cp bin com.example.cobol.JCLFileAccessSample

# JCLサンプルとテストケースの実行
java -cp bin com.example.cobol.JCLSampleAndTestCases
```

### 3. プログラム例

#### 固定長レコードの読み書き

```java
// DCB情報を作成
JCLDCBInfo dcbInfo = new JCLDCBInfo("INPUT", "TEST.FIXED");
dcbInfo.setDCBParameter("RECFM", "F");
dcbInfo.setDCBParameter("LRECL", "80");
dcbInfo.setDCBParameter("DSORG", "PS");

// ファイルアクセス制御オブジェクトを作成
JCLFileAccessController controller = new JCLFileAccessController(dcbInfo);

// ファイルをオープン
int result = controller.openFile("test.txt", "rw");

if (result == JCLFileAccessController.SUCCESS) {
    // レコードを書き込み
    String record = "This is a test record";
    byte[] recordBytes = record.getBytes(StandardCharsets.UTF_8);
    controller.writeRecord(recordBytes);
    
    // ファイルをクローズ
    controller.closeFile();
}
```

#### 可変長レコードの読み書き

```java
// DCB情報を作成
JCLDCBInfo dcbInfo = new JCLDCBInfo("INPUT", "TEST.VARIABLE");
dcbInfo.setDCBParameter("RECFM", "V");
dcbInfo.setDCBParameter("LRECL", "255");
dcbInfo.setDCBParameter("DSORG", "PS");

// ファイルアクセス制御オブジェクトを作成
JCLFileAccessController controller = new JCLFileAccessController(dcbInfo);

// ファイルをオープン
int result = controller.openFile("test.txt", "rw");

if (result == JCLFileAccessController.SUCCESS) {
    // 可変長レコードを書き込み
    String[] records = {"Short", "Medium length record", "Very long record..."};
    
    for (String record : records) {
        byte[] recordBytes = record.getBytes(StandardCharsets.UTF_8);
        controller.writeRecord(recordBytes);
    }
    
    // ファイルをクローズ
    controller.closeFile();
}
```

#### JCLステートメントから解析

```java
// JCLステートメント
String jcl = "//INPUT DD DSN=TEST.FILE,DISP=SHR,DCB=(RECFM=FB,LRECL=80,BLKSIZE=800,DSORG=PS)";

// DCB情報を解析
JCLDCBInfo dcbInfo = JCLDCBInfo.parseFromJCL(jcl);

// DCB情報を表示
System.out.println(dcbInfo.getDetailedInfo());
```

## JCLサンプル

### 固定長レコード

```jcl
//FIXEDLEN JOB (ACCT),'FIXED LENGTH RECORDS',CLASS=A,MSGCLASS=X
//STEP1    EXEC PGM=YOURPROG
//INPUT    DD DSN=YOUR.DATASET.INPUT,DISP=SHR,
//            DCB=(RECFM=F,LRECL=80,DSORG=PS)
//OUTPUT   DD DSN=YOUR.DATASET.OUTPUT,DISP=(NEW,CATLG),
//            DCB=(RECFM=F,LRECL=80,DSORG=PS),
//            SPACE=(TRK,(10,5))
```

### 可変長レコード

```jcl
//VARLEN   JOB (ACCT),'VARIABLE LENGTH RECORDS',CLASS=A,MSGCLASS=X
//STEP1    EXEC PGM=YOURPROG
//INPUT    DD DSN=YOUR.DATASET.INPUT,DISP=SHR,
//            DCB=(RECFM=V,LRECL=255,DSORG=PS)
//OUTPUT   DD DSN=YOUR.DATASET.OUTPUT,DISP=(NEW,CATLG),
//            DCB=(RECFM=V,LRECL=255,DSORG=PS),
//            SPACE=(TRK,(10,5))
```

### ブロック化レコード

```jcl
//BLOCKED  JOB (ACCT),'BLOCKED RECORDS',CLASS=A,MSGCLASS=X
//STEP1    EXEC PGM=YOURPROG
//INPUT    DD DSN=YOUR.DATASET.INPUT,DISP=SHR,
//            DCB=(RECFM=FB,LRECL=80,BLKSIZE=800,DSORG=PS)
//OUTPUT   DD DSN=YOUR.DATASET.OUTPUT,DISP=(NEW,CATLG),
//            DCB=(RECFM=FB,LRECL=80,BLKSIZE=800,DSORG=PS),
//            SPACE=(TRK,(10,5))
```

## テストケース

テストケースは`test_cases`ディレクトリに生成されます：

- `fixed_length_test_cases.txt`: 固定長レコードのテストケース
- `variable_length_test_cases.txt`: 可変長レコードのテストケース
- `blocked_test_cases.txt`: ブロック化レコードのテストケース
- `error_handling_test_cases.txt`: エラーハンドリングのテストケース

## ディレクトリ構造

```
cobol/
├── java/
│   └── com/
│       └── example/
│           └── cobol/
│               ├── JCLDCBInfo.java              # DCB情報管理クラス
│               ├── JCLFileAccessController.java # ファイルアクセス制御クラス
│               ├── JCLFileAccessSample.java     # サンプルプログラム
│               └── JCLSampleAndTestCases.java   # JCLサンプルとテストケース
├── bin/                                         # コンパイル済みクラスファイル
├── test_data/                                   # テストデータ
├── jcl_samples/                                 # JCLサンプル
├── test_cases/                                  # テストケース
├── run_jcl_file_access.bat                      # 実行バッチファイル
└── JCL_FILE_ACCESS_README.md                    # このファイル
```

## エラーハンドリング

すべてのファイル操作はエラーコードを返します：

```java
int result = controller.openFile("test.txt", "r");

if (result != JCLFileAccessController.SUCCESS) {
    System.err.println("Error: " + JCLFileAccessController.getErrorMessage(result));
}
```

## パフォーマンス最適化

### ブロック化の利用

ブロック化レコードを使用することで、I/O操作の回数を減らし、パフォーマンスを向上させることができます：

```java
// ブロック化レコードのDCB情報
dcbInfo.setDCBParameter("RECFM", "FB");
dcbInfo.setDCBParameter("LRECL", "80");
dcbInfo.setDCBParameter("BLKSIZE", "800");  // 10レコード/ブロック
```

### バッファリング

複数のバッファを使用することで、I/O性能を向上させることができます：

```java
dcbInfo.setDCBParameter("BUFNO", "3");  // 3つのバッファを使用
```

## 注意事項

1. **文字エンコーディング**
   - デフォルトでUTF-8を使用
   - EBCDIC変換が必要な場合は、別途実装が必要

2. **ファイルサイズ制限**
   - 大容量ファイルの処理時は、メモリ使用量に注意
   - ブロック化レコードを使用して効率化

3. **スレッドセーフティ**
   - 各`JCLFileAccessController`インスタンスは単一スレッドでの使用を想定
   - マルチスレッド環境では、インスタンスを分けて使用

## ライセンス

このプログラムは教育目的で作成されています。

## 作成者

COBOL-Java移植プロジェクト

## 更新履歴

- 2025-10-05: 初版リリース
  - JCL DCB情報解析機能
  - ファイルアクセス制御機能
  - 固定長・可変長・ブロック化レコードのサポート
  - JCLサンプルとテストケースの生成機能


# COBOL Development Environment

このディレクトリは、Windows環境でのCOBOL開発環境を提供します。

## ディレクトリ構造

```
cobol/
├── src/           # メインのCOBOLソースファイル
├── samples/       # サンプルプログラム
├── bin/           # コンパイル済み実行ファイル
├── lib/           # ライブラリファイル
├── docs/          # ドキュメント
├── scripts/       # ビルド・実行スクリプト
├── Makefile       # Makefile（GNU Make使用時）
├── setup.bat      # 環境セットアップスクリプト
└── README.md      # このファイル
```

## 前提条件

### GnuCOBOLのインストール

COBOL開発環境を使用するには、GnuCOBOL（OpenCOBOL）をインストールする必要があります。

#### インストール方法

1. **公式サイトからダウンロード**
   - [GnuCOBOL公式サイト](https://sourceforge.net/projects/gnucobol/)
   - Windows用のバイナリをダウンロードしてインストール

2. **Chocolateyを使用**
   ```cmd
   choco install gnucobol
   ```

3. **MSYS2を使用**
   ```cmd
   pacman -S mingw-w64-x86_64-gnucobol
   ```

#### 環境変数の設定

GnuCOBOLのインストール後、PATH環境変数にGnuCOBOLのbinディレクトリを追加してください。

## セットアップ

1. **環境セットアップ**
   ```cmd
   setup.bat
   ```

2. **プログラムのビルド**
   ```cmd
   scripts\build.bat
   ```
   または
   ```cmd
   make
   ```

## サンプルプログラム

### 1. Hello World (`samples/hello.cob`)
基本的なCOBOLプログラムの例。ループ処理とメッセージ表示をデモンストレーションします。

### 2. Calculator (`samples/calculator.cob`)
四則演算を行う電卓プログラム。ユーザー入力の処理方法を示します。

### 3. File I/O (`samples/fileio.cob`)
ファイルの読み書き操作の例。`input.txt`を読み込んで`output.txt`に書き出します。

### 4. Employee Management (`src/employee.cob`)
従業員管理システムの例。データ構造とメニュー処理を示します。

## 使用方法

### ビルドスクリプトの使用

```cmd
# 全プログラムをビルド
scripts\build.bat

# 特定のプログラムを実行
scripts\run.bat

# テスト実行
scripts\test.bat

# クリーンアップ
scripts\clean.bat
```

### Makefileの使用

```cmd
# 全プログラムをビルド
make

# デバッグビルド
make debug

# プログラム実行
make run

# クリーンアップ
make clean

# ヘルプ表示
make help
```

### 手動コンパイル

```cmd
# 基本的なコンパイル
cobc -x -o program.exe program.cob

# デバッグ情報付きコンパイル
cobc -x -g -o program.exe program.cob

# 最適化コンパイル
cobc -x -O2 -o program.exe program.cob
```

## COBOLの基本構文

### プログラム構造
```cobol
IDENTIFICATION DIVISION.
PROGRAM-ID. PROGRAM-NAME.
AUTHOR. AUTHOR-NAME.
DATE-WRITTEN. DATE.

ENVIRONMENT DIVISION.

DATA DIVISION.
WORKING-STORAGE SECTION.
01 WS-VARIABLE PIC X(10).

PROCEDURE DIVISION.
MAIN-PROCEDURE.
    DISPLAY 'Hello, COBOL!'
    STOP RUN.
```

### データ型
- `PIC X(n)` - 文字列（n文字）
- `PIC 9(n)` - 数値（n桁）
- `PIC 9(n)V99` - 小数点付き数値
- `PIC ZZZ9.99` - 編集済み数値（ゼロサプレス）

### 制御構造
```cobol
# IF文
IF condition THEN
    statement
ELSE
    statement
END-IF

# PERFORM文
PERFORM procedure-name

# ループ
PERFORM VARYING counter FROM 1 BY 1
UNTIL counter > 10
    statement
END-PERFORM
```

## トラブルシューティング

### よくある問題

1. **GnuCOBOLが見つからない**
   - PATH環境変数を確認
   - GnuCOBOLが正しくインストールされているか確認

2. **コンパイルエラー**
   - COBOLの構文を確認
   - ファイルエンコーディングを確認（UTF-8推奨）

3. **実行時エラー**
   - 必要なファイルが存在するか確認
   - ファイルのアクセス権限を確認

### デバッグ

```cmd
# デバッグ情報付きでコンパイル
cobc -x -g -debug -o program.exe program.cob

# 実行時にデバッグ情報を表示
program.exe
```

## 参考資料

- [GnuCOBOL公式ドキュメント](https://gnucobol.sourceforge.io/)
- [COBOL言語仕様](https://www.iso.org/standard/74515.html)
- [COBOLプログラミングガイド](https://www.ibm.com/docs/en/cobol-zos)

## COBOLからJavaへの変換詳細解説

### Comprehensive Sample Program の変換

`comprehensive_sample.cob`（683行）が`ComprehensiveSample.java`（992行）に変換された詳細な理由と技術的背景を解説します。

#### 1. 変換の背景と目的

##### **教育目的**
- COBOL開発者のJava学習支援
- レガシーシステムの現代的実装例の提供
- COBOLとJavaの機能比較学習
- システム移行の参考実装

##### **技術的必要性**
- プラットフォーム独立性の向上
- 現代的な開発環境への対応
- 保守性と拡張性の向上
- パフォーマンスとメモリ管理の改善

#### 2. 言語特性の根本的違い

##### **COBOLの特徴**
```cobol
IDENTIFICATION DIVISION.
PROGRAM-ID. COMPREHENSIVE-SAMPLE.
ENVIRONMENT DIVISION.
DATA DIVISION.
PROCEDURE DIVISION.
```
- **手続き型言語**: 手順に従って処理を実行
- **固定位置データ処理**: PIC句による厳密なデータ型定義
- **メインフレーム依存**: 特定の環境に最適化
- **手動メモリ管理**: プログラマーが明示的に制御

##### **Javaの特徴**
```java
public class ComprehensiveSample {
    private List<CustomerRecord> customerRecords;
    public void executeProgram() { ... }
}
```
- **オブジェクト指向言語**: データと処理をカプセル化
- **型安全性**: コンパイル時型チェック
- **クロスプラットフォーム**: JVM上で実行
- **自動メモリ管理**: ガベージコレクション

#### 3. 詳細な変換パターン

##### **データ定義の変換**

**COBOL（固定位置データ）:**
```cobol
01  CUSTOMER-RECORD.
    05  CUSTOMER-ID             PIC 9(7).
    05  CUSTOMER-NAME           PIC X(30).
    05  CUSTOMER-ADDRESS. 
        10  STREET-ADDRESS      PIC X(40).
        10  CITY               PIC X(20).
        10  STATE              PIC X(2).
        10  ZIP-CODE           PIC X(10).
    05  CUSTOMER-PHONE          PIC X(15).
    05  CREDIT-LIMIT           PIC 9(8)V99.
    05  CUSTOMER-STATUS        PIC X(1).
```

**Java（オブジェクト指向）:**
```java
public static class CustomerRecord {
    private int customerId;           // PIC 9(7) → int
    private String customerName;      // PIC X(30) → String
    private Address customerAddress; // グループ項目 → ネストしたクラス
    private String customerPhone;    // PIC X(15) → String
    private BigDecimal creditLimit;  // PIC 9(8)V99 → BigDecimal
    private char customerStatus;     // PIC X(1) → char
}

public static class Address {
    private String streetAddress;    // PIC X(40) → String
    private String city;             // PIC X(20) → String
    private String state;            // PIC X(2) → String
    private String zipCode;          // PIC X(10) → String
}
```

##### **配列とテーブルの変換**

**COBOL（OCCURS句）:**
```cobol
01  SALES-TABLE.
    05  MONTHLY-SALES OCCURS 12 TIMES
            INDEXED BY MONTH-INDEX.
        10  MONTH-NAME           PIC X(10).
        10  MONTH-SALES          PIC 9(8)V99 VALUE 0.
        10  MONTH-CUSTOMERS      PIC 9(4) VALUE 0.
```

**Java（コレクション）:**
```java
public static class MonthlySalesData {
    private String monthName;
    private BigDecimal monthSales;
    private int monthCustomers;
    
    public void addToMonthSales(BigDecimal amount) {
        this.monthSales = this.monthSales.add(amount);
    }
}

private final List<MonthlySalesData> monthlySales = new ArrayList<>();
```

##### **ファイル処理の変換**

**COBOL（ファイル制御）:**
```cobol
FILE-CONTROL.
    SELECT CUSTOMER-MASTER 
        ASSIGN TO 'CUSTOMER.MAST'
        ORGANIZATION IS SEQUENTIAL
        STATUS IS FILE-STATUS.

PROCEDURE DIVISION.
    OPEN INPUT CUSTOMER-MASTER
    PERFORM UNTIL EOF-REACHED
        READ CUSTOMER-MASTER
            AT END
                MOVE 'Y' TO EOF-FLAG
            NOT AT END
                PERFORM PROCESS-SINGLE-CUSTOMER
        END-READ
    END-PERFORM
    CLOSE CUSTOMER-MASTER
```

**Java（ストリーム処理）:**
```java
private static final String CUSTOMER_MASTER_FILE = "CUSTOMER.MAST";

private void processCustomerFile() {
    try (BufferedReader reader = new BufferedReader(new FileReader(CUSTOMER_MASTER_FILE))) {
        String line;
        while ((line = reader.readLine()) != null) {
            processCustomerLine(line);
        }
    } catch (IOException e) {
        System.err.println("Error reading customer file: " + e.getMessage());
        recordsError++;
    }
}
```

##### **制御構造の変換**

**COBOL（EVALUATE文）:**
```cobol
EVALUATE CUSTOMER-STATUS
    WHEN 'A'
        PERFORM PROCESS-ACTIVE-CUSTOMER
    WHEN 'I'
        PERFORM PROCESS-INACTIVE-CUSTOMER
    WHEN 'S'
        PERFORM PROCESS-SUSPENDED-CUSTOMER
    WHEN OTHER
        PERFORM PROCESS-UNKNOWN-CUSTOMER
END-EVALUATE
```

**Java（switch文）:**
```java
switch (customer.getCustomerStatus()) {
    case 'A':
        processActiveCustomer(customer);
        break;
    case 'I':
        processInactiveCustomer(customer);
        break;
    case 'S':
        processSuspendedCustomer(customer);
        break;
    default:
        processUnknownCustomer(customer);
}
```

**COBOL（PERFORM VARYING文）:**
```cobol
PERFORM VARYING MAIN-LOOP-COUNTER FROM 1 BY 1
    UNTIL MAIN-LOOP-COUNTER > 10
    DISPLAY 'Loop iteration: ' MAIN-LOOP-COUNTER
    PERFORM PROCESS-LOOP-ITERATION
END-PERFORM
```

**Java（for文）:**
```java
for (int i = 1; i <= 10; i++) {
    System.out.println("Loop iteration: " + i);
    processLoopIteration(i);
}
```

##### **文字列操作の変換**

**COBOL（STRING文）:**
```cobol
STRING REPORT-TITLE DELIMITED BY SIZE
       ' - ' DELIMITED BY SIZE
       CURRENT-DATE DELIMITED BY SIZE
          INTO REPORT-SUBTITLE
          ON OVERFLOW
              DISPLAY 'String operation overflow'
END-STRING
```

**Java（StringBuilder）:**
```java
private String buildFullName(String firstName, String lastName) {
    return firstName + " " + lastName;
}
```

**COBOL（UNSTRING文）:**
```cobol
UNSTRING TRANSACTION-DATE DELIMITED BY ALL SPACES
         INTO YEAR-PART, MONTH-PART, DAY-PART
         ON OVERFLOW
             DISPLAY 'Unstring operation overflow'
END-UNSTRING
```

**Java（substring）:**
```java
private String[] parseDate(String dateString) {
    return new String[] {
        dateString.substring(0, 4), // Year
        dateString.substring(4, 6), // Month
        dateString.substring(6, 8)   // Day
    };
}
```

##### **数値計算の変換**

**COBOL（COMPUTE文）:**
```cobol
COMPUTE CALCULATED-TAX = TOTAL-AMOUNT * TAX-RATE
         ROUNDED MODE ROUND-HALF-EVEN

COMPUTE CALCULATED-COMMISSION = TOTAL-AMOUNT * COMMISSION-RATE
         ROUNDED MODE ROUND-HALF-EVEN
```

**Java（BigDecimal）:**
```java
private static final BigDecimal TAX_RATE = new BigDecimal("0.08");
private static final BigDecimal COMMISSION_RATE = new BigDecimal("0.05");

calculatedTax = sales.getTotalAmount().multiply(TAX_RATE)
        .setScale(2, RoundingMode.HALF_UP);

calculatedCommission = sales.getTotalAmount().multiply(COMMISSION_RATE)
        .setScale(2, RoundingMode.HALF_UP);
```

##### **検索とソートの変換**

**COBOL（SEARCH文）:**
```cobol
SET MONTH-INDEX TO 1
SEARCH MONTHLY-SALES
    WHEN MONTH-NAME(MONTH-INDEX) = 'December'
        DISPLAY 'Found December sales data'
        PERFORM PROCESS-DECEMBER-SALES
END-SEARCH
```

**Java（Stream API）:**
```java
private void searchCustomerByStatus(char targetStatus) {
    List<CustomerRecord> foundCustomers = customerRecords.stream()
            .filter(customer -> customer.getCustomerStatus() == targetStatus)
            .collect(Collectors.toList());
    
    System.out.println("Found " + foundCustomers.size() + " matching customers");
}
```

**COBOL（SORT文）:**
```cobol
SORT SORT-WORKFILE ON ASCENDING KEY SORT-SALES-TOTAL
    INPUT PROCEDURE IS PREPARE-SORT-DATA
    OUTPUT PROCEDURE IS PROCESS-SORTED-DATA
```

**Java（Stream.sorted）:**
```java
List<SalesRecord> sortedSales = salesRecords.stream()
        .sorted((s1, s2) -> s2.getTotalAmount().compareTo(s1.getTotalAmount()))
        .collect(Collectors.toList());
```

#### 4. 現代的なJava機能の活用

##### **例外処理**
```java
try (BufferedReader reader = new BufferedReader(new FileReader(CUSTOMER_MASTER_FILE))) {
    // ファイル処理
} catch (IOException e) {
    System.err.println("Error reading customer file: " + e.getMessage());
    recordsError++;
}
```

##### **日時処理**
```java
private static final DateTimeFormatter TIMESTAMP_FORMAT = 
    DateTimeFormatter.ofPattern("yyyy-MM-dd HH:mm:ss");

LocalDateTime startTime = LocalDateTime.now();
logMessage("Program start time: " + startTime.format(TIMESTAMP_FORMAT));
```

##### **ログ機能**
```java
private final StringBuilder logMessages = new StringBuilder();

private void logMessage(String message) {
    String timestamp = LocalDateTime.now().format(TIMESTAMP_FORMAT);
    logMessages.append(timestamp).append(" - ").append(message).append("\n");
}
```

#### 5. パフォーマンスとメモリ管理の改善

##### **メモリ管理**
- **COBOL**: 手動メモリ管理、固定サイズ配列
- **Java**: 自動ガベージコレクション、動的配列（ArrayList）

##### **数値計算の精度**
- **COBOL**: 固定小数点演算
- **Java**: BigDecimalによる高精度計算

##### **エラーハンドリング**
- **COBOL**: ファイルステータスコードによる手動チェック
- **Java**: 例外処理による自動エラーハンドリング

#### 6. 変換による利点

##### **開発効率の向上**
- 豊富なIDEサポート
- 強力なデバッガー
- 自動補完機能

##### **保守性の向上**
- オブジェクト指向設計
- カプセル化によるデータ保護
- メソッドの再利用性

##### **拡張性の向上**
- 継承とポリモーフィズム
- インターフェースによる抽象化
- デザインパターンの適用

##### **テスト容易性**
- 単体テストフレームワーク（JUnit）
- モックオブジェクト
- テストカバレッジツール

#### 7. 変換時の注意点

##### **データ型の選択**
- COBOLのPIC句を適切なJava型にマッピング
- 数値計算ではBigDecimalを使用
- 文字列処理ではStringBuilderを活用

##### **エラーハンドリング**
- COBOLのファイルステータスをJavaの例外処理に変換
- 適切な例外クラスの選択
- ログ出力の実装

##### **パフォーマンス考慮**
- 大量データ処理時のメモリ使用量
- ストリーム処理の活用
- 適切なコレクションクラスの選択

#### 8. 学習効果

この変換により以下の学習効果が得られます：

1. **COBOL機能の理解**: 各COBOL機能の動作原理を理解
2. **Java機能の習得**: 現代的なJava機能の実践的学習
3. **システム設計**: レガシーシステムの現代的実装方法
4. **移行戦略**: COBOLからJavaへの移行パターンの習得

## ライセンス

この開発環境は教育目的で提供されています。商用利用の際は、使用するライブラリやツールのライセンスを確認してください。 

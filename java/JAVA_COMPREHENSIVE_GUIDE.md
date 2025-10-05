# COBOL to Java Comprehensive Migration Guide

## 概要

このガイドでは、COBOLプログラム `comprehensive_sample.cob` を完全にJavaに移植した `ComprehensiveSample.java` の実装詳細とCOBOL to Java変換技法について説明します。

## ファイル構成

### Javaファイル
- **ComprehensiveSample.java**: メインプログラム（完全移植版）
- **ComprehensiveSampleRunner.java**: サンプルデータ作成・実行支援クラス
- **run_comprehensive_sample.bat**: Windows用実行スクリプト

## COBOL to Java 変換マッピング

### 1. DIVISION構造の変換

| COBOL構造 | Java実装 |
|------------|----------|
| `IDENTIFICATION DIVISION` | Class定義、package宣言、javadoc |
| `ENVIRONMENT DIVISION` | Static final constants、設定変数 |
| `DATA DIVISION` | Class fields、データ構造クラス |
| `PROCEDURE DIVISION` | Method implementations |

```java
// COBOL: IDENTIFICATION DIVISION
// Java: Class definition
public class ComprehensiveSample {
    
    // COBOL: ENVIRONMENT DIVISION
    // Java: Constants and configuration
    private static final String PROGRAM_TITLE = "COBOL Comprehensive Sample";
    
    // COBOL: DATA DIVISION
    // Java: Data structures and fields
    private final List<CustomerRecord> customerRecords = new HashSet<>();
    
    // COBOL: PROCEDURE DIVISION
    // Java: Method implementations
    public void executeProgram() {
        // Implementation here
    }
}
```

### 2. データ型の変換

#### 基本データ型

| COBOL | Java型 | 例 |
|-------|--------|-----|
| `PIC 9(5)` | `int` | `private int customerId` |
| `PIC 9(8)V99` | `BigDecimal` | `private BigDecimal totalAmount` |
| `PIC X(30)` | `String` | `private String customerName` |
| `PIC A(20)` | `String` | `private String alphabeticData` |
| `PIC 9(5) COMP` | `int` | `private int binaryNumber` |
| `PIC 9(7)V99 COMP-3` | `BigDecimal` | `private BigDecimal packedDecimal` |

#### 複合データ構造

```java
// COBOL: 01 CUSTOMER-RECORD
public static class CustomerRecord {
    private int customerId;           // PIC 9(7)
    private String customerName;      // PIC X(30)
    private Address customerAddress;  // 05 CUSTOMER-ADDRESS
    // ...
}

// COBOL: 01 CUSTOMER-ADDRESS (grouping)
public static class Address {
    private String streetAddress;     // PIC X(40)
    private String city;             // PIC X(20)
    private String state;            // PIC X(2)
    private String zipCode;          // PIC X(10)
}
```

#### 配列とテーブル（OCCURS句）

```java
// COBOL: OCCURS 12 TIMES
private final List<MonthlySalesData> monthlySales = new ArrayList<>();

// COBOL: OCCURS 1 TO 100 TIMES DEPENDING ON COUNT
private final List<ProductData> products = new ArrayList<>();

// COBOL: SEARCH 文
List<CustomerRecord> results = customerRecords.stream()
    .filter(customer -> customer.getStatus() == 'A')
    .collect(Collectors.toList());
```

### 3. 計算・演算の変換

#### 基本計算

```java
// COBOL: ADD AMOUNT TO TOTAL
// Java: BigDecimal arithmetic
total = total.add(amount);

// COBOL: COMPUTE RESULT = AMOUNT * RATE ROUNDED
// Java: BigDecimal with rounding
result = amount.multiply(rate).setScale(2, RoundingMode.HALF_UP);

// COBOL: DIVIDE A BY B GIVING C
// Java: BigDecimal division
c = a.divide(b, 2, RoundingMode.HALF_UP);
```

#### エラーハンドリング

```java
// COBOL: ON SIZE ERROR
// Java: ArithmeticException handling
try {
    BigDecimal result = divisor.divide(divisor);
} catch (ArithmeticException e) {
    System.err.println("Division by zero error");
}
```

### 4. ファイル操作の変換

```java
// COBOL: OPEN INPUT FILE / READ FILE / CLOSE FILE
// Java: BufferedReader with try-with-resources
try (BufferedReader reader = new BufferedReader(new FileReader(filename))) {
    String line;
    while ((line = reader.readLine()) != null) {
        processLine(line);
    }
} catch (IOException e) {
    System.err.println("File operation error: " + e.getMessage());
}

// COBOL: WRITE RECORD TO FILE
// Java: PrintWriter output
try (PrintWriter writer = new PrintWriter(new FileWriter(outputFile))) {
    writer.println("Report data");
} catch (IOException e) {
    System.err.println("File write error: " + e.getMessage());
}
```

### 5. 文字列操作の変換

```java
// COBOL: STRING A DELIMITED BY SIZE INTO R
// Java: StringBuilder concatenation
StringBuilder sb = new StringBuilder();
sb.append(firstName).append(" ").append(lastName);
String result = sb.toString();

// COBOL: UNSTRING DATE DELIMITED BY '-' INTO Y, M, D
// Java: String splitting
String[] parts = dateString.split("-");
String year = parts[0];
String month = parts[1];
String day = parts[2];

// COBOL: INSPECT TEXT REPLACING ALL SPACES BY '-'
// Java: String replacement
String result = text.replace(" ", "-");

// COBOL: INSPECT TEXT TALLYING COUNT FOR CHARACTERS
// Java: Character counting
int count = text.indexOf(delimiter);
```

### 6. 条件分岐の変換

```java
// COBOL: IF condition THEN ... ELSE ... END-IF
// Java: if-else statements
if (customer.getStatus() == 'A') {
    processActiveCustomer(customer);
} else if (customer.getStatus() == 'I') {
    processInactiveCustomer(customer);
} else {
    processUnknownCustomer(customer);
}

// COBOL: EVALUATE condition WHEN ... WHEN ... END-EVALUATE
// Java: switch-case statements
switch (customer.getStatus()) {
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

### 7. ループ処理の変換

```java
// COBOL: PERFORM VARYING counter FROM 1 BY 1 UNTIL counter > 10
// Java: for loop
for (int i = 1; i <= 10; i++) {
    processLoopIteration(i);
}

// COBOL: PERFORM UNTIL eof-flag = 'Y'
// Java: while loop
while (!eofReached) {
    String line = reader.readLine();
    if (line == null) {
        eofReached = true;
    } else {
        processLine(line);
    }
}

// COBOL: PERFORM WITH TEST BEFORE UNTIL condition
// Java: while loop with precondition
while (retryCount < 3) {
    if (validateData()) break;
    retryCount++;
}

// COBOL: PERFORM WITH TEST AFTER UNTIL condition  
// Java: do-while loop (postcondition)
do {
    processRecord();
} while (hasMoreRecords());
```

### 8. 検索・ソートの変換

```java
// COBOL: SEARCH MONTHLY-SALES WHEN month-name(index) = 'December'
// Java: Stream filtering
Optional<MonthlySalesData> december = monthlySales.stream()
    .filter(month -> month.getMonthName().equals("December"))
    .findFirst();

if (december.isPresent()) {
    System.out.println("Found December: " + december.get().getMonthSales());
}

// COBOL: SORT SORT-WORKFILE ON ASCENDING KEY field
// Java: Stream sorting
List<SalesRecord> sortedSales = salesRecords.stream()
    .sorted((s1, s2) -> s1.getTotalAmount().compareTo(s2.getTotalAmount()))
    .collect(Collectors.toList());
```

### 9. レポート出力の変換

```java
// COBOL: WRITE-LOG paragraph / DISPLAY message
// Java: Logging and PrintWriter
private void writeReportHeader(PrintWriter writer) {
    writer.println("=====================================");
    writer.println("COBOL Comprehensive Sample Report");
    writer.println("Generated: " + LocalDateTime.now().format(format));
    writer.println();
}

// COBOL: LINE-COUNT management / PAGE-BREAK
// Java: Page control logic
if (lineCount >= linesPerPage) {
    writer.println("\f"); // Form feed
    writeReportHeader(writer);
    lineCount = 1;
}
writer.println(dataLine);
lineCount++;
```

## 実装の特徴

### 1. Object-Oriented Design
- COBOL のレコード構造をJavaクラスに変換
- データとメソッドをカプセル化
- 継承やポリモーフィズムの活用

### 2. Collection Framework
- List<T>, Map<K,V>の使用
- Stream API による関数型プログラミング
- Iterator や Enhanced for-loop の活用

### 3. Exception Handling
- try-with-resources文による自動リソース管理
- IOExceptionの適切な処理
- ArithmeticExceptionの捕捉

### 4. BigDecimal活用
- 金融計算の精度保証
- カスタム丸めモードの設定
- COBOLのPIC V句に相当する小数点処理

### 5. Modern Java Features
- LocalDateTime による日時処理
- String.format() による表示制御
- StringBuilder による効率的な文字列結合

## 実行方法

### Windows環境
```cmd
cd C:\devlop\cobol\java
run_comprehensive_sample.bat
```

### Javaコマンドライン実行
```bash
# 1. コンパイル
javac ComprehensiveSample.java
javac ComprehensiveSampleRunner.java

# 2. サンプルデータ作成とプログラム実行
java ComprehensiveSampleRunner

# 3. 直接実行（データファイルが存在する場合）
java ComprehensiveSample
```

## 実行結果例

```
=====================================
COBOL Comprehensive Sample Program
Java Implementation
=====================================

Setting up Java environment...
Compiling Java programs...
✓ Compilation completed

Executing comprehensive sample program...
Initializing program...
✓ Monthly sales table initialized

Processing Customer Master File...
✓ Customer processing completed (3 records)

Processing Sales Transaction File...
✓ Sales processing completed (5 transactions)

Generating Statistics...
Financial Summary:
==================
Gross Sales:     $5,675.00
Calculated Tax:   $454.00
Commission:       $283.75
Weighted Average: $2,610.50

Demonstrating String Operations...
✓ String manipulation completed

Demonstrating Conditional Logic...
✓ Customer evaluation completed

Demonstrating Loop Constructs...
✓ Loop processing completed

Generating Detailed Report...
✓ Report file created: DAILY-REPORT.TXT
```

## COBOL vs Java比較表

| 特性 | COBOL | Java Implementation |
|------|-------|--------------------|
| **型安全性** | 実行時チェック | コンパイル時チェック ✓ |
| **メモリ管理** | 手動管理 | 自動GC ✓ |
| **ポータビリティ** | プラットフォーム依存 | JVM搭載ホスト ✓ |
| **デバッグ** | Print文中心 | IDE/Debugger ✓ |
| **テスト** | 手動テスト | JUnit/自動化 ✓ |
| **パフォーマンス** | 高速（最適化済み） | JVM最適化 ✓ |
| **保守性** | 高い（標準化） | 高い（OO設計） ✓ |
| **開発効率** | 中程度 | 高い（ツール支援） ✓ |

## まとめ

この完全移植により、以下が実現されています：

1. **機能等価性**: COBOLの全機能をJavaで再現
2. **データ整合性**: 数値計算精度の保持
3. **エラー処理**: 適切な例外処理の実装
4. **拡張性**: OOPによる機能拡張への対応
5. **保守性**: コードの可読性と理解しやすさ
6. **テスト容易性**: JUnit等テストフレームワークとの統合

COBOLスキルをJava移行に活用する際の具体的な技法と実装例が完全に網羅されており、レガシーシステムの現代化に直結する実践的な学習リソースとなっています。

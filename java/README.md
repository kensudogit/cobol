# COBOL非インタラクティブ処理 - Java版

## 概要

このディレクトリには、COBOLの非インタラクティブ処理サンプルをJavaに変換したプログラムが含まれています。

## ファイル構成

### Javaソースファイル
- `NonInteractiveSample.java` - ファイルベース入力処理のJava版
- `BatchProcessingSample.java` - バッチ処理のJava版
- `AutomatedDataGeneration.java` - 自動データ生成のJava版

### 設定ファイル
- `pom.xml` - Mavenプロジェクト設定
- `config/sample_config.dat` - サンプル設定ファイル

### 実行スクリプト
- `run_non_interactive_samples.bat` - Windows用実行スクリプト
- `run_non_interactive_samples.sh` - Linux/Mac用実行スクリプト

## 機能

### 1. NonInteractiveSample.java
- **機能**: ファイルベース入力処理
- **特徴**:
  - 設定ファイルからのパラメータ読み込み
  - 入力データファイルの自動処理
  - ログファイルへの自動出力
  - エラーハンドリングとリトライ機能
  - 統計情報の自動収集

### 2. BatchProcessingSample.java
- **機能**: バッチ処理
- **特徴**:
  - コマンドライン引数の処理
  - 環境変数の読み込み
  - エラーハンドリングとリトライ機能
  - 処理結果の自動通知
  - バッチIDの自動生成

### 3. AutomatedDataGeneration.java
- **機能**: 自動データ生成
- **特徴**:
  - ランダムデータの生成
  - データパターンの自動適用
  - 大量データの一括生成
  - データ検証の自動化
  - 統計情報の自動計算

## 実行方法

### 前提条件
- Java 11以上
- Maven 3.6以上

### 1. プロジェクトのビルド
```bash
mvn clean compile
```

### 2. テストの実行
```bash
mvn test
```

### 3. 実行可能JARの作成
```bash
mvn clean package
```

### 4. 個別プログラムの実行

#### Non-Interactive Sample
```bash
mvn exec:java -Dexec.mainClass="com.example.cobol.NonInteractiveSample"
```

#### Batch Processing Sample
```bash
mvn exec:java -Dexec.mainClass="com.example.cobol.BatchProcessingSample"
```

#### Automated Data Generation
```bash
mvn exec:java -Dexec.mainClass="com.example.cobol.AutomatedDataGeneration"
```

### 5. 実行スクリプトの使用

#### Windows
```cmd
run_non_interactive_samples.bat
```

#### Linux/Mac
```bash
chmod +x run_non_interactive_samples.sh
./run_non_interactive_samples.sh
```

## 設定ファイル

### CONFIG.DAT
```
TAX_RATE 0.08
COMMISSION_RATE 0.05
MINIMUM_AMOUNT 100.00
MAXIMUM_AMOUNT 999999.99
PROCESSING_LIMIT 1000
```

### PARAMS.DAT
```
MODE,A
RETRY_COUNT,3
```

### INPUT.DAT
```
CUSTOMER001,John Smith,1500.50,20250106,Y
CUSTOMER002,Jane Doe,2300.75,20250106,A
CUSTOMER003,Bob Johnson,850.25,20250106,Y
CUSTOMER004,Alice Brown,3200.00,20250106,A
CUSTOMER005,Charlie Wilson,1200.00,20250106,Y
```

## Java版の特徴

### 1. オブジェクト指向設計
- データクラスの分離
- メソッドの適切な分割
- カプセル化の実装

### 2. 例外処理
- try-catch文による適切な例外処理
- ファイルI/Oエラーの処理
- データ形式エラーの処理

### 3. コレクション使用
- List、MapなどのJavaコレクション活用
- 型安全性の向上

### 4. 日時処理
- LocalDateTimeクラスの使用
- DateTimeFormatterによる日時フォーマット

### 5. 並行処理
- AtomicInteger、AtomicLongによるスレッドセーフな処理
- ThreadLocalRandomによるランダム数値生成

## COBOLとの比較

| 項目 | COBOL | Java |
|------|-------|------|
| ファイル処理 | OPEN/READ/CLOSE | Files.newBufferedReader |
| 文字列操作 | STRING/UNSTRING | String.split/format |
| 数値計算 | COMPUTE | Math.round |
| 日時処理 | ACCEPT FROM DATE | LocalDateTime.now() |
| エラー処理 | IF文 | try-catch文 |
| データ構造 | 01レベルの定義 | クラス定義 |

## トラブルシューティング

### よくある問題

1. **ファイルが見つからない**
   - ファイルパスの確認
   - ファイルの存在確認
   - 権限の確認

2. **Mavenの依存関係エラー**
   - `mvn clean install`の実行
   - ネットワーク接続の確認

3. **Javaバージョンエラー**
   - Java 11以上の確認
   - JAVA_HOME環境変数の設定

4. **メモリ不足**
   - JVMヒープサイズの調整
   - `-Xmx`オプションの使用

## 拡張機能

### 1. データベース連携
- JDBCドライバーの追加
- データベース接続の実装

### 2. Web API連携
- HTTPクライアントの追加
- REST API呼び出しの実装

### 3. ログ機能の強化
- Logback設定のカスタマイズ
- ログレベルの動的変更

### 4. 設定管理の改善
- Propertiesファイルの使用
- 環境別設定の実装

## ライセンス

このプロジェクトは教育目的で作成されています。
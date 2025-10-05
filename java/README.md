# COBOL to Java Migration

このディレクトリには、COBOLプログラムをJavaに移植したファイルが含まれています。

##。

### 1. **Employee.java**
**元のCOBOLファイル:** `src/employee.cob`
- **機能:** 従業員情報管理システム
- **特徴:** メニュー型アプリケーション、従業員情報の登録・表示・管理
- **移植のポイント:**
  - COBOLのレコード構造体をJavaクラスに変換
  - WORKING-STORAGE SECTIONをクラスフィールドに変換
  - PROCEDURE DIVISIONをメソッドに分割
  - EVALUATE文をswitch文に変換

**元のCOBOLファイル:** `samples/calculator.cob`
- **機能:** 四則演算電卓プログラム
- **特徴:** ユーザー入力による計算処理
- **移植のポイント:**
  - 数値計算にBigDecimalを使用（COBOLの固定小数点に対応）
  - COMPUTE文をJavaの演算子に変換
  - ゼロ除算エラーハンドリング

### 3. **HelloWorld.java**
**元のCOBOLファイル:** `samples/hello.cob`
- **機能:** Hello Worldプログラム
- **特徴:** メッセージ表示とループ処理のデモンストレーション
- **移植のポイント:**
  - COBOLのDISPLAY文をSystem.out.printlnに変換
  - PERFORM VARYING文をfor文に変換

### 4. FileIO.java
**元のCOBOLファイル:** `samples/fileio.cob`
- **機能:** ファイル入出力プログラム
- **特徴:** input.txt読み込み、output.txt書き出し
- **移植のポイント:**
  - COBOLのファイル操作をJavaのBufferedReader/BufferedWriterに変換
  - FILE-CONTROL節の概念をJavaのファイル操作に変換
  - AT END/NOT AT ENDの処理をwhile文に変換

### 5. **LambdaFunction.java** ⭐ NEW
**元のCOBOLファイル:** `java/lambda_function.cob`
- **機能:** AWS Lambda関数（コンテンツ検索・管理システム）
- **特徴:** 
  - HTTP リクエスト処理（GET, POST, OPTIONS）
  - コンテンツ検索機能
  - カテゴリ管理
  - 外部コンテンツ検索
  - ヘルスチェック
  - レート制限管理
- **移植のポイント:**
  - COBOLのラムダ関数をAWS Lambda Java SDKに変換
  - PROCEDURE DIVISION の各段落をJavaメソッドに分割
  - COBOLの環境変数をシステム環境変数に変換

## コンパイルと実行

### 前提条件
- Java Development Kit (JDK) 8以上が必要です

### コンパイル
```bash
javac *.java
```

### 実行例
```bash
# 従業員管理システム
java Employee

# 電卓プログラム
java Calculator

# Hello World
java HelloWorld

# ファイルI/O（input.txtファイルが必要）
java FileIO

# Lambda関数（クラスパスにaws-lambda-java-coreが必要）
# Maven実行: mvn exec:java -Dexec.mainClass="LambdaFunction"
```

## COBOLからJavaへの主な変換パターン

### データ型の変換
| COBOL | Java |
|-------|------|
| PIC 9(n) | int, long |
| PIC 9(n)V99 | BigDecimal |
| PIC X(n) | String |
| PIC X(1) | char, boolean |

### 制御構造の変換
| COBOL | Java |
|-------|------|
| PERFORM UNTIL | while/do-while |
| PERFORM VARYING | for |
| EVALUATE | switch |
| IF-ELSE-END-IF | if-else |

### ファイル操作の変換
| COBOL | Java |
|-------|------|
| OPEN INPUT/OUTPUT | FileReader/FileWriter |
| READ | BufferedReader.readLine() |
| WRITE | BufferedWriter.write() |
| CLOSE | close() |

## 追加ファイル

### 6. **pom.xml**
- Maven プロジェクト設定ファイル
- AWS Lambda Java SDK の依存関係定義
- ビルド設定とパッケージング設定

### 7. **DEPLOYMENT_GUIDE.md**
- AWS Lambda へのデプロイメント手順
- API Gateway の設定方法
- 環境変数の設定手順
- トラブルシューティングガイド

## 注意事項
- FileIO.javaを実行するには、同ディレクトリにinput.txtファイルを作成してください
- LambdaFunction.javaは AWS Lambda 環境での実行を前提としています
- すべてのプログラムは入力検証を行っていますが、予期しない入力に対しては適切なエラーメッセージを表示します
- COBOLの固定幅フィールドの概念は、Javaでは文字列処理で実装されています
- Lambda関数のデプロイには、DEPLOYMENT_GUIDE.mdを参照してください

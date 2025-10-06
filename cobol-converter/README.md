# COBOL to Java Converter

React + Vite + TypeScript環境で構築されたCOBOLプログラムをJavaプログラムに変換するWebアプリケーションです。

## 🚀 機能

- **ファイルアップロード**: .cob/.cblファイルのドラッグ&ドロップ対応
- **リアルタイム変換**: COBOLコードをリアルタイムでJavaコードに変換
- **コード編集**: ブラウザ上でCOBOLコードの編集が可能
- **ダウンロード**: 変換されたJavaファイルのダウンロード機能
- **エラーハンドリング**: 変換エラーの詳細表示

## 📋 対応するCOBOL構文

### DIVISION
- `IDENTIFICATION DIVISION` - プログラム名の抽出
- `ENVIRONMENT DIVISION` - 環境設定
- `DATA DIVISION` - 変数定義
- `PROCEDURE DIVISION` - 処理ロジック

### データ定義
- `PICTURE`句 - データ型の判定
- `VALUE`句 - 初期値の設定
- レベル番号による階層構造

### 処理文
- `DISPLAY` - System.out.printlnへの変換
- `MOVE` - 代入文への変換
- `IF/END-IF` - 条件分岐の変換
- `PERFORM` - メソッド呼び出しへの変換

## 🚫 現在の制約事項と未対応機能

### 1. データ定義の制約

#### ❌ 未対応のPIC句
```cobol
05 SIGNED-NUMBER         PIC S9(5)         VALUE -12345.   ❌ 未対応
05 LEADING-ZEROS         PIC 09(5)        VALUE ZEROES.   ❌ 未対応
05 TRAILING-ZEROS       PIC 9(5)0         VALUE ZEROES.   ❌ 未対応
05 EDIT-MASK            PIC ZZZ,ZZZ.99   VALUE ZEROES.   ❌ 未対応
05 NATIONAL-FIELD       PIC N(20)         VALUE SPACES.   ❌ 未対応
```

#### ❌ 未対応のUSAGE句
```cobol
05 BINARY-NUMBER        PIC 9(5) COMP     VALUE 100.      ❌ 未対応
05 PACKED-DECIMAL       PIC 9(7)V99 COMP-3 VALUE 12345.67. ❌ 未対応
05 FLOATING-POINT        PIC 9(7)V99 COMP-1 VALUE 12345.67. ❌ 未対応
05 DOUBLE-PRECISION      PIC 9(7)V99 COMP-2 VALUE 12345.67. ❌ 未対応
```

### 2. ファイル処理の制約

#### ❌ 未対応のファイル編成
```cobol
SELECT SALES-TRANSACTIONS
    ASSIGN TO 'SALES.TRAN'
    ORGANIZATION IS RELATIVE           ❌ 未対応
    ACCESS MODE IS SEQUENTIAL          ❌ 未対応
    RELATIVE KEY IS REL-KEY-ANDEX      ❌ 未対応
    STATUS IS FILE-STATUS.

SELECT REPORT-OUTPUT
    ASSIGN TO 'DAILY-REPORT.TXT'
    ORGANIZATION IS LINE SEQUENTIAL    ❌ 未対応
    STATUS IS FILE-STATUS.
```

#### ❌ 未対応のファイル操作
```cobol
OPEN INPUT CUSTOMER-MASTER             ❌ 未対応
OPEN OUTPUT REPORT-OUTPUT              ❌ 未対応
OPEN I-O SALES-TRANSACTIONS            ❌ 未対応
READ CUSTOMER-MASTER AT END            ❌ 未対応
WRITE REPORT-LINE                      ❌ 未対応
CLOSE CUSTOMER-MASTER                  ❌ 未対応
```

### 3. 配列・テーブル処理の制約

#### ❌ 未対応のOCCURS句
```cobol
01 SALES-TABLE.
   05 SALES-MONTHLY OCCURS 12 TIMES.   ❌ 未対応
       10 MONTH-NAME    PIC X(10).
       10 SALES-AMOUNT  PIC 9(7)V99.
       10 CUSTOMER-COUNT PIC 9(4).

01 PRODUCT-TABLE OCCURS 100 TIMES       ❌ 未対応
    INDEXED BY PRODUCT-INDEX.           ❌ 未対応
```

#### ❌ 未対応の表組み制御
```cobol
SEARCH SALES-TABLE                     ❌ 未対応
    WHEN SALES-MONTHLY(PRODUCT-INDEX) = SEARCH-VALUE
        PERFORM PROCESS-FOUND-ITEM
END-SEARCH

SET PRODUCT-INDEX TO 1                  ❌ 未対応
PERFORM VARYING PRODUCT-INDEX          ❌ 未対応
    FROM 1 BY 1 UNTIL PRODUCT-INDEX > 100
    PERFORM PROCESS-PRODUCT
END-PERFORM
```

### 4. 文字列操作の制約

#### ❌ 未対応の文字列操作
```cobol
STRING REPORT-TITLE DELIMITED BY SIZE  ❌ 未対応
      ' - ' DELIMITED BY SIZE
      CURRENT-DATE DELIMITED BY SIZE
         INTO REPORT-SUBTITLE
         ON OVERFLOW
             DISPLAY 'String operation overflow'
END-STRING

UNSTRING TRANSACTION-DATE DELIMITED BY ALL SPACES  ❌ 未対応
         INTO YEAR-PART, MONTH-PART, DAY-PART
         ON OVERFLOW
             DISPLAY 'Unstring operation overflow'
END-UNSTRING

INSPECT CUSTOMER-NAME TALLYING        ❌ 未対応
        COUNT-OF-A FOR CHARACTERS BEFORE INITIAL '-'

INSPECT CUSTOMER-NAME REPLACING ALL SPACES BY '-'  ❌ 未対応
```

### 5. 算術演算の制約

#### ❌ 未対応の算術演算
```cobol
COMPUTE GROSS-SALES = GROSS-SALES - CALCULATED-TAX  ❌ 未対応

DIVIDE GROSS-SALES BY 2 GIVING WEIGHTED-AVERAGE    ❌ 未対応
        ROUNDED MODE ROUND-HALF-EVEN

COMPUTE PERCENTAGE-VALUE =                          ❌ 未対応
    (CALCULATED-COMMISSION / GROSS-SALES) * 100
        ROUNDED MODE ROUND-HALF-EVEN
```

### 6. 条件分岐の制約

#### ❌ 未対応の条件分岐
```cobol
EVALUATE TRUE                          ❌ 未対応
    WHEN CREDIT-LIMIT GREATER THAN OR EQUAL TO 50000
        DISPLAY 'Premium customer'
        PERFORM PREMIUM-CUSTOMER-PROCESSING
    WHEN CREDIT-LIMIT GREATER THAN OR EQUAL TO 10000
        DISPLAY 'Standard customer'
        PERFORM STANDARD-CUSTOMER-PROCESSING
    WHEN OTHER
        DISPLAY 'Credit limit not set'
END-EVALUATE

IF (CUSTOMER-STATUS = 'A') AND (CREDIT-LIMIT > 0)  ❌ 未対応
    PERFORM PROCESS-VALID-CUSTOMER
END-IF
```

### 7. ループ処理の制約

#### ❌ 未対応のループ構文
```cobol
PERFORM VARYING WS-COUNTER FROM 1 BY 1  ❌ 未対応
    UNTIL WS-COUNTER > 5
    DISPLAY 'Counter: ' WS-COUNTER
END-PERFORM

PERFORM PROCESS-ITEM                    ❌ 未対応
    VARYING INDEX-VAR FROM 1 BY 1
    UNTIL INDEX-VAR > MAX-COUNT
    AFTER SUB-INDEX FROM 1 BY 1
    UNTIL SUB-INDEX > SUB-MAX
END-PERFORM
```

### 8. ソート処理の制約

#### ❌ 未対応のソート機能
```cobol
SORT SORT-WORKFILE                      ❌ 未対応
    ON ASCENDING KEY SORT-CUSTOMER-ID
    ON DESCENDING KEY SORT-SALES-TOTAL
    USING SORT-INFILE
    GIVING SORT-OUTFILE
```

### 9. レポート機能の制約

#### ❌ 未対応のレポート機能
```cobol
WRITE REPORT-LINE FROM REPORT-HEADER    ❌ 未対応
    AFTER ADVANCING PAGE

WRITE REPORT-LINE FROM DETAIL-LINE      ❌ 未対応
    AFTER ADVANCING 2 LINES

WRITE REPORT-LINE FROM TOTAL-LINE       ❌ 未対応
    AFTER ADVANCING 3 LINES
```

## 🔧 手動対応が必要な主要項目

### 1. データ型の拡張
- **SIGNED数値型の対応**: `PIC S9(5)`形式の符号付き数値
- **COMP、COMP-1、COMP-2、COMP-3の実装**: バイナリ、パック形式、浮動小数点
- **編集マスクの対応**: `Z`、`*`、`$`、カンマ等の表示形式

### 2. ファイル処理の完全実装
- **相対編成ファイルの対応**: `ORGANIZATION IS RELATIVE`
- **インデックス編成ファイルの対応**: `ORGANIZATION IS INDEXED`
- **ファイルのOPEN/CLOSE処理**: `OPEN INPUT/OUTPUT/I-O`
- **READ/WRITE文の詳細実装**: `AT END`、`INVALID KEY`等の条件処理

### 3. 配列・テーブル処理
- **OCCURS句の完全対応**: 多次元配列、可変長配列
- **INDEXED BY句の実装**: インデックス変数の管理
- **SEARCH文の実装**: 線形検索、二分検索
- **SET文の実装**: インデックス値の設定

### 4. 文字列操作の実装
- **STRING文の実装**: 文字列の結合処理
- **UNSTRING文の実装**: 文字列の分割処理
- **INSPECT文の実装**: 文字列の検索、置換、カウント

### 5. 高度な算術演算
- **ROUNDED句の実装**: 四捨五入処理
- **MODE句の実装**: 丸めモードの指定
- **複雑な算術式の解析**: 括弧、優先順位の処理

### 6. 条件分岐の拡張
- **EVALUATE文の実装**: 多分岐条件処理
- **複合条件の詳細対応**: `AND`、`OR`、`NOT`の組み合わせ

### 7. ループ処理の完全実装
- **PERFORM VARYING文の実装**: カウンタ付きループ
- **ネストしたループの対応**: `AFTER`句による多重ループ

### 8. ソート・レポート機能
- **SORT文の実装**: ファイルの並び替え処理
- **レポート制御の実装**: ページ制御、行制御

## 📊 変換精度の目安

| 機能カテゴリ | 対応率 | 備考 |
|------------|--------|------|
| 基本データ定義 | 70% | PIC句の基本形式のみ対応 |
| ファイル処理 | 20% | SELECT文の基本形式のみ対応 |
| 配列・テーブル | 10% | OCCURS句は未対応 |
| 文字列操作 | 5% | STRING/UNSTRING/INSPECT未対応 |
| 算術演算 | 30% | 基本演算のみ対応 |
| 条件分岐 | 40% | IF文の基本形式のみ対応 |
| ループ処理 | 20% | PERFORM文の基本形式のみ対応 |
| ソート・レポート | 0% | 未対応 |

## 🎯 推奨される拡張順序

1. **基本データ型の拡張** - SIGNED数値、COMP形式
2. **ファイル処理の実装** - OPEN/CLOSE/READ/WRITE
3. **配列処理の実装** - OCCURS句、SEARCH文
4. **文字列操作の実装** - STRING/UNSTRING/INSPECT
5. **高度な算術演算** - ROUNDED句、複雑な式
6. **条件分岐の拡張** - EVALUATE文
7. **ループ処理の完全実装** - PERFORM VARYING
8. **ソート・レポート機能** - SORT文、レポート制御

## 🛠️ 技術スタック

- **Frontend**: React 18 + TypeScript
- **Build Tool**: Vite
- **Styling**: CSS3 (レスポンシブデザイン)
- **File Handling**: FileReader API

## 📦 インストールと起動

### 前提条件
- Node.js 16.0以上
- npm または yarn

### セットアップ
```bash
# プロジェクトディレクトリに移動
cd C:\devlop\cobol\cobol-converter

# 依存関係のインストール
npm install

# 開発サーバーの起動
npm run dev
```

### ビルド
```bash
# プロダクションビルド
npm run build

# ビルド結果のプレビュー
npm run preview
```

## 🎯 使用方法

1. **COBOLファイルの選択**
   - .cob/.cblファイルをドラッグ&ドロップ
   - または「ファイルを選択」ボタンをクリック

2. **コードの確認・編集**
   - 左側のテキストエリアでCOBOLコードを確認
   - 必要に応じてコードを編集

3. **変換実行**
   - 「変換実行」ボタンをクリック
   - 変換処理が実行されます

4. **結果の確認**
   - 右側に生成されたJavaコードが表示
   - エラーや警告があれば下部に表示

5. **ファイルのダウンロード**
   - 「Javaファイルをダウンロード」ボタンで.javaファイルを保存

## 📁 プロジェクト構造

```
cobol-converter/
├── src/
│   ├── App.tsx              # メインアプリケーションコンポーネント
│   ├── main.tsx             # アプリケーションのエントリーポイント
│   ├── index.css            # スタイルシート
│   └── cobolConverter.ts    # COBOLパーサーとJavaコンバーター
├── public/                  # 静的ファイル
├── index.html              # HTMLテンプレート
├── vite.config.ts          # Vite設定
├── tsconfig.json           # TypeScript設定
└── package.json            # プロジェクト設定
```

## 🔧 カスタマイズ

### COBOLパーサーの拡張
`src/cobolConverter.ts`の`CobolParser`クラスを拡張して、より多くのCOBOL構文に対応できます。

### Javaコンバーターの拡張
`src/cobolConverter.ts`の`JavaConverter`クラスを拡張して、より高度なJavaコード生成が可能です。

### UIのカスタマイズ
`src/index.css`でスタイルを変更し、`src/App.tsx`でUIコンポーネントをカスタマイズできます。

## 🐛 トラブルシューティング

### よくある問題

1. **ファイルが読み込めない**
   - ファイル形式が.cob/.cblであることを確認
   - ファイルが破損していないか確認

2. **変換エラーが発生する**
   - COBOLコードの構文を確認
   - 対応していない構文がないか確認

3. **開発サーバーが起動しない**
   - Node.jsのバージョンを確認
   - `npm install`を再実行

## 📝 ライセンス

このプロジェクトはMITライセンスの下で公開されています。

## 🤝 貢献

バグ報告や機能要望は、GitHubのIssuesでお知らせください。

---

**注意**: このツールは教育・学習目的で作成されています。本格的なCOBOLプログラムの変換には、より高度なパーサーとコンバーターが必要な場合があります。

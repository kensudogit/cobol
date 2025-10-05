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

## ライセンス

この開発環境は教育目的で提供されています。商用利用の際は、使用するライブラリやツールのライセンスを確認してください。
"# cobol" 

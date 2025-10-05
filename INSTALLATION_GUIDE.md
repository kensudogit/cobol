# GnuCOBOL インストールガイド

## Windows環境でのGnuCOBOLインストール手順

### 方法1: 公式サイトからダウンロード

1. **GnuCOBOLのダウンロード**
   - [GnuCOBOL公式サイト](https://sourceforge.net/projects/gnucobol/)にアクセス
   - Windows用の最新版をダウンロード（例: `gnucobol-3.2_win.7z`）

2. **解凍と配置**
   ```
   C:\gnucobol\ に解凍
   ```

3. **環境変数の設定**
   - `Path`環境変数に追加: `C:\gnucobol\bin`
   - 新しい環境変数を作成:
     - 変数名: `COB_CONFIG_DIR`
     - 値: `C:\gnucobol\config`

### 方法2: Chocolateyを使用

1. **Chocolateyのインストール**
   ```cmd
   # PowerShellを管理者権限で実行
   Set-ExecutionPolicy Bypass -Scope Process -Force; [System.Net.ServicePointManager]::SecurityProtocol = [System.Net.ServicePointManager]::SecurityProtocol -bor 3072; iex ((New-Object System.Net.WebClient).DownloadString('https://community.chocolatey.org/install.ps1'))
   ```

2. **GnuCOBOLのインストール**
   ```cmd
   choco install gnucobol
   ```

### 方法3: MSYS2を使用

1. **MSYS2のインストール**
   - [MSYS2公式サイト](https://www.msys2.org/)からインストーラーをダウンロード

2. **GnuCOBOLのインストール**
   ```cmd
   pacman -S mingw-w64-x86_64-gnucobol
   ```

## インストール確認

コマンドプロンプトで以下を実行:

```cmd
cobc --version
```

正常にインストールされていれば、バージョン情報が表示されます。

## トラブルシューティング

### PATHが通らない場合
1. システム環境変数の設定を確認
2. コマンドプロンプトを再起動
3. 管理者権限でコマンドプロンプトを実行

### コンパイルエラーが発生する場合
1. GnuCOBOLのバージョンを確認
2. ファイルエンコーディングをUTF-8に設定
3. ファイルの改行コードをLFに統一

## 次のステップ

インストール完了後、以下のコマンドで開発環境をテストしてください:

```cmd
cd C:\devlop\cobol
setup.bat
scripts\build.bat
scripts\test.bat
```

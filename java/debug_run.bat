@echo off
echo ========================================
echo ComprehensiveSample デバッグ実行
echo ========================================

cd /d C:\devlop\cobol\java

echo.
echo 1. Javaファイルのコンパイル...
javac -cp . com\example\cobol\*.java

if %errorlevel% neq 0 (
    echo コンパイルエラーが発生しました
    pause
    exit /b 1
)

echo コンパイル完了
echo.

echo 2. デバッグ実行開始...
echo.
java -cp . com.example.cobol.ComprehensiveSampleDebugger

echo.
echo 3. 生成されたファイルを確認...
if exist DAILY-REPORT.TXT (
    echo ✅ レポートファイルが生成されました: DAILY-REPORT.TXT
) else (
    echo ❌ レポートファイルが生成されませんでした
)

if exist lambda.log (
    echo ✅ ログファイルが生成されました: lambda.log
) else (
    echo ❌ ログファイルが生成されませんでした
)

echo.
echo デバッグ実行完了
pause

@echo off
echo ========================================
echo LambdaFunction デバッグ実行
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

echo 2. LambdaFunction デバッグ実行開始...
echo.
java -cp . com.example.cobol.LambdaFunction

echo.
echo 3. 生成されたファイルを確認...
if exist lambda.log (
    echo ✅ ログファイルが生成されました: lambda.log
    echo.
    echo ログファイルの内容:
    type lambda.log
) else (
    echo ❌ ログファイルが生成されませんでした
)

echo.
echo デバッグ実行完了
pause

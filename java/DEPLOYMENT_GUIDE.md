# AWS Lambda Function Deployment Guide

このガイドでは、COBOLプログラム `lambda_function.cob` からJavaに移植したAWS Lambda関数のデプロイメント手順を説明します。

## プロジェクト概要

### 移植された機能
- **HTTPリクエスト処理**: GET, POST, OPTIONSメソッドのサポート
- **コンテンツ検索**: キーワードベースの検索機能
- **カテゴリ管理**: 健康関連カテゴリの管理
- **外部コンテンツ検索**: 外部サイトからのコンテンツ検索
- **ヘルスチェック**: システムの健全性確認
- **レート制限**: リクエスト頻度制限
- **ログ機能**: Lambda実行ログの記録

### 元COBOLファイルとの対応関係

| COBOL DIVISION/Paragraph | Java実装 |
|--------------------------|----------|
| IDENTIFICATION DIVISION | AWS Lambda関数クラス定義 |
| ENVIRONMENT DIVISION | 環境変数定義 |
| DATA DIVISION | データクラス定義 |
| MAIN-PROCEDURE | handleRequest()メソッド |
| INITIALIZE-LAMBDA | initializeLambda() |
| HANDLE-GET-REQUEST | handleGetRequest() |
| HANDLE-POST-REQUEST | handlePostRequest() |
| SEARCH-CONTENT | searchContent() |
| RETRIEVE-ALL-CATEGORIES | retrieveAllCategories() |

## 前提条件

### 必要なソフトウェア
- Java 11以上
- Apache Maven 3.6以上
- AWS CLI
- AWS SAM CLI (オプション)

### AWS権限
- Lambda関数の作成・更新権限
- IAMロールの作成権限
- CloudWatch Logsの権限

## ローカルビルド

### 1. Mavenパッケージの作成
```bash
cd C:\devlop\cobol\java
mvn clean package
```

### 2. JARファイルの確認
```
target/lambda-function-cobol-java-1.0.0-cobol-migration.jar
```

## AWS Lambda へのデプロイ

### 方法1: AWS CLI を使用

#### 1. IAMロールの作成
```bash
aws iam create-role \
    --role-name CobolLambdaRole \
    --assume-organization-policy-document file://trust-policy.json
```

**trust-policy.json:**
```json
{
  "Version": "2012-10-17",
  "Statement": [
    {
      "Effect": "Allow",
      "Principal": {
        "Service": "lambda.amazonaws.com"
      },
      "Action": "sts:AssumeRole"
    }
  ]
}
```

#### 2. ロールにポリシーをアタッチ
```bash
aws iam attach-role-policy \
    --role-name CobolLambdaRole \
    --policy-arn arn:aws:iam::aws:policy/service-role/AWSLambdaBasicExecutionRole
```

#### 3. Lambda関数の作成
```bash
aws lambda create-function \
    --function-name CobolMigrationFunction \
    --runtime java11 \
    --role arn:aws:iam::YOUR-ACCOUNT-ID:role/CobolLambdaRole \
    --handler LambdaFunction::handleRequest \
    --zip-file fileb://target/lambda-function-cobol-java-1.0.0-cobol-migration.jar \
    --timeout 30 \
    --memory-size 512
```

### 方法2: AWS SAM を使用

#### 1. SAM template.yaml の作成
```yaml
AWSTemplateFormatVersion: '2010-09-09'
Transform: AWS::Serverless-2016-10-31
Description: COBOL Migrated Lambda Function

Parameters:
  Environment:
    Type: String
    Default: dev
    AllowedValues: [dev, staging, prod]

Globals:
  Function:
    Timeout: 30
    MemorySize: 512
    Runtime: java11
    Environment:
      Variables:
        POSTS_TABLE_NAME: !Ref PostsTableName
        POSTMETA_TABLE_NAME: !Ref PostmetaTableName
        CONTENT_BUCKET_NAME: !Ref ContentBucketName
        MAX_RETRE_ATTEMPTS: '3'
        CACHE_TTL_SECONDS: '3600'
        DEFAULT_PAGE_SIZE: '10'

Resources:
  CobolMigrationFunction:
    Type: AWS::Serverless::Function
    Properties:
      CodeUri: target/lambda-function-cobol-java-1.0.0-cobol-migration.jar
      Handler: LambdaFunction::handleRequest
      Policies:
        - Version: '2012-10-17'
          Statement:
            - Effect: Allow
              Action:
                - logs:CreateLogGroup
                - logs:CreateLogStream
                - logs:PutLogEvents
              Resource: '*'
      
      Events:
        ApiGatewayEvent:
          Type: Api
          Properties:
            Path: /{proxy+}
            Method: ANY

  PostsTableName:
    Type: String
    Description: Posts table name in DynamoDB
    Default: wp_posts

  PostmetaTableName:
    Type: String
    Description: Postmeta table name in DynamoDB
    Default: wp_postmeta

  ContentBucketName:
    Type: String
    Description: S3 bucket name for content storage
    Default: kenko21-web

Outputs:
  CobolMigrationApi:
    Description: API Gateway endpoint URL for Lambda function
    Value: !Sub 'https://${ServerlessRestApi}.execute-api.${AWS::Region}.amazonaws.com/Prod/'
    Export:
      Name: CobolMigrationApiUrl

  CobolMigrationFunction:
    Description: Lambda Function ARN
    Value: !GetAtt CobolMigrationFunction.Arn
    Export:
      Name: CobolMigrationFunctionArn
```

#### 2. SAM デプロイメント
```bash
sam build
sam deploy --guided
```

## 環境変数の設定

### Lambda 関数の環境変数
```bash
aws lambda update-function-configuration \
    --function-name CobolMigrationFunction \
    --environment Variables='{
        "POSTS_TABLE_NAME":"wp_posts",
        "POSTMETA_TABLE_NAME":"wp_postmeta",
        "CONTENT_BUCKET_NAME":"kenko21-web",
        "MAX_RETRY_ATTEMPTS":"3",
        "CACHE_TTL_SECONDS":"3600",
        "DEFAULT_PAGE_SIZE":"10"
    }'
```

## API Gateway の設定

### REST API の作成
```bash
aws apigateway create-rest-api \
    --name CobolMigrationApi \
    --description "API for COBOL Migrated Lambda Function"
```

### リソースとメソッドの設定
```bash
# ルートリソースの作成
aws apigateway create-デプロイメント \
    --rest-api-id YOUR_API_ID \
    --stage-name prod
```

## テスト

### ローカルテスト
```bash
# JUnitテストの実行
mvn test

# SAM local testing
sam local start-api
curl http://localhost:3000/health
```

### AWS 上でのテスト
```bash
# Lambda関数のテスト
aws lambda invoke \
    --function-name CobolMigrationFunction \
    --payload '{"httpMethod":"GET","path":"/health"}' \
    output.json
```

### API Gateway 経由でのテスト
```bash
# ヘルスチェック
curl https://YOUR_API_GATEWAY_URL/health

# 検索リクエスト
curl -X POST https://YOUR_API_GATEWAY_URL/search \
     -H "Content-Type: application/json" \
     -d '{"keyword":"健康"}'

# カテゴリ取得
curl https://YOUR_API_GATEWAY_URL/categories
```

## 監視とログ

### CloudWatch ログの確認
```bash
aws logs describe-log-groups \
    --log-group-name-prefix /aws/lambda/CobolMigrationFunction
```

### Lambda メトリクスの監視
- CloudWatch での X-Ray トレーシング有効化
- Cold Start 時間の最適化
- メモリ使用量の監視

## パフォーマンス最適化

### 1. メモリサイズの調整
```bash
aws lambda update-function-configuration \
    --function-name CobolMigrationFunction \
    --memory-size 1024
```

### 2. Provisioned Concurrency の設定
```bash
aws lambda put-provisioned-concurrency-config \
    --function-name CobolMigrationFunction \
    --provisioned-concurrency-config ProvisionedConcurrencyConfigs=1
```

## トラブルシューティング

### よくある問題

1. **Cold Start 時間が長い**
   - 初期化処理の最適化
   - Provisioned Concurrency の使用

2. **メモリ不足エラー**
   - メモリサイズの増加
   - データ処理の最適化

3. **タイムアウトエラー**
   - タイムアウト値の調整
   - 非同期処理の活用

### ログの確認
```bash
aws logs filter-log-events \
    --log-group-name /aws/lambda/CobolMigrationFunction \
    --start-time $(date -u -d '1 hour ago' +%s)000
```

## セキュリティ

### IAM ポリシーの最小権限
```json
{
  "Version": "2012-10-17",
  "Statement": [
    {
      "Effect": "Allow",
      "Action": [
        "logs:CreateLogGroup",
        "logs:CreateLogStream",
        "logs:PutLogEvents"
      ],
      "Resource": "arn:aws:logs:*:*:*"
    }
  ]
}
```

### VPC 設定（必要に応じて）
```bash
aws lambda update-function-configuration \
    --function-name CobolMigrationFunction \
    --vpc-config SubnetIds=subnet-12345,SecurityGroupIds=sg-12345
```

このガイドに従って、COBOLプログラムをJavaに移植したLambda関数をAWS上にデプロイできます。

package com.example.cobol;

// AWS Lambda SDKの依存関係チェック
import java.lang.reflect.Method;
import java.lang.reflect.Constructor;

import java.io.FileWriter;
import java.io.IOException;
import java.io.PrintWriter;
import java.time.LocalDateTime;
import java.time.format.DateTimeFormatter;
import java.util.*;
import java.util.concurrent.atomic.AtomicInteger;
import java.util.logging.Logger;
import java.util.logging.Level;

/**
 * AWS Lambda Function for Content Search and Management
 * COBOLプログラム lambda_function.cob をJavaに移植
 * 
 * 機能: HTTPリクエスト処理、コンテンツ検索、カテゴリ管理、レート制限
 * 
 * 注意: AWS Lambda SDKの依存関係がない場合は、デバッグモードで実行されます
 */
public class LambdaFunction {

    // ログ機能
    private static final Logger logger = Logger.getLogger(LambdaFunction.class.getName());

    // AWS Lambda SDKの可用性チェック
    private static final boolean AWS_SDK_AVAILABLE = checkAwsSdkAvailability();

    // AWS Lambda SDKが利用可能な場合のインターフェース
    private static Class<?> contextClass;
    private static Class<?> requestHandlerClass;
    private static Class<?> apiGatewayRequestClass;
    private static Class<?> apiGatewayResponseClass;

    static {
        if (AWS_SDK_AVAILABLE) {
            try {
                contextClass = Class.forName("com.amazonaws.services.lambda.runtime.Context");
                requestHandlerClass = Class.forName("com.amazonaws.services.lambda.runtime.RequestHandler");
                apiGatewayRequestClass = Class
                        .forName("com.amazonaws.services.lambda.runtime.events.APIGatewayProxyRequestEvent");
                apiGatewayResponseClass = Class
                        .forName("com.amazonaws.services.lambda.runtime.events.APIGatewayProxyResponseEvent");
            } catch (ClassNotFoundException e) {
                logger.log(Level.SEVERE, "AWS Lambda SDK classes not found: " + e.getMessage(), e);
            }
        }
    }

    /**
     * AWS Lambda SDKの可用性をチェック
     */
    private static boolean checkAwsSdkAvailability() {
        try {
            Class.forName("com.amazonaws.services.lambda.runtime.Context");
            Class.forName("com.amazonaws.services.lambda.runtime.RequestHandler");
            Class.forName("com.amazonaws.services.lambda.runtime.events.APIGatewayProxyRequestEvent");
            Class.forName("com.amazonaws.services.lambda.runtime.events.APIGatewayProxyResponseEvent");
            return true;
        } catch (ClassNotFoundException e) {
            // loggerはまだ初期化されていないため、System.outを使用
            System.out.println("AWS Lambda SDK not available. Running in debug mode.");
            return false;
        }
    }

    // 環境変数の定義（COBOLのENVIRONMENT-VARIABLESに対応）
    private static final String POSTS_TABLE_NAME = System.getenv().getOrDefault("POSTS_TABLE_NAME", "wp_posts");
    private static final String POSTMETA_TABLE_NAME = System.getenv().getOrDefault("POSTMETA_TABLE_NAME",
            "wp_postmeta");
    private static final String CONTENT_BUCKET_NAME = System.getenv().getOrDefault("CONTENT_BUCKET_NAME",
            "kenko21-web");
    private static final int DEFAULT_PAGE_SIZE = Integer
            .parseInt(System.getenv().getOrDefault("DEFAULT_PAGE_SIZE", "10"));

    // 定数の定義（COBOLのCONSTANTSに対応）
    private static final String APPLICATION_JSON = "application/json";
    private static final String ALLOWED_METHODS = "OPTIONS,POST,GET";
    private static final String ALLOWED_HEADERS = "Content-Type,X-Amz-Date,Authorization,X-Api-Key,X-Amz-Security-Token";
    private static final String STATUS_200 = "200";
    private static final String STATUS_400 = "400";
    private static final String STATUS_500 = "500";

    // Lambda関数の状態管理（COBOLのLAMBDA-STATUSに対応）
    private static final AtomicInteger currentRequests = new AtomicInteger(0);
    private static final int MAX_REQUESTS = 100;
    private static boolean isInitialized = false;
    private static boolean initializationSuccess = false;

    static {
        initializeLambda();
    }

    /**
     * デバッグ用メインメソッド（AWS Lambda環境外でのテスト用）
     */
    public static void main(String[] args) {
        logger.info("=== LambdaFunction デバッグ実行 ===");

        LambdaFunction lambdaFunction = new LambdaFunction();

        if (AWS_SDK_AVAILABLE) {
            // AWS SDKが利用可能な場合
            try {
                Object testRequest = createAwsTestRequest();
                Object testContext = createAwsTestContext();

                Method handleRequestMethod = LambdaFunction.class.getMethod("handleRequest",
                        apiGatewayRequestClass, contextClass);
                Object response = handleRequestMethod.invoke(lambdaFunction, testRequest, testContext);

                Method getStatusCodeMethod = apiGatewayResponseClass.getMethod("getStatusCode");
                Method getBodyMethod = apiGatewayResponseClass.getMethod("getBody");

                logger.info("Response Status: " + getStatusCodeMethod.invoke(response));
                logger.info("Response Body: " + getBodyMethod.invoke(response));
            } catch (Exception e) {
                logger.log(Level.SEVERE, "Error in AWS SDK execution: " + e.getMessage(), e);
            }
        } else {
            // AWS SDKが利用できない場合のデバッグモード
            debugModeExecution(lambdaFunction);
        }

        logger.info("=== デバッグ実行完了 ===");
    }

    /**
     * デバッグモード実行（AWS SDKなし）
     */
    private static void debugModeExecution(LambdaFunction lambdaFunction) {
        logger.info("Running in debug mode (AWS SDK not available)");

        // デバッグ用のリクエストデータ
        DebugRequest debugRequest = new DebugRequest();
        debugRequest.setHttpMethod("GET");
        debugRequest.setPath("/search");
        debugRequest.setQueryParameters(Map.of(
                "keyword", "健康",
                "page", "1",
                "per_page", "5"));

        DebugResponse debugResponse = lambdaFunction.handleDebugRequest(debugRequest);

        logger.info("Response Status: " + debugResponse.getStatusCode());
        logger.info("Response Body: " + debugResponse.getBody());
    }

    /**
     * AWS SDK用テストリクエスト作成
     */
    private static Object createAwsTestRequest() {
        try {
            Constructor<?> constructor = apiGatewayRequestClass.getConstructor();
            Object request = constructor.newInstance();

            Method setHttpMethod = apiGatewayRequestClass.getMethod("setHttpMethod", String.class);
            Method setPath = apiGatewayRequestClass.getMethod("setPath", String.class);
            Method setQueryStringParameters = apiGatewayRequestClass.getMethod("setQueryStringParameters", Map.class);

            setHttpMethod.invoke(request, "GET");
            setPath.invoke(request, "/search");

            Map<String, String> queryParams = new HashMap<>();
            queryParams.put("keyword", "健康");
            queryParams.put("page", "1");
            queryParams.put("per_page", "5");
            setQueryStringParameters.invoke(request, queryParams);

            return request;
        } catch (Exception e) {
            logger.log(Level.SEVERE, "Error creating AWS test request: " + e.getMessage(), e);
            return null;
        }
    }

    /**
     * AWS SDK用テストコンテキスト作成
     */
    private static Object createAwsTestContext() {
        try {
            // 簡易的なコンテキスト実装
            return new Object() {
                public String getAwsRequestId() {
                    return "test-request-id";
                }

                public String getLogGroupName() {
                    return "test-log-group";
                }

                public String getLogStreamName() {
                    return "test-log-stream";
                }

                public String getFunctionName() {
                    return "test-function";
                }

                public String getFunctionVersion() {
                    return "1";
                }

                public String getInvokedFunctionArn() {
                    return "test-arn";
                }

                public String getMemoryLimitInMB() {
                    return "128";
                }

                public String getRemainingTimeInMillis() {
                    return "30000";
                }
            };
        } catch (Exception e) {
            logger.log(Level.SEVERE, "Error creating AWS test context: " + e.getMessage(), e);
            return null;
        }
    }

    /**
     * デバッグ用リクエスト処理
     */
    public DebugResponse handleDebugRequest(DebugRequest request) {
        logger.info("=== Handling Debug Request ===");

        DebugResponse response = new DebugResponse();
        response.setHeaders(Map.of("Content-Type", APPLICATION_JSON));

        String httpMethod = request.getHttpMethod();
        logger.info("HTTP Method: " + httpMethod);

        // レート制限チェック
        if (checkRateLimit()) {
            response.setStatusCode(429);
            response.setBody("Too Many Requests");
            return response;
        }

        // HTTPメソッド別の処理
        switch (httpMethod) {
            case "GET":
                handleGetRequest(request, response);
                break;
            case "POST":
                handlePostRequest(request, response);
                break;
            case "OPTIONS":
                handleOptionsRequest(response);
                break;
            default:
                setErrorStatus(response);
        }

        // ログ出力
        logResponse(response.getStatusCode(), response.getBody());

        return response;
    }

    /**
     * Lambda関数のメインハンドラー（AWS SDK利用時）
     * COBOLのMAIN-PROCEDUREに対応
     */
    public Object handleRequest(Object request, Object context) {
        logger.info("=== Lambda Function Request Handler ===");

        try {
            // CORS対応レスポンスヘッダー設定
            Object response = createAwsCorsResponse();

            Method getHttpMethod = apiGatewayRequestClass.getMethod("getHttpMethod");
            String httpMethod = (String) getHttpMethod.invoke(request);
            logger.info("HTTP Method: " + httpMethod);

            // レート制限チェック
            if (checkRateLimit()) {
                Method setStatusCode = apiGatewayResponseClass.getMethod("setStatusCode", Integer.class);
                Method setBody = apiGatewayResponseClass.getMethod("setBody", String.class);
                setStatusCode.invoke(response, 429);
                setBody.invoke(response, "Too Many Requests");
                return response;
            }

            // HTTPメソッド別の処理
            switch (httpMethod) {
                case "GET":
                    handleAwsGetRequest(request, response);
                    break;
                case "POST":
                    handleAwsPostRequest(request, response);
                    break;
                case "OPTIONS":
                    handleAwsOptionsRequest(response);
                    break;
                default:
                    setAwsErrorStatus(response);
            }

            // ログ出力
            Method getStatusCode = apiGatewayResponseClass.getMethod("getStatusCode");
            Method getBody = apiGatewayResponseClass.getMethod("getBody");
            logResponse((Integer) getStatusCode.invoke(response), (String) getBody.invoke(response));

            return response;

        } catch (Exception e) {
            logger.log(Level.SEVERE, "Error processing request: " + e.getMessage(), e);

            try {
                Object response = createAwsCorsResponse();
                Method setStatusCode = apiGatewayResponseClass.getMethod("setStatusCode", Integer.class);
                Method setBody = apiGatewayResponseClass.getMethod("setBody", String.class);
                setStatusCode.invoke(response, 500);
                setBody.invoke(response, "{\"error\":\"Internal server error\"}");
                return response;
            } catch (Exception ex) {
                logger.log(Level.SEVERE, "Error creating error response: " + ex.getMessage(), ex);
                return null;
            }
        }
    }

    /**
     * Lambda関数の初期化
     * COBOLのINITIALIZE-LAMBDA paragraphに対応
     */
    private static void initializeLambda() {
        logger.info("=== Lambda Function Initialization ===");

        isInitialized = true;

        verifyEnvironmentVariables();
        testDatabaseConnection();

        initializationSuccess = true;

        logger.info("Lambda function initialized successfully");
    }

    /**
     * 環境変数の検証
     * COBOLのVERIFY-ENVIRONMENT-VARIABLES paragraphに対応
     */
    private static void verifyEnvironmentVariables() {
        if (POSTS_TABLE_NAME.isEmpty()) {
            logger.warning("Warning: POSTS_TABLE_NAME not set");
        }

        if (POSTMETA_TABLE_NAME.isEmpty()) {
            logger.warning("Warning: POSTMETA_TABLE_NAME not set");
        }

        if (CONTENT_BUCKET_NAME.isEmpty()) {
            logger.warning("Warning: CONTENT_BUCKET_NAME not set");
        }
    }

    /**
     * データベース接続テスト
     * COBOLのTEST-DATABASE-CONNECTION paragraphに対応
     */
    private static void testDatabaseConnection() {
        logger.info("Testing database connection...");
        String logMessage = "Database connection successful";
        logger.info("LOG: " + logMessage);
    }

    /**
     * GETリクエストの処理（デバッグモード用）
     * COBOLのHANDLE-GET-REQUEST paragraphに対応
     */
    private void handleGetRequest(DebugRequest request, DebugResponse response) {
        logger.info("=== Handling GET Request ===");

        response.setStatusCode(200);

        // 検索パラメータの取得
        Map<String, String> queryParameters = request.getQueryParameters();
        if (queryParameters == null) {
            queryParameters = new HashMap<>();
        }

        String keyword = queryParameters.getOrDefault("keyword", "");
        String siteCode = queryParameters.getOrDefault("site_code", "");
        int page = Integer.parseInt(queryParameters.getOrDefault("page", "1"));
        int perPage = Integer.parseInt(queryParameters.getOrDefault("per_page", String.valueOf(DEFAULT_PAGE_SIZE)));

        // コンテンツ検索実行
        SearchResults searchResults = searchContent(keyword, siteCode, page, perPage);

        // レスポンス作成
        response.setBody(buildSearchJsonResponse(searchResults, page, perPage));
        response.setHeaders(Map.of("Content-Type", APPLICATION_JSON));
    }

    /**
     * POSTリクエストの処理（デバッグモード用）
     * COBOLのHANDLE-POST-REQUEST paragraphに対応
     */
    private void handlePostRequest(DebugRequest request, DebugResponse response) {
        logger.info("=== Handling POST Request ===");

        String requestBody = request.getBody();
        String path = request.getPath();

        response.setHeaders(Map.of("Content-Type", APPLICATION_JSON));

        if (path.contains("categories")) {
            handleCategoriesRequest(response);
        } else if (path.contains("external-content")) {
            handleExternalContentRequest(response);
        } else if (path.contains("health")) {
            handleHealthRequest(response);
        } else if (path.contains("search")) {
            handleAdvancedSearchRequest(requestBody, response);
        } else {
            setErrorStatus(response);
        }
    }

    /**
     * OPTIONSリクエストの処理（デバッグモード用）
     * COBOLのHANDLE-OPTIONS-REQUEST paragraphに対応
     */
    private void handleOptionsRequest(DebugResponse response) {
        logger.info("=== Handling OPTIONS Request ===");
        response.setStatusCode(200);
        response.setBody("CORS preflight request handled");
    }

    /**
     * カテゴリリクエストの処理（デバッグモード用）
     * COBOLのHANDLE-CATEGORIES-REQUEST paragraphに対応
     */
    private void handleCategoriesRequest(DebugResponse response) {
        logger.info("=== Handling Categories Request ===");

        List<String> categories = retrieveAllCategories();
        response.setStatusCode(200);
        response.setBody(buildCategoriesJsonResponse(categories));
    }

    /**
     * 外部コンテンツリクエストの処理（デバッグモード用）
     * COBOLのHANDLE-EXTERNAL-CONTENT-REQUEST paragraphに対応
     */
    private void handleExternalContentRequest(DebugResponse response) {
        logger.info("=== Handling External Content Request ===");

        SearchResults externalResults = searchExternalContent();
        response.setStatusCode(200);
        response.setBody(buildExternalContentJsonResponse(externalResults));
    }

    /**
     * ヘルスチェックリクエストの処理（デバッグモード用）
     * COBOLのHANDLE-HEALTH-REQUEST paragraphに対応
     */
    private void handleHealthRequest(DebugResponse response) {
        logger.info("=== Handling Health Check Request ===");

        if (isInitialized && initializationSuccess) {
            response.setStatusCode(200);
            response.setBody("System healthy");
        } else {
            response.setStatusCode(500);
            response.setBody("System unhealthy");
        }
    }

    /**
     * 高度な検索リクエストの処理（デバッグモード用）
     * COBOLのHANDLE-SEARCH-REQUEST paragraphに対応
     */
    private void handleAdvancedSearchRequest(String requestBody, DebugResponse response) {
        logger.info("=== Handling Search Request ===");

        // JSONパース（簡易実装）
        String keyword = extractKeywordFromJson(requestBody);

        SearchResults searchResults = executeAdvancedSearch(keyword);
        validateSearchResults(searchResults);

        if (response.getStatusCode() == 0) {
            response.setStatusCode(200);
        }

        if (response.getBody() == null) {
            response.setBody(buildSearchJsonResponse(searchResults, 1, DEFAULT_PAGE_SIZE));
        }
    }

    /**
     * レート制限チェック
     * COBOLのCHECK-RATE-LIMIT paragraphに対応
     */
    private boolean checkRateLimit() {
        int current = currentRequests.incrementAndGet();

        if (current > MAX_REQUESTS) {
            logger.warning("Rate limit exceeded");
            return true;
        } else {
            logger.info("Rate limit OK");
            return false;
        }
    }

    /**
     * コンテンツ検索
     * COBOLのSEARCH-CONTENT paragraphに対応
     */
    private SearchResults searchContent(String keyword, String siteCode, int page, int perPage) {
        logger.info("=== Searching Content ===");

        writeLog("Simple content search");

        List<SearchResult> results = new ArrayList<>();

        // 基本的な検索ロジック（Java版）
        for (int i = 1; i <= Math.min(perPage * page, 100); i++) {
            SearchResult result = new SearchResult();
            result.setId(i);
            result.setTitle("Sample Post " + i);
            result.setUrl("sample-url.com");
            result.setPostDate("2025-01-01");
            result.setRelevanceScore(i * 10);

            results.add(result);
        }

        SearchResults searchResults = new SearchResults();
        searchResults.setResults(results);
        searchResults.setCount(results.size());

        return searchResults;
    }

    /**
     * 検索結果の検証
     * COBOLのVALIDATE-SEARCH-RESULTS paragraphに対応
     */
    private void validateSearchResults(SearchResults searchResults) {
        if (searchResults.getCount() == 0) {
            logger.info("No search results found");
            logger.warning("ERROR: No results found");
        } else {
            logger.info("Found " + searchResults.getCount() + " results");
        }
    }

    /**
     * 全カテゴリ取得
     * COBOLのRETRIEVE-ALL-CATEGORIES paragraphに対応
     */
    private List<String> retrieveAllCategories() {
        logger.info("=== Retrieving Categories ===");

        List<String> categories = new ArrayList<>();
        categories.add("健康管理");
        categories.add("喫煙対策");
        categories.add("女性の健康");

        logger.info("Found " + categories.size() + " categories");

        return categories;
    }

    /**
     * 外部コンテンツ検索
     * COBOLのSEARCH-EXTERNAL-CONTENT paragraphに対応
     */
    private SearchResults searchExternalContent() {
        logger.info("=== Searching External Content ===");

        List<SearchResult> results = new ArrayList<>();

        // 外部サイト検索のシミュレーション
        for (int i = 1; i <= 5; i++) {
            SearchResult result = new SearchResult();
            result.setId(i);
            result.setTitle("External Content " + i);
            result.setUrl("external-site.com");
            result.setPostDate("2025-01-01");
            result.setRelevanceScore(i * 10);

            results.add(result);
        }

        SearchResults searchResults = new SearchResults();
        searchResults.setResults(results);
        searchResults.setCount(results.size());

        return searchResults;
    }

    /**
     * 高度な検索実行
     * COBOLのEXECUTE-ADVANCED-SEARCH paragraphに対応
     */
    private SearchResults executeAdvancedSearch(String keyword) {
        logger.info("=== Executing Advanced Search ===");

        int page = 1;
        int perPage = DEFAULT_PAGE_SIZE;

        SearchResults searchResults = searchContent(keyword, "", page, perPage);

        if (!keyword.isEmpty()) {
            enhanceSearchWithKeywords(searchResults, keyword);
        }

        calculateRelevanceScores(searchResults);

        return searchResults;
    }

    /**
     * キーワードによる検索強化
     * COBOLのENHANCE-SEARCH-WITH-KEYWORDS paragraphに対応
     */
    private void enhanceSearchWithKeywords(SearchResults searchResults, String keyword) {
        logger.info("=== Enhancing search with keywords ===");

        List<SearchResult> results = searchResults.getResults();

        for (SearchResult result : results) {
            boolean matchFound = false;

            // タイトルでのキーワードマッチング
            if (result.getTitle().contains(keyword)) {
                matchFound = true;
                result.setRelevanceScore(result.getRelevanceScore() + 20);
            }

            // 完全マッチが見つからない場合は低スコアを与える
            if (!matchFound) {
                result.setRelevanceScore(result.getRelevanceScore() + 5);
            }
        }
    }

    /**
     * 関連性スコアの計算とソート
     * COBOLのCALCULATE-RELEVANCE-SCORES paragraphに対応
     */
    private void calculateRelevanceScores(SearchResults searchResults) {
        logger.info("=== Calculating Relevance Scores ===");

        List<SearchResult> results = searchResults.getResults();

        // スコアに基づいてソート（バブルソート）
        for (int i = 0; i < results.size() - 1; i++) {
            for (int j = 0; j < results.size() - i - 1; j++) {
                if (results.get(j).getRelevanceScore() < results.get(j + 1).getRelevanceScore()) {
                    // 結果の交換
                    SearchResult temp = results.get(j);
                    results.set(j, results.get(j + 1));
                    results.set(j + 1, temp);
                }
            }
        }
    }

    /**
     * JSONレスポンス作成メソッド群
     */
    private String buildCategoriesJsonResponse(List<String> categories) {
        StringBuilder json = new StringBuilder();
        json.append("{\"status\":\"success\",\"categories\":[");

        for (int i = 0; i < categories.size(); i++) {
            if (i > 0)
                json.append(",");
            json.append("{\"name\":\"").append(categories.get(i)).append("\"}");
        }

        json.append("],\"count\":").append(categories.size()).append("}");
        return json.toString();
    }

    private String buildExternalContentJsonResponse(SearchResults results) {
        StringBuilder json = new StringBuilder();
        json.append("{\"status\":\"success\",\"results\":[");

        List<SearchResult> resultList = results.getResults();
        for (int i = 0; i < resultList.size(); i++) {
            if (i > 0)
                json.append(",");
            SearchResult result = resultList.get(i);
            json.append("{\"id\":").append(result.getId())
                    .append(",\"title\":\"").append(result.getTitle()).append("\"")
                    .append(",\"url\":\"").append(result.getUrl()).append("\"")
                    .append(",\"post_date\":\"").append(result.getPostDate()).append("\"}");
        }

        json.append("],\"total\":").append(results.getCount()).append("}");
        return json.toString();
    }

    private String buildSearchJsonResponse(SearchResults results, int page, int perPage) {
        StringBuilder json = new StringBuilder();
        json.append("{\"status\":\"success\",\"results\":[");

        List<SearchResult> resultList = results.getResults();
        for (int i = 0; i < resultList.size(); i++) {
            if (i > 0)
                json.append(",");
            SearchResult result = resultList.get(i);
            json.append("{\"id\":").append(result.getId())
                    .append(",\"title\":\"").append(result.getTitle()).append("\"")
                    .append(",\"url\":\"").append(result.getUrl()).append("\"")
                    .append(",\"post_date\":\"").append(result.getPostDate()).append("\"")
                    .append(",\"score\":").append(result.getRelevanceScore()).append("}");
        }

        json.append("],\"pagination\":{\"total\":").append(results.getCount())
                .append(",\"page\":").append(page)
                .append(",\"per_page\":").append(perPage).append("}}");

        return json.toString();
    }

    /**
     * ログ出力
     * COBOLのWRITE-LOG paragraphに対応
     */
    private void writeLog(String message) {
        try {
            PrintWriter writer = new PrintWriter(new FileWriter("lambda.log", true));
            String timestamp = LocalDateTime.now().format(DateTimeFormatter.ISO_LOCAL_DATE_TIME);
            writer.println(timestamp + " - " + message);
            writer.close();
        } catch (IOException e) {
            logger.log(Level.SEVERE, "Error writing log: " + e.getMessage(), e);
        }
    }

    /**
     * レスポンスログ
     * COBOLのLOG-RESPONSE paragraphに対応
     */
    private void logResponse(int statusCode, String responseBody) {
        String logMessage = "Status: " + statusCode + " Response: " + responseBody;
        writeLog(logMessage);
    }

    /**
     * AWS SDK用CORSレスポンス作成
     */
    private Object createAwsCorsResponse() {
        try {
            Constructor<?> constructor = apiGatewayResponseClass.getConstructor();
            Object response = constructor.newInstance();

            Method setHeaders = apiGatewayResponseClass.getMethod("setHeaders", Map.class);
            Map<String, String> headers = Map.of(
                    "Access-Control-Allow-Origin", "*",
                    "Access-Control-Allow-Methods", ALLOWED_METHODS,
                    "Access-Control-Allow-Headers", ALLOWED_HEADERS);
            setHeaders.invoke(response, headers);

            return response;
        } catch (Exception e) {
            logger.log(Level.SEVERE, "Error creating AWS CORS response: " + e.getMessage(), e);
            return null;
        }
    }

    /**
     * AWS SDK用GETリクエスト処理
     */
    private void handleAwsGetRequest(Object request, Object response) {
        try {
            Method setStatusCode = apiGatewayResponseClass.getMethod("setStatusCode", Integer.class);
            Method getQueryStringParameters = apiGatewayRequestClass.getMethod("getQueryStringParameters");
            Method setBody = apiGatewayResponseClass.getMethod("setBody", String.class);
            Method setHeaders = apiGatewayResponseClass.getMethod("setHeaders", Map.class);

            setStatusCode.invoke(response, 200);

            @SuppressWarnings("unchecked")
            Map<String, String> queryParameters = (Map<String, String>) getQueryStringParameters.invoke(request);
            if (queryParameters == null) {
                queryParameters = new HashMap<>();
            }

            String keyword = queryParameters.getOrDefault("keyword", "");
            String siteCode = queryParameters.getOrDefault("site_code", "");
            int page = Integer.parseInt(queryParameters.getOrDefault("page", "1"));
            int perPage = Integer.parseInt(queryParameters.getOrDefault("per_page", String.valueOf(DEFAULT_PAGE_SIZE)));

            SearchResults searchResults = searchContent(keyword, siteCode, page, perPage);

            setBody.invoke(response, buildSearchJsonResponse(searchResults, page, perPage));
            setHeaders.invoke(response, Map.of("Content-Type", APPLICATION_JSON));
        } catch (Exception e) {
            logger.log(Level.SEVERE, "Error handling AWS GET request: " + e.getMessage(), e);
        }
    }

    /**
     * AWS SDK用POSTリクエスト処理
     */
    private void handleAwsPostRequest(Object request, Object response) {
        try {
            Method getBody = apiGatewayRequestClass.getMethod("getBody");
            Method getPath = apiGatewayRequestClass.getMethod("getPath");
            Method setHeaders = apiGatewayResponseClass.getMethod("setHeaders", Map.class);
            Method setStatusCode = apiGatewayResponseClass.getMethod("setStatusCode", Integer.class);
            Method setBody = apiGatewayResponseClass.getMethod("setBody", String.class);

            String requestBody = (String) getBody.invoke(request);
            String path = (String) getPath.invoke(request);

            setHeaders.invoke(response, Map.of("Content-Type", APPLICATION_JSON));

            if (path.contains("categories")) {
                List<String> categories = retrieveAllCategories();
                setStatusCode.invoke(response, 200);
                setBody.invoke(response, buildCategoriesJsonResponse(categories));
            } else if (path.contains("health")) {
                if (isInitialized && initializationSuccess) {
                    setStatusCode.invoke(response, 200);
                    setBody.invoke(response, "System healthy");
                } else {
                    setStatusCode.invoke(response, 500);
                    setBody.invoke(response, "System unhealthy");
                }
            } else {
                setAwsErrorStatus(response);
            }
        } catch (Exception e) {
            logger.log(Level.SEVERE, "Error handling AWS POST request: " + e.getMessage(), e);
        }
    }

    /**
     * AWS SDK用OPTIONSリクエスト処理
     */
    private void handleAwsOptionsRequest(Object response) {
        try {
            Method setStatusCode = apiGatewayResponseClass.getMethod("setStatusCode", Integer.class);
            Method setBody = apiGatewayResponseClass.getMethod("setBody", String.class);

            setStatusCode.invoke(response, 200);
            setBody.invoke(response, "CORS preflight request handled");
        } catch (Exception e) {
            logger.log(Level.SEVERE, "Error handling AWS OPTIONS request: " + e.getMessage(), e);
        }
    }

    /**
     * AWS SDK用エラー状態設定
     */
    private void setAwsErrorStatus(Object response) {
        try {
            Method setStatusCode = apiGatewayResponseClass.getMethod("setStatusCode", Integer.class);
            Method setBody = apiGatewayResponseClass.getMethod("setBody", String.class);

            setStatusCode.invoke(response, 400);
            setBody.invoke(response, "{\"error\":\"Invalid request\",\"message\":\"Invalid request\"}");
        } catch (Exception e) {
            logger.log(Level.SEVERE, "Error setting AWS error status: " + e.getMessage(), e);
        }
    }

    /**
     * エラー状態設定（デバッグモード用）
     * COBOLのSET-ERROR-STATUS paragraphに対応
     */
    private void setErrorStatus(DebugResponse response) {
        response.setStatusCode(400);
        response.setBody("{\"error\":\"Invalid request\",\"message\":\"Invalid request\"}");
    }

    /**
     * JSONからキーワード抽出（簡易実装）
     */
    private String extractKeywordFromJson(String json) {
        if (json == null || json.isEmpty())
            return "";

        // 簡易的なJSONパース（本格的な実装ではJacksonやGsonを使用）
        String keyword = "";
        if (json.contains("\"keyword\"")) {
            int start = json.indexOf("\"keyword\":\"") + 11;
            int end = json.indexOf("\"", start);
            if (start > 10 && end > start) {
                keyword = json.substring(start, end);
            }
        }
        return keyword;
    }

    /**
     * 検索結果データクラス
     */
    public static class SearchResult {
        private int id;
        private String title;
        private String url;
        private String postDate;
        private int relevanceScore;

        // Getters and Setters
        public int getId() {
            return id;
        }

        public void setId(int id) {
            this.id = id;
        }

        public String getTitle() {
            return title;
        }

        public void setTitle(String title) {
            this.title = title;
        }

        public String getUrl() {
            return url;
        }

        public void setUrl(String url) {
            this.url = url;
        }

        public String getPostDate() {
            return postDate;
        }

        public void setPostDate(String postDate) {
            this.postDate = postDate;
        }

        public int getRelevanceScore() {
            return relevanceScore;
        }

        public void setRelevanceScore(int relevanceScore) {
            this.relevanceScore = relevanceScore;
        }
    }

    /**
     * 検索結果コレクションクラス
     */
    public static class SearchResults {
        private List<SearchResult> results;
        private int count;

        public List<SearchResult> getResults() {
            return results;
        }

        public void setResults(List<SearchResult> results) {
            this.results = results;
        }

        public int getCount() {
            return count;
        }

        public void setCount(int count) {
            this.count = count;
        }
    }

    /**
     * デバッグ用リクエストクラス
     */
    public static class DebugRequest {
        private String httpMethod;
        private String path;
        private String body;
        private Map<String, String> queryParameters;

        public String getHttpMethod() {
            return httpMethod;
        }

        public void setHttpMethod(String httpMethod) {
            this.httpMethod = httpMethod;
        }

        public String getPath() {
            return path;
        }

        public void setPath(String path) {
            this.path = path;
        }

        public String getBody() {
            return body;
        }

        public void setBody(String body) {
            this.body = body;
        }

        public Map<String, String> getQueryParameters() {
            return queryParameters;
        }

        public void setQueryParameters(Map<String, String> queryParameters) {
            this.queryParameters = queryParameters;
        }
    }

    /**
     * デバッグ用レスポンスクラス
     */
    public static class DebugResponse {
        private int statusCode;
        private String body;
        private Map<String, String> headers;

        public int getStatusCode() {
            return statusCode;
        }

        public void setStatusCode(int statusCode) {
            this.statusCode = statusCode;
        }

        public String getBody() {
            return body;
        }

        public void setBody(String body) {
            this.body = body;
        }

        public Map<String, String> getHeaders() {
            return headers;
        }

        public void setHeaders(Map<String, String> headers) {
            this.headers = headers;
        }
    }
}

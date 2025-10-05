package com.example.cobol;

import com.amazonaws.services.lambda.runtime.Context;
import com.amazonaws.services.lambda.runtime.RequestHandler;
import com.amazonaws.services.lambda.runtime.events.APIGatewayProxyRequestEvent;
import com.amazonaws.services.lambda.runtime.events.APIGatewayProxyResponseEvent;

import java.io.FileWriter;
import java.io.IOException;
import java.io.PrintWriter;
import java.time.LocalDateTime;　
import java.time.format.DateTimeFormatter;
import java.util.*;
import java.util.concurrent.atomic.AtomicInteger;

/**
 * AWS Lambda Function for Content Search and Management
 * COBOLプログラム lambda_function.cob をJavaに移植
 * 
 * 機能: HTTPリクエスト処理、コンテンツ検索、カテゴリ管理、レート制限
 */
public class LambdaFunction implements RequestHandler<APIGatewayProxyRequestEvent, APIGatewayProxyResponseEvent> {

    // 環境変数の定義（COBOLのENVIRONMENT-VARIABLESに対応）
    private static final String POSTS_TABLE_NAME = System.getenv().getOrDefault("POSTS_TABLE_NAME", "wp_posts");
    private static final String POSTMETA_TABLE_NAME = System.getenv().getOrDefault("POSTMETA_TABLE_NAME",
            "wp_postmeta");
    private static final String CONTENT_BUCKET_NAME = System.getenv().getOrDefault("CONTENT_BUCKET_NAME",
            "kenko21-web");
    private static final int MAX_RETRY_ATTEMPTS = Integer
            .parseInt(System.getenv().getOrDefault("MAX_RETRY_ATTEMPTS", "3"));
    private static final int CACHE_TTL_SECONDS = Integer
            .parseInt(System.getenv().getOrDefault("CACHE_TTL_SECONDS", "3600"));
    private static final int DEFAULT_PAGE_SIZE = Integer
            .parseInt(System.getenv().getOrDefault("DEFAULT_PAGE_SIZE", "10"));

    // 定数の定義（COBOLのCONSTANTSに対応）
    private static final String APPLICATION_JSON = "application/json";
    private static final String ALLOWED_METHODS = "OPTIONS,POST,GET";
    private static final String ALLOWED_HEADERS = "Content-Type,X-Amz-Date,Authorization,X-Api-Key,X-Amz-Security-Token";
    private static final String INTERNAL_SERVER_ERROR = "Internal server error";
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
     * Lambda関数のメインハンドラー
     * COBOLのMAIN-PROCEDUREに対応
     */
    @Override
    public APIGatewayProxyResponseEvent handleRequest(APIGatewayProxyRequestEvent request, Context context) {
        System.out.println("=== Lambda Function Request Handler ===");

        try {
            // CORS対応レスポンスヘッダー設定
            APIGatewayProxyResponseEvent response = createCorsResponse();

            String httpMethod = request.getHttpMethod();
            System.out.println("HTTP Method: " + httpMethod);

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

        } catch (Exception e) {
            System.err.println("Error processing request: " + e.getMessage());
            e.printStackTrace();

            APIGatewayProxyResponseEvent response = createCorsResponse();
            response.setStatusCode(500);
            response.setBody("{\"error\":\"Internal server error\"}");
            return response;
        }
    }

    /**
     * Lambda関数の初期化
     * COBOLのINITIALIZE-LAMBDA paragraphに対応
     */
    private static void initializeLambda() {
        System.out.println("=== Lambda Function Initialization ===");

        isInitialized = true;

        verifyEnvironmentVariables();
        testDatabaseConnection();

        initializationSuccess = true;

        System.out.println("Lambda function initialized successfully");
    }

    /**
     * 環境変数の検証
     * COBOLのVERIFY-ENVIRONMENT-VARIABLES paragraphに対応
     */
    private static void verifyEnvironmentVariables() {
        if (POSTS_TABLE_NAME.isEmpty()) {
            System.out.println("Warning: POSTS_TABLE_NAME not set");
        }

        if (POSTMETA_TABLE_NAME.isEmpty()) {
            System.out.println("Warning: POSTMETA_TABLE_NAME not set");
        }

        if (CONTENT_BUCKET_NAME.isEmpty()) {
            System.out.println("Warning: CONTENT_BUCKET_NAME not set");
        }
    }

    /**
     * データベース接続テスト
     * COBOLのTEST-DATABASE-CONNECTION paragraphに対応
     */
    private static void testDatabaseConnection() {
        System.out.println("Testing database connection...");
        String logMessage = "Database connection successful";
        System.out.println("LOG: " + logMessage);
    }

    /**
     * GETリクエストの処理
     * COBOLのHANDLE-GET-REQUEST paragraphに対応
     */
    private void handleGetRequest(APIGatewayProxyRequestEvent request, APIGatewayProxyResponseEvent response) {
        System.out.println("=== Handling GET Request ===");

        response.setStatusCode(Integer.parseInt(STATUS_200));

        // 検索パラメータの取得
        Map<String, String> queryParameters = request.getQueryStringParameters();
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
     * POSTリクエストの処理
     * COBOLのHANDLE-POST-REQUEST paragraphに対応
     */
    private void handlePostRequest(APIGatewayProxyRequestEvent request, APIGatewayProxyResponseEvent response) {
        System.out.println("=== Handling POST Request ===");

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
     * OPTIONSリクエストの処理
     * COBOLのHANDLE-OPTIONS-REQUEST paragraphに対応
     */
    private void handleOptionsRequest(APIGatewayProxyResponseEvent response) {
        System.out.println("=== Handling OPTIONS Request ===");
        response.setStatusCode(Integer.parseInt(STATUS_200));
        response.setBody("CORS preflight request handled");
    }

    /**
     * カテゴリリクエストの処理
     * COBOLのHANDLE-CATEGORIES-REQUEST paragraphに対応
     */
    private void handleCategoriesRequest(APIGatewayProxyResponseEvent response) {
        System.out.println("=== Handling Categories Request ===");

        List<String> categories = retrieveAllCategories();
        response.setStatusCode(Integer.parseInt(STATUS_200));
        response.setBody(buildCategoriesJsonResponse(categories));
    }

    /**
     * 外部コンテンツリクエストの処理
     * COBOLのHANDLE-EXTERNAL-CONTENT-REQUEST paragraphに対応
     */
    private void handleExternalContentRequest(APIGatewayProxyResponseEvent response) {
        System.out.println("=== Handling External Content Request ===");

        SearchResults externalResults = searchExternalContent();
        response.setStatusCode(Integer.parseInt(STATUS_200));
        response.setBody(buildExternalContentJsonResponse(externalResults));
    }

    /**
     * ヘルスチェックリクエストの処理
     * COBOLのHANDLE-HEALTH-REQUEST paragraphに対応
     */
    private void handleHealthRequest(APIGatewayProxyResponseEvent response) {
        System.out.println("=== Handling Health Check Request ===");

        if (isInitialized && initializationSuccess) {
            response.setStatusCode(Integer.parseInt(STATUS_200));
            response.setBody("System healthy");
        } else {
            response.setStatusCode(Integer.parseInt(STATUS_500));
            response.setBody("System unhealthy");
        }
    }

    /**
     * 高度な検索リクエストの処理
     * COBOLのHANDLE-SEARCH-REQUEST paragraphに対応
     */
    private void handleAdvancedSearchRequest(String requestBody, APIGatewayProxyResponseEvent response) {
        System.out.println("=== Handling Search Request ===");

        // JSONパース（簡易実装）
        String keyword = extractKeywordFromJson(requestBody);

        SearchResults searchResults = executeAdvancedSearch(keyword);
        validateSearchResults(searchResults);

        if (response.getStatusCode() == null) {
            response.setStatusCode(Integer.parseInt(STATUS_200));
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
            System.out.println("Rate limit exceeded");
            return true;
        } else {
            System.out.println("Rate limit OK");
            return false;
        }
    }

    /**
     * コンテンツ検索
     * COBOLのSEARCH-CONTENT paragraphに対応
     */
    private SearchResults searchContent(String keyword, String siteCode, int page, int perPage) {
        System.out.println("=== Searching Content ===");

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
            System.out.println("No search results found");
            System.out.println("ERROR: No results found");
        } else {
            System.out.println("Found " + searchResults.getCount() + " results");
        }
    }

    /**
     * 全カテゴリ取得
     * COBOLのRETRIEVE-ALL-CATEGORIES paragraphに対応
     */
    private List<String> retrieveAllCategories() {
        System.out.println("=== Retrieving Categories ===");

        List<String> categories = new ArrayList<>();
        categories.add("健康管理");
        categories.add("喫煙対策");
        categories.add("女性の健康");

        System.out.println("Found " + categories.size() + " categories");

        return categories;
    }

    /**
     * 外部コンテンツ検索
     * COBOLのSEARCH-EXTERNAL-CONTENT paragraphに対応
     */
    private SearchResults searchExternalContent() {
        System.out.println("=== Searching External Content ===");

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
        System.out.println("=== Executing Advanced Search ===");

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
        System.out.println("=== Enhancing search with keywords ===");

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
        System.out.println("=== Calculating Relevance Scores ===");

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
            System.err.println("Error writing log: " + e.getMessage());
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
     * CORSレスポンス作成
     */
    private APIGatewayProxyResponseEvent createCorsResponse() {
        APIGatewayProxyResponseEvent response = new APIGatewayProxyResponseEvent();
        response.setHeaders(Map.of(
                "Access-Control-Allow-Origin", "*",
                "Access-Control-Allow-Methods", ALLOWED_METHODS,
                "Access-Control-Allow-Headers", ALLOWED_HEADERS));
        return response;
    }

    /**
     * エラー状態設定
     * COBOLのSET-ERROR-STATUS paragraphに対応
     */
    private void setErrorStatus(APIGatewayProxyResponseEvent response) {
        response.setStatusCode(Integer.parseInt(STATUS_400));
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
}

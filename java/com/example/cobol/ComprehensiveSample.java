package com.example.cobol;

import java.io.*;
import java.math.BigDecimal;
import java.math.RoundingMode;
import java.time.LocalDateTime;
import java.time.format.DateTimeFormatter;
import java.util.*;
import java.util.stream.Collectors;
import java.util.Properties;
import java.util.logging.Logger;
import java.util.logging.Level;

/**
 * COBOL Comprehensive Sample Program - Java Implementation
 * COBOLプログラム comprehensive_sample.cob をJavaに完全移植
 * 
 * このプログラムでは以下の機能を実装しています：
 * - 4つのDIVISION構成のJava対応
 * - データ型と変数定義
 * - ファイル入出力処理
 * - 配列とコレクション操作
 * - 文字列操作
 * - 数学演算
 * - 条件分岐とループ処理
 * - データ検索とソート
 * - レポート生成機能
 */
public class ComprehensiveSample {

    // ================================================================
    // 環境変数と定数定義（COBOLのENVIRONMENT/DATA DIVISIONに対応）
    // ================================================================

    // プログラム定数
    private static final String PROGRAM_TITLE = "=====================================\n" +
            "COBOL Comprehensive Sample Program\n" +
            "=====================================";

    // メッセージ定数
    private static final String RECORDS_PROCESSED_MSG = "Records processed: ";
    private static final String RECORDS_ERROR_MSG = "Records with errors: ";
    private static final String CUSTOMER_PREFIX = "Customer ";

    // ファイルパス定義（実際に使用されるもののみ）
    private static final String CUSTOMER_MASTER_FILE = "CUSTOMER.MAST";
    private static final String SALES_TRANSACTIONS_FILE = "SALES.TRAN";
    private static final String REPORT_OUTPUT_FILE = "DAILY-REPORT.TXT";
    private static final String CONFIG_FILE = "config.properties";

    // 設定値
    private static Properties config = new Properties();

    // ログ機能
    private static final Logger logger = Logger.getLogger(ComprehensiveSample.class.getName());

    // ================================================================
    // データ構造クラス定義（COBOLのFILE SECTION/WORKING-STORAGEに対応）
    // ================================================================

    /**
     * 顧客レコード構造体
     * COBOLの01 CUSTOMER-RECORDに対応
     */
    public static class CustomerRecord {
        private int customerId;
        private String customerName;
        private Address customerAddress;
        private String customerPhone;
        private BigDecimal creditLimit;
        private char customerStatus;
        private String dateCreated;

        // デフォルトコンストラクタ
        public CustomerRecord() {
            this.customerAddress = new Address();
            this.creditLimit = BigDecimal.ZERO;
            this.customerStatus = 'A';
        }

        // Getters and Setters
        public int getCustomerId() {
            return customerId;
        }

        public void setCustomerId(int customerId) {
            this.customerId = customerId;
        }

        public String getCustomerName() {
            return customerName;
        }

        public void setCustomerName(String customerName) {
            this.customerName = customerName;
        }

        public Address getCustomerAddress() {
            return customerAddress;
        }

        public void setCustomerAddress(Address customerAddress) {
            this.customerAddress = customerAddress;
        }

        public String getCustomerPhone() {
            return customerPhone;
        }

        public void setCustomerPhone(String customerPhone) {
            this.customerPhone = customerPhone;
        }

        public BigDecimal getCreditLimit() {
            return creditLimit;
        }

        public void setCreditLimit(BigDecimal creditLimit) {
            this.creditLimit = creditLimit;
        }

        public char getCustomerStatus() {
            return customerStatus;
        }

        public void setCustomerStatus(char customerStatus) {
            this.customerStatus = customerStatus;
        }

        public String getDateCreated() {
            return dateCreated;
        }

        public void setDateCreated(String dateCreated) {
            this.dateCreated = dateCreated;
        }

        @Override
        public String toString() {
            return String.format("Customer[id=%d, name=%s, status=%c, limit=%s]",
                    customerId, customerName, customerStatus, creditLimit);
        }
    }

    /**
     * 住所構造体
     * COBOLの05 CUSTOMER-ADDRESSに対応
     */
    public static class Address {
        private String streetAddress;
        private String city;
        private String state;
        private String zipCode;

        public Address() {
            // デフォルトコンストラクタ - フィールドはデフォルト値で初期化
        }

        public String getStreetAddress() {
            return streetAddress;
        }

        public void setStreetAddress(String streetAddress) {
            this.streetAddress = streetAddress;
        }

        public String getCity() {
            return city;
        }

        public void setCity(String city) {
            this.city = city;
        }

        public String getState() {
            return state;
        }

        public void setState(String state) {
            this.state = state;
        }

        public String getZipCode() {
            return zipCode;
        }

        public void setZipCode(String zipCode) {
            this.zipCode = zipCode;
        }

        @Override
        public String toString() {
            return String.format("%s, %s, %s %s", streetAddress, city, state, zipCode);
        }
    }

    /**
     * 売上トランザクションレコード
     * COBOLの01 SALES-RECORDに対応
     */
    public static class SalesRecord {
        private long transactionId;
        private int customerId;
        private String productCode;
        private int quantity;
        private BigDecimal unitPrice;
        private BigDecimal totalAmount;
        private String transactionDate;
        private int salespersonId;

        public SalesRecord() {
            this.unitPrice = BigDecimal.ZERO;
            this.totalAmount = BigDecimal.ZERO;
        }

        // Getters and Setters
        public long getTransactionId() {
            return transactionId;
        }

        public void setTransactionId(long transactionId) {
            this.transactionId = transactionId;
        }

        public int getCustomerId() {
            return customerId;
        }

        public void setCustomerId(int customerId) {
            this.customerId = customerId;
        }

        public String getProductCode() {
            return productCode;
        }

        public void setProductCode(String productCode) {
            this.productCode = productCode;
        }

        public int getQuantity() {
            return quantity;
        }

        public void setQuantity(int quantity) {
            this.quantity = quantity;
        }

        public BigDecimal getUnitPrice() {
            return unitPrice;
        }

        public void setUnitPrice(BigDecimal unitPrice) {
            this.unitPrice = unitPrice;
        }

        public BigDecimal getTotalAmount() {
            return totalAmount;
        }

        public void setTotalAmount(BigDecimal totalAmount) {
            this.totalAmount = totalAmount;
        }

        public String getTransactionDate() {
            return transactionDate;
        }

        public void setTransactionDate(String transactionDate) {
            this.transactionDate = transactionDate;
        }

        public int getSalespersonId() {
            return salespersonId;
        }

        public void setSalespersonId(int salespersonId) {
            this.salespersonId = salespersonId;
        }

        @Override
        public String toString() {
            return String.format("Sales[txn=%d, customer=%d, product=%s, amount=%s]",
                    transactionId, customerId, productCode, totalAmount);
        }
    }

    /**
     * 月別売上統計（COBOLのOCCURS句に対応）
     */
    public static class MonthlySalesData {
        private String monthName;
        private BigDecimal monthSales;
        private int monthCustomers;

        public MonthlySalesData(String monthName) {
            this.monthName = monthName;
            this.monthSales = BigDecimal.ZERO;
            this.monthCustomers = 0;
        }

        public String getMonthName() {
            return monthName;
        }

        public BigDecimal getMonthSales() {
            return monthSales;
        }

        public void addToMonthSales(BigDecimal amount) {
            this.monthSales = this.monthSales.add(amount);
        }

        public int getMonthCustomers() {
            return monthCustomers;
        }

        public void addCustomer() {
            this.monthCustomers++;
        }

        @Override
        public String toString() {
            return String.format("%s: $%s (%d customers)",
                    monthName, formatCurrency(monthSales), monthCustomers);
        }

        private String formatCurrency(BigDecimal amount) {
            return String.format("%,.2f", amount);
        }
    }

    // ================================================================
    // 作業用変数と状態管理（COBOLのWORKING-STORAGEに対応）
    // ================================================================

    private final List<CustomerRecord> customerRecords = new ArrayList<>();
    private final List<SalesRecord> salesRecords = new ArrayList<>();
    private final List<MonthlySalesData> monthlySales = new ArrayList<>();

    // 統計変数
    private BigDecimal grossSales = BigDecimal.ZERO;
    private BigDecimal calculatedTax = BigDecimal.ZERO;
    private BigDecimal calculatedCommission = BigDecimal.ZERO;
    private BigDecimal weightedAverage = BigDecimal.ZERO;
    private BigDecimal percentageValue = BigDecimal.ZERO;

    // キャッシュ用変数
    private boolean statisticsCalculated = false;
    private Map<Character, Long> statusCountCache = new HashMap<>();

    // 処理制御変数
    private int recordsProcessed = 0;
    private int recordsError = 0;
    private final StringBuilder logMessages = new StringBuilder();
    private static final DateTimeFormatter TIMESTAMP_FORMAT = DateTimeFormatter.ofPattern("yyyy-MM-dd HH:mm:ss");

    /**
     * メインエントリーポイント
     * COBOLのMAIN-PROCEDUREに対応
     */
    public static void main(String[] args) {
        logger.info(PROGRAM_TITLE);

        ComprehensiveSample program = new ComprehensiveSample();

        try {
            program.executeProgram();
        } catch (Exception e) {
            logger.log(Level.SEVERE, "Program execution failed: " + e.getMessage(), e);
        }
    }

    /**
     * プログラムのメイン実行フロー
     */
    public void executeProgram() {
        logger.info("Starting COBOL Comprehensive Sample Program (Java Implementation)");

        // COBOLのMAIN-PROCEDUREに対応
        initializeProgram();

        processCustomerFile();

        processSalesTransactions();

        generateStatistics();

        generateReportHeading();

        processProductTable();

        demonstrateComputationalData();

        performMathematicalCalculations();

        demonstrateStringOperations();

        demonstrateConditionalLogic();

        demonstrateLoopConstructs();

        demonstrateSearchOperations();

        demonstrateSortOperations();

        generateDetailReport();

        finalizeProgram();
    }

    // ================================================================
    // 初期化処理（COBOLのINITIALIZE-PROGRAM paragraphに対応）
    // ================================================================

    private void initializeProgram() {
        logger.info("Initializing program...");

        // 設定ファイルの読み込み
        loadConfiguration();

        // 月名テーブルの初期化
        initializeMonthlyTable();

        // 現在の日付と時刻を記録
        LocalDateTime startTime = LocalDateTime.now();
        logMessage("Program start time: " + startTime.format(TIMESTAMP_FORMAT));

        logger.info("Program initialized successfully");
    }

    private void loadConfiguration() {
        try (FileInputStream fis = new FileInputStream(CONFIG_FILE)) {
            config.load(fis);
            logMessage("Configuration loaded from " + CONFIG_FILE);
        } catch (IOException e) {
            logger.warning("Could not load configuration file, using defaults: " + e.getMessage());
            // デフォルト設定
            config.setProperty("tax.rate", "0.08");
            config.setProperty("commission.rate", "0.05");
            config.setProperty("log.level", "INFO");
        }
    }

    private void initializeMonthlyTable() {
        String[] monthNames = {
                "January  ", "February ", "March    ", "April    ",
                "May      ", "June     ", "July     ", "August   ",
                "September", "October  ", "November ", "December "
        };

        for (String monthName : monthNames) {
            monthlySales.add(new MonthlySalesData(monthName));
        }

        logMessage("Monthly sales table initialized with " + monthNames.length + " entries");
    }

    // ================================================================
    // ファイル処理（COBOLのPROCESS-CUSTOMER-FILE paragraphに対応）
    // ================================================================

    private void processCustomerFile() {
        logger.info("Processing Customer Master File...");

        try (BufferedReader reader = new BufferedReader(new FileReader(CUSTOMER_MASTER_FILE))) {
            String line;
            while ((line = reader.readLine()) != null) {
                processCustomerLine(line);
            }
        } catch (IOException e) {
            logger.severe("Error reading customer file: " + e.getMessage());
            recordsError++;
        }

        logger.info("Customer file processing completed");
        logger.info(RECORDS_PROCESSED_MSG + recordsProcessed);
        logger.info(RECORDS_ERROR_MSG + recordsError);
    }

    private void processCustomerLine(String line) {
        if (line.length() < 125) {
            logMessage("WARNING: Customer record too short (" + line.length() + " chars), skipping");
            recordsError++;
            return; // 最小レコード長チェック
        }

        CustomerRecord customer = new CustomerRecord();

        try {
            // COBOLの固定位置データ読み込みを模擬
            customer.setCustomerId(Integer.parseInt(line.substring(0, 7).trim()));
            customer.setCustomerName(line.substring(7, 37).trim());
            customer.getCustomerAddress().setStreetAddress(line.substring(37, 77).trim());
            customer.getCustomerAddress().setCity(line.substring(77, 97).trim());
            customer.getCustomerAddress().setState(line.substring(97, 99).trim());
            customer.getCustomerAddress().setZipCode(line.substring(99, 109).trim());
            customer.setCustomerPhone(line.substring(109, 124).trim());
            customer.setCreditLimit(new BigDecimal(line.substring(124, 133).trim()));
            customer.setCustomerStatus(line.charAt(133));
            customer.setDateCreated(line.substring(134, 142).trim());

            // データ検証
            validateCustomerRecord(customer);

            customerRecords.add(customer);
            processSingleCustomer(customer);
            recordsProcessed++;
            logMessage("Successfully processed customer: " + customer.getCustomerId());

        } catch (NumberFormatException e) {
            logMessage("ERROR: Invalid number format in customer record: " + e.getMessage());
            recordsError++;
        } catch (StringIndexOutOfBoundsException e) {
            logMessage("ERROR: String index out of bounds in customer record: " + e.getMessage());
            recordsError++;
        } catch (Exception e) {
            logMessage("ERROR: Unexpected error processing customer record: " + e.getMessage());
            recordsError++;
        }
    }

    private void processSingleCustomer(CustomerRecord customer) {
        // COBOLの評価文（EVALUATE）に対応
        switch (customer.getCustomerStatus()) {
            case 'A':
                processActiveCustomer(customer);
                break;
            case 'I':
                processInactiveCustomer(customer);
                break;
            case 'S':
                processSuspendedCustomer(customer);
                break;
            default:
                processUnknownCustomer(customer);
        }
    }

    private void processSalesTransactions() {
        logger.info("Processing Sales Transaction File...");

        try (BufferedReader reader = new BufferedReader(new FileReader(SALES_TRANSACTIONS_FILE))) {
            String line;
            while ((line = reader.readLine()) != null) {
                processSalesLine(line);
            }
        } catch (IOException e) {
            logger.severe("Error reading sales file: " + e.getMessage());
            recordsError++;
        }

        logger.info("Sales transactions processing completed");
    }

    private void processSalesLine(String line) {
        if (line.length() < 50)
            return;

        try {
            SalesRecord sales = new SalesRecord();

            // COBOL的な固定位置データ読み込み
            sales.setTransactionId(Long.parseLong(line.substring(0, 8).trim()));
            sales.setCustomerId(Integer.parseInt(line.substring(8, 15).trim()));
            sales.setProductCode(line.substring(15, 25).trim());
            sales.setQuantity(Integer.parseInt(line.substring(25, 30).trim()));
            sales.setUnitPrice(new BigDecimal(line.substring(30, 38).trim()));
            sales.setTotalAmount(new BigDecimal(line.substring(38, 47).trim()));
            sales.setTransactionDate(line.substring(47, 55).trim());
            sales.setSalespersonId(Integer.parseInt(line.substring(55, 59).trim()));

            salesRecords.add(sales);
            processTransactionRecord(sales);

        } catch (Exception e) {
            logger.severe("Error processing sales record: " + e.getMessage());
            recordsError++;
        }
    }

    private void processTransactionRecord(SalesRecord sales) {
        // 設定ファイルから税率と手数料率を取得
        BigDecimal taxRate = new BigDecimal(config.getProperty("tax.rate", "0.08"));
        BigDecimal commissionRate = new BigDecimal(config.getProperty("commission.rate", "0.05"));

        // COBOLの演算処理に対応
        calculatedTax = sales.getTotalAmount().multiply(taxRate)
                .setScale(2, RoundingMode.HALF_UP);

        calculatedCommission = sales.getTotalAmount().multiply(commissionRate)
                .setScale(2, RoundingMode.HALF_UP);

        grossSales = grossSales.add(sales.getTotalAmount());

        // 月別売上への加算
        addToMonthlySales(sales);

        logMessage("Processed transaction: " + sales.getTransactionId());
    }

    private void addToMonthlySales(SalesRecord sales) {
        try {
            int month = Integer.parseInt(sales.getTransactionDate().substring(4, 6));
            // 1-12の範囲で月別統計を更新
            if (month >= 1 && month <= 12) {
                MonthlySalesData monthData = monthlySales.get(month - 1);
                monthData.addToMonthSales(sales.getTotalAmount());
                monthData.addCustomer();
            }
        } catch (Exception e) {
            logMessage("Error processing monthly sales data: " + e.getMessage());
        }
    }

    // ================================================================
    // 統計処理（COBOLのGENERATE-STATISTICS paragraphに対応）
    // ================================================================

    private void generateStatistics() {
        logger.info("Generating Statistics...");

        if (!statisticsCalculated) {
            calculateWeightedAverage();
            calculatePercentages();
            calculateStatusCounts();
            statisticsCalculated = true;
        }

        displayFinancialSummary();
        displayStatusStatistics();

        logger.info("Statistics generation completed");
    }

    private void calculateWeightedAverage() {
        // COBOLの算術演算に対応
        BigDecimal taxableAmount = grossSales.subtract(calculatedTax);

        if (taxableAmount.compareTo(BigDecimal.ZERO) > 0) {
            weightedAverage = taxableAmount.divide(new BigDecimal("2"), 3, RoundingMode.HALF_UP);
        }
    }

    private void calculatePercentages() {
        if (grossSales.compareTo(BigDecimal.ZERO) > 0) {
            percentageValue = calculatedCommission.divide(grossSales, 4, RoundingMode.HALF_EVEN)
                    .multiply(new BigDecimal("100"))
                    .setScale(2, RoundingMode.HALF_UP);
        }
    }

    private void calculateStatusCounts() {
        statusCountCache = customerRecords.parallelStream()
                .collect(Collectors.groupingBy(
                        CustomerRecord::getCustomerStatus,
                        Collectors.counting()));
    }

    private void displayFinancialSummary() {
        logger.info("Financial Summary:");
        logger.info("==================");
        logger.info("Gross Sales:     " + formatCurrency(grossSales));
        logger.info("Calculated Tax:   " + formatCurrency(calculatedTax));
        logger.info("Commission:       " + formatCurrency(calculatedCommission));
        logger.info("Weighted Average: " + formatCurrency(weightedAverage));
        logger.info("Percentage:       " + percentageValue + "%");
    }

    private void displayStatusStatistics() {
        logger.info("Customer Status Statistics:");
        logger.info("===========================");
        statusCountCache.forEach((status, count) -> {
            String statusName = getStatusName(status);
            logger.info(statusName + ": " + count + " customers");
        });
        logger.info("Total Customers: " + customerRecords.size());
    }

    private String getStatusName(char status) {
        switch (status) {
            case 'A':
                return "Active";
            case 'I':
                return "Inactive";
            case 'S':
                return "Suspended";
            default:
                return "Unknown (" + status + ")";
        }
    }

    // ================================================================
    // 文字列操作（COBOLのDEMONSTRATE-STRING-OPERATIONS paragraphに対応）
    // ================================================================

    private void demonstrateStringOperations() {
        logger.info("Demonstrating String Operations...");

        // STRING文（文字列結合）のJava実装
        String fullName = buildFullName("John", "Smith");
        logger.info("Full name constructed: " + fullName);

        // UNSTRING文（文字列分解）のJava実装
        String[] dateParts = parseDate("20250128");
        logger.info("Date parsed - Year: " + dateParts[0] +
                ", Month: " + dateParts[1] +
                ", Day: " + dateParts[2]);

        // INSPECT文（文字列検査・置換）のJava実装
        String testString = "Hello World Program";
        logger.info("Original string: " + testString);
        logger.info("After space replacement: " + testString.replace(" ", "-"));
        logger.info("Character count before 'W': " + countCharsBefore(testString, 'W'));

        logger.info("String operations completed");
    }

    private String buildFullName(String firstName, String lastName) {
        return firstName + " " + lastName; // COBOLのSTRING文に対応
    }

    private String[] parseDate(String dateString) {
        // COBOLのUNSTRING文に対応
        return new String[] {
                dateString.substring(0, 4), // Year
                dateString.substring(4, 6), // Month
                dateString.substring(6, 8) // Day
        };
    }

    private int countCharsBefore(String text, char delimiter) {
        // COBOLのINSPECT TALLYINGに対応
        int index = text.indexOf(delimiter);
        return index >= 0 ? index : text.length();
    }

    // ================================================================
    // 条件分岐（COBOLのDEMONSTRATE-CONDITIONAL-LOGIC paragraphに対応）
    // ================================================================

    private void demonstrateConditionalLogic() {
        logger.info("Demonstrating Conditional Logic...");

        // 顧客ステータスの評価
        for (CustomerRecord customer : customerRecords) {
            evaluateCustomerStatus(customer);
            evaluateCreditLimit(customer);
        }

        logger.info("Conditional logic demonstration completed");
    }

    private void evaluateCustomerStatus(CustomerRecord customer) {
        // COBOLのIF-ELSE文に対応
        if (customer.getCustomerStatus() == 'A') {
            logger.info(CUSTOMER_PREFIX + customer.getCustomerId() + " is active");
        } else if (customer.getCustomerStatus() == 'I') {
            logger.info(CUSTOMER_PREFIX + customer.getCustomerId() + " is inactive");
        } else {
            logger.info(CUSTOMER_PREFIX + customer.getCustomerId() + " status unknown");
        }
    }

    private void evaluateCreditLimit(CustomerRecord customer) {
        // COBOLのEVALUATE文（多分岐）に対応
        BigDecimal creditLimit = customer.getCreditLimit();
        String customerType;

        if (creditLimit.compareTo(new BigDecimal("50000")) >= 0) {
            customerType = "Premium customer";
        } else if (creditLimit.compareTo(new BigDecimal("10000")) >= 0) {
            customerType = "Standard customer";
        } else if (creditLimit.compareTo(BigDecimal.ZERO) > 0) {
            customerType = "Basic customer";
        } else {
            customerType = "Credit limit not set";
        }

        logger.info(CUSTOMER_PREFIX + customer.getCustomerId() +
                " credit evaluation: " + customerType);
    }

    // ================================================================
    // ループ処理（COBOLのDEMONSTRATE-LOOP-CONSTRUCTS paragraphに対応）
    // ================================================================

    private void demonstrateLoopConstructs() {
        logger.info("Demonstrating Loop Constructs...");

        // FOR文（COBOLのPERFORM VARYING文に対応）
        demonstrateForLoop();

        // 配列を使ったループ
        demonstrateIndexedLoop();

        // ネストしたループ
        demonstrateNestedLoops();

        logger.info("Loop constructs demonstration completed");
    }

    private void demonstrateForLoop() {
        logger.info("For loop demonstration:");
        for (int i = 1; i <= 10; i++) {
            logger.info("  Counter: " + i);
            processLoopIteration(i);
        }
    }

    private void demonstrateIndexedLoop() {
        logger.info("Indexed loop demonstration:");
        for (int i = 0; i < monthlySales.size(); i++) {
            MonthlySalesData monthData = monthlySales.get(i);
            logger.info("  " + monthData.toString());
        }
    }

    private void demonstrateNestedLoops() {
        logger.info("Nested loop demonstration:");
        for (int monthIndex = 0; monthIndex < 3; monthIndex++) { // 最初の3ヶ月
            logger.info("  Month: " + monthlySales.get(monthIndex).getMonthName());
            for (int productIndex = 0; productIndex < 3; productIndex++) {
                logger.info("    Product " + (productIndex + 1) + " calculation");
            }
        }
    }

    // ================================================================
    // 検索・ソート（COBOLのDEMONSTRATE-SEARCH/SORT OPERATIONSに対応）
    // ================================================================

    private void demonstrateSearchOperations() {
        logger.info("Demonstrating Search Operations...");

        // 配列検索（COBOLのSEARCH文に対応）
        searchCustomerByStatus('A');
        searchCustomerByName("Smith");

        logger.info("Search operations demonstration completed");
    }

    private void searchCustomerByStatus(char targetStatus) {
        logger.info("Searching customers with status: " + targetStatus);

        List<CustomerRecord> foundCustomers = customerRecords.parallelStream()
                .filter(customer -> customer.getCustomerStatus() == targetStatus)
                .toList();

        logger.info("Found " + foundCustomers.size() + " matching customers");
        foundCustomers.forEach(customer -> logger.info("  " + customer.toString()));
    }

    private void searchCustomerByName(String targetName) {
        logger.info("Searching customers with name containing: " + targetName);

        List<CustomerRecord> foundCustomers = customerRecords.parallelStream()
                .filter(customer -> customer.getCustomerName().toLowerCase().contains(targetName.toLowerCase()))
                .toList();

        logger.info("Found " + foundCustomers.size() + " matching customers");
        foundCustomers.forEach(customer -> logger.info("  " + customer.toString()));
    }

    private void demonstrateSortOperations() {
        logger.info("Demonstrating Sort Operations...");

        // 売上金額順でソート
        List<SalesRecord> sortedSales = salesRecords.stream()
                .sorted((s1, s2) -> s2.getTotalAmount().compareTo(s1.getTotalAmount()))
                .toList();

        logger.info("Sales records sorted by amount (descending):");
        for (SalesRecord sales : sortedSales) {
            logger.info("  $" + formatCurrency(sales.getTotalAmount()) +
                    " - Transaction " + sales.getTransactionId());
        }

        logger.info("Sort operations demonstration completed");
    }

    // ================================================================
    // レポート生成（COBOLのGENERATE-DETAIL-REPORT paragraphに対応）
    // ================================================================

    private void generateDetailReport() {
        logger.info("Generating Detailed Report...");

        try (PrintWriter writer = new PrintWriter(new FileWriter(REPORT_OUTPUT_FILE))) {
            writeReportHeading(writer);
            writeMonthlySummary(writer);
            writeFinancialSummary(writer);
            writeReportFooting(writer);

            logger.info("Detailed report generated: " + REPORT_OUTPUT_FILE);
        } catch (IOException e) {
            logger.severe("Error generating report: " + e.getMessage());
        }
    }

    private void writeReportHeading(PrintWriter writer) {
        writer.println(PROGRAM_TITLE);
        writer.println("Sales Analysis and Customer Statistics");
        writer.println("Generated on: " + LocalDateTime.now().format(TIMESTAMP_FORMAT));
        writer.println();
        writer.println("Monthly Sales Summary:");
    }

    private void writeMonthlySummary(PrintWriter writer) {
        for (MonthlySalesData monthData : monthlySales) {
            writer.println(monthData.toString());
        }
        writer.println();
    }

    private void writeFinancialSummary(PrintWriter writer) {
        writer.println("Financial Summary:");
        writer.println("==================");
        writer.println("Gross Sales:     " + formatCurrency(grossSales));
        writer.println("Calculated Tax:   " + formatCurrency(calculatedTax));
        writer.println("Commission:       " + formatCurrency(calculatedCommission));
        writer.println("Weighted Average: " + formatCurrency(weightedAverage));
        writer.println("Percentage:       " + percentageValue + "%");
        writer.println();
    }

    private void writeReportFooting(PrintWriter writer) {
        writer.println("=====================================");
        writer.println("Report completed at: " + LocalDateTime.now().format(TIMESTAMP_FORMAT));
        writer.println("Records processed: " + recordsProcessed);
        writer.println("Records with errors: " + recordsError);
    }

    // ================================================================
    // その他の処理メソッド
    // ================================================================

    private void generateReportHeading() {
        logger.info("Generating Report Heading...");
        logger.info(PROGRAM_TITLE);
        logger.info("Sales Analysis and Customer Statistics");
        logger.info("Generated on: " + LocalDateTime.now().format(TIMESTAMP_FORMAT));
    }

    private void processProductTable() {
        logger.info("Processing Product Table...");
        logger.info("Product table processing completed");
    }

    private void demonstrateComputationalData() {
        logger.info("Demonstrating Computational Data...");

        // BigDecimalを使用したCOBOL的計算
        BigDecimal amount1 = new BigDecimal("12345.67");
        BigDecimal amount2 = new BigDecimal("5432.10");

        BigDecimal sum = amount1.add(amount2);
        BigDecimal product = amount1.multiply(new BigDecimal("2"));
        BigDecimal division = amount1.divide(new BigDecimal("3"), 2, RoundingMode.HALF_UP);

        logger.info("Computational examples:");
        logger.info("  Amount1: " + formatCurrency(amount1));
        logger.info("  Amount2: " + formatCurrency(amount2));
        logger.info("  Sum: " + formatCurrency(sum));
        logger.info("  Product: " + formatCurrency(product));
        logger.info("  Division: " + formatCurrency(division));

        logger.info("Computational data demonstration completed");
    }

    private void performMathematicalCalculations() {
        logger.info("Performing Mathematical Calculations...");

        // COBOLの基本演算をJava実装
        BigDecimal baseAmount = new BigDecimal("10000.00");
        BigDecimal taxRate = new BigDecimal("0.08");
        BigDecimal commissionRate = new BigDecimal("0.05");

        BigDecimal taxAmount = baseAmount.multiply(taxRate);
        BigDecimal commissionAmount = baseAmount.multiply(commissionRate);
        BigDecimal netAmount = baseAmount.subtract(taxAmount);

        logger.info("Mathematical calculation results:");
        logger.info("  Base Amount: " + formatCurrency(baseAmount));
        logger.info("  Tax Amount (8%): " + formatCurrency(taxAmount));
        logger.info("  Commission (5%): " + formatCurrency(commissionAmount));
        logger.info("  Net Amount: " + formatCurrency(netAmount));

        logger.info("Mathematical calculations completed");
    }

    private void finalizeProgram() {
        logger.info("Finalizing program...");

        LocalDateTime endTime = LocalDateTime.now();
        logMessage("Program end time: " + endTime.format(TIMESTAMP_FORMAT));

        logger.info("Program execution completed successfully");
        logger.info("Records processed: " + recordsProcessed);
        logger.info("Records with errors: " + recordsError);
        logger.info("=====================================");

        // ログファイル出力
        writeLogFile();
    }

    // ================================================================
    // データ検証メソッド
    // ================================================================

    private void validateCustomerRecord(CustomerRecord customer) {
        if (customer.getCustomerId() <= 0) {
            throw new IllegalArgumentException("Invalid customer ID: " + customer.getCustomerId());
        }

        if (customer.getCustomerName() == null || customer.getCustomerName().trim().isEmpty()) {
            throw new IllegalArgumentException("Customer name cannot be empty");
        }

        if (customer.getCreditLimit().compareTo(BigDecimal.ZERO) < 0) {
            throw new IllegalArgumentException("Credit limit cannot be negative: " + customer.getCreditLimit());
        }

        if (customer.getCustomerStatus() != 'A' && customer.getCustomerStatus() != 'I' &&
                customer.getCustomerStatus() != 'S') {
            throw new IllegalArgumentException("Invalid customer status: " + customer.getCustomerStatus());
        }

        // 日付形式の検証（YYYYMMDD）
        if (customer.getDateCreated() != null && !customer.getDateCreated().trim().isEmpty()) {
            validateDateFormat(customer.getDateCreated());
        }
    }

    private void validateDateFormat(String dateString) {
        if (dateString.length() != 8) {
            throw new IllegalArgumentException("Invalid date format: " + dateString + " (expected YYYYMMDD)");
        }

        try {
            int year = Integer.parseInt(dateString.substring(0, 4));
            int month = Integer.parseInt(dateString.substring(4, 6));
            int day = Integer.parseInt(dateString.substring(6, 8));

            if (year < 1900 || year > 2100) {
                throw new IllegalArgumentException("Invalid year: " + year);
            }
            if (month < 1 || month > 12) {
                throw new IllegalArgumentException("Invalid month: " + month);
            }
            if (day < 1 || day > 31) {
                throw new IllegalArgumentException("Invalid day: " + day);
            }
        } catch (NumberFormatException e) {
            throw new IllegalArgumentException("Invalid date format: " + dateString);
        }
    }

    // ================================================================
    // ユーティリティメソッド
    // ================================================================

    private String formatCurrency(BigDecimal amount) {
        return String.format("%,.2f", amount);
    }

    private void logMessage(String message) {
        String timestamp = LocalDateTime.now().format(TIMESTAMP_FORMAT);
        logMessages.append(timestamp).append(" - ").append(message).append("\n");
    }

    private void writeLogFile() {
        try (PrintWriter logWriter = new PrintWriter(new FileWriter("lambda.log"))) {
            logWriter.print(logMessages.toString());
        } catch (IOException e) {
            logger.severe("Error writing log file: " + e.getMessage());
        }
    }

    // 各状態別処理メソッド
    private void processActiveCustomer(CustomerRecord customer) {
        logMessage("Processing active customer: " + customer.getCustomerId());
    }

    private void processInactiveCustomer(CustomerRecord customer) {
        logMessage("Processing inactive customer: " + customer.getCustomerId());
    }

    private void processSuspendedCustomer(CustomerRecord customer) {
        logMessage("Processing suspended customer: " + customer.getCustomerId());
    }

    private void processUnknownCustomer(CustomerRecord customer) {
        logMessage("Processing unknown status customer: " + customer.getCustomerId());
    }

    private void processLoopIteration(int iteration) {
        // 単独処理の具体例
        if (iteration % 3 == 0) {
            logger.info("    Multiple of 3 found: " + iteration);
        }
    }

    // ================================================================
    // デバッグ用メソッド（ステップ実行用）
    // ================================================================

    public void debugInitializeProgram() {
        System.out.println("🔍 デバッグ: 初期化処理開始");
        initializeProgram();
        System.out.println("✅ デバッグ: 初期化処理完了");
    }

    public void debugProcessCustomerFile() {
        System.out.println("🔍 デバッグ: 顧客ファイル処理開始");
        processCustomerFile();
        System.out.println("✅ デバッグ: 顧客ファイル処理完了 - 処理件数: " + recordsProcessed);
    }

    public void debugProcessSalesTransactions() {
        System.out.println("🔍 デバッグ: 売上トランザクション処理開始");
        processSalesTransactions();
        System.out.println("✅ デバッグ: 売上トランザクション処理完了");
    }

    public void debugGenerateStatistics() {
        System.out.println("🔍 デバッグ: 統計生成開始");
        generateStatistics();
        System.out.println("✅ デバッグ: 統計生成完了");
    }

    public void debugDemonstrateStringOperations() {
        System.out.println("🔍 デバッグ: 文字列操作デモ開始");
        demonstrateStringOperations();
        System.out.println("✅ デバッグ: 文字列操作デモ完了");
    }

    public void debugDemonstrateConditionalLogic() {
        System.out.println("🔍 デバッグ: 条件分岐デモ開始");
        demonstrateConditionalLogic();
        System.out.println("✅ デバッグ: 条件分岐デモ完了");
    }

    public void debugDemonstrateLoopConstructs() {
        System.out.println("🔍 デバッグ: ループ処理デモ開始");
        demonstrateLoopConstructs();
        System.out.println("✅ デバッグ: ループ処理デモ完了");
    }

    public void debugDemonstrateSearchOperations() {
        System.out.println("🔍 デバッグ: 検索操作デモ開始");
        demonstrateSearchOperations();
        System.out.println("✅ デバッグ: 検索操作デモ完了");
    }

    public void debugDemonstrateSortOperations() {
        System.out.println("🔍 デバッグ: ソート操作デモ開始");
        demonstrateSortOperations();
        System.out.println("✅ デバッグ: ソート操作デモ完了");
    }

    public void debugGenerateDetailReport() {
        System.out.println("🔍 デバッグ: レポート生成開始");
        generateDetailReport();
        System.out.println("✅ デバッグ: レポート生成完了");
    }

    public void debugFinalizeProgram() {
        System.out.println("🔍 デバッグ: 終了処理開始");
        finalizeProgram();
        System.out.println("✅ デバッグ: 終了処理完了");
    }

    // デバッグ用の状態表示メソッド
    public void debugPrintState() {
        System.out.println("\n📊 現在の状態:");
        System.out.println("  - 処理済みレコード数: " + recordsProcessed);
        System.out.println("  - エラーレコード数: " + recordsError);
        System.out.println("  - 顧客レコード数: " + customerRecords.size());
        System.out.println("  - 売上レコード数: " + salesRecords.size());
        System.out.println("  - 総売上: " + formatCurrency(grossSales));
        System.out.println("  - 計算済み税金: " + formatCurrency(calculatedTax));
        System.out.println("  - 計算済み手数料: " + formatCurrency(calculatedCommission));
    }
}

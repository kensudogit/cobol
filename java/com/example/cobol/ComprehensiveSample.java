package com.example.cobol;

import java.io.*;
import java.math.BigDecimal;
import java.math.RoundingMode;
import java.nio.file.*;
import java.time.LocalDate;
import java.time.LocalDateTime;
import java.time.format.DateTimeFormatter;
import java.util.*;
import java.util.stream.Collectors;

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

    // ファイルパス定義（実際に使用されるもののみ）
    private static final String CUSTOMER_MASTER_FILE = "CUSTOMER.MAST";
    private static final String SALES_TRANSACTIONS_FILE = "SALES.TRAN";
    private static final String REPORT_OUTPUT_FILE = "DAILY-REPORT.TXT";

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
                    monthName, formatCurrencyStatic(monthSales), monthCustomers);
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

    // 計算用定数
    private static final BigDecimal TAX_RATE = new BigDecimal("0.08");
    private static final BigDecimal COMMISSION_RATE = new BigDecimal("0.05");

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
        System.out.println(PROGRAM_TITLE);
        System.out.println();

        ComprehensiveSample program = new ComprehensiveSample();

        try {
            program.executeProgram();
        } catch (Exception e) {
            System.err.println("Program execution failed: " + e.getMessage());
            e.printStackTrace();
        }
    }

    /**
     * プログラムのメイン実行フロー
     */
    public void executeProgram() {
        System.out.println("Starting COBOL Comprehensive Sample Program (Java Implementation)");
        System.out.println();

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
        System.out.println("Initializing program...");

        // 月名テーブルの初期化
        initializeMonthlyTable();

        // 現在の日付と時刻を記録
        LocalDateTime startTime = LocalDateTime.now();
        logMessage("Program start time: " + startTime.format(TIMESTAMP_FORMAT));

        System.out.println("Program initialized successfully");
        System.out.println();
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
        System.out.println("Processing Customer Master File...");

        try (BufferedReader reader = new BufferedReader(new FileReader(CUSTOMER_MASTER_FILE))) {
            String line;
            while ((line = reader.readLine()) != null) {
                processCustomerLine(line);
            }
        } catch (IOException e) {
            System.err.println("Error reading customer file: " + e.getMessage());
            recordsError++;
        }

        System.out.println("Customer file processing completed");
        System.out.println("Records processed: " + recordsProcessed);
        System.out.println("Records with errors: " + recordsError);
        System.out.println();
    }

    private void processCustomerLine(String line) {
        if (line.length() < 125)
            return; // 最小レコード長チェック

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

            customerRecords.add(customer);
            processSingleCustomer(customer);
            recordsProcessed++;

        } catch (Exception e) {
            System.err.println("Error processing customer record: " + e.getMessage());
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
        System.out.println("Processing Sales Transaction File...");

        try (BufferedReader reader = new BufferedReader(new FileReader(SALES_TRANSACTIONS_FILE))) {
            String line;
            while ((line = reader.readLine()) != null) {
                processSalesLine(line);
            }
        } catch (IOException e) {
            System.err.println("Error reading sales file: " + e.getMessage());
            recordsError++;
        }

        System.out.println("Sales transactions processing completed");
        System.out.println();
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
            System.err.println("Error processing sales record: " + e.getMessage());
            recordsError++;
        }
    }

    private void processTransactionRecord(SalesRecord sales) {
        // COBOLの演算処理に対応
        calculatedTax = sales.getTotalAmount().multiply(TAX_RATE)
                .setScale(2, RoundingMode.HALF_UP);

        calculatedCommission = sales.getTotalAmount().multiply(COMMISSION_RATE)
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
        System.out.println("Generating Statistics...");

        calculateWeightedAverage();
        calculatePercentages();
        displayFinancialSummary();

        System.out.println("Statistics generation completed");
        System.out.println();
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

    private void displayFinancialSummary() {
        System.out.println("Financial Summary:");
        System.out.println("==================");
        System.out.println("Gross Sales:     " + formatCurrency(grossSales));
        System.out.println("Calculated Tax:   " + formatCurrency(calculatedTax));
        System.out.println("Commission:       " + formatCurrency(calculatedCommission));
        System.out.println("Weighted Average: " + formatCurrency(weightedAverage));
        System.out.println("Percentage:       " + percentageValue + "%");
        System.out.println();
    }

    // ================================================================
    // 文字列操作（COBOLのDEMONSTRATE-STRING-OPERATIONS paragraphに対応）
    // ================================================================

    private void demonstrateStringOperations() {
        System.out.println("Demonstrating String Operations...");

        // STRING文（文字列結合）のJava実装
        String fullName = buildFullName("John", "Smith");
        System.out.println("Full name constructed: " + fullName);

        // UNSTRING文（文字列分解）のJava実装
        String[] dateParts = parseDate("20250128");
        System.out.println("Date parsed - Year: " + dateParts[0] +
                ", Month: " + dateParts[1] +
                ", Day: " + dateParts[2]);

        // INSPECT文（文字列検査・置換）のJava実装
        String testString = "Hello World Program";
        System.out.println("Original string: " + testString);
        System.out.println("After space replacement: " + testString.replace(" ", "-"));
        System.out.println("Character count before 'W': " + countCharsBefore(testString, 'W'));

        System.out.println("String operations completed");
        System.out.println();
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

    // ================================
    // 条件分岐（COBOLのDEMONSTRATE-CONDITIONAL-LOGIC paragraphに対応）
    // ================================================================

    private void demonstrateConditionalLogic() {
        System.out.println("Demonstrating Conditional Logic...");

        // 顧客ステータスの評価
        for (CustomerRecord customer : customerRecords) {
            evaluateCustomerStatus(customer);
            evaluateCreditLimit(customer);
        }

        System.out.println("Conditional logic demonstration completed");
        System.out.println();
    }

    private void evaluateCustomerStatus(CustomerRecord customer) {
        // COBOLのIF-ELSE文に対応
        if (customer.getCustomerStatus() == 'A') {
            System.out.println("Customer " + customer.getCustomerId() + " is active");
        } else if (customer.getCustomerStatus() == 'I') {
            System.out.println("Customer " + customer.getCustomerId() + " is inactive");
        } else {
            System.out.println("Customer " + customer.getCustomerId() + " status unknown");
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

        System.out.println("Customer " + customer.getCustomerId() +
                " credit evaluation: " + customerType);
    }

    // ================================================================
    // ループ処理（COBOLのDEMONSTRATE-LOOP-CONSTRUCTS paragraphに対応）
    // ================================================================

    private void demonstrateLoopConstructs() {
        System.out.println("Demonstrating Loop Constructs...");

        // FOR文（COBOLのPERFORM VARYING文に対応）
        demonstrateForLoop();

        // 配列を使ったループ
        demonstrateIndexedLoop();

        // ネストしたループ
        demonstrateNestedLoops();

        System.out.println("Loop constructs demonstration completed");
        System.out.println();
    }

    private void demonstrateForLoop() {
        System.out.println("For loop demonstration:");
        for (int i = 1; i <= 10; i++) {
            System.out.println("  Counter: " + i);
            processLoopIteration(i);
        }
    }

    private void demonstrateIndexedLoop() {
        System.out.println("Indexed loop demonstration:");
        for (int i = 0; i < monthlySales.size(); i++) {
            MonthlySalesData monthData = monthlySales.get(i);
            System.out.println("  " + monthData.toString());
        }
    }

    private void demonstrateNestedLoops() {
        System.out.println("Nested loop demonstration:");
        for (int monthIndex = 0; monthIndex < 3; monthIndex++) { // 最初の3ヶ月
            System.out.println("  Month: " + monthlySales.get(monthIndex).getMonthName());
            for (int productIndex = 0; productIndex < 3; productIndex++) {
                System.out.println("    Product " + (productIndex + 1) + " calculation");
            }
        }
    }

    // ================================================================
    // 検索・ソート（COBOLのDEMONSTRATE-SEARCH/SORT OPERATIONSに対応）
    // ================================================================

    private void demonstrateSearchOperations() {
        System.out.println("Demonstrating Search Operations...");

        // 配列検索（COBOLのSEARCH文に対応）
        searchCustomerByStatus('A');
        searchCustomerByName("Smith");

        System.out.println("Search operations demonstration completed");
        System.out.println();
    }

    private void searchCustomerByStatus(char targetStatus) {
        System.out.println("Searching customers with status: " + targetStatus);

        List<CustomerRecord> foundCustomers = customerRecords.stream()
                .filter(customer -> customer.getCustomerStatus() == targetStatus)
                .collect(Collectors.toList());

        System.out.println("Found " + foundCustomers.size() + " matching customers");
        for (CustomerRecord customer : foundCustomers) {
            System.out.println("  " + customer.toString());
        }
    }

    private void searchCustomerByName(String targetName) {
        System.out.println("Searching customers with name containing: " + targetName);

        List<CustomerRecord> foundCustomers = customerRecords.stream()
                .filter(customer -> customer.getCustomerName().contains(targetName))
                .collect(Collectors.toList());

        System.out.println("Found " + foundCustomers.size() + " matching customers");
        for (CustomerRecord customer : foundCustomers) {
            System.out.println("  " + customer.toString());
        }
    }

    private void demonstrateSortOperations() {
        System.out.println("Demonstrating Sort Operations...");

        // 売上金額順でソート
        List<SalesRecord> sortedSales = salesRecords.stream()
                .sorted((s1, s2) -> s2.getTotalAmount().compareTo(s1.getTotalAmount()))
                .collect(Collectors.toList());

        System.out.println("Sales records sorted by amount (descending):");
        for (SalesRecord sales : sortedSales) {
            System.out.println("  $" + formatCurrency(sales.getTotalAmount()) +
                    " - Transaction " + sales.getTransactionId());
        }

        System.out.println("Sort operations demonstration completed");
        System.out.println();
    }

    // ================================================================
    // レポート生成（COBOLのGENERATE-DETAIL-REPORT paragraphに対応）
    // ================================================================

    private void generateDetailReport() {
        System.out.println("Generating Detailed Report...");

        try (PrintWriter writer = new PrintWriter(new FileWriter(REPORT_OUTPUT_FILE))) {
            writeReportHeading(writer);
            writeMonthlySummary(writer);
            writeFinancialSummary(writer);
            writeReportFooting(writer);

            System.out.println("Detailed report generated: " + REPORT_OUTPUT_FILE);
        } catch (IOException e) {
            System.err.println("Error generating report: " + e.getMessage());
        }

        System.out.println();
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
        System.out.println("Generating Report Heading...");
        System.out.println(PROGRAM_TITLE);
        System.out.println("Sales Analysis and Customer Statistics");
        System.out.println("Generated on: " + LocalDateTime.now().format(TIMESTAMP_FORMAT));
        System.out.println();
    }

    private void processProductTable() {
        System.out.println("Processing Product Table...");
        System.out.println("Product table processing completed");
        System.out.println();
    }

    private void demonstrateComputationalData() {
        System.out.println("Demonstrating Computational Data...");

        // BigDecimalを使用したCOBOL的計算
        BigDecimal amount1 = new BigDecimal("12345.67");
        BigDecimal amount2 = new BigDecimal("5432.10");

        BigDecimal sum = amount1.add(amount2);
        BigDecimal product = amount1.multiply(new BigDecimal("2"));
        BigDecimal division = amount1.divide(new BigDecimal("3"), 2, RoundingMode.HALF_UP);

        System.out.println("Computational examples:");
        System.out.println("  Amount1: " + formatCurrency(amount1));
        System.out.println("  Amount2: " + formatCurrency(amount2));
        System.out.println("  Sum: " + formatCurrency(sum));
        System.out.println("  Product: " + formatCurrency(product));
        System.out.println("  Division: " + formatCurrency(division));

        System.out.println("Computational data demonstration completed");
        System.out.println();
    }

    private void performMathematicalCalculations() {
        System.out.println("Performing Mathematical Calculations...");

        // COBOLの基本演算をJava実装
        BigDecimal baseAmount = new BigDecimal("10000.00");
        BigDecimal taxRate = new BigDecimal("0.08");
        BigDecimal commissionRate = new BigDecimal("0.05");

        BigDecimal taxAmount = baseAmount.multiply(taxRate);
        BigDecimal commissionAmount = baseAmount.multiply(commissionRate);
        BigDecimal netAmount = baseAmount.subtract(taxAmount);

        System.out.println("Mathematical calculation results:");
        System.out.println("  Base Amount: " + formatCurrency(baseAmount));
        System.out.println("  Tax Amount (8%): " + formatCurrency(taxAmount));
        System.out.println("  Commission (5%): " + formatCurrency(commissionAmount));
        System.out.println("  Net Amount: " + formatCurrency(netAmount));

        System.out.println("Mathematical calculations completed");
        System.out.println();
    }

    private void finalizeProgram() {
        System.out.println("Finalizing program...");

        LocalDateTime endTime = LocalDateTime.now();
        logMessage("Program end time: " + endTime.format(TIMESTAMP_FORMAT));

        System.out.println("Program execution completed successfully");
        System.out.println("Records processed: " + recordsProcessed);
        System.out.println("Records with errors: " + recordsError);
        System.out.println("=====================================");

        // ログファイル出力
        writeLogFile();
    }

    // ================================================================
    // ユーティリティメソッド
    // ================================================================

    private String formatCurrency(BigDecimal amount) {
        return String.format("%,.2f", amount);
    }

    private static String formatCurrencyStatic(BigDecimal amount) {
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
            System.err.println("Error writing log file: " + e.getMessage());
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
            System.out.println("    Multiple of 3 found: " + iteration);
        }
    }
}

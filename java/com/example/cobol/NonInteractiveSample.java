package com.example.cobol;

import java.io.*;
import java.nio.file.*;
import java.time.LocalDateTime;
import java.time.format.DateTimeFormatter;
import java.util.*;
import java.util.concurrent.atomic.AtomicInteger;
import java.util.concurrent.atomic.AtomicLong;

/**
 * Non-Interactive COBOL Sample Program - Java Version
 * 
 * This program implements the following non-interactive processing features:
 * - File-based input data reading
 * - Parameter file usage
 * - Batch processing automation
 * - Configuration file processing control
 * - Log file output
 * 
 * @author COBOL Education Project
 * @version 1.0
 * @date 2025
 */
public class NonInteractiveSample {

    // ================================================================
    // File Control Variables
    // ================================================================

    private static final String INPUT_DATA_FILE = "INPUT.DAT";
    private static final String PARAMETER_FILE = "PARAMS.DAT";
    private static final String CONFIG_FILE = "CONFIG.DAT";
    private static final String LOG_FILE = "PROCESS.LOG";
    private static final String OUTPUT_FILE = "OUTPUT.DAT";

    private static final String FILE_STATUS_OK = "00";
    private static final String FILE_STATUS_EOF = "10";
    private static final String FILE_STATUS_ERROR = "23";

    // ================================================================
    // Processing Control Variables
    // ================================================================

    private String processingMode = "";
    private boolean batchMode = true;
    private boolean autoProcessing = true;
    private final AtomicInteger errorCount = new AtomicInteger(0);
    private final AtomicInteger successCount = new AtomicInteger(0);

    // ================================================================
    // Parameter Variables
    // ================================================================

    private double taxRate = 0.08;
    private double commissionRate = 0.05;
    private double minimumAmount = 100.00;
    private double maximumAmount = 999999.99;
    private int processingLimit = 1000;
    private int retryCount = 3;

    // ================================================================
    // Calculation Fields
    // ================================================================

    private final AtomicLong grossSales = new AtomicLong(0);
    private final AtomicLong calculatedTax = new AtomicLong(0);
    private final AtomicLong calculatedCommission = new AtomicLong(0);
    private final AtomicLong netAmount = new AtomicLong(0);
    private final AtomicLong totalProcessed = new AtomicLong(0);
    private double averageAmount = 0.0;

    // ================================================================
    // Date/Time Management
    // ================================================================

    private LocalDateTime currentDate;
    private LocalDateTime currentTime;
    private LocalDateTime processingStartTime;
    private LocalDateTime processingEndTime;

    // ================================================================
    // Log Control
    // ================================================================

    private enum LogLevel {
        INFO('I'), WARNING('W'), ERROR('E');

        private final char level;

        LogLevel(char level) {
            this.level = level;
        }

        public char getLevel() {
            return level;
        }
    }

    private PrintWriter logWriter;

    // ================================================================
    // Data Structures
    // ================================================================

    public static class CustomerRecord {
        private int customerId;
        private String customerName;
        private double transactionAmount;
        private String transactionDate;
        private char processingFlag;

        // Constructors
        public CustomerRecord() {
        }

        public CustomerRecord(int customerId, String customerName,
                double transactionAmount, String transactionDate,
                char processingFlag) {
            this.customerId = customerId;
            this.customerName = customerName;
            this.transactionAmount = transactionAmount;
            this.transactionDate = transactionDate;
            this.processingFlag = processingFlag;
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

        public double getTransactionAmount() {
            return transactionAmount;
        }

        public void setTransactionAmount(double transactionAmount) {
            this.transactionAmount = transactionAmount;
        }

        public String getTransactionDate() {
            return transactionDate;
        }

        public void setTransactionDate(String transactionDate) {
            this.transactionDate = transactionDate;
        }

        public char getProcessingFlag() {
            return processingFlag;
        }

        public void setProcessingFlag(char processingFlag) {
            this.processingFlag = processingFlag;
        }

        @Override
        public String toString() {
            return String.format("%d,%s,%.2f,%s,%c",
                    customerId, customerName, transactionAmount, transactionDate, processingFlag);
        }
    }

    public static class OutputRecord {
        private int processedCustomerId;
        private double processedAmount;
        private String processingResult;
        private String processingTimestamp;

        // Constructors
        public OutputRecord() {
        }

        public OutputRecord(int processedCustomerId, double processedAmount,
                String processingResult, String processingTimestamp) {
            this.processedCustomerId = processedCustomerId;
            this.processedAmount = processedAmount;
            this.processingResult = processingResult;
            this.processingTimestamp = processingTimestamp;
        }

        // Getters and Setters
        public int getProcessedCustomerId() {
            return processedCustomerId;
        }

        public void setProcessedCustomerId(int processedCustomerId) {
            this.processedCustomerId = processedCustomerId;
        }

        public double getProcessedAmount() {
            return processedAmount;
        }

        public void setProcessedAmount(double processedAmount) {
            this.processedAmount = processedAmount;
        }

        public String getProcessingResult() {
            return processingResult;
        }

        public void setProcessingResult(String processingResult) {
            this.processingResult = processingResult;
        }

        public String getProcessingTimestamp() {
            return processingTimestamp;
        }

        public void setProcessingTimestamp(String processingTimestamp) {
            this.processingTimestamp = processingTimestamp;
        }

        @Override
        public String toString() {
            return String.format("%d,%.2f,%s,%s",
                    processedCustomerId, processedAmount, processingResult, processingTimestamp);
        }
    }

    // ================================================================
    // Main Procedure
    // ================================================================

    public static void main(String[] args) {
        System.out.println("=====================================");
        System.out.println("Non-Interactive COBOL Sample Program");
        System.out.println("=====================================");

        NonInteractiveSample program = new NonInteractiveSample();

        try {
            program.initializeProgram();
            program.loadConfiguration();
            program.loadParameters();
            program.processInputData();
            program.generateStatistics();
            program.generateSummaryReport();
            program.finalizeProgram();

        } catch (Exception e) {
            System.err.println("Program execution failed: " + e.getMessage());
            e.printStackTrace();
            System.exit(1);
        }

        System.out.println("Program completed successfully");
    }

    // ================================================================
    // Initialization Processing
    // ================================================================

    public void initializeProgram() throws IOException {
        System.out.println("Initializing program...");

        // Initialize variables
        currentDate = LocalDateTime.now();
        currentTime = LocalDateTime.now();
        processingStartTime = LocalDateTime.now();

        // Open log file
        logWriter = new PrintWriter(new FileWriter(LOG_FILE, true));
        writeLogMessage(LogLevel.INFO, "Program initialized successfully");

        System.out.println("Program initialized successfully");
    }

    // ================================================================
    // Configuration File Loading
    // ================================================================

    public void loadConfiguration() throws IOException {
        System.out.println("Loading configuration from file...");

        if (!Files.exists(Paths.get(CONFIG_FILE))) {
            System.out.println("Warning: Configuration file not found");
            writeLogMessage(LogLevel.WARNING, "Configuration file not found");
            return;
        }

        try (BufferedReader reader = Files.newBufferedReader(Paths.get(CONFIG_FILE))) {
            String line;
            while ((line = reader.readLine()) != null) {
                processConfigRecord(line);
            }
        }

        System.out.println("Configuration loaded successfully");
    }

    private void processConfigRecord(String line) {
        String[] parts = line.split("\\s+", 2);
        if (parts.length != 2)
            return;

        String configKey = parts[0];
        String configValue = parts[1];

        switch (configKey) {
            case "TAX_RATE":
                taxRate = Double.parseDouble(configValue);
                break;
            case "COMMISSION_RATE":
                commissionRate = Double.parseDouble(configValue);
                break;
            case "MINIMUM_AMOUNT":
                minimumAmount = Double.parseDouble(configValue);
                break;
            case "MAXIMUM_AMOUNT":
                maximumAmount = Double.parseDouble(configValue);
                break;
            case "PROCESSING_LIMIT":
                processingLimit = Integer.parseInt(configValue);
                break;
            default:
                System.out.println("Unknown config key: " + configKey);
        }
    }

    // ================================================================
    // Parameter File Loading
    // ================================================================

    public void loadParameters() throws IOException {
        System.out.println("Loading parameters from file...");

        if (!Files.exists(Paths.get(PARAMETER_FILE))) {
            System.out.println("Warning: Parameter file not found");
            writeLogMessage(LogLevel.WARNING, "Parameter file not found");
            return;
        }

        try (BufferedReader reader = Files.newBufferedReader(Paths.get(PARAMETER_FILE))) {
            String line;
            while ((line = reader.readLine()) != null) {
                processParameterRecord(line);
            }
        }

        System.out.println("Parameters loaded successfully");
    }

    private void processParameterRecord(String line) {
        String[] parts = line.split(",", 2);
        if (parts.length != 2)
            return;

        String paramType = parts[0];
        String paramValue = parts[1];

        switch (paramType) {
            case "MODE":
                processingMode = paramValue;
                break;
            case "RETRY_COUNT":
                retryCount = Integer.parseInt(paramValue);
                break;
            default:
                System.out.println("Unknown parameter type: " + paramType);
        }
    }

    // ================================================================
    // Input Data Processing
    // ================================================================

    public void processInputData() throws IOException {
        System.out.println("Processing input data...");

        if (!Files.exists(Paths.get(INPUT_DATA_FILE))) {
            System.out.println("Error: Input data file not found");
            writeLogMessage(LogLevel.ERROR, "Input data file not found");
            return;
        }

        int recordCounter = 0;

        try (BufferedReader reader = Files.newBufferedReader(Paths.get(INPUT_DATA_FILE));
                PrintWriter writer = new PrintWriter(new FileWriter(OUTPUT_FILE))) {

            String line;
            while ((line = reader.readLine()) != null && recordCounter < processingLimit) {
                recordCounter++;
                CustomerRecord record = parseCustomerRecord(line);
                processSingleRecord(record, writer);
            }
        }

        System.out.println("Input data processing completed");
        System.out.println("Records processed: " + recordCounter);
    }

    private CustomerRecord parseCustomerRecord(String line) {
        String[] parts = line.split(",");
        if (parts.length != 5) {
            throw new IllegalArgumentException("Invalid record format: " + line);
        }

        return new CustomerRecord(
                Integer.parseInt(parts[0].replaceAll("\\D", "")), // Extract numbers only
                parts[1].trim(),
                Double.parseDouble(parts[2]),
                parts[3].trim(),
                parts[4].charAt(0));
    }

    private void processSingleRecord(CustomerRecord record, PrintWriter writer) {
        if (validateRecord(record)) {
            calculateAmounts(record);
            writeOutputRecord(record, writer);
            successCount.incrementAndGet();
        } else {
            errorCount.incrementAndGet();
            writeLogMessage(LogLevel.ERROR, "Record validation failed for customer: " + record.getCustomerId());
        }
    }

    private boolean validateRecord(CustomerRecord record) {
        if (record.getCustomerId() == 0) {
            writeLogMessage(LogLevel.ERROR, "Invalid customer ID");
            return false;
        }

        if (record.getTransactionAmount() < minimumAmount ||
                record.getTransactionAmount() > maximumAmount) {
            writeLogMessage(LogLevel.WARNING, "Amount out of range");
            return true; // Still process but log warning
        }

        return true;
    }

    private void calculateAmounts(CustomerRecord record) {
        double amount = record.getTransactionAmount();

        long tax = Math.round(amount * taxRate);
        long commission = Math.round(amount * commissionRate);
        long net = Math.round(amount - tax - commission);

        calculatedTax.set(tax);
        calculatedCommission.set(commission);
        netAmount.set(net);

        totalProcessed.addAndGet(Math.round(amount));
    }

    private void writeOutputRecord(CustomerRecord record, PrintWriter writer) {
        OutputRecord outputRecord = new OutputRecord(
                record.getCustomerId(),
                netAmount.get() / 100.0, // Convert back to decimal
                "SUCCESS",
                LocalDateTime.now().format(DateTimeFormatter.ofPattern("yyyyMMddHHmmss")));

        writer.println(outputRecord.toString());
    }

    // ================================================================
    // Statistics Generation
    // ================================================================

    public void generateStatistics() {
        System.out.println("Generating statistics...");

        int totalRecords = successCount.get() + errorCount.get();
        if (totalRecords > 0) {
            averageAmount = totalProcessed.get() / 100.0 / totalRecords;
        }

        System.out.println("Statistics generation completed");
    }

    // ================================================================
    // Summary Report Generation
    // ================================================================

    public void generateSummaryReport() {
        System.out.println("Generating summary report...");

        System.out.println("=====================================");
        System.out.println("PROCESSING SUMMARY REPORT");
        System.out.println("=====================================");
        System.out.println("Total Records Processed: " + (successCount.get() + errorCount.get()));
        System.out.println("Successful Records:      " + successCount.get());
        System.out.println("Error Records:          " + errorCount.get());
        System.out.println("Total Amount Processed:  " + (totalProcessed.get() / 100.0));
        System.out.println("Average Amount:         " + averageAmount);
        System.out.println("Tax Rate Applied:       " + taxRate);
        System.out.println("Commission Rate:        " + commissionRate);
        System.out.println("=====================================");

        writeLogMessage(LogLevel.INFO, "Processing completed successfully");
    }

    // ================================================================
    // Log Functionality
    // ================================================================

    private void writeLogMessage(LogLevel level, String message) {
        String timestamp = LocalDateTime.now().format(DateTimeFormatter.ofPattern("yyyyMMddHHmmss"));
        String logEntry = String.format("%s [%c] %s", timestamp, level.getLevel(), message);

        if (logWriter != null) {
            logWriter.println(logEntry);
            logWriter.flush();
        }

        System.out.println(logEntry);
    }

    // ================================================================
    // Finalization Processing
    // ================================================================

    public void finalizeProgram() {
        System.out.println("Finalizing program...");

        processingEndTime = LocalDateTime.now();

        System.out.println("Program execution completed successfully");
        System.out.println(
                "Start time: " + processingStartTime.format(DateTimeFormatter.ofPattern("yyyy-MM-dd HH:mm:ss")));
        System.out
                .println("End time:   " + processingEndTime.format(DateTimeFormatter.ofPattern("yyyy-MM-dd HH:mm:ss")));
        System.out.println("=====================================");

        if (logWriter != null) {
            logWriter.close();
        }
    }
}

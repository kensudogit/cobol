package com.example.cobol;

import java.io.*;
import java.nio.file.*;
import java.time.LocalDateTime;
import java.time.format.DateTimeFormatter;
import java.util.*;
import java.util.concurrent.ThreadLocalRandom;

/**
 * Automated Data Generation Sample Program - Java Version
 * 
 * This program implements the following automation features:
 * - Test data automatic generation
 * - Random data generation
 * - Automatic data pattern application
 * - Bulk data generation
 * - Automated data validation
 * 
 * @author COBOL Education Project
 * @version 1.0
 * @date 2025
 */
public class AutomatedDataGeneration {

    // ================================================================
    // Data Generation Control Variables
    // ================================================================

    private static final String GENERATION_CONFIG_FILE = "GENCONFIG.DAT";
    private static final String GENERATED_DATA_FILE = "GENDATA.DAT";
    private static final String DATA_TEMPLATE_FILE = "TEMPLATE.DAT";
    private static final String GENERATION_LOG_FILE = "GENLOG.DAT";

    private int totalRecords = 0;
    private int recordsGenerated = 0;
    private char generationMode = 'A'; // A=Auto, M=Manual
    private String dataPattern = "RANDOM";
    private LocalDateTime generationStartTime;
    private LocalDateTime generationEndTime;

    // ================================================================
    // Random Data Generator
    // ================================================================

    private long randomSeed = 12345;
    private ThreadLocalRandom random = ThreadLocalRandom.current();

    // ================================================================
    // Data Pattern Variables
    // ================================================================

    private List<String> namePatterns = new ArrayList<>();
    private List<String> categoryPatterns = new ArrayList<>();
    private List<Character> statusPatterns = new ArrayList<>();

    // ================================================================
    // Statistics
    // ================================================================

    private double totalAmount = 0.0;
    private double averageAmount = 0.0;
    private double minAmount = Double.MAX_VALUE;
    private double maxAmount = 0.0;
    private int[] statusDistribution = new int[5];

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

    public static class GeneratedRecord {
        private int recordSequence;
        private int generatedId;
        private String generatedName;
        private double generatedAmount;
        private String generatedDate;
        private char generatedStatus;
        private String generatedCategory;

        public GeneratedRecord(int recordSequence, int generatedId, String generatedName,
                double generatedAmount, String generatedDate,
                char generatedStatus, String generatedCategory) {
            this.recordSequence = recordSequence;
            this.generatedId = generatedId;
            this.generatedName = generatedName;
            this.generatedAmount = generatedAmount;
            this.generatedDate = generatedDate;
            this.generatedStatus = generatedStatus;
            this.generatedCategory = generatedCategory;
        }

        // Getters
        public int getRecordSequence() {
            return recordSequence;
        }

        public int getGeneratedId() {
            return generatedId;
        }

        public String getGeneratedName() {
            return generatedName;
        }

        public double getGeneratedAmount() {
            return generatedAmount;
        }

        public String getGeneratedDate() {
            return generatedDate;
        }

        public char getGeneratedStatus() {
            return generatedStatus;
        }

        public String getGeneratedCategory() {
            return generatedCategory;
        }

        @Override
        public String toString() {
            return String.format("%08d,%07d,%s,%.2f,%s,%c,%s",
                    recordSequence, generatedId, generatedName, generatedAmount,
                    generatedDate, generatedStatus, generatedCategory);
        }
    }

    // ================================================================
    // Main Procedure
    // ================================================================

    public static void main(String[] args) {
        System.out.println("=====================================");
        System.out.println("Automated Data Generation Program");
        System.out.println("=====================================");

        AutomatedDataGeneration program = new AutomatedDataGeneration();

        try {
            program.initializeGeneration();
            program.loadGenerationConfig();
            program.loadDataTemplates();
            program.generateDataRecords();
            program.generateStatistics();
            program.writeGenerationReport();
            program.finalizeGeneration();

        } catch (Exception e) {
            System.err.println("Data generation failed: " + e.getMessage());
            e.printStackTrace();
            System.exit(1);
        }

        System.out.println("Data generation completed successfully");
    }

    // ================================================================
    // Generation Processing Initialization
    // ================================================================

    public void initializeGeneration() throws IOException {
        System.out.println("Initializing data generation...");

        // Set generation start time
        generationStartTime = LocalDateTime.now();

        // Initialize statistics
        totalAmount = 0.0;
        averageAmount = 0.0;
        minAmount = Double.MAX_VALUE;
        maxAmount = 0.0;
        Arrays.fill(statusDistribution, 0);

        // Open log file
        logWriter = new PrintWriter(new FileWriter(GENERATION_LOG_FILE, true));
        writeLogEntry(LogLevel.INFO, "Data generation started");

        System.out.println("Data generation initialized");
    }

    // ================================================================
    // Generation Configuration Loading
    // ================================================================

    public void loadGenerationConfig() throws IOException {
        System.out.println("Loading generation configuration...");

        if (!Files.exists(Paths.get(GENERATION_CONFIG_FILE))) {
            System.out.println("Warning: Configuration file not found");
            writeLogEntry(LogLevel.WARNING, "Configuration file not found");
            return;
        }

        try (BufferedReader reader = Files.newBufferedReader(Paths.get(GENERATION_CONFIG_FILE))) {
            String line;
            while ((line = reader.readLine()) != null) {
                processConfigRecord(line);
            }
        }

        System.out.println("Generation configuration loaded");
    }

    private void processConfigRecord(String line) {
        String[] parts = line.split(",", 2);
        if (parts.length != 2)
            return;

        String configType = parts[0];
        String configValue = parts[1];

        switch (configType) {
            case "TOTAL_RECORDS":
                totalRecords = Integer.parseInt(configValue);
                break;
            case "GENERATION_MODE":
                generationMode = configValue.charAt(0);
                break;
            case "DATA_PATTERN":
                dataPattern = configValue;
                break;
            case "RANDOM_SEED":
                randomSeed = Long.parseLong(configValue);
                random = ThreadLocalRandom.current();
                break;
            default:
                System.out.println("Unknown config type: " + configType);
        }
    }

    // ================================================================
    // Data Template Loading
    // ================================================================

    public void loadDataTemplates() throws IOException {
        System.out.println("Loading data templates...");

        if (!Files.exists(Paths.get(DATA_TEMPLATE_FILE))) {
            System.out.println("Warning: Template file not found");
            writeLogEntry(LogLevel.WARNING, "Template file not found");
            return;
        }

        try (BufferedReader reader = Files.newBufferedReader(Paths.get(DATA_TEMPLATE_FILE))) {
            String line;
            while ((line = reader.readLine()) != null) {
                processTemplateRecord(line);
            }
        }

        System.out.println("Data templates loaded");
    }

    private void processTemplateRecord(String line) {
        String[] parts = line.split(",", 2);
        if (parts.length != 2)
            return;

        String templatePattern = parts[0];
        String templateType = parts[1];

        switch (templateType) {
            case "NAME":
                if (namePatterns.size() < 20) {
                    namePatterns.add(templatePattern);
                }
                break;
            case "CATEGORY":
                if (categoryPatterns.size() < 10) {
                    categoryPatterns.add(templatePattern);
                }
                break;
            case "STATUS":
                if (statusPatterns.size() < 5) {
                    statusPatterns.add(templatePattern.charAt(0));
                }
                break;
            default:
                System.out.println("Unknown template type: " + templateType);
        }
    }

    // ================================================================
    // Data Record Generation
    // ================================================================

    public void generateDataRecords() throws IOException {
        System.out.println("Generating data records...");

        try (PrintWriter writer = new PrintWriter(new FileWriter(GENERATED_DATA_FILE))) {
            for (recordsGenerated = 1; recordsGenerated <= totalRecords; recordsGenerated++) {
                GeneratedRecord record = generateSingleRecord();
                writer.println(record.toString());

                if (recordsGenerated % 1000 == 0) {
                    System.out.println("Generated " + recordsGenerated + " records...");
                }
            }
        }

        System.out.println("Data generation completed");
        System.out.println("Total records generated: " + recordsGenerated);
    }

    private GeneratedRecord generateSingleRecord() {
        int generatedId = generateRandomId();
        String generatedName = generateRandomName();
        double generatedAmount = generateRandomAmount();
        String generatedDate = generateRandomDate();
        char generatedStatus = generateRandomStatus();
        String generatedCategory = generateRandomCategory();

        // Update statistics
        totalAmount += generatedAmount;

        if (generatedAmount < minAmount) {
            minAmount = generatedAmount;
        }

        if (generatedAmount > maxAmount) {
            maxAmount = generatedAmount;
        }

        return new GeneratedRecord(
                recordsGenerated, generatedId, generatedName, generatedAmount,
                generatedDate, generatedStatus, generatedCategory);
    }

    private int generateRandomId() {
        return random.nextInt(1, 9999999);
    }

    private String generateRandomName() {
        if (namePatterns.isEmpty()) {
            return "GeneratedName" + random.nextInt(1000);
        }

        int index = random.nextInt(namePatterns.size());
        return namePatterns.get(index);
    }

    private double generateRandomAmount() {
        return random.nextDouble(100.0, 999999.0);
    }

    private String generateRandomDate() {
        LocalDateTime baseDate = LocalDateTime.now();
        int randomDays = random.nextInt(365);
        LocalDateTime randomDate = baseDate.plusDays(randomDays);
        return randomDate.format(DateTimeFormatter.ofPattern("yyyyMMdd"));
    }

    private char generateRandomStatus() {
        if (statusPatterns.isEmpty()) {
            char[] statuses = { 'A', 'I', 'S', 'P', 'C' };
            return statuses[random.nextInt(statuses.length)];
        }

        int index = random.nextInt(statusPatterns.size());
        char status = statusPatterns.get(index);
        statusDistribution[index]++;
        return status;
    }

    private String generateRandomCategory() {
        if (categoryPatterns.isEmpty()) {
            String[] categories = { "Premium", "Standard", "Basic", "Economy", "Luxury" };
            return categories[random.nextInt(categories.length)];
        }

        int index = random.nextInt(categoryPatterns.size());
        return categoryPatterns.get(index);
    }

    // ================================================================
    // Statistics Generation
    // ================================================================

    public void generateStatistics() {
        System.out.println("Generating statistics...");

        // Calculate average amount
        if (recordsGenerated > 0) {
            averageAmount = totalAmount / recordsGenerated;
        }

        System.out.println("Statistics generated successfully");
    }

    // ================================================================
    // Generation Report Writing
    // ================================================================

    public void writeGenerationReport() {
        System.out.println("Writing generation report...");

        generationEndTime = LocalDateTime.now();

        System.out.println("=====================================");
        System.out.println("DATA GENERATION REPORT");
        System.out.println("=====================================");
        System.out.println("Generation Mode:      " + generationMode);
        System.out.println("Data Pattern:         " + dataPattern);
        System.out.println("Start Time:           "
                + generationStartTime.format(DateTimeFormatter.ofPattern("yyyy-MM-dd HH:mm:ss")));
        System.out.println("End Time:             "
                + generationEndTime.format(DateTimeFormatter.ofPattern("yyyy-MM-dd HH:mm:ss")));
        System.out.println("Total Records:        " + totalRecords);
        System.out.println("Records Generated:    " + recordsGenerated);
        System.out.println("Total Amount:         " + String.format("%.2f", totalAmount));
        System.out.println("Average Amount:       " + String.format("%.2f", averageAmount));
        System.out.println("Minimum Amount:       " + String.format("%.2f", minAmount));
        System.out.println("Maximum Amount:       " + String.format("%.2f", maxAmount));
        System.out.println("=====================================");

        // Write report to log file
        writeLogEntry(LogLevel.INFO, "Generation report completed");
    }

    // ================================================================
    // Log Functionality
    // ================================================================

    private void writeLogEntry(LogLevel level, String message) {
        String timestamp = LocalDateTime.now().format(DateTimeFormatter.ofPattern("yyyyMMddHHmmss"));
        String logEntry = String.format("%s [%c] %s", timestamp, level.getLevel(), message);

        if (logWriter != null) {
            logWriter.println(logEntry);
            logWriter.flush();
        }

        System.out.println(logEntry);
    }

    // ================================================================
    // Generation Processing Finalization
    // ================================================================

    public void finalizeGeneration() {
        System.out.println("Finalizing generation...");

        if (logWriter != null) {
            logWriter.close();
        }

        System.out.println("Data generation finalized successfully");
        System.out.println("=====================================");
    }
}

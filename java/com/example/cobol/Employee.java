import java.util.Scanner;
import java.math.BigDecimal;
import java.time.LocalDate;
import java.time.format.DateTimeFormatter;

/**
 * 従業員情報管理システム
 * COBOLプログラム employee.cob をJavaに移植
 * 
 * 機能: 従業員情報の登録・表示・管理
 */
public class Employee {

    // 従業員レコード構造体に対応するクラス
    static class EmployeeRecord {
        private int empId; // 従業員ID（5桁の数値）
        private String empName; // 従業員名（30文字の文字列）
        private String department; // 部署名（20文字の文字列）
        private BigDecimal salary; // 給与（7桁整数+2桁小数）
        private String hireDate; // 入社日（10文字の文字列）

        public EmployeeRecord(int empId, String empName, String department,
                BigDecimal salary, String hireDate) {
            this.empId = empId;
            this.empName = empName;
            this.department = department;
            this.salary = salary;
            this.hireDate = hireDate;
        }

        // Getters and Setters
        public int getEmpId() {
            return empId;
        }

        public void setEmpId(int empId) {
            this.empId = empId;
        }

        public String getEmpName() {
            return empName;
        }

        public void setEmpName(String empName) {
            this.empName = empName;
        }

        public String getDepartment() {
            return department;
        }

        public void setDepartment(String department) {
            this.department = department;
        }

        public BigDecimal getSalary() {
            return salary;
        }

        public void setSalary(BigDecimal salary) {
            this.salary = salary;
        }

        public String getHireDate() {
            return hireDate;
        }

        public void setHireDate(String hireDate) {
            this.hireDate = hireDate;
        }
    }

    private static Scanner scanner = new Scanner(System.in);
    private static EmployeeRecord currentEmployee = new EmployeeRecord(0, "", "", BigDecimal.ZERO, "");
    private static boolean continueFlag = true;

    /**
     * メイン処理
     * COBOLのMAIN-PROCEDUREに対応
     */
    public static void main(String[] args) {
        displayHeader();

        while (continueFlag) {
            displayMenu();
            int choice = getUserChoice();

            switch (choice) {
                case 1:
                    addEmployee();
                    break;
                case 2:
                    displayEmployee();
                    break;
                case 3:
                    continueFlag = false;
                    break;
                default:
                    System.out.println("Invalid choice! Please try again.");
            }

            System.out.println();
        }

        System.out.println("Thank you for using Employee Management System!");
    }

    /**
     * プログラムヘッダー表示
     * COBOLのDISPLAY文に対応
     */
    private static void displayHeader() {
        System.out.println("==========================================");
        System.out.println("Employee Management System");
        System.out.println("==========================================");
        System.out.println();
    }

    /**
     * メニュー表示と選択入力
     * COBOLのPERFORM UNTILに対応
     */
    private static void displayMenu() {
        System.out.println("1. Add Employee");
        System.out.println("2. Display Employee");
        System.out.println("3. Exit");
        System.out.print("Enter your choice (1-3): ");
    }

    /**
     * ユーザー選択入力
     * COBOLのACCEPT文に対応
     */
    private static int getUserChoice() {
        try {
            int choice = scanner.nextInt();
            scanner.nextLine(); // 改行文字を消費
            return choice;
        } catch (Exception e) {
            scanner.nextLine(); // エラー時も改行文字を消費
            return -1; // 無効な選択を返す
        }
    }

    /**
     * 従業員追加サブルーチン
     * COBOLのADD-EMPLOYEE paragraphに対応
     */
    private static void addEmployee() {
        try {
            System.out.print("Enter Employee ID: ");
            int empId = scanner.nextInt();
            scanner.nextLine();

            System.out.print("Enter Employee Name: ");
            String empName = scanner.nextLine();

            System.out.print("Enter Department: ");
            String department = scanner.nextLine();

            System.out.print("Enter Salary: ");
            BigDecimal salary = scanner.nextBigDecimal();
            scanner.nextLine();

            System.out.print("Enter Hire Date (YYYY-MM-DD): ");
            String hireDate = scanner.nextLine();

            // 従業員レコードを更新
            currentEmployee = new EmployeeRecord(empId, empName, department, salary, hireDate);

            System.out.println("Employee added successfully!");
            System.out.println();

        } catch (Exception e) {
            System.out.println("Error: Invalid input format.");
            scanner.nextLine();
        }
    }

    /**
     * 従業員表示サブルーチン
     * COBOLのDISPLAY-EMPLOYEE paragraphに対応
     */
    private static void displayEmployee() {
        System.out.println("Employee Information:");
        System.out.println("====================");

        System.out.println("ID: " + currentEmployee.getEmpId());
        System.out.println("Name: " + currentEmployee.getEmpName());
        System.out.println("Department: " + currentEmployee.getDepartment());

        // 給与をカンマ区切り形式で表示（COBOLのWS-DISPLAY-SALARYに対応）
        String formattedSalary = String.format("%,.2f", currentEmployee.getSalary());
        System.out.println("Salary: $" + formattedSalary);

        System.out.println("Hire Date: " + currentEmployee.getHireDate());
        System.out.println();
    }
}

package com.example.cobol;

import java.util.logging.Level;
import java.util.logging.Logger;

/**
 * ComprehensiveSample デバッグ用メインクラス
 * ステップ実行デバッグ用のエントリーポイント
 */
public class ComprehensiveSampleDebugger {

    private static final Logger logger = Logger.getLogger(ComprehensiveSampleDebugger.class.getName());

    public static void main(String[] args) {
        System.out.println("=== ComprehensiveSample デバッグ開始 ===");

        // ログレベルを設定
        Logger.getLogger("").setLevel(Level.ALL);

        ComprehensiveSample program = new ComprehensiveSample();

        try {
            // ステップ実行デバッグ用のメソッド呼び出し
            debugStepByStep(program);
        } catch (Exception e) {
            logger.log(Level.SEVERE, "デバッグ中にエラーが発生しました: " + e.getMessage(), e);
        }

        System.out.println("=== デバッグ完了 ===");
    }

    /**
     * ステップ実行デバッグ用メソッド
     * 各処理を段階的に実行してデバッグ
     */
    private static void debugStepByStep(ComprehensiveSample program) {
        System.out.println("\n--- ステップ1: 初期化処理 ---");
        program.debugInitializeProgram();

        System.out.println("\n--- ステップ2: 顧客ファイル処理 ---");
        program.debugProcessCustomerFile();

        System.out.println("\n--- ステップ3: 売上トランザクション処理 ---");
        program.debugProcessSalesTransactions();

        System.out.println("\n--- ステップ4: 統計生成 ---");
        program.debugGenerateStatistics();

        System.out.println("\n--- ステップ5: 文字列操作デモ ---");
        program.debugDemonstrateStringOperations();

        System.out.println("\n--- ステップ6: 条件分岐デモ ---");
        program.debugDemonstrateConditionalLogic();

        System.out.println("\n--- ステップ7: ループ処理デモ ---");
        program.debugDemonstrateLoopConstructs();

        System.out.println("\n--- ステップ8: 検索・ソートデモ ---");
        program.debugDemonstrateSearchOperations();
        program.debugDemonstrateSortOperations();

        System.out.println("\n--- ステップ9: レポート生成 ---");
        program.debugGenerateDetailReport();

        System.out.println("\n--- ステップ10: 終了処理 ---");
        program.debugFinalizeProgram();
    }
}

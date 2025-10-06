//NONINT JOB (ACCT),'NON-INTERACTIVE COBOL',
//         CLASS=A,MSGCLASS=X,MSGLEVEL=(1,1),NOTIFY=&SYSUID
//* ================================================================
//* 非インタラクティブCOBOL処理のJCLサンプル
//* 
//* このJCLでは以下の非インタラクティブ処理を実装します：
//* - ファイルベースの入力処理
//* - パラメータファイルの使用
//* - バッチ処理の自動化
//* - エラーハンドリング
//* - ログ出力の自動化
//* ================================================================
//
//* ================================================================
//* ステップ1: 入力ファイルの準備
//* ================================================================
//PREPARE EXEC PGM=IEBGENER
//SYSPRINT DD SYSOUT=*
//SYSIN    DD DUMMY
//SYSUT1   DD *
CUSTOMER001,John Smith,1500.50,20250106,Y
CUSTOMER002,Jane Doe,2300.75,20250106,A
CUSTOMER003,Bob Johnson,850.25,20250106,Y
CUSTOMER004,Alice Brown,3200.00,20250106,A
CUSTOMER005,Charlie Wilson,1200.00,20250106,Y
/*
//SYSUT2   DD DSN=&SYSUID..INPUT.DATA,DISP=(NEW,CATLG),
//            SPACE=(TRK,(1,1)),RECFM=FB,LRECL=80
//
//* ================================================================
//* ステップ2: パラメータファイルの準備
//* ================================================================
//PARAMS EXEC PGM=IEBGENER
//SYSPRINT DD SYSOUT=*
//SYSIN    DD DUMMY
//SYSUT1   DD *
TAX_RATE,0.08
COMMISSION_RATE,0.05
MINIMUM_AMOUNT,100.00
MAXIMUM_AMOUNT,999999.99
PROCESSING_LIMIT,1000
RETRY_COUNT,3
/*
//SYSUT2   DD DSN=&SYSUID..PARAMS.DATA,DISP=(NEW,CATLG),
//            SPACE=(TRK,(1,1)),RECFM=FB,LRECL=80
//
//* ================================================================
//* ステップ3: 設定ファイルの準備
//* ================================================================
//CONFIG EXEC PGM=IEBGENER
//SYSPRINT DD SYSOUT=*
//SYSIN    DD DUMMY
//SYSUT1   DD *
TAX_RATE 0.08
COMMISSION_RATE 0.05
MINIMUM_AMOUNT 100.00
MAXIMUM_AMOUNT 999999.99
PROCESSING_LIMIT 1000
/*
//SYSUT2   DD DSN=&SYSUID..CONFIG.DATA,DISP=(NEW,CATLG),
//            SPACE=(TRK,(1,1)),RECFM=FB,LRECL=80
//
//* ================================================================
//* ステップ4: COBOLプログラムのコンパイル
//* ================================================================
//COMPILE EXEC PGM=IGYCRCTL,PARM='NOSEQ,LIB,APOST'
//STEPLIB  DD DSN=IGY.SIGYCOMP,DISP=SHR
//SYSIN    DD DSN=&SYSUID..COBOL.SOURCE(NONINT),DISP=SHR
//SYSLIB   DD DSN=&SYSUID..COBOL.COPYLIB,DISP=SHR
//SYSPRINT DD SYSOUT=*
//SYSOUT   DD SYSOUT=*
//SYSUT1   DD UNIT=SYSDA,SPACE=(CYL,(1,1))
//SYSUT2   DD UNIT=SYSDA,SPACE=(CYL,(1,1))
//SYSUT3   DD UNIT=SYSDA,SPACE=(CYL,(1,1))
//SYSUT4   DD UNIT=SYSDA,SPACE=(CYL,(1,1))
//SYSUT5   DD UNIT=SYSDA,SPACE=(CYL,(1,1))
//SYSUT6   DD UNIT=SYSDA,SPACE=(CYL,(1,1))
//SYSUT7   DD UNIT=SYSDA,SPACE=(CYL,(1,1))
//SYSUT8   DD UNIT=SYSDA,SPACE=(CYL,(1,1))
//SYSUT9   DD UNIT=SYSDA,SPACE=(CYL,(1,1))
//SYSUT10  DD UNIT=SYSDA,SPACE=(CYL,(1,1))
//SYSUT11  DD UNIT=SYSDA,SPACE=(CYL,(1,1))
//SYSUT12  DD UNIT=SYSDA,SPACE=(CYL,(1,1))
//SYSUT13  DD UNIT=SYSDA,SPACE=(CYL,(1,1))
//SYSUT14  DD UNIT=SYSDA,SPACE=(CYL,(1,1))
//SYSUT15  DD UNIT=SYSDA,SPACE=(CYL,(1,1))
//SYSLIN   DD DSN=&SYSUID..COBOL.OBJECT(NONINT),DISP=(NEW,CATLG),
//            SPACE=(TRK,(1,1)),RECFM=FB,LRECL=80
//
//* ================================================================
//* ステップ5: COBOLプログラムのリンク
//* ================================================================
//LINK EXEC PGM=IEWL,PARM='LIST,MAP'
//SYSLIB   DD DSN=CEE.SCEELKED,DISP=SHR
//SYSPRINT DD SYSOUT=*
//SYSOUT   DD SYSOUT=*
//SYSUT1   DD UNIT=SYSDA,SPACE=(CYL,(1,1))
//SYSLIN   DD DSN=&SYSUID..COBOL.OBJECT(NONINT),DISP=SHR
//SYSLMOD  DD DSN=&SYSUID..COBOL.LOADLIB(NONINT),DISP=(NEW,CATLG),
//            SPACE=(TRK,(1,1)),RECFM=U
//
//* ================================================================
//* ステップ6: 非インタラクティブCOBOLプログラムの実行
//* ================================================================
//RUN EXEC PGM=NONINT
//STEPLIB  DD DSN=&SYSUID..COBOL.LOADLIB,DISP=SHR
//INPUT    DD DSN=&SYSUID..INPUT.DATA,DISP=SHR
//PARAMS   DD DSN=&SYSUID..PARAMS.DATA,DISP=SHR
//CONFIG   DD DSN=&SYSUID..CONFIG.DATA,DISP=SHR
//OUTPUT   DD DSN=&SYSUID..OUTPUT.DATA,DISP=(NEW,CATLG),
//            SPACE=(TRK,(1,1)),RECFM=FB,LRECL=80
//LOG      DD DSN=&SYSUID..PROCESS.LOG,DISP=(NEW,CATLG),
//            SPACE=(TRK,(1,1)),RECFM=FB,LRECL=132
//SYSOUT   DD SYSOUT=*
//SYSPRINT DD SYSOUT=*
//
//* ================================================================
//* ステップ7: 結果の検証
//* ================================================================
//VERIFY EXEC PGM=IEBGENER
//SYSPRINT DD SYSOUT=*
//SYSIN    DD DUMMY
//SYSUT1   DD DSN=&SYSUID..OUTPUT.DATA,DISP=SHR
//SYSUT2   DD SYSOUT=*
//
//* ================================================================
//* ステップ8: ログの表示
//* ================================================================
//LOGSHOW EXEC PGM=IEBGENER
//SYSPRINT DD SYSOUT=*
//SYSIN    DD DUMMY
//SYSUT1   DD DSN=&SYSUID..PROCESS.LOG,DISP=SHR
//SYSUT2   DD SYSOUT=*
//
//* ================================================================
//* ステップ9: クリーンアップ（オプション）
//* ================================================================
//CLEANUP EXEC PGM=IDCAMS
//SYSPRINT DD SYSOUT=*
//SYSIN    DD *
DELETE &SYSUID..INPUT.DATA PURGE
DELETE &SYSUID..PARAMS.DATA PURGE
DELETE &SYSUID..CONFIG.DATA PURGE
/*
//
//* ================================================================
//* エラー処理の例
//* ================================================================
//IF (COMPILE.RC > 0) THEN
//ERROR1 EXEC PGM=IEBGENER
//SYSPRINT DD SYSOUT=*
//SYSIN    DD DUMMY
//SYSUT1   DD *
COMPILE ERROR OCCURRED - CHECK COMPILATION OUTPUT
/*
//SYSUT2   DD SYSOUT=*
//ENDIF
//
//IF (LINK.RC > 0) THEN
//ERROR2 EXEC PGM=IEBGENER
//SYSPRINT DD SYSOUT=*
//SYSIN    DD DUMMY
//SYSUT1   DD *
LINK ERROR OCCURRED - CHECK LINK OUTPUT
/*
//SYSUT2   DD SYSOUT=*
//ENDIF
//
//IF (RUN.RC > 0) THEN
//ERROR3 EXEC PGM=IEBGENER
//SYSPRINT DD SYSOUT=*
//SYSIN    DD DUMMY
//SYSUT1   DD *
RUNTIME ERROR OCCURRED - CHECK PROGRAM LOGS
/*
//SYSUT2   DD SYSOUT=*
//ENDIF
//
//* ================================================================
//* 成功時の通知
//* ================================================================
//SUCCESS EXEC PGM=IEBGENER
//SYSPRINT DD SYSOUT=*
//SYSIN    DD DUMMY
//SYSUT1   DD *
NON-INTERACTIVE COBOL PROCESSING COMPLETED SUCCESSFULLY
/*
//SYSUT2   DD SYSOUT=*
//
//* ================================================================
//* 処理完了
//* ================================================================

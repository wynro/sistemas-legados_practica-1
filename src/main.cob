       IDENTIFICATION DIVISION.
       PROGRAM-ID. MAINHRMS.

       DATA DIVISION.
       WORKING-STORAGE SECTION.
       77 CHOICE PIC 9.

       PROCEDURE DIVISION.
       MAIN-PARA.
           DISPLAY(1 1) ERASE.
           DISPLAY (3 15) "*******************************************".
           DISPLAY (5 15) "     HUMAN RESOURCE MANAGEMENT SYSTEM      ".
           DISPLAY (7 15) "*******************************************".
           DISPLAY(10 25) "1. HRMS WRITE".
           DISPLAY(12 25) "2. HRMS READ".
           DISPLAY(14 25) "3. EXIT".
           DISPLAY(16 25) "ENTER YOUR CHOICE :".
           ACCEPT(16 46) CHOICE.
           IF CHOICE = 1
              CALL "EMPWRITE"
              CANCEL "EMPWRITE"
              GO TO MAIN-PARA
           ELSE
             IF CHOICE = 2
                CALL "EMPREAD"
                CANCEL "EMPREAD"
                GO TO MAIN-PARA
             ELSE
                STOP RUN.

       IDENTIFICATION DIVISION.
       PROGRAM-ID. EMPREAD.

       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT EMPFILE ASSIGN TO DISK
           ORGANIZATION IS INDEXED
           ACCESS MODE IS DYNAMIC
           RECORD KEY IS EEMPID
           FILE STATUS IS FSE.

           SELECT LEAVEFILE ASSIGN TO DISK
           ORGANIZATION IS INDEXED
           ACCESS MODE IS DYNAMIC
           RECORD KEY IS LEMPID
           FILE STATUS IS FSL.

           SELECT BRANCHFILE ASSIGN TO DISK
           ORGANIZATION IS INDEXED
           ACCESS MODE IS DYNAMIC
           RECORD KEY IS BBRID
           FILE STATUS IS FSB.

           SELECT DESIGNATIONFILE ASSIGN TO DISK
           ORGANIZATION IS SEQUENTIAL
           ACCESS MODE IS SEQUENTIAL
           FILE STATUS IS FSDES.

           SELECT DEPARTMENTFILE ASSIGN TO DISK
           ORGANIZATION IS INDEXED
           ACCESS MODE IS DYNAMIC
           RECORD KEY IS DEPCODE
           FILE STATUS IS FSDEP.

           SELECT REVISIONFILE ASSIGN TO DISK
           ORGANIZATION IS INDEXED
           ACCESS MODE IS DYNAMIC
           RECORD KEY IS RREVID
           ALTERNATE RECORD KEY IS REMPID
           FILE STATUS IS FSR.

           SELECT PAYMENTFILE ASSIGN TO DISK
           ORGANIZATION IS INDEXED
           ACCESS MODE IS DYNAMIC
           RECORD KEY IS PEMPID
           FILE STATUS IS FSP.

           SELECT CONFIRMATIONFILE ASSIGN TO DISK
           ORGANIZATION IS INDEXED
           ACCESS MODE IS DYNAMIC
           RECORD KEY IS CCONID
           ALTERNATE RECORD KEY IS CEMPID
           FILE STATUS IS FSC.

           SELECT GRADEFILE ASSIGN TO DISK
           ORGANIZATION IS SEQUENTIAL
           ACCESS MODE IS SEQUENTIAL
           FILE STATUS IS FSG.

           SELECT TRANSFERFILE ASSIGN TO DISK
           ORGANIZATION IS INDEXED
           ACCESS MODE IS DYNAMIC
           RECORD KEY IS TTRFID
           FILE STATUS IS FST.

           SELECT EMPPERSONALFILE ASSIGN TO DISK
           ORGANIZATION IS INDEXED
           ACCESS MODE IS DYNAMIC
           RECORD KEY IS EPEMPID
           FILE STATUS IS FSEP.

       DATA DIVISION.
       FILE SECTION.
       FD EMPFILE
           LABEL RECORDS ARE STANDARD
           VALUE OF FILE-ID IS "EMP.DAT".
       01 EMPREC.
           02 EEMPID    PIC X(6).
           02 EEMPNAME  PIC X(25).
           02 EEMPADDR  PIC X(30).
           02 EPHONE    PIC X(10).
           02 EDOJ      PIC X(10).
           02 EDIP      PIC X(10).
           02 EUG       PIC X(4).
           02 EPG       PIC X(4).
           02 EPROFQ    PIC X(4).
           02 ESKILL    PIC X(10).
           02 EGRDNO    PIC 99.
           02 EBRNID    PIC X(6).
           02 EDESID    PIC X(6).

       FD LEAVEFILE
           LABEL RECORDS ARE STANDARD
           VALUE OF FILE-ID IS "LEAVE.DAT".
       01 LEAVEREC.
           02 LEMPID    PIC X(6).
           02 LFMDATE   PIC X(10).
           02 LTODATE   PIC X(10).
           02 LLEVCAT   PIC X(3).

       FD BRANCHFILE
           LABEL RECORDS ARE STANDARD
           VALUE OF FILE-ID IS "BRANCH.DAT".
       01 BRANCHREC.
           02 BBRID    PIC X(6).
           02 BBRNAME  PIC X(15).
           02 BBRADD   PIC X(30).
           02 BBRPH    PIC X(10).
           02 BEMAIL   PIC X(20).
           02 BMGRNAME PIC X(25).

       FD DESIGNATIONFILE
           LABEL RECORDS ARE STANDARD
           VALUE OF FILE-ID IS "DESIG.DAT".
       01 DESIGNATIONREC.
           02 DESID    PIC X(6).
           02 DESIGN   PIC X(15).
           02 DESHRT   PIC X(4).

       FD DEPARTMENTFILE
           LABEL RECORDS ARE STANDARD
           VALUE OF FILE-ID IS "DEPART.DAT".
       01 DEPARTMENTREC.
           02 DEPCODE  PIC X(6).
           02 DEPNAME  PIC X(20).

       FD REVISIONFILE
           LABEL RECORDS ARE STANDARD
           VALUE OF FILE-ID IS "REVISION.DAT".
       01 REVISIONREC.
           02 RREVID   PIC X(6).
           02 REMPID   PIC X(6).
           02 RDESCODE PIC X(6).
           02 RBASIC   PIC 9(6)V99.
           02 RHRA     PIC 9(6)V99.
           02 RDPA     PIC 9(6)V99.
           02 RPPA     PIC 9(6)V99.
           02 REDUA    PIC 9(6)V99.
           02 RTECHJR  PIC 9(6)V99.
           02 RLUNCHA  PIC 9(6)V99.
           02 RCONVEY  PIC 9(6)V99.
           02 RBUSATR  PIC 9(6)V99.
           02 RLTA     PIC 9(6)V99.
           02 RPF      PIC 9(6)V99.
           02 RESI     PIC 9(6)V99.
           02 RREVDATE PIC X(10).

       FD PAYMENTFILE
           LABEL RECORDS ARE STANDARD
           VALUE OF FILE-ID IS "PAYMENT.DAT".
       01 PAYMENTREC.
           02 PEMPID   PIC X(6).
           02 PBASIC   PIC 9(6)V99.
           02 PDA      PIC 9(6)V99.
           02 PCCA     PIC 9(6)V99.
           02 PHRA     PIC 9(6)V99.
           02 PDPA     PIC 9(6)V99.
           02 PPPA     PIC 9(6)V99.
           02 PEDUA    PIC 9(6)V99.
           02 PTECHJR  PIC 9(6)V99.
           02 PLUNCHA  PIC 9(6)V99.
           02 PCONVEY  PIC 9(6)V99.
           02 PBUSATR  PIC 9(6)V99.
           02 PLTA     PIC 9(6)V99.
           02 PPF      PIC 9(6)V99.
           02 PESI     PIC 9(6)V99.
           02 PGRTY    PIC 9(6)V99.
           02 PPTAX    PIC 9(6)V99.
           02 PITAX    PIC 9(6)V99.
           02 PLOAN    PIC 9(8)V99.
           02 PLOANDA  PIC 9(8)V99.
           02 POTHERD  PIC 9(6)V99.
           02 PPERINC  PIC 9(6)V99.
           02 PMEDI    PIC 9(6)V99.
           02 PBOOK    PIC 9(6)V99.
           02 PENTER   PIC 9(6)V99.
           02 PTPH     PIC 9(6)V99.
           02 PHOUSE   PIC 9(6)V99.
           02 PVEHMAN  PIC 9(6)V99.
           02 PCREDIT  PIC 9(6)V99.
           02 PCLUB    PIC 9(6)V99.
           02 PCL      PIC 99.
           02 PSL      PIC 99.
           02 PPL      PIC 99.
           02 PLLOP    PIC 999.
           02 POTHERL  PIC 999.

       FD CONFIRMATIONFILE
           LABEL RECORDS ARE STANDARD
           VALUE OF FILE-ID IS "CONFIRM.DAT".
       01 CONFIRMATIONREC.
           02 CCONID   PIC X(6).
           02 CEMPID   PIC X(6).
           02 CCDATE   PIC X(6).

       FD GRADEFILE
           LABEL RECORDS ARE STANDARD
           VALUE OF FILE-ID IS "GRADE.DAT".
       01 GRADEREC.
           02 GGRADE   PIC 99.
           02 GDESIGN  PIC X(25).

       FD TRANSFERFILE
           LABEL RECORDS ARE STANDARD
           VALUE OF FILE-ID IS "TRANSFER.DAT".
       01 TRANSFERREC.
           02 TTRFID   PIC X(6).
           02 TEMPID   PIC X(6).
           02 TOBRID   PIC X(6).
           02 TTRFDT   PIC X(10).

       FD EMPPERSONALFILE
           LABEL RECORDS ARE STANDARD
           VALUE OF FILE-ID IS "EMPPER.DAT".
       01 EMPPERSONALREC.
           02 EPEMPID  PIC X(6).
           02 EPTADD   PIC X(30).
           02 EPTPH    PIC X(10).
           02 EPDOB    PIC X(10).
           02 EPPOB    PIC X(10).
           02 EPLANG   PIC X(15).
           02 EPBLOOD  PIC X(4).
           02 EPWEIGHT PIC 999.
           02 EPHEIGHT PIC 999.
           02 EPVISION PIC X(15).
           02 EPFATHER PIC X(25).
           02 EPDOBF   PIC X(10).
           02 EPMOTHER PIC X(25).
           02 EPDOBM   PIC X(10).
           02 EPSPOUSE PIC X(25).
           02 EPCHILD  PIC X(25).
           02 EPDOBC   PIC X(10).

       WORKING-STORAGE SECTION.
       77 FSE   PIC XX.
       77 FSL   PIC XX.
       77 FSB   PIC XX.
       77 FSDES PIC XX.
       77 FSDEP PIC XX.
       77 FSR   PIC XX.
       77 FSP   PIC XX.
       77 FSC   PIC XX.
       77 FSG   PIC XX.
       77 FST   PIC XX.
       77 FSEP  PIC XX.
       77 DES   PIC X(6).
       77 GR    PIC 99.
       77 CHOICE PIC 99.

       PROCEDURE DIVISION.
       MAIN-PARA.
           DISPLAY(1 1) ERASE.
           DISPLAY (3 10) "*******************************************".
           DISPLAY (5 10) "     HUMAN RESOURCE MANAGEMENT SYSTEM      ".
           DISPLAY (7 10) "*******************************************".
           DISPLAY(11 5) " 1. EMPLOYEE FILE".
           DISPLAY(12 5) " 2. LEAVE FILE".
           DISPLAY(13 5) " 3. BRANCH FILE".
           DISPLAY(14 5) " 4. DESIGNATION FILE".
           DISPLAY(15 5) " 5. DEPARTMENT FILE".
           DISPLAY(16 5) " 6. REVISION FILE".
           DISPLAY(17 5) " 7. PAYMENT FILE".
           DISPLAY(18 5) " 8. CONFIRMATION FILE".
           DISPLAY(19 5) " 9. GRADE FILE".
           DISPLAY(20 5) "10. TRANSFER FILE".
           DISPLAY(21 5) "11. EMPLOYEE PERSONAL FILE".
           DISPLAY(22 5) "12. EXIT".
           DISPLAY(23 25) "ENTER U R CHOICE :".
           ACCEPT(23 45) CHOICE.
           IF CHOICE = 1
              GO TO EMP-PARA
           ELSE
             IF CHOICE = 2
                GO TO LEAVE-PARA
             ELSE
               IF CHOICE = 3
                  GO TO BRANCH-PARA
               ELSE
                 IF CHOICE = 4
                    GO TO DESIGNATION-PARA
                 ELSE
                   IF CHOICE = 5
                      GO TO DEPARTMENT-PARA
                   ELSE
                     IF CHOICE = 6
                        GO TO REVISION-PARA
                     ELSE
                       IF CHOICE = 7
                          GO TO PAYMENT-PARA
                       ELSE
                          IF CHOICE = 8
                             GO TO CONFIRMATION-PARA
                          ELSE
                            IF CHOICE = 9
                               GO TO GRADE-PARA
                            ELSE
                              IF CHOICE = 10
                                 GO TO TRANSFER-PARA
                              ELSE
                                IF CHOICE = 11
                                   GO TO EMPPERSONAL-PARA
                                 ELSE
                                   EXIT PROGRAM.

       EMP-PARA.
           DISPLAY(1 1) ERASE.
           OPEN INPUT EMPFILE.
           DISPLAY "ENTER CODE :".
           ACCEPT EEMPID.
           DISPLAY(1 1) ERASE.
           READ EMPFILE INVALID KEY GO TO ERROR-EMP-PARA.
           DISPLAY(1 1) " CODE                 :" EEMPID.
           DISPLAY(2 1) " NAME                 :" EEMPNAME.
           DISPLAY(3 1) " ADDRESS              :" EEMPADDR.
           DISPLAY(4 1) " PHONE                :" EPHONE.
           DISPLAY(5 1) " DATE OF JOIN         :" EDOJ.
           DISPLAY(6 1) " DIPLOMA              :" EDIP.
           DISPLAY(7 1) " UG                   :" EUG.
           DISPLAY(8 1) " PG                   :" EPG.
           DISPLAY(9 1) " PROFESSIONAL QUALITY :" EPROFQ.
           DISPLAY(10 1)" SKILL SET            :" ESKILL.
           DISPLAY(11 1)" GRADE NUMBER         :" EGRDNO.
           DISPLAY(12 1)" BRANCH CODE          :" EBRNID.
           DISPLAY(13 1)" DESIGNATION CODE     :" EDESID.
           CLOSE EMPFILE.
           DISPLAY(20 10)
             "PRESS ENTER TO RETURN TO HRMS READ MENU".
           STOP ' '.
           GO TO MAIN-PARA.

       LEAVE-PARA.
           DISPLAY(1 1) ERASE.
           OPEN INPUT LEAVEFILE.
           DISPLAY "ENTER CODE :".
           ACCEPT LEMPID.
           DISPLAY(1 1) ERASE.
           READ LEAVEFILE INVALID KEY GO TO ERROR-LEAVE-PARA.
           DISPLAY(1 1) " CODE           :" LEMPID.
           DISPLAY(2 1) " DATE           :" LFMDATE.
           DISPLAY(3 1) " DATE           :" LTODATE.
           DISPLAY(4 1) " LEAVE CATEGORY :" LLEVCAT.
           CLOSE LEAVEFILE.
           DISPLAY(20 10)
             "PRESS ENTER TO RETURN TO HRMS READ MENU".
           STOP ' '.
           GO TO MAIN-PARA.

        BRANCH-PARA.
           DISPLAY(1 1) ERASE.
           OPEN INPUT BRANCHFILE.
           DISPLAY " BRANCH CODE :".
           ACCEPT BBRID.
           DISPLAY(1 1) ERASE.
           READ BRANCHFILE INVALID KEY GO TO ERROR-BRANCH-PARA.
           DISPLAY(1 1) " BRANCH CODE    :" BBRID.
           DISPLAY(2 1) " BRANCH NAME    :" BBRNAME.
           DISPLAY(3 1) " BRANCH ADDRESS :" BBRADD.
           DISPLAY(4 1) " PHONE          :" BBRPH.
           DISPLAY(5 1) " E-MAIL         :" BEMAIL .
           DISPLAY(5 1) " MANAGER NAME   :" BMGRNAME.
           CLOSE BRANCHFILE.
           DISPLAY(20 10)
             "PRESS ENTER TO RETURN TO HRMS READ MENU".
           STOP ' '.
           GO TO MAIN-PARA.

       DESIGNATION-PARA.
           DISPLAY(1 1) ERASE.
           OPEN INPUT DESIGNATIONFILE.
           DISPLAY "ENTER THE DESIGNATION CODE :".
           ACCEPT DES.
           DISPLAY(1 1) ERASE.
           PERFORM DES-READ-PARA UNTIL FSDES = 10.
       DES-READ-PARA.
           READ DESIGNATIONFILE AT END GO TO DES-EXIT-PARA.
           IF DESID = DES
           DISPLAY(1 1) " DESIGNATION CODE     :" DESID
           DISPLAY(2 1) " DESIGNATION          :" DESIGN
           DISPLAY(3 1) " DESIGNATION IN SHORT :" DESHRT.
       DES-EXIT-PARA.
           CLOSE DESIGNATIONFILE.
           DISPLAY ' '.
           DISPLAY ' '.
           DISPLAY "PRESS ENTER TO RETURN TO HRMS READ MENU".
           STOP ' '.
           GO TO MAIN-PARA.

       DEPARTMENT-PARA.
           DISPLAY(1 1) ERASE.
           OPEN INPUT DEPARTMENTFILE.
           DISPLAY "ENTER DEP CODE :".
           ACCEPT DEPCODE.
           DISPLAY(1 1) ERASE.
           READ DEPARTMENTFILE INVALID KEY
                     GO TO ERROR-DEPARTMENT-PARA.
           DISPLAY(1 1) " DEPARTMENT CODE :" DEPCODE.
           DISPLAY(2 1) " DEPARTMENT NAME :" DEPNAME.
           CLOSE DEPARTMENTFILE.
           DISPLAY(20 10)
             "PRESS ENTER TO RETURN TO HRMS READ MENU".
           STOP ' '.
           GO TO MAIN-PARA.

       REVISION-PARA.
           DISPLAY(1 1) ERASE.
           OPEN INPUT REVISIONFILE.
           DISPLAY "ENTER REVISION CODE:".
           ACCEPT RREVID.
           DISPLAY(1 1) ERASE.
           READ REVISIONFILE INVALID KEY
                    GO TO ERROR-REVISION-PARA.
           DISPLAY(1 1) " REVISION CODE           :" RREVID.
           DISPLAY(2 1) " EMPLOYEE CODE           :" REMPID.
           DISPLAY(3 1) " DESIGNATION CODE        :" RDESCODE.
           DISPLAY(4 1) " BASIC                   :" RBASIC.
           DISPLAY(5 1) " HRA                     :" RHRA.
           DISPLAY(6 1) " DPA                     :" RDPA.
           DISPLAY(7 1) " PPA                     :" RPPA.
           DISPLAY(8 1) " EDUCATIONAL ALLOWANCE   :" REDUA.
           DISPLAY(9 1) " TECHNICAL JOURNAL       :" RTECHJR.
           DISPLAY(10 1) " LUNCH ALLOWANCE        :" RLUNCHA.
           DISPLAY(11 1) " CONVEYANCE             :" RCONVEY.
           DISPLAY(12 1) " BUSINESS ATTIREMENT    :" RBUSATR.
           DISPLAY(13 1) " LEAVE TRAVEL ALLOWANCE :" RLTA.
           DISPLAY(14 1) " PF                     :" RPF.
           DISPLAY(15 1) " ESI                    :" RESI.
           DISPLAY(16 1) " REVISED DATE           :" RREVDATE.
           CLOSE REVISIONFILE.
           DISPLAY(20 10)
             "PRESS ENTER TO RETURN TO HRMS READ MENU".
           STOP ' '.
           GO TO MAIN-PARA.

       PAYMENT-PARA.
           DISPLAY(1 1) ERASE.
           OPEN INPUT PAYMENTFILE.
           DISPLAY "ENTER EMP CODE :".
           ACCEPT PEMPID.
           DISPLAY(1 1) ERASE.
           READ PAYMENTFILE INVALID KEY GO TO ERROR-PAYMENT-PARA.
           DISPLAY(1 1) " EMPLOYEE CODE                  :" PEMPID.
           DISPLAY(2 1) " BASIC                          :" PBASIC.
           DISPLAY(3 1) " DEARNESS ALLOWANCE             :" PDA.
           DISPLAY(4 1) " CITY COMPENSATORY ALLOWANCE    :" PCCA.
           DISPLAY(5 1) " HRA                            :" PHRA.
           DISPLAY(6 1) " DPA                            :" PDPA.
           DISPLAY(7 1) " PPA                            :" PPPA.
           DISPLAY(8 1) " EDUCATIONAL ALLOWANCE          :" PEDUA.
           DISPLAY(9 1) " TECHNICAL JOURNAL              :" PTECHJR.
           DISPLAY(10 1) " LUNCH ALLOWANCE               :" PLUNCHA.
           DISPLAY(11 1) " CONVEYANCE                    :" PCONVEY.
           DISPLAY(12 1) " BUSINESS ATTIREMENT           :" PBUSATR.
           DISPLAY(13 1) " LEAVE TRAVEL ALLOWANCE        :" PLTA.
           DISPLAY(14 1) " PF                            :" PPF.
           DISPLAY(15 1) " ESI                           :" PESI.
           DISPLAY(16 1) " GRATUITY                      :" PGRTY.
           DISPLAY(17 1) " PROFESSIONAL TAX              :" PPTAX.
           DISPLAY(18 1) " INCOME TAX                    :" PITAX.
           DISPLAY(19 1) " LOAN                          :" PLOAN.
           DISPLAY(20 1) " LOAN DEDUCTION AMOUNT         :" PLOANDA.
           DISPLAY(21 1) " OTHER DEDUCTION               :" POTHERD.
           DISPLAY(22 1) " PERFORMANCE INCENTIVE         :" PPERINC.
           DISPLAY(23 1) " MEDICAL REIMBURSEMENT         :" PMEDI.
           DISPLAY(24 1) " BOOK REIMBURSEMENT            :" PBOOK.
           DISPLAY(1 1) ERASE.
           DISPLAY(1 1) " ENTERTAINMENT                  :" PENTER.
           DISPLAY(2 1) " PHONE                          :" PTPH.
           DISPLAY(3 1) " HOUSE RELATED                  :" PHOUSE.
           DISPLAY(4 1) " VEHICLE MAINTENANCE            :" PVEHMAN.
           DISPLAY(5 1) " CREDIT CARD                    :" PCREDIT.
           DISPLAY(6 1) " CLUB                           :" PCLUB.
           DISPLAY(7 1) " CASUAL LEAVE                   :" PCL.
           DISPLAY(8 1) " SICK LEAVE                     :" PSL.
           DISPLAY(9 1) " PAID LEAVE                     :" PPL
           DISPLAY(10 1) " LEAVE LOSS OF PAY             :" PLLOP.
           DISPLAY(11 1) " OTHER LEAVES                  :" POTHERL.
           CLOSE PAYMENTFILE.
           DISPLAY(20 10)
             "PRESS ENTER TO RETURN TO HRMS READ MENU".
           STOP ' '.
           GO TO MAIN-PARA.

       CONFIRMATION-PARA.
           DISPLAY(1 1) ERASE.
           OPEN INPUT CONFIRMATIONFILE.
           DISPLAY "ENTER CODE :".
           ACCEPT CCONID.
           DISPLAY(1 1) ERASE.
           READ CONFIRMATIONFILE INVALID KEY
                   GO TO ERROR-CONFIRMATION-PARA.
           DISPLAY(1 1) " CONFIRMATION CODE :" CCONID.
           DISPLAY(2 1) " EMPLOYEE CODE     :" CEMPID.
           DISPLAY(3 1) " CONFIRMATION DATE :" CCDATE.
           CLOSE CONFIRMATIONFILE.
           DISPLAY(20 10)
             "PRESS ENTER TO RETURN TO HRMS READ MENU".
           STOP ' '.
           GO TO MAIN-PARA.

       GRADE-PARA.
           DISPLAY(1 1) ERASE.
           OPEN INPUT GRADEFILE.
           DISPLAY "ENTER GRADE NO. :".
           ACCEPT GR.
           DISPLAY(1 1) ERASE.
           PERFORM GR-READ-PARA UNTIL FSG = 10.
       GR-READ-PARA.
           READ GRADEFILE AT END GO TO GR-EXIT-PARA.
           IF GGRADE = GR
           DISPLAY(1 1) " GRADE NO.   :" GGRADE.
           DISPLAY(2 1) " DESIGNATION :" GDESIGN.
       GR-EXIT-PARA.
           CLOSE GRADEFILE.
           DISPLAY ' '.
           DISPLAY ' '.
           DISPLAY "PRESS ENTER TO RETURN TO HRMS READ MENU".
           STOP ' '.
           GO TO MAIN-PARA.

       TRANSFER-PARA.
           DISPLAY(1 1) ERASE.
           OPEN INPUT TRANSFERFILE.
           DISPLAY "ENTER TRANSFER CODE :".
           ACCEPT TTRFID.
           DISPLAY(1 1) ERASE.
           READ TRANSFERFILE INVALID KEY GO TO ERROR-TRANSFER-PARA.
           DISPLAY(1 1) " TRANSFER CODE     :" TTRFID.
           DISPLAY(2 1) " EMP CODE          :" TEMPID.
           DISPLAY(3 1) " OLD BRANCH CODE   :" TOBRID.
           DISPLAY(4 1) " TRANSFER DATE     :" TTRFDT.
           CLOSE TRANSFERFILE.
           DISPLAY(20 10)
             "PRESS ENTER TO RETURN TO HRMS READ MENU".
           STOP ' '.
           GO TO MAIN-PARA.

       EMPPERSONAL-PARA.
           DISPLAY(1 1) ERASE.
           OPEN INPUT EMPPERSONALFILE.
           DISPLAY "ENTER EMP CODE :".
           ACCEPT EPEMPID.
           DISPLAY(1 1) ERASE.
           READ EMPPERSONALFILE INVALID KEY
                           GO TO ERROR-EMPPERSONAL-PARA.
           DISPLAY(1 1) " EMPLOYEE CODE     :" EPEMPID.
           DISPLAY(2 1) " TEMPORARY ADDRESS :" EPTADD.
           DISPLAY(3 1) " PHONE             :" EPTPH.
           DISPLAY(4 1) " DOB               :" EPDOB.
           DISPLAY(5 1) " POB               :" EPPOB.
           DISPLAY(6 1) " LANGUAGE KNOWN    :" EPLANG.
           DISPLAY(7 1) " BLOOD GROUP       :" EPBLOOD.
           DISPLAY(8 1) " WEIGHT            :" EPWEIGHT.
           DISPLAY(9 1) " HEIGHT            :" EPHEIGHT.
           DISPLAY(10 1) " VISION           :" EPVISION.
           DISPLAY(11 1) " FATHER'S NAME    :" EPFATHER.
           DISPLAY(12 1) " DOB OF FATHER    :" EPDOBF.
           DISPLAY(13 1) " MOTHER'S NAME    :" EPMOTHER.
           DISPLAY(14 1) " DOB OF MOTHER    :" EPDOBM.
           DISPLAY(15 1) " SPOUSE NAME      :" EPSPOUSE.
           DISPLAY(16 1) " CHILD NAME       :" EPCHILD.
           DISPLAY(17 1) " DOB OF CHILD     :" EPDOBC.
           CLOSE EMPPERSONALFILE.
           DISPLAY(20 10)
             "PRESS ENTER TO RETURN TO HRMS READ MENU".
           STOP ' '.
           GO TO MAIN-PARA.

       ERROR-EMP-PARA.
           CLOSE EMPFILE.
           DISPLAY(1 1) ERASE.
           DISPLAY(12 30) "INVALID CODE".
           DISPLAY(20 10)
             "PRESS ENTER TO RETURN TO HRMS READ MENU".
           STOP ' '.
           GO TO MAIN-PARA.

       ERROR-LEAVE-PARA.
           CLOSE LEAVEFILE.
           DISPLAY(1 1) ERASE.
           DISPLAY(12 30) "INVALID CODE".
           DISPLAY(20 10)
             "PRESS ENTER TO RETURN TO HRMS READ MENU".
           STOP ' '.
           GO TO MAIN-PARA.

       ERROR-BRANCH-PARA.
           CLOSE BRANCHFILE.
           DISPLAY(1 1) ERASE.
           DISPLAY(12 30) "INVALID CODE".
           DISPLAY(20 10)
             "PRESS ENTER TO RETURN TO HRMS READ MENU".
           STOP ' '.
           GO TO MAIN-PARA.

       ERROR-DEPARTMENT-PARA.
           CLOSE DEPARTMENTFILE.
           DISPLAY(1 1) ERASE.
           DISPLAY(12 30) "INVALID CODE".
           DISPLAY(20 10)
             "PRESS ENTER TO RETURN TO HRMS READ MENU".
           STOP ' '.
           GO TO MAIN-PARA.

       ERROR-REVISION-PARA.
           CLOSE REVISIONFILE.
           DISPLAY(1 1) ERASE.
           DISPLAY(12 30) "INVALID CODE".
           DISPLAY(20 10)
             "PRESS ENTER TO RETURN TO HRMS READ MENU".
           STOP ' '.
           GO TO MAIN-PARA.

       ERROR-PAYMENT-PARA.
           CLOSE PAYMENTFILE.
           DISPLAY(1 1) ERASE.
           DISPLAY(12 30) "INVALID CODE".
           DISPLAY(20 10)
             "PRESS ENTER TO RETURN TO HRMS READ MENU".
           STOP ' '.
           GO TO MAIN-PARA.

       ERROR-CONFIRMATION-PARA.
           CLOSE CONFIRMATIONFILE.
           DISPLAY(1 1) ERASE.
           DISPLAY(12 30) "INVALID CODE".
           DISPLAY(20 10)
             "PRESS ENTER TO RETURN TO HRMS READ MENU".
           STOP ' '.
           GO TO MAIN-PARA.

       ERROR-TRANSFER-PARA.
           CLOSE TRANSFERFILE.
           DISPLAY(1 1) ERASE.
           DISPLAY(12 30) "INVALID CODE".
           DISPLAY(20 10)
             "PRESS ENTER TO RETURN TO HRMS READ MENU".
           STOP ' '.
           GO TO MAIN-PARA.

       ERROR-EMPPERSONAL-PARA.
           CLOSE EMPPERSONALFILE.
           DISPLAY(1 1) ERASE.
           DISPLAY(12 30) "INVALID CODE".
           DISPLAY(20 10)
             "PRESS ENTER TO RETURN TO HRMS READ MENU".
           STOP ' '.
           GO TO MAIN-PARA.

       IDENTIFICATION DIVISION.
       PROGRAM-ID. EMP.

       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT EMPFILE ASSIGN TO DISK
           ORGANIZATION IS INDEXED
           ACCESS MODE IS DYNAMIC
           RECORD KEY IS EEMPID
           FILE STATUS IS FSO.

           SELECT LEAVEFILE ASSIGN TO DISK
           ORGANIZATION IS INDEXED
           ACCESS MODE IS DYNAMIC
           RECORD KEY IS LEMPID
           FILE STATUS IS FSL.

           SELECT BRANCHFILE ASSIGN TO DISK
           ORGANIZATION IS INDEXED
           ACCESS MODE IS DYNAMIC
           RECORD KEY IS BBRID
           FILE STATUS IS FSB.

           SELECT DESIGNATIONFILE ASSIGN TO DISK
           ORGANIZATION IS SEQUENTIAL
           ACCESS MODE IS SEQUENTIAL
           FILE STATUS IS FSDES.

           SELECT DEPARTMENTFILE ASSIGN TO DISK
           ORGANIZATION IS INDEXED
           ACCESS MODE IS DYNAMIC
           RECORD KEY IS DEPCODE
           FILE STATUS IS FSDEP.

           SELECT REVISIONFILE ASSIGN TO DISK
           ORGANIZATION IS INDEXED
           ACCESS MODE IS DYNAMIC
           RECORD KEY IS RREVID
           ALTERNATE RECORD KEY IS REMPID
           FILE STATUS IS FSR.

           SELECT PAYMENTFILE ASSIGN TO DISK
           ORGANIZATION IS INDEXED
           ACCESS MODE IS DYNAMIC
           RECORD KEY IS PEMPID
           FILE STATUS IS FSP.

           SELECT CONFIRMATIONFILE ASSIGN TO DISK
           ORGANIZATION IS INDEXED
           ACCESS MODE IS DYNAMIC
           RECORD KEY IS CCONID
           ALTERNATE RECORD KEY IS CEMPID
           FILE STATUS IS FSC.

           SELECT GRADEFILE ASSIGN TO DISK
           ORGANIZATION IS SEQUENTIAL
           ACCESS MODE IS SEQUENTIAL
           FILE STATUS IS FSG.

           SELECT TRANSFERFILE ASSIGN TO DISK
           ORGANIZATION IS INDEXED
           ACCESS MODE IS DYNAMIC
           RECORD KEY IS TTRFID
           FILE STATUS IS FST.

           SELECT EMPPERSONALFILE ASSIGN TO DISK
           ORGANIZATION IS INDEXED
           ACCESS MODE IS DYNAMIC
           RECORD KEY IS EPEMPID
           FILE STATUS IS FSEP.

       DATA DIVISION.
       FILE SECTION.
       FD EMPFILE
           LABEL RECORDS ARE STANDARD
           VALUE OF FILE-ID IS "EMP.DAT".
       01 EMPREC.
           02 EEMPID    PIC X(6).
           02 EEMPNAME  PIC X(25).
           02 EEMPADDR  PIC X(30).
           02 EPHONE    PIC X(10).
           02 EDOJ      PIC X(10).
           02 EDIP      PIC X(10).
           02 EUG       PIC X(4).
           02 EPG       PIC X(4).
           02 EPROFQ    PIC X(4).
           02 ESKILL    PIC X(10).
           02 EGRDNO    PIC 99.
           02 EBRNID    PIC X(6).
           02 EDESID    PIC X(6).

       FD LEAVEFILE
           LABEL RECORDS ARE STANDARD
           VALUE OF FILE-ID IS "LEAVE.DAT".
       01 LEAVEREC.
           02 LEMPID    PIC X(6).
           02 LFMDATE   PIC X(10).
           02 LTODATE   PIC X(10).
           02 LLEVCAT   PIC X(3).

       FD BRANCHFILE
           LABEL RECORDS ARE STANDARD
           VALUE OF FILE-ID IS "BRANCH.DAT".
       01 BRANCHREC.
           02 BBRID    PIC X(6).
           02 BBRNAME  PIC X(15).
           02 BBRADD   PIC X(30).
           02 BBRPH    PIC X(10).
           02 BEMAIL   PIC X(20).
           02 BMGRNAME PIC X(25).

       FD DESIGNATIONFILE
           LABEL RECORDS ARE STANDARD
           VALUE OF FILE-ID IS "DESIG.DAT".
       01 DESIGNATIONREC.
           02 DESID    PIC X(6).
           02 DESIGN   PIC X(15).
           02 DESHRT   PIC X(4).

       FD DEPARTMENTFILE
           LABEL RECORDS ARE STANDARD
           VALUE OF FILE-ID IS "DEPART.DAT".
       01 DEPARTMENTREC.
           02 DEPCODE  PIC X(6).
           02 DEPNAME  PIC X(20).

       FD REVISIONFILE
           LABEL RECORDS ARE STANDARD
           VALUE OF FILE-ID IS "REVISION.DAT".
       01 REVISIONREC.
           02 RREVID   PIC X(6).
           02 REMPID   PIC X(6).
           02 RDESCODE PIC X(6).
           02 RBASIC   PIC 9(6)V99.
           02 RHRA     PIC 9(6)V99.
           02 RDPA     PIC 9(6)V99.
           02 RPPA     PIC 9(6)V99.
           02 REDUA    PIC 9(6)V99.
           02 RTECHJR  PIC 9(6)V99.
           02 RLUNCHA  PIC 9(6)V99.
           02 RCONVEY  PIC 9(6)V99.
           02 RBUSATR  PIC 9(6)V99.
           02 RLTA     PIC 9(6)V99.
           02 RPF      PIC 9(6)V99.
           02 RESI     PIC 9(6)V99.
           02 RREVDATE PIC X(10).

       FD PAYMENTFILE
           LABEL RECORDS ARE STANDARD
           VALUE OF FILE-ID IS "PAYMENT.DAT".
       01 PAYMENTREC.
           02 PEMPID   PIC X(6).
           02 PBASIC   PIC 9(6)V99.
           02 PDA      PIC 9(6)V99.
           02 PCCA     PIC 9(6)V99.
           02 PHRA     PIC 9(6)V99.
           02 PDPA     PIC 9(6)V99.
           02 PPPA     PIC 9(6)V99.
           02 PEDUA    PIC 9(6)V99.
           02 PTECHJR  PIC 9(6)V99.
           02 PLUNCHA  PIC 9(6)V99.
           02 PCONVEY  PIC 9(6)V99.
           02 PBUSATR  PIC 9(6)V99.
           02 PLTA     PIC 9(6)V99.
           02 PPF      PIC 9(6)V99.
           02 PESI     PIC 9(6)V99.
           02 PGRTY    PIC 9(6)V99.
           02 PPTAX    PIC 9(6)V99.
           02 PITAX    PIC 9(6)V99.
           02 PLOAN    PIC 9(8)V99.
           02 PLOANDA  PIC 9(8)V99.
           02 POTHERD  PIC 9(6)V99.
           02 PPERINC  PIC 9(6)V99.
           02 PMEDI    PIC 9(6)V99.
           02 PBOOK    PIC 9(6)V99.
           02 PENTER   PIC 9(6)V99.
           02 PTPH     PIC 9(6)V99.
           02 PHOUSE   PIC 9(6)V99.
           02 PVEHMAN  PIC 9(6)V99.
           02 PCREDIT  PIC 9(6)V99.
           02 PCLUB    PIC 9(6)V99.
           02 PCL      PIC 99.
           02 PSL      PIC 99.
           02 PPL      PIC 99.
           02 PLLOP    PIC 999.
           02 POTHERL  PIC 999.

       FD CONFIRMATIONFILE
           LABEL RECORDS ARE STANDARD
           VALUE OF FILE-ID IS "CONFIRM.DAT".
       01 CONFIRMATIONREC.
           02 CCONID   PIC X(6).
           02 CEMPID   PIC X(6).
           02 CCDATE   PIC X(6).

       FD GRADEFILE
           LABEL RECORDS ARE STANDARD
           VALUE OF FILE-ID IS "GRADE.DAT".
       01 GRADEREC.
           02 GGRADE   PIC 99.
           02 GDESIGN  PIC X(25).

       FD TRANSFERFILE
           LABEL RECORDS ARE STANDARD
           VALUE OF FILE-ID IS "TRANSFER.DAT".
       01 TRANSFERREC.
           02 TTRFID   PIC X(6).
           02 TEMPID   PIC X(6).
           02 TOBRID   PIC X(6).
           02 TTRFDT   PIC X(10).

       FD EMPPERSONALFILE
           LABEL RECORDS ARE STANDARD
           VALUE OF FILE-ID IS "EMPPER.DAT".
       01 EMPPERSONALREC.
           02 EPEMPID  PIC X(6).
           02 EPTADD   PIC X(30).
           02 EPTPH    PIC X(10).
           02 EPDOB    PIC X(10).
           02 EPPOB    PIC X(10).
           02 EPLANG   PIC X(15).
           02 EPBLOOD  PIC X(4).
           02 EPWEIGHT PIC 999.
           02 EPHEIGHT PIC 999.
           02 EPVISION PIC X(15).
           02 EPFATHER PIC X(25).
           02 EPDOBF   PIC X(10).
           02 EPMOTHER PIC X(25).
           02 EPDOBM   PIC X(10).
           02 EPSPOUSE PIC X(25).
           02 EPCHILD  PIC X(25).
           02 EPDOBC   PIC X(10).

       WORKING-STORAGE SECTION.
       77 FSO   PIC XX.
       77 FSL   PIC XX.
       77 FSB   PIC XX.
       77 FSDES PIC XX.
       77 FSDEP PIC XX.
       77 FSR   PIC XX.
       77 FSP   PIC XX.
       77 FSC   PIC XX.
       77 FSG   PIC XX.
       77 FST   PIC XX.
       77 FSEP  PIC XX.
       77 CHOICE PIC 99.

       PROCEDURE DIVISION.
       MAIN-PARA.
           DISPLAY(1 1) ERASE.
           DISPLAY (3 10) "*******************************************".
           DISPLAY (5 10) "     HUMAN RESOURCE MANAGEMENT SYSTEM      ".
           DISPLAY (7 10) "*******************************************".
           DISPLAY(11 5) " 1. EMPLOYEE FILE".
           DISPLAY(12 5) " 2. LEAVE FILE".
           DISPLAY(13 5) " 3. BRANCH FILE".
           DISPLAY(14 5) " 4. DESIGNATION FILE".
           DISPLAY(15 5) " 5. DEPARTMENT FILE".
           DISPLAY(16 5) " 6. REVISION FILE".
           DISPLAY(17 5) " 7. PAYMENT FILE".
           DISPLAY(18 5) " 8. CONFIRMATION FILE".
           DISPLAY(19 5) " 9. GRADE FILE".
           DISPLAY(20 5) "10. TRANSFER FILE".
           DISPLAY(21 5) "11. EMPLOYEE PERSONAL FILE".
           DISPLAY(22 5) "12. EXIT".
           DISPLAY(23 25) "ENTER U R CHOICE :".
           ACCEPT(23 45) CHOICE.
           IF CHOICE = 1
              GO TO EMP-PARA
           ELSE
             IF CHOICE = 2
                GO TO LEAVE-PARA
             ELSE
               IF CHOICE = 3
                  GO TO BRANCH-PARA
               ELSE
                 IF CHOICE = 4
                    GO TO DESIGNATION-PARA
                 ELSE
                   IF CHOICE = 5
                      GO TO DEPARTMENT-PARA
                   ELSE
                     IF CHOICE = 6
                        GO TO REVISION-PARA
                     ELSE
                       IF CHOICE = 7
                          GO TO PAYMENT-PARA
                       ELSE
                          IF CHOICE = 8
                             GO TO CONFIRMATION-PARA
                          ELSE
                            IF CHOICE = 9
                               GO TO GRADE-PARA
                            ELSE
                              IF CHOICE = 10
                                 GO TO TRANSFER-PARA
                              ELSE
                                IF CHOICE = 11
                                   GO TO EMPPERSONAL-PARA
                                 ELSE
                                   EXIT PROGRAM.

       EMP-PARA.
           DISPLAY(1 1) ERASE.
           OPEN I-O EMPFILE.
           IF FSO = 30
              OPEN OUTPUT EMPFILE.
           DISPLAY(1 1) "ENTER CODE :".
           ACCEPT(1 35) EEMPID.
           DISPLAY(2 1) "ENTER NAME :".
           ACCEPT(2 35) EEMPNAME.
           DISPLAY(3 1) "ENTER ADDRESS :".
           ACCEPT(3 35) EEMPADDR.
           DISPLAY(4 1) "ENTER PHONE :".
           ACCEPT(4 35) EPHONE.
           DISPLAY(5 1) "ENTER DATE OF JOIN :".
           ACCEPT(5 35) EDOJ.
           DISPLAY(6 1) "ENTER DIPLOMA :".
           ACCEPT(6 35) EDIP.
           DISPLAY(7 1) "ENTER UG :".
           ACCEPT(7 35) EUG.
           DISPLAY(8 1) "ENTER PG :".
           ACCEPT(8 35) EPG.
           DISPLAY(9 1) "ENTER PROFESSIONAL QUALITY :".
           ACCEPT(9 35) EPROFQ.
           DISPLAY(10 1) "ENTER SKILL SET :".
           ACCEPT(10 35) ESKILL.
           DISPLAY(11 1) "ENTER GRADE NUMBER :".
           ACCEPT(11 35) EGRDNO.
           DISPLAY(12 1) "ENTER BRANCH CODE :".
           ACCEPT(12 35) EBRNID.
           DISPLAY(13 1) "ENTER DESIGNATION CODE :".
           ACCEPT(13 35) EDESID.
           WRITE EMPREC.
           CLOSE EMPFILE.
           GO TO MAIN-PARA.

       LEAVE-PARA.
           DISPLAY(1 1) ERASE.
           OPEN I-O LEAVEFILE.
           IF FSL = 30
              OPEN OUTPUT LEAVEFILE.
           DISPLAY(1 1) "ENTER CODE :".
           ACCEPT(1 35) LEMPID.
           DISPLAY(2 1) "ENTER FROM DATE :".
           ACCEPT(2 35) LFMDATE.
           DISPLAY(3 1) "ENTER TO DATE :".
           ACCEPT(3 35) LTODATE.
           DISPLAY(4 1) "ENTER LEAVE CATEGORY :".
           ACCEPT(4 35) LLEVCAT.
           WRITE LEAVEREC.
           CLOSE LEAVEFILE.
           GO TO MAIN-PARA.

       BRANCH-PARA.
           DISPLAY(1 1) ERASE.
           OPEN I-O BRANCHFILE.
           IF FSL = 30
              OPEN OUTPUT BRANCHFILE.
           DISPLAY(1 1) "ENTER BRANCH CODE :".
           ACCEPT(1 35) BBRID.
           DISPLAY(2 1) "ENTER BRANCH NAME :".
           ACCEPT(2 35) BBRNAME.
           DISPLAY(3 1) "ENTER BRANCH ADDRESS :".
           ACCEPT(3 35) BBRADD.
           DISPLAY(4 1) "ENTER PHONE :".
           ACCEPT(4 35) BBRPH.
           DISPLAY(5 1) "ENTER E-MAIL :".
           ACCEPT(5 35) BEMAIL.
           DISPLAY(5 1) "ENTER MANAGER NAME :".
           ACCEPT(5 35) BMGRNAME.
           WRITE BRANCHREC.
           CLOSE BRANCHFILE.
           GO TO MAIN-PARA.

       DESIGNATION-PARA.
           DISPLAY(1 1) ERASE.
           OPEN EXTEND DESIGNATIONFILE.
           DISPLAY(1 1) "ENTER DESIGNATION CODE :".
           ACCEPT(1 35) DESID.
           DISPLAY(2 1) "ENTER DESIGNATION :".
           ACCEPT(2 35) DESIGN.
           DISPLAY(3 1) "ENTER DES IN SHORT :".
           ACCEPT(3 35) DESHRT.
           WRITE DESIGNATIONREC.
           CLOSE DESIGNATIONFILE.
           GO TO MAIN-PARA.

       DEPARTMENT-PARA.
           DISPLAY(1 1) ERASE.
           OPEN I-O DEPARTMENTFILE.
           IF FSDEP = 30
              OPEN OUTPUT DEPARTMENTFILE.
           DISPLAY(1 1) "ENTER DEPARTMENT CODE :".
           ACCEPT(1 35) DEPCODE.
           DISPLAY(2 1) "ENTER DEPARTMENT NAME :".
           ACCEPT(2 35) DEPNAME.
           WRITE DEPARTMENTREC.
           CLOSE DEPARTMENTFILE.
           GO TO MAIN-PARA.

       REVISION-PARA.
           DISPLAY(1 1) ERASE.
           OPEN I-O REVISIONFILE.
           IF FSR = 30
              OPEN OUTPUT REVISIONFILE.
           DISPLAY(1 1) "ENTER REVISION CODE :".
           ACCEPT(1 35) RREVID.
           DISPLAY(2 1) "ENTER EMPLOYEE CODE :".
           ACCEPT(2 35) REMPID.
           DISPLAY(3 1) "ENTER DESIGNATION CODE :".
           ACCEPT(3 35) RDESCODE.
           DISPLAY(4 1) "ENTER BASIC :".
           ACCEPT(4 35) RBASIC.
           DISPLAY(5 1) "ENTER HRA :".
           ACCEPT(5 35) RHRA.
           DISPLAY(6 1) "ENTER DPA :".
           ACCEPT(6 35) RDPA.
           DISPLAY(7 1) "ENTER PPA :".
           ACCEPT(7 35) RPPA.
           DISPLAY(8 1) "ENTER EDUCATIONAL ALLOWANCE :".
           ACCEPT(8 35) REDUA.
           DISPLAY(9 1) "ENTER TECH. JOURNAL :".
           ACCEPT(9 35) RTECHJR.
           DISPLAY(10 1) "ENTER LUNCH ALLOWANCE :".
           ACCEPT(10 35) RLUNCHA.
           DISPLAY(11 1) "ENTER CONVEYANCE :".
           ACCEPT(11 35) RCONVEY.
           DISPLAY(12 1) "ENTER BUSINESS ATTIREMENT :".
           ACCEPT(12 35) RBUSATR.
           DISPLAY(13 1) "ENTER LEAVE TRAVEL ALLOWANCE :".
           ACCEPT(13 35) RLTA.
           DISPLAY(14 1) "ENTER PF :".
           ACCEPT(14 35) RPF.
           DISPLAY(15 1) "ENTER ESI :".
           ACCEPT(15 35) RESI.
           DISPLAY(16 1) "ENTER REVISED DATE :".
           ACCEPT(16 35) RREVDATE.
           WRITE REVISIONREC.
           CLOSE REVISIONFILE.
           GO TO MAIN-PARA.

       PAYMENT-PARA.
           DISPLAY(1 1) ERASE.
           OPEN I-O PAYMENTFILE.
           IF FSP = 30
              OPEN OUTPUT PAYMENTFILE.
           DISPLAY(1 1) "ENTER EMPLOYEE CODE :".
           ACCEPT(1 35) PEMPID.
           DISPLAY(2 1) "ENTER BASIC :".
           ACCEPT(2 35) PBASIC.
           DISPLAY(3 1) "ENTER DA :".
           ACCEPT(3 35) PDA.
           DISPLAY(4 1) "ENTER CCA :".
           ACCEPT(4 35) PCCA.
           DISPLAY(5 1) "ENTER HRA :".
           ACCEPT(5 35) PHRA.
           DISPLAY(6 1) "ENTER DPA :".
           ACCEPT(6 35) PDPA.
           DISPLAY(7 1) "ENTER PPA :".
           ACCEPT(7 35) PPPA.
           DISPLAY(8 1) "ENTER EDUCATIONAL ALLOWANCE :".
           ACCEPT(8 35) PEDUA.
           DISPLAY(9 1) "ENTER TECH. JOURNAL :".
           ACCEPT(9 35) PTECHJR.
           DISPLAY(10 1) "ENTER LUNCH ALLOWANCE :".
           ACCEPT(10 35) PLUNCHA.
           DISPLAY(11 1) "ENTER CONVEYANCE :".
           ACCEPT(11 35) PCONVEY.
           DISPLAY(12 1) "ENTER BUSINESS ATTIREMENT :".
           ACCEPT(12 35) PBUSATR.
           DISPLAY(13 1) "ENTER LEAVE TRAVEL ALLOWANCE :".
           ACCEPT(13 35) PLTA.
           DISPLAY(14 1) "ENTER PF :".
           ACCEPT(14 35) PPF.
           DISPLAY(15 1) "ENTER ESI :".
           ACCEPT(15 35) PESI.
           DISPLAY(16 1) "ENTER GRATUITY :".
           ACCEPT(16 35) PGRTY.
           DISPLAY(17 1) "ENTER PROFESSIONAL TAX :".
           ACCEPT(17 35) PPTAX.
           DISPLAY(18 1) "ENTER INCOME TAX :".
           ACCEPT(18 35) PITAX.
           DISPLAY(19 1) "ENTER LOAN :".
           ACCEPT(19 35) PLOAN.
           DISPLAY(20 1) "ENTER LOAN DEDUCTION AMOUNT :".
           ACCEPT(20 35) PLOANDA.
           DISPLAY(21 1) "ENTER OTHER DEDUCTION :".
           ACCEPT(21 35) POTHERD.
           DISPLAY(22 1) "ENTER PERFORMANCE INCENTIVE :".
           ACCEPT(22 35) PPERINC.
           DISPLAY(23 1) "ENTER MEDICAL REIMBURSEMENT :".
           ACCEPT(23 35) PMEDI.
           DISPLAY(24 1) "ENTER BOOK REIMBURSEMENT :".
           ACCEPT(24 35) PBOOK.
           DISPLAY(1 1) ERASE.
           DISPLAY(1 1) "ENTER ENTERTAINMENT :".
           ACCEPT(1 35) PENTER.
           DISPLAY(2 1) "ENTER PHONE :".
           ACCEPT(2 35) PTPH.
           DISPLAY(3 1) "ENTER HOUSE RELATED :".
           ACCEPT(3 35) PHOUSE.
           DISPLAY(4 1) "ENTER VEHICLE MAINTENANCE :".
           ACCEPT(4 35) PVEHMAN.
           DISPLAY(5 1) "ENTER CREDIT CARD :".
           ACCEPT(5 35) PCREDIT.
           DISPLAY(6 1) "ENTER CLUB :".
           ACCEPT(6 35) PCLUB.
           DISPLAY(7 1) "ENTER CLUB :".
           ACCEPT(7 35) PCLUB.
           DISPLAY(8 1) "ENTER CLUB :".
           ACCEPT(8 35) PCLUB.
           DISPLAY(9 1) "ENTER CASUAL LEAVE :".
           ACCEPT(9 35) PCL.
           DISPLAY(10 1) "ENTER SICK LEAVE :".
           ACCEPT(10 35) PSL.
           DISPLAY(11 1) "ENTER PAID LEAVE :".
           ACCEPT(11 35) PPL.
           DISPLAY(12 1) "ENTER LEAVE LOSS OF PAY :".
           ACCEPT(12 35) PLLOP.
           DISPLAY(13 1) "ENTER OTHER LEAVES :".
           ACCEPT(13 35) POTHERL.
           WRITE PAYMENTREC.
           CLOSE PAYMENTFILE.
           GO TO MAIN-PARA.

       CONFIRMATION-PARA.
           DISPLAY(1 1) ERASE.
           OPEN I-O CONFIRMATIONFILE.
           IF FSC = 30
              OPEN OUTPUT CONFIRMATIONFILE.
           DISPLAY(1 1) "ENTER CONFIRMATION CODE :".
           ACCEPT(1 35) CCONID.
           DISPLAY(2 1) "ENTER EMP CODE :".
           ACCEPT(2 35) CEMPID.
           DISPLAY(3 1) "ENTER CONFIRMATION DATE :".
           ACCEPT(3 35) CCDATE.
           WRITE CONFIRMATIONREC.
           CLOSE CONFIRMATIONFILE.
           GO TO MAIN-PARA.

       GRADE-PARA.
           DISPLAY(1 1) ERASE.
           OPEN EXTEND GRADEFILE.
           DISPLAY(1 1) "ENTER GRADE NO. :".
           ACCEPT(1 35) GGRADE.
           DISPLAY(2 1) "ENTER DESIGNATION :".
           ACCEPT(2 35) GDESIGN.
           WRITE GRADEREC.
           CLOSE GRADEFILE.
           GO TO MAIN-PARA.

       TRANSFER-PARA.
           DISPLAY(1 1) ERASE.
           OPEN I-O TRANSFERFILE.
           IF FST = 30
              OPEN OUTPUT TRANSFERFILE.
           DISPLAY(1 1) "ENTER TRANSFER CODE :".
           ACCEPT(1 35) TTRFID.
           DISPLAY(2 1) "ENTER EMP CODE :".
           ACCEPT(2 35) TEMPID.
           DISPLAY(3 1) "ENTER OLD BRANCH CODE :".
           ACCEPT(3 35) TOBRID.
           DISPLAY(4 1) "ENTER TRANSFER DATE :".
           ACCEPT(4 35) TTRFDT.
           WRITE TRANSFERREC.
           CLOSE TRANSFERFILE.
           GO TO MAIN-PARA.

       EMPPERSONAL-PARA.
           DISPLAY(1 1) ERASE.
           OPEN I-O EMPPERSONALFILE.
           IF FSEP = 30
              OPEN OUTPUT EMPPERSONALFILE.
           DISPLAY(1 1) "ENTER EMP CODE :".
           ACCEPT(1 35) EPEMPID.
           DISPLAY(2 1) "ENTER TEMP ADDRESS :".
           ACCEPT(2 35) EPTADD.
           DISPLAY(3 1) "ENTER PHONE :".
           ACCEPT(3 35) EPTPH.
           DISPLAY(4 1) "ENTER DOB :".
           ACCEPT(4 35) EPDOB.
           DISPLAY(5 1) "ENTER POB :".
           ACCEPT(5 35) EPPOB.
           DISPLAY(6 1) "ENTER LANGUAGE KNOWN :".
           ACCEPT(6 35) EPLANG.
           DISPLAY(7 1) "ENTER BLOOD GROUP :".
           ACCEPT(7 35) EPBLOOD.
           DISPLAY(8 1) "ENTER WEIGHT :".
           ACCEPT(8 35) EPWEIGHT.
           DISPLAY(9 1) "ENTER HEIGHT :".
           ACCEPT(9 35) EPHEIGHT.
           DISPLAY(10 1) "ENTER VISION :".
           ACCEPT(10 35) EPVISION.
           DISPLAY(11 1) "ENTER FATHER'S NAME :".
           ACCEPT(11 35) EPFATHER.
           DISPLAY(12 1) "ENTER DOB OF FATHER :".
           ACCEPT(12 35) EPDOBF.
           DISPLAY(13 1) "ENTER MOTHER'S NAME :".
           ACCEPT(13 35) EPMOTHER.
           DISPLAY(14 1) "ENTER DOB OF MOTHER :".
           ACCEPT(14 35) EPDOBM.
           DISPLAY(15 1) "ENTER SPOUSE NAME :".
           ACCEPT(15 35) EPSPOUSE.
           DISPLAY(16 1) "ENTER CHILD NAME :".
           ACCEPT(16 35) EPCHILD.
           DISPLAY(17 1) "ENTER DOB OF CHILD :".
           ACCEPT(17 35) EPDOBC.
           WRITE EMPPERSONALREC.
           CLOSE EMPPERSONALFILE.
           GO TO MAIN-PARA.

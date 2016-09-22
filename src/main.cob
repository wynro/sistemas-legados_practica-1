       IDENTIFICATION DIVISION.
       PROGRAM-ID. MAINHRMS.

       DATA DIVISION.
       WORKING-STORAGE SECTION.
       77 CHOICE PIC 9.
                                        
       PROCEDURE DIVISION.
       MAIN-PARA.
           DISPLAY SPACES AT 0101 WITH ERASE EOS.
           DISPLAY "*******************************************"
                AT 0315.
           DISPLAY "     HUMAN RESOURCE MANAGEMENT SYSTEM      "
                AT 0515.
           DISPLAY "*******************************************"
                AT 0715.
           DISPLAY "1. HRMS WRITE" AT 1025.
           DISPLAY "2. HRMS READ" AT 1225.
           DISPLAY "3. EXIT" AT 1425.
           DISPLAY "ENTER YOUR CHOICE :" AT 1625.
           ACCEPT CHOICE AT 1646.
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
                                     VALUE OF FILE-ID IS "./LEAVE.DAT".
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
           DISPLAY SPACES AT 0101 WITH ERASE EOS.
           DISPLAY "*******************************************"
                  AT 0310.
           DISPLAY "     HUMAN RESOURCE MANAGEMENT SYSTEM      "
                  AT 0510.
           DISPLAY "*******************************************"
                  AT 0710.
           DISPLAY " 1. EMPLOYEE FILE" AT 1105.
           DISPLAY " 2. LEAVE FILE" AT 1205.
           DISPLAY " 3. BRANCH FILE" AT 1305.
           DISPLAY " 4. DESIGNATION FILE" AT 1405.
           DISPLAY " 5. DEPARTMENT FILE" AT 1505.
           DISPLAY " 6. REVISION FILE" AT 1605.
           DISPLAY " 7. PAYMENT FILE" AT 1705.
           DISPLAY " 8. CONFIRMATION FILE" AT 1805.
           DISPLAY " 9. GRADE FILE" AT 1905.
           DISPLAY "10. TRANSFER FILE" AT 2005.
           DISPLAY "11. EMPLOYEE PERSONAL FILE" AT 2105.
           DISPLAY "12. EXIT" AT 2205.
           DISPLAY "ENTER YOUR CHOICE :" AT 2325.
           ACCEPT CHOICE AT 2345.
           IF CHOICE = 1
               GO TO EMP-PARA
           ELSE
           IF CHOICE = 2
               GO TO LEAVE-PARA
           ELSE
           *> IF CHOICE = 3
           *>     GO TO BRANCH-PARA
           *> ELSE
           *>     IF CHOICE = 4
           *>         GO TO DESIGNATION-PARA
           *>     ELSE
           *>         IF CHOICE = 5
           *>             GO TO DEPARTMENT-PARA
           *>         ELSE
           *>             IF CHOICE = 6
           *>                 GO TO REVISION-PARA
           *>             ELSE
           *>                 IF CHOICE = 7
           *>                     GO TO PAYMENT-PARA
           *>                 ELSE
           *>                     IF CHOICE = 8
           *>                         GO TO CONFIRMATION-PARA
           *>                     ELSE
           *>                         IF CHOICE = 9
           *>                             GO TO GRADE-PARA
           *>                         ELSE
           *>                             IF CHOICE = 10
           *>                                 GO TO TRANSFER-PARA
           *>                             ELSE
           *>                                 IF CHOICE = 11
           *>                                     GO TO EMPPERSONAL-PARA
           *>                                 ELSE
               EXIT PROGRAM.

       
       EMP-PARA.
           DISPLAY SPACES AT 0101 WITH ERASE EOS.
           OPEN INPUT EMPFILE.
           DISPLAY "ENTER CODE :".
           ACCEPT EEMPID.
           DISPLAY SPACES AT 0101 WITH ERASE EOS.
           READ EMPFILE *> INVALID KEY GO TO ERROR-EMP-PARA.
           DISPLAY " CODE                 :" EEMPID AT 0101.
           DISPLAY " NAME                 :" EEMPNAME AT 0201.
           DISPLAY " ADDRESS              :" EEMPADDR AT 0301.
           DISPLAY " PHONE                :" EPHONE AT 0401.
           DISPLAY " DATE OF JOIN         :" EDOJ AT 0501.
           DISPLAY " DIPLOMA              :" EDIP AT 0601.
           DISPLAY " UG                   :" EUG AT 0701.
           DISPLAY " PG                   :" EPG AT 0801.
           DISPLAY " PROFESSIONAL QUALITY :" EPROFQ AT 0901.
           DISPLAY " SKILL SET            :" ESKILL AT 1001.
           DISPLAY " GRADE NUMBER         :" EGRDNO AT 1101.
           DISPLAY " BRANCH CODE          :" EBRNID AT 1201.
           DISPLAY " DESIGNATION CODE     :" EDESID AT 1301.
           CLOSE EMPFILE.
           DISPLAY "PRESS ENTER TO RETURN TO HRMS READ MENU" AT 2020.
           STOP ' '.
           GO TO MAIN-PARA.

       LEAVE-PARA.
           DISPLAY SPACE AT 0101 WITH ERASE EOS.
           OPEN INPUT LEAVEFILE.
           DISPLAY "ENTER CODE :".
           ACCEPT LEMPID.
           DISPLAY SPACE AT 0101 WITH ERASE EOS.
           READ LEAVEFILE *> INVALID KEY GO TO ERROR-LEAVE-PARA.
           DISPLAY " CODE           :" LEMPID AT 0101.
           DISPLAY " DATE           :" LFMDATE AT 0201.
           DISPLAY " DATE           :" LTODATE AT 0301.
           DISPLAY " LEAVE CATEGORY :" LLEVCAT AT 0401.
           CLOSE LEAVEFILE.
           DISPLAY "PRESS ENTER TO RETURN TO HRMS READ MENU" AT 2010.
           STOP ' '.
           GO TO MAIN-PARA.
       END PROGRAM EMPREAD.

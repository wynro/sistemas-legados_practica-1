IDENTIFICATION DIVISION.
PROGRAM-ID. MAINHRMS.
DATA DIVISION.
WORKING-STORAGE SECTION.
    77 CHOICE PIC 9.

PROCEDURE DIVISION.
MAIN-PARA.
    DISPLAY SPACES AT 0101 WITH ERASE EOS.
    DISPLAY "*******************************************" AT 0315.
    DISPLAY "     HUMAN RESOURCE MANAGEMENT SYSTEM      " AT 0515.
    DISPLAY "*******************************************" AT 0715.
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

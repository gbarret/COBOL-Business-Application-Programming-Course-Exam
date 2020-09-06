//USER48G JOB ,
// MSGCLASS=H,MSGLEVEL=(1,1),TIME=(,4),REGION=144M,COND=(16,LT)
//*
// SET COBPGM='FINAL01'
//**** Compile JCL ******
//STP0000 EXEC PROC=ELAXFCOC,
// CICS=,
// DB2=,
// COMP=
//COBOL.SYSPRINT DD SYSOUT=*
//SYSLIN DD DISP=SHR,
//        DSN=&SYSUID..COBOBJS.OBJ(&COBPGM.)
//COBOL.SYSLIB DD DISP=SHR,
//        DSN=&SYSUID..EXAM.DEV.COPYLIB
//COBOL.SYSXMLSD DD DUMMY
//COBOL.SYSIN DD DISP=SHR,
//        DSN=&SYSUID..EXAM.DEV.COBOL(&COBPGM.)
//*COBOL.SYSIN DD DISP=SHR,
//*        DSN=&SYSUID..EXAM.DEV.COBOL(&COBPGM.)
//****Link/Edit Step ******
//LKED EXEC PROC=ELAXFLNK
//LINK.SYSLIB DD DSN=CEE.SCEELKED,
//        DISP=SHR
//        DD DSN=&SYSUID..EXAM.DEV.LOAD,
//        DISP=SHR
//LINK.OBJ0000 DD DISP=SHR,
//        DSN=&SYSUID..COBOBJS.OBJ(&COBPGM.)
//LINK.SYSLIN DD *
     INCLUDE OBJ0000
/*
//LINK.SYSLMOD   DD  DISP=SHR,
//        DSN=&SYSUID..EXAM.DEV.LOAD(&COBPGM.)
//*
//** Go (Run) Step. Add //DD cards when needed ******
//GO    EXEC   PROC=ELAXFGO,GO=&COBPGM.,
//        LOADDSN=&SYSUID..EXAM.DEV.LOAD
//******* ADDITIONAL RUNTIME JCL HERE ******
//ACCTSORT DD DSN=&SYSUID..LEARN.ACCT.SORT.DATA,DISP=SHR
//PAYROLL  DD DSN=&SYSUID..LEARN.PAYROL01,DISP=SHR
//PAYROL3A  DD DSN=&SYSUID..LEARN.PAYROL3A,DISP=SHR
//PAYCHECK DD SYSOUT=*
//ACCTREC  DD DSN=&SYSUID..LEARN.ACCT.DATA,DISP=SHR
//RENTALS  DD DSN=&SYSUID..LEARN.RENTALS,DISP=SHR
//RENTSORT  DD DSN=&SYSUID..LEARN.RENTSORT,DISP=SHR
//INFILE   DD DSN=&SYSUID..LEARN.ACCT.DATA,DISP=SHR
//*OUTFILE  DD DSN=&SYSUID..LEARN.OUTFILE,DISP=SHR
//HOSPIN   DD DSN=&SYSUID..LEARN.HOSPIN,DISP=SHR
//INVALS   DD DSN=&SYSUID..LEARN.INVALS,DISP=SHR
//FAVIN    DD DSN=&SYSUID..LEARN.FAVIN,DISP=SHR
//TYPEFILE DD DSN=&SYSUID..LEARN.INS-TYPE,DISP=SHR
//PRTLINE  DD SYSOUT=*
//B37      DD DSN=&SYSUID..LEARN.B37,DISP=SHR
//CLAIM    DD DSN=&SYSUID..LEARN.INSCLAIM,DISP=SHR
//RFPIN    DD DSN=&SYSUID..LEARN.FAVRFP,DISP=SHR
//STDNTFL  DD DSN=&SYSUID..LEARN.STUDENT,DISP=SHR
//EMPROJ   DD DSN=&SYSUID..LEARN.EMP.PROJ,DISP=SHR
//*EMPROJ   DD DSN=&SYSUID..LEARN.EMP.PROJ.SORTBY.NAME,DISP=SHR
//*EMPROJ   DD DSN=&SYSUID..LEARN.EMP.PROJ.SORTBY.PROJ,DISP=SHR
//EMPFILE  DD DSN=&SYSUID..LEARN.EMPFILE,DISP=SHR
//PARTSUPP  DD DSN=&SYSUID..LEARN.PARTSUPP,DISP=SHR
//STATEZIP DD DSN=&SYSUID..LEARN.STATE.ADDRESS.ZIP,DISP=SHR
//STNAMES  DD DSN=&SYSUID..LEARN.STATES.NAME,DISP=SHR
//STABBREV DD DSN=&SYSUID..LEARN.STATES.ABBREV,DISP=SHR
//REPORTFL DD SYSOUT=*
//HOSPOUT  DD SYSOUT=*
//OUTFILE  DD SYSOUT=*
//*RFPIN    DD *
11111111111111111111111111111111111111111111111111111111111111111111111
22222222222222222222222222222222222222222222222222222222222222222222222
33333333333333333333333333333333333333333333333333333333333333333333333
44444444444444444444444444444444444444444444444444444444444444444444444
//PROPOSAL DD SYSOUT=*
//CLAIMRPT DD SYSOUT=*
//RPTFILE  DD SYSOUT=*
//ERRFILE  DD SYSOUT=*
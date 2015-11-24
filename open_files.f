!vvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvC
!                                                                      C
!  Module name: OPEN_FILES                                             C
!  Purpose: open all the files for this run                            C
!                                                                      C
!  Author: P. Nicoletti                               Date: 12-DEC-91  C
!  Reviewer: P. Nicoletti, W. Rogers, M. Syamlal      Date: 24-JAN-92  C
!                                                                      C
!  Revision Number:                                                    C
!  Purpose:                                                            C
!  Author:                                            Date: dd-mmm-yy  C
!  Reviewer:                                          Date: dd-mmm-yy  C
!                                                                      C
!  Literature/Document References:                                     C
!                                                                      C
!  Variables referenced:                                               C
!  Variables modified:                                                 C
!                                                                      C
!  Local variables: EXT, FILE_NAME, LC, NB                             C
!                                                                      C
!^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^C
!
      SUBROUTINE OPEN_FILES(RUN_NAME, RUN_TYPE, N_SPX1) 
!...Translated by Pacific-Sierra Research VAST-90 2.06G5  12:17:31  12/09/98  

!...Switches: -xf
!
!-----------------------------------------------
!   M o d u l e s 
!-----------------------------------------------
      USE machine 
      USE funits 
      USE compar 
      USE physprop      
      IMPLICIT NONE
!-----------------------------------------------
!   D u m m y   A r g u m e n t s
!-----------------------------------------------
!
!                     Error index: 0 - no error, 1 could not open file
      INTEGER         IER
!
!                   run_name (as specified in input file)
      CHARACTER*(*) RUN_NAME
!
!                   run_type (as specified in input file)
      CHARACTER*(*) RUN_TYPE
!
!                   number of single precision output files (param.inc)
      INTEGER       N_SPX1
!
! local variables
!
!                   Answer
      CHARACTER     ANS
!
!                   extension to filename
      CHARACTER     EXT*4
!
!                   run_name + extension
      CHARACTER     FILE_NAME*64
!
!
!                   Log file name: dmp mode adds processor no to file name
      CHARACTER     LOGFILE*60
!
!                   Loop counter
      INTEGER       LC
!
!                   index to first blank character in run_name
      INTEGER       NB, NBL

      CHARACTER     EXT_END*35
!-----------------------------------------------

      ext_end = '123456789ABCDEFGHIJKLMNOPQRSTUVWXYZ'
!
! DETERMINE THE FIRST BLANK CHARCATER IN RUN_NAME
!

!//PAR_I/O all PEs must exec this check in order to avoid Bcast of NB
      DO LC = 1, LEN(RUN_NAME) 
         IF (RUN_NAME(LC:LC) == ' ') THEN 
            NB = LC 
            GO TO 125 
         ENDIF
	 LOGFILE(LC:LC) = RUN_NAME(LC:LC) 
      END DO 
      WRITE (*, *) 'RUN_NAME TOOOOOOO LOOOONG' 
      call mfix_exit(myPE) 
!
  125 CONTINUE 
      IF (NB + 7 > LEN(FILE_NAME)) THEN 
         WRITE (*, *) 'RUN_NAME TOOOOOOO LOOOONG' 
         call mfix_exit(myPE) 
      ENDIF 

!
      NBL = NB
      if( numPEs > 1 ) then
        write(LOGFILE(NB:NB+3),'(I3.3)') myPE
	NBL = NB + 3
      endif
!
      IF(DMP_LOG)Then
        CALL OPEN_FILE (LOGFILE, NBL, UNIT_LOG, '.LOG', FILE_NAME, 'NEW', &
          'SEQUENTIAL', 'FORMATTED', 132, IER) 
        IF (IER /= 0) THEN 
          CALL OPEN_FILE (LOGFILE, NBL, UNIT_LOG, '.LOG', FILE_NAME, 'OLD', &
            'SEQUENTIAL', 'FORMATTED', 132, IER) 
          IF (IER /= 0) GO TO 500
          DO WHILE(IER ==0)
            READ(UNIT_LOG,'(a)', IOSTAT = IER)ANS
          ENDDO
          BACKSPACE(UNIT_LOG)
        ENDIF
      ENDIF

!Dufek
      if ( myPE == PE_IO ) then

!OPEN(88888,FILE='Cd1')
!OPEN(88889,FILE='Cd2')
!OPEN(88890,FILE='Cd3')
!OPEN(88891,FILE='Cd4')

! OPEN(98000,FILE='ReP')
!OPEN(UNIT=1001,FILE='VOL')
!OPEN(UNIT=1002,FILE='S_RO1')
!OPEN(UNIT=1003,FILE='S_RO2')
!OPEN(UNIT=1004,FILE='S_RO3')
!OPEN(UNIT=1005,FILE='S_RO4')
!OPEN(UNIT=1006,FILE='GMU')
!OPEN(UNIT=1007,FILE='SVIS1')
!OPEN(UNIT=1008,FILE='SVIS2')
!OPEN(UNIT=1009,FILE='SVIS3')
!OPEN(UNIT=1010,FILE='SVIS4')

!OPEN(UNIT=1011,FILE='PRE')
!OPEN(UNIT=1012,FILE='T_G')
!OPEN(UNIT=1013,FILE='T_S1')
!OPEN(UNIT=1014,FILE='T_S2')
!OPEN(UNIT=1015,FILE='T_S3')
!OPEN(UNIT=1016,FILE='T_S4')

!OPEN(UNIT=1017,FILE='GR_E1')
!OPEN(UNIT=1018,FILE='GR_E2')
!OPEN(UNIT=1019,FILE='GR_E3')
!OPEN(UNIT=1020,FILE='GR_E4')


!OPEN(UNIT=1021,FILE='GAU')
!OPEN(UNIT=1022,FILE='GAV')
!OPEN(UNIT=1023,FILE='SU1')
!OPEN(UNIT=1024,FILE='SV1')
!OPEN(UNIT=1025,FILE='SU2')
!OPEN(UNIT=1026,FILE='SV2')
!OPEN(UNIT=1027,FILE='SU3')
!OPEN(UNIT=1028,FILE='SV3')
!OPEN(UNIT=1029,FILE='SU4')
!OPEN(UNIT=1030,FILE='SV4')
!OPEN(UNIT=1031,FILE='RO_G')
!OPEN(UNIT=1032,FILE='TURB_K')
!OPEN(UNIT=1033,FILE='TURB_E')
!OPEN(UNIT=2000,FILE='DENSEMIX')
!OPEN(UNIT=8000,FILE='LOSS1')
!OPEN(UNIT=8001,FILE='LOSS2')


!OPEN(UNIT=8005,FILE='DRAG1')


!OPEN(UNIT=5000,FILE='LEAK_FAC1')
!OPEN(UNIT=5001,FILE='LEAK_FAC2')
!OPEN(UNIT=5002,FILE='LEAK_FAC3')
!OPEN(UNIT=5003,FILE='LEAK_FAC4')
!OPEN(UNIT=5004,FILE='EscapeMax')
!OPEN(UNIT=5005,FILE='EscapeMin')


!OPEN(5010,FILE='SUM_WATER_LOSS')
!OPEN(5011,FILE='SUM_DEPOSIT')
!OPEN(5012,FILE='WATER_LOSS1')
!OPEN(5013,FILE='WATER_LOSS2')
!OPEN(5014,FILE='WATER_LOSS3')
!OPEN(5015,FILE='WATER_LOSS4')

!OPEN(5016,FILE='DEPOSIT1')
!OPEN(5017,FILE='DEPOSIT2')
!OPEN(5018,FILE='DEPOSIT3')
!OPEN(5019,FILE='DEPOSIT4')
!






!OPEN(10012,FILE='GRIDMAP_x_s')
!OPEN(10013,FILE='GRIDMAP_y_s')
!OPEN(10014,FILE='GRIDMAP_x_v')
!OPEN(10015,FILE='GRIDMAP_y_v')


IF (Write_Level==0) THEN
ELSE IF (Write_Level==1) THEN

OPEN(800000,FILE='GAS_FLUX')
OPEN(800001,FILE='P_FLUX1')
OPEN(800002,FILE='P_FLUX2')
OPEN(800003,FILE='P_FLUX3')

OPEN(100001,FILE='USRCHECK') 
OPEN(20001,FILE='P_G',form='unformatted')
!OPEN(20004,FILE='MU_G',form='unformatted')
OPEN(20006,FILE='EP_G',form='unformatted')
!OPEN(80006,FILE='EP_Gb',form='unformatted')
!OPEN(20010,FILE='MU_S1',form='formatted')
OPEN(20012,FILE='T_G',form='unformatted')
OPEN(20013,FILE='T_S1',form='unformatted')
OPEN(20014,FILE='T_S2',form='unformatted')
OPEN(20043,FILE='T_S3',form='unformatted')

OPEN(20015,FILE='U_G',form='unformatted')
OPEN(20016,FILE='V_G',form='unformatted')
OPEN(20017,FILE='U_S1',form='unformatted')
OPEN(20018,FILE='U_S2',form='unformatted')
OPEN(20019,FILE='V_S1',form='unformatted')
OPEN(20020,FILE='V_S2',form='unformatted')
OPEN(20007,FILE='RO_G',form='unformatted')
OPEN(20023,FILE='ROP_S1',form='unformatted')
!OPEN(20024,FILE='X_S2',form='unformatted')
!OPEN(99922,FILE='GX',form='unformatted')
!OPEN(99921,FILE='GY',form='unformatted')
!OPEN(99920,FILE='GZ',form='unformatted')

!OPEN(20025, FILE='X_S3',form='unformatted')
OPEN(20026, FILE='U_S3',form='unformatted')
OPEN(20027, FILE='V_S3',form='unformatted')

!OPEN(20028,FILE='THETA_M1',form='unformatted')
!OPEN(20029,FILE='THETA_M2',form='unformatted')
!OPEN(20030,FILE='THETA_M3',form='unformatted')

!OPEN(20031,FILE='ASH_C',form='unformatted')
!OPEN(20032,FILE='ASH_F',form='unformatted')
OPEN(20033,FILE='ROP_S2',form='unformatted')
OPEN(20034,FILE='ROP_S3',form='unformatted')
OPEN(20035,FILE='MU_G',form='unformatted')

!OPEN(20036,FILE='COLL_RATE',form='formatted')
!OPEN(20037,FILE='C_RATE_SUM',form='formatted')

!OPEN(909991,FILE='X_G1',form='formatted')

!OPEN(20060,FILE='HREF',form='formatted')

!OPEN(20026,FILE='X_G1',form='formatted')
OPEN(20038,FILE='W_G',form='unformatted')
OPEN(20039,FILE='W_S1',form='unformatted')
OPEN(20040,FILE='W_S2',form='unformatted')
OPEN(20041,FILE='X_G2',form='unformatted')
OPEN(20042,FILE='W_S3',form='unformatted')

!IF(Nscalar==1) THEN
!OPEN(20024,FILE='SCAL_1')
!END IF
ELSE IF (Write_Level==2) THEN

OPEN(20001,FILE='P_G')
OPEN(20002,FILE='K_TURB_G')
OPEN(20003,FILE='E_TURB_G')
OPEN(20004,FILE='MU_G')
OPEN(20005,FILE='P_STAR')
OPEN(20006,FILE='EP_G')
OPEN(20007,FILE='RO_G')
OPEN(20008,FILE='ROP_S1')
OPEN(20009,FILE='ROP_S2')
OPEN(20010,FILE='MU_S1')
OPEN(20011,FILE='MU_S2')
OPEN(20012,FILE='T_G')
OPEN(20013,FILE='T_S1')
OPEN(20014,FILE='T_S2')
OPEN(20043,FILE='T_S3')
OPEN(20015,FILE='U_G')
OPEN(20016,FILE='V_G')
OPEN(20017,FILE='U_S1')
OPEN(20018,FILE='U_S2')
OPEN(20019,FILE='V_S1')
OPEN(20020,FILE='V_S2')
OPEN(20021,FILE='THETA_M1')
OPEN(20022,FILE='THETA_M2')

OPEN(20023,FILE='EP_S1')
OPEN(20026,FILE='X_G1')
OPEN(20027,FILE='X_G2')


!IF(Nscalar==1) THEN
!!OPEN(20024,FILE='SCAL_1')
!END IF

OPEN(20030,FILE='K_s')
OPEN(20031,FILE='K_g')
OPEN(20032,FILE='C_pg')
OPEN(30000,FILE='TEST_DEN')
END IF





OPEN(UNIT=9999,FILE='SPEED_LAG')











!
IF (LAGRAN) THEN
 OPEN(UNIT=9000,FILE='PARTICLES1')
 OPEN(UNIT=9001,FILE='PART_X')
 OPEN(UNIT=9002,FILE='PART_Y')
 OPEN(UNIT=9003,FILE='PART_Z')
 OPEN(UNIT=9099,FILE='IMPACT_CONDITIONS')
! OPEN(UNIT=9001,FILE='PARTICLES2')
! OPEN(UNIT=9002,FILE='PARTICLES3')
! OPEN(UNIT=9003,FILE='PARTICLES4')
! OPEN(UNIT=9004,FILE='PARTICLES5')

! OPEN(UNIT=9005,FILE='P_SAMPLER1')
! OPEN(UNIT=9006,FILE='P_SAMPLER2')
! OPEN(UNIT=9007,FILE='P_SAMPLER3')
! OPEN(UNIT=9008,FILE='P_SAMPLER4')
! OPEN(UNIT=9009,FILE='P_SAMPLER5')




 OPEN(UNIT=9010,FILE='NUMBER_PARTICLES')

END IF




!OPEN(UNIT=9020,FILE='FREQ1')
!OPEN(UNIT=9021,FILE='FREQ2')
!OPEN(UNIT=79000,FILE='BAROGRAPH1')
!OPEN(UNIT=79001,FILE='BAROGRAPH2')
!OPEN(UNIT=79002,FILE='BAROGRAPH3')
!OPEN(UNIT=79003,FILE='BAROGRAPH4')
!OPEN(UNIT=79004,FILE='BAROGRAPH5')
!OPEN(UNIT=79005,FILE='BAROGRAPH6')
!OPEN(UNIT=79006,FILE='BAROGRAPH7')
!OPEN(UNIT=79007,FILE='BAROGRAPH8')
!OPEN(UNIT=79008,FILE='BAROGRAPH9')
!OPEN(UNIT=79009,FILE='BAROGRAPH10')

!OPEN(UNIT=79010,FILE='BAROGRAPH11')
!OPEN(UNIT=79011,FILE='BAROGRAPH12')

!OPEN(UNIT=79012,FILE='BAROGRAPH13')
!OPEN(UNIT=79013,FILE='BAROGRAPH14')
!OPEN(UNIT=79014,FILE='BAROGRAPH15')
!OPEN(UNIT=79015,FILE='BAROGRAPH16')
!OPEN(UNIT=79016,FILE='BAROGRAPH17')
!OPEN(UNIT=79017,FILE='BAROGRAPH18')
!OPEN(UNIT=79018,FILE='BAROGRAPH19')
!OPEN(UNIT=79019,FILE='BAROGRAPH20')
!OPEN(UNIT=79020,FILE='BAROGRAPH21')
!OPEN(UNIT=79021,FILE='BAROGRAPH22')
!OPEN(UNIT=79022,FILE='BAROGRAPH23')
END IF
!PE

!//PAR_I/O only PE 0 opens the ASCI output (.out), restart (.res) and species (.spX) files 
      if ( myPE == PE_IO ) then
!
        CALL OPEN_FILE (RUN_NAME, NB, UNIT_OUT, '.OUT', FILE_NAME, 'UNKNOWN', &
         'SEQUENTIAL', 'FORMATTED', 132, IER) 
!
!
        EXT = '.SPx' 
        SELECT CASE (TRIM(RUN_TYPE))  
        CASE ('NEW')  
          CALL OPEN_FILE (RUN_NAME, NB, UNIT_RES, '.RES', FILE_NAME, 'NEW', &
            'DIRECT', 'UNFORMATTED', OPEN_N1, IER) 
          IF (IER /= 0) THEN 
            WRITE (*, 1001) FILE_NAME 
            GO TO 600 
          ENDIF 

          DO LC = 1, N_SPX1 
            ext(4:4) = ext_end(LC:LC)
            CALL OPEN_FILE (RUN_NAME, NB, UNIT_SPX + LC, EXT, FILE_NAME, 'NEW'&
               , 'DIRECT', 'UNFORMATTED', OPEN_N1, IER) 
            IF (IER /= 0) GO TO 500 
          END DO 
        CASE ('RESTART_1')  
          CALL OPEN_FILE (RUN_NAME, NB, UNIT_RES, '.RES', FILE_NAME, 'OLD', &
            'DIRECT', 'UNFORMATTED', OPEN_N1, IER) 
          IF (IER /= 0) THEN 
            WRITE (*, 1002) FILE_NAME 
            GO TO 600 
          ENDIF 
          DO LC = 1, N_SPX1 
            ext(4:4) = ext_end(LC:LC)
            CALL OPEN_FILE (RUN_NAME, NB, UNIT_SPX + LC, EXT, FILE_NAME, 'OLD'&
               , 'DIRECT', 'UNFORMATTED', OPEN_N1, IER) 
            IF (IER /= 0) GO TO 500 
          END DO 
        CASE ('RESTART_2')  
          CALL OPEN_FILE (RUN_NAME, NB, UNIT_RES, '.RES', FILE_NAME, 'OLD', &
            'DIRECT', 'UNFORMATTED', OPEN_N1, IER) 
          IF (IER /= 0) THEN 
            WRITE (*, 1002) FILE_NAME 
            GO TO 600 
          ENDIF 
          DO LC = 1, N_SPX1 
            ext(4:4) = ext_end(LC:LC)
            CALL OPEN_FILE (RUN_NAME, NB, UNIT_SPX + LC, EXT, FILE_NAME, 'NEW'&
               , 'DIRECT', 'UNFORMATTED', OPEN_N1, IER) 
            IF (IER /= 0) GO TO 500 
          END DO 
        CASE DEFAULT 
          WRITE (*, *) ' OPEN_FILES: DO NOT KNOW HOW TO PROCESS' 
          WRITE (*, *) ' RUN_TYPE in the input file' 
          call mfix_exit(myPE) 
        END SELECT 
      endif   ! end of myPE=PE_IO if block

      RETURN  
  500 CONTINUE 
      WRITE (*, 1100) myPE,FILE_NAME  !//PAR_I/O added myPE for output
  600 CONTINUE 
      CALL SLUMBER 
      call mfix_exit(myPE) 
!
 1000 FORMAT(I1) 
 1001 FORMAT(/70('*')//' From: OPEN_FILES',/&
         ' Error: NEW run -- .RES file should NOT be in the run directory'/&
         ' Cannot open new file -- ',A,/70('*')/) 
 1002 FORMAT(/70('*')//' From: OPEN_FILES',/&
         ' Error: RESTART run -- .RES file should be in the run directory'/&
         ' Cannot open existing file -- ',A,/70('*')/) 
 1100 FORMAT(/70('*')//'(PE ',I3,'): From: OPEN_FILES',/' Error: Cannot open file -- ',A,/&
         70('*')/) 
      END SUBROUTINE OPEN_FILES 

!// Comments on the modifications for DMP version implementation      
!// 001 Include header file and common declarations for parallelization
!//PAR_I/O Root Processor handles all file I/O except the LOG files
!// 990 Replace STOP with mfix_exit(myPE) to terminate all processors

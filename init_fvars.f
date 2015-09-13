!
!vvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvC
!                                                                      C
!  Module name: INIT_FVARS                                             C
!  Purpose: Initialize all field variables as undefined                C
!                                                                      C
!  Author: M. Syamlal                                 Date: 23-JAN-94  C
!  Reviewer:                                          Date:            C
!                                                                      C
!  Revision Number:                                                    C
!  Purpose:                                                            C
!  Author:                                            Date:            C
!  Reviewer:                                          Date:            C
!                                                                      C
!  Literature/Document References:                                     C
!                                                                      C
!  Variables referenced: ROP_g, EP_g, ROP_s, IJKMAX2, MMAX, U_s, V_s,  C
!                        W_s                                           C
!                                                                      C
!  Variables modified: ROP_go, ROP_so, IJK, M, U_so, V_so, W_so C
!                                                                      C
!  Local variables: NONE                                               C
!                                                                      C
!^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^C
!
      SUBROUTINE INIT_FVARS 
!...Translated by Pacific-Sierra Research VAST-90 2.06G5  12:17:31  12/09/98  
!...Switches: -xf
!
!-----------------------------------------------
!   M o d u l e s 
!-----------------------------------------------
      USE param 
      USE param1 
      USE parallel 
      USE fldvar
      USE geometry
      USE physprop
      USE indices
      USE scalars
      USE rxns
      USE run
      USE compar
      USE mpi_utility
      IMPLICIT NONE
!-----------------------------------------------
!   G l o b a l   P a r a m e t e r s
!-----------------------------------------------
!-----------------------------------------------
!   L o c a l   P a r a m e t e r s
!-----------------------------------------------
!-----------------------------------------------
!   L o c a l   V a r i a b l e s
!-----------------------------------------------
!
!                      Indices
      INTEGER          IJK
!
!                      Solids phase
      INTEGER          M
!
!                      Species index
      INTEGER          N
!-----------------------------------------------




CALL MPI_BARRIER(MPI_COMM_WORLD,mpierr)   !//PAR_I/O enforce barrier here

    IF(myPE.eq.root) then

!OPEN(555002,FILE='VENT_CONDITIONS1')
!OPEN(555003,FILE='VENT_CONDITIONS2')
!OPEN(555004,FILE='VENT_CONDITIONS3')
!OPEN(555005,FILE='CRATER_LOC2')

!REWIND(555002)
!READ(555002,500) VENT_CONDITIONS1

!REWIND(555003)
!READ(555003,500) VENT_CONDITIONS2

!REWIND(555004)
!READ(555004,500) VENT_CONDITIONS3

!REWIND(555005)
!READ(555005,600) CRATER_LOC2





END IF
CALL MPI_BARRIER(MPI_COMM_WORLD,mpierr)   !//PAR_I/O enforce barrier here

!// Broadcast the values
         call bcast(VENT_CONDITIONS1, PE_IO)     !//PAR_I/O BCAST2d
CALL MPI_BARRIER(MPI_COMM_WORLD,mpierr)   !//PAR_I/O enforce barrier here

      call bcast(VENT_CONDITIONS2, PE_IO)     !//PAR_I/O BCAST2d
CALL MPI_BARRIER(MPI_COMM_WORLD,mpierr)   !//PAR_I/O enforce barrier here

      call bcast(VENT_CONDITIONS3, PE_IO)     !//PAR_I/O BCAST2d
CALL MPI_BARRIER(MPI_COMM_WORLD,mpierr)   !//PAR_I/O enforce barrier here

     call bcast(CRATER_LOC2, PE_IO)     !//PAR_I/O BCAST2d









      IF (IJKMAX2 > 0) THEN 
!Dufek changed undefined's to zeros
         !LOSS1(IJKSTART3:IJKEND3) = ZERO
         !LOSS2(IJKSTART3:IJKEND3) = ZERO



         

         EP_G(IJKSTART3:IJKEND3) = ZERO!UNDEFINED 
         P_G(IJKSTART3:IJKEND3) = ZERO!UNDEFINED 
         P_STAR(IJKSTART3:IJKEND3) = ZERO 
         RO_G(IJKSTART3:IJKEND3) = ZERO!UNDEFINED 
         ROP_G(IJKSTART3:IJKEND3) = ZERO!UNDEFINED 
         T_G(IJKSTART3:IJKEND3) = ZERO 
         U_G(IJKSTART3:IJKEND3) = ZERO!UNDEFINED 
         V_G(IJKSTART3:IJKEND3) = ZERO!UNDEFINED 
         W_G(IJKSTART3:IJKEND3) = ZERO!UNDEFINED 
         IF (NMAX(0) > 0) THEN 
            X_G(IJKSTART3:IJKEND3,:NMAX(0)) = ZERO 
         ENDIF 
      ENDIF 
      
      IF(Nscalar > 0) Scalar(IJKSTART3:IJKEND3,:Nscalar) = ZERO
      IF(nRR > 0) ReactionRates(IJKSTART3:IJKEND3,:nRR) = ZERO

      IF(K_Epsilon) THEN
        K_Turb_G(IJKSTART3:IJKEND3) = ZERO
        E_Turb_G(IJKSTART3:IJKEND3) = ZERO
      ENDIF

!!$omp parallel do private(M,IJK,N)
      DO M = 1, MMAX 
         IF (IJKMAX2 > 0) THEN 
            ROP_S(IJKSTART3:IJKEND3,M) = ZERO!UNDEFINED 
            T_S(IJKSTART3:IJKEND3,M) = ZERO 
            THETA_M(IJKSTART3:IJKEND3,M) = ZERO 
            P_S(IJKSTART3:IJKEND3,M) = ZERO!UNDEFINED 
            U_S(IJKSTART3:IJKEND3,M) = ZERO!UNDEFINED 
            V_S(IJKSTART3:IJKEND3,M) = ZERO!UNDEFINED 
            W_S(IJKSTART3:IJKEND3,M) = ZERO!UNDEFINED 
	    P_S(IJKSTART3:IJKEND3,M) = ZERO!UNDEFINED
	    KTH_S(IJKSTART3:IJKEND3,M) = ZERO!UNDEFINED
            IF (NMAX(M) > 0) THEN 
               X_S(IJKSTART3:IJKEND3,M,:NMAX(M)) = ZERO 
            ENDIF 
         ENDIF 
      END DO 
      RETURN  


500 FORMAT(7F30.10)
600 FORMAT(240F20.10)

      END SUBROUTINE INIT_FVARS 

!// Comments on the modifications for DMP version implementation      
!// 001 Include header file and common declarations for parallelization
!// 120 Replaced the index for initialization :ijkmax2 --> IJKSTART3:IJKEND3

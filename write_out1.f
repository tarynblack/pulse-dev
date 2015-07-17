!vvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvC
!                                                                      C
!  Module name: WRITE_OUT1                                             C
!  Purpose: write out the field variables to standard output           C
!                                                                      C
!  Author: P. Nicoletti                               Date: 03-DEC-91  C
!  Reviewer: W. Rogers, M. Syamlal, S. Venkatesan     Date: 31-JAN-92  C
!                                                                      C
!  Revision Number:                                                    C
!  Purpose:                                                            C
!  Author:                                            Date: dd-mmm-yy  C
!  Reviewer:                                          Date: dd-mmm-yy  C
!                                                                      C
!  Literature/Document References:                                     C
!                                                                      C
!  Variables referenced: TIME, P_g, EP_g, RO_g, ROP_g, MMAX, ROP_s     C
!                        T_g, T_s, U_g, V_g, W_g, U_s, V_s, W_s C
!  Variables modified: None                                            C
!                                                                      C
!  Local variables: LC, N                                              C
!                                                                      C
!^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^C
!
      SUBROUTINE WRITE_OUT1 
!...Translated by Pacific-Sierra Research VAST-90 2.06G5  12:17:31  12/09/98  
!...Switches: -xf
!-----------------------------------------------
!   M o d u l e s 
!-----------------------------------------------
      USE param 
      USE param1 
      USE physprop
      USE fldvar
      USE run
      USE scalars
      USE funits 
      USE rxns
      USE compar             !//d
      USE mpi_utility        !//d
      
!Dufek 
     USE visc_s
     USE visc_g
     USE drag
     USE energy
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
      INTEGER :: LC, N 
      LOGICAL JUNK_IT_ALL


!-----------------------------------------------
!
      double precision, allocatable :: array1(:)    !//d
!


      if (myPE == PE_IO) then
         allocate (array1(ijkmax3))     !//d
      else
         allocate (array1(1))           !//d
      end if
!
!             form feed character = CHAR(12)
!

JUNK_IT_ALL=.TRUE.

IF (.NOT. JUNK_IT_ALL) THEN

      if (myPE == PE_IO) WRITE (UNIT_OUT, 1000) CHAR(12), TIME 
!     call MPI_Barrier(MPI_COMM_WORLD,mpierr)  !//PAR_I/O enforce barrier here
      call gather (P_g,array1,root)    !//
!     call MPI_Barrier(MPI_COMM_WORLD,mpierr)  !//PAR_I/O enforce barrier here
      if (myPE == PE_IO) CALL OUT_ARRAY (array1, 'P_g') 
!
      if (myPE == PE_IO) WRITE (UNIT_OUT, 1050) CHAR(12), TIME 
!     call MPI_Barrier(MPI_COMM_WORLD,mpierr)  !//PAR_I/O enforce barrier here
      call gather (P_star,array1,root)    !//
!     call MPI_Barrier(MPI_COMM_WORLD,mpierr)  !//PAR_I/O enforce barrier here
      if (myPE == PE_IO) CALL OUT_ARRAY (array1, 'P_star') 
!
      if (myPE == PE_IO) WRITE (UNIT_OUT, 1100) CHAR(12), TIME 
!     call MPI_Barrier(MPI_COMM_WORLD,mpierr)  !//PAR_I/O enforce barrier here
      call gather (EP_g,array1,root)    !//
!     call MPI_Barrier(MPI_COMM_WORLD,mpierr)  !//PAR_I/O enforce barrier here
      if (myPE == PE_IO) CALL OUT_ARRAY (array1, 'EP_g') 
!
      if (myPE == PE_IO) WRITE (UNIT_OUT, 1200) CHAR(12), TIME 
!     call MPI_Barrier(MPI_COMM_WORLD,mpierr)  !//PAR_I/O enforce barrier here
      call gather (RO_g,array1,root)    !//
!     call MPI_Barrier(MPI_COMM_WORLD,mpierr)  !//PAR_I/O enforce barrier here
      if (myPE == PE_IO) CALL OUT_ARRAY (array1, 'RO_g') 
!
      DO LC = 1, MMAX 
         if (myPE == PE_IO) WRITE (UNIT_OUT, 1400) CHAR(12), LC, TIME 
!        call MPI_Barrier(MPI_COMM_WORLD,mpierr)  !//PAR_I/O enforce barrier here
         call gather (ROP_s(:,LC),array1,root)    !//
!        call MPI_Barrier(MPI_COMM_WORLD,mpierr)  !//PAR_I/O enforce barrier here
         if (myPE == PE_IO) CALL OUT_ARRAY (array1, 'ROP_s') 
      END DO 
      if (myPE == PE_IO) WRITE (UNIT_OUT, 1500) CHAR(12), TIME 
!     call MPI_Barrier(MPI_COMM_WORLD,mpierr)  !//PAR_I/O enforce barrier here
      call gather (T_g,array1,root)    !//
!     call MPI_Barrier(MPI_COMM_WORLD,mpierr)  !//PAR_I/O enforce barrier here
      if (myPE == PE_IO) CALL OUT_ARRAY (array1, 'T_g') 
!
      DO LC = 1, MMAX 
         if (myPE == PE_IO) WRITE (UNIT_OUT, 1600) CHAR(12), LC, TIME 
!        call MPI_Barrier(MPI_COMM_WORLD,mpierr)  !//PAR_I/O enforce barrier here
         call gather (T_s(:,LC),array1,root)    !//
!        call MPI_Barrier(MPI_COMM_WORLD,mpierr)  !//PAR_I/O enforce barrier here
         if (myPE == PE_IO) CALL OUT_ARRAY (array1, 'T_s') 
      END DO 
      IF (SPECIES_EQ(0)) THEN 
         DO N = 1, NMAX(0) 
            if (myPE == PE_IO) WRITE (UNIT_OUT, 1710) CHAR(12), N, TIME 
!           call MPI_Barrier(MPI_COMM_WORLD,mpierr)  !//PAR_I/O enforce barrier here
            call gather (X_g(:,N),array1,root)    !//
!           call MPI_Barrier(MPI_COMM_WORLD,mpierr)  !//PAR_I/O enforce barrier here
            if (myPE == PE_IO) CALL OUT_ARRAY (array1, 'X_g') 
         END DO 
      ENDIF 
!

END IF

!Dufek 2_18_05     
!Write out solid visc., velocity, vol. frac.  ---For now assume only one particle species
!& this isn't DMP activated version


!write(10000,100) Ep_g
!write(10001,100) Mu_s(:,1)
!write(10002,100) U_s(:,1)
!write(10003,100) V_s(:,1)
!write(10004,100) F_GS(:,1)
!write(10005,100) P_s(:,1)

!write(10006,100) P_g(:)

!write(10007,100) T_g(:)
!write(10008,100) T_s(:,1)


!write(10009,100) gama_gs(:,1)
!write(10010,100) k_g(:)
!write(10011,*) MAGMA



IF (Write_Level==0) THEN
ELSE IF (Write_Level==1) THEN
call gather (P_g,array1,root)    !//
call MPI_Barrier(MPI_COMM_WORLD,mpierr)  !//PAR_I/O enforce barrier here
if (myPE == PE_IO) WRITE(20001,100) array1

!call gather (MU_GT,array1,root)    !//
!call MPI_Barrier(MPI_COMM_WORLD,mpierr)  !//PAR_I/O enforce barrier here
!if (myPE == PE_IO) WRITE(20004) array1


call MPI_Barrier(MPI_COMM_WORLD,mpierr) !//PAR_I/O enforce barrier here
call gather (EP_G,array1,root)    !//
call MPI_Barrier(MPI_COMM_WORLD,mpierr)  !//PAR_I/O enforce barrier here
if (myPE == PE_IO) WRITE(20006,100) array1

!call gather (MU_S(:,1),array1,root)    !//
!call MPI_Barrier(MPI_COMM_WORLD,mpierr)  !//PAR_I/O enforce barrier here
!if (myPE == PE_IO) WRITE(20010,100) array1

call MPI_Barrier(MPI_COMM_WORLD,mpierr) !//PAR_I/O enforce barrier here
call gather (T_g,array1,root)    !//
call MPI_Barrier(MPI_COMM_WORLD,mpierr)  !//PAR_I/O enforce barrier here
if (myPE == PE_IO) WRITE(20012,100) array1

call gather (T_S(:,1),array1,root)    !//
call MPI_Barrier(MPI_COMM_WORLD,mpierr)  !//PAR_I/O enforce barrier here
if (myPE == PE_IO) WRITE(20013,100) array1

call gather (T_S(:,2),array1,root)    !//
call MPI_Barrier(MPI_COMM_WORLD,mpierr)  !//PAR_I/O enforce barrier here
if (myPE == PE_IO) WRITE(20014,100) array1



call gather (U_G,array1,root)    !//
call MPI_Barrier(MPI_COMM_WORLD,mpierr)  !//PAR_I/O enforce barrier here
if (myPE == PE_IO) WRITE(20015,100) array1

call gather (V_G,array1,root)    !//
call MPI_Barrier(MPI_COMM_WORLD,mpierr)  !//PAR_I/O enforce barrier here
if (myPE == PE_IO) WRITE(20016,100) array1

call gather (U_S(:,1),array1,root)    !//
call MPI_Barrier(MPI_COMM_WORLD,mpierr)  !//PAR_I/O enforce barrier here
if (myPE == PE_IO) WRITE(20017,100) array1

call gather (U_S(:,2),array1,root)    !//
call MPI_Barrier(MPI_COMM_WORLD,mpierr)  !//PAR_I/O enforce barrier here
if (myPE == PE_IO) WRITE(20018,100) array1

call gather (V_S(:,1),array1,root)    !//
call MPI_Barrier(MPI_COMM_WORLD,mpierr)  !//PAR_I/O enforce barrier here
if (myPE == PE_IO) WRITE(20019,100) array1

call gather (V_S(:,2),array1,root)    !//
call MPI_Barrier(MPI_COMM_WORLD,mpierr)  !//PAR_I/O enforce barrier here
if (myPE == PE_IO) WRITE(20020,100) array1

call MPI_Barrier(MPI_COMM_WORLD,mpierr) !//PAR_I/O enforce barrier here
call gather (ROP_S(:,1),array1,root)    !//
call MPI_Barrier(MPI_COMM_WORLD,mpierr)  !//PAR_I/O enforce barrier here
if (myPE == PE_IO) WRITE(20023,100) array1

call MPI_Barrier(MPI_COMM_WORLD,mpierr) !//PAR_I/O enforce barrier here
call gather (ROP_S(:,2),array1,root)    !//
call MPI_Barrier(MPI_COMM_WORLD,mpierr)  !//PAR_I/O enforce barrier here
if (myPE == PE_IO) WRITE(20033,100) array1

!call MPI_Barrier(MPI_COMM_WORLD,mpierr) !//PAR_I/O enforce barrier here
!call gather (R_HUM,array1,root)    !//
!call MPI_Barrier(MPI_COMM_WORLD,mpierr)  !//PAR_I/O enforce barrier here
!if (myPE == PE_IO) WRITE(20034,100) array1


!call MPI_Barrier(MPI_COMM_WORLD,mpierr) !//PAR_I/O enforce barrier here
!call gather (RR_TEMP,array1,root)    !//
!call MPI_Barrier(MPI_COMM_WORLD,mpierr)  !//PAR_I/O enforce barrier here
!if (myPE == PE_IO) WRITE(20035,100) array1

!call MPI_Barrier(MPI_COMM_WORLD,mpierr) !//PAR_I/O enforce barrier here
!call gather (COLL_RATE,array1,root)    !//
!call MPI_Barrier(MPI_COMM_WORLD,mpierr)  !//PAR_I/O enforce barrier here
!if (myPE == PE_IO) WRITE(20036,100) array1

!WRITE(20037,100) C_RATE_SUM

!call gather (X_S(:,2,1),array1,root)    !//
!call MPI_Barrier(MPI_COMM_WORLD,mpierr)  !//PAR_I/O enforce barrier here
!if (myPE == PE_IO) WRITE(20024) array1


call gather (RO_G(:),array1,root) !//
call MPI_Barrier(MPI_COMM_WORLD,mpierr) !//PAR_I/O enforce barrier here
if (myPE == PE_IO) WRITE(20007,100) array1


call gather (W_G,array1,root)    !//
call MPI_Barrier(MPI_COMM_WORLD,mpierr)  !//PAR_I/O enforce barrier here
if (myPE == PE_IO) WRITE(20038,100) array1

call gather (W_S(:,1),array1,root)    !//
call MPI_Barrier(MPI_COMM_WORLD,mpierr)  !//PAR_I/O enforce barrier here
if (myPE == PE_IO) WRITE(20039,100) array1

call gather (W_S(:,2),array1,root)    !//
call MPI_Barrier(MPI_COMM_WORLD,mpierr)  !//PAR_I/O enforce barrier here
if (myPE == PE_IO) WRITE(20040,100) array1

call MPI_Barrier(MPI_COMM_WORLD,mpierr) !//PAR_I/O enforce barrier here
call gather(X_g(:,2),array1,root) !//
call MPI_Barrier(MPI_COMM_WORLD,mpierr) !//PAR_I/O enforce barrier here
if (myPE==PE_IO) WRITE(20041,100) array1


!call gather (X_S(:,2,2),array1,root) !//
!call MPI_Barrier(MPI_COMM_WORLD,mpierr) !//PAR_I/P enforce barrier here
!if (myPE ==PE_IO) WRITE(20025) array1


!call gather(U_S(:,2),array1,root) !//
!call MPI_Barrier(MPI_COMM_WORLD,mpierr) !//PAR_I/O enforce barrier here
!if (myPE==PE_IO) WRITE(20026) array1

!call gather(V_S(:,2),array1,root) !//
!call MPI_Barrier(MPI_COMM_WORLD,mpierr) !//PAR_I/O enforce barrier here
!if (myPE==PE_IO) WRITE(20027) array1


!call gather(THETA_M(:,1),array1,root) !//
!call MPI_Barrier(MPI_COMM_WORLD,mpierr) !//PAR_I/O enforce barrier here
!if (myPE==PE_IO) WRITE(20028) array1

!call gather(THETA_M(:,2),array1,root) !//
!call MPI_Barrier(MPI_COMM_WORLD,mpierr) !//PAR_I/O enforce barrier here
!if (myPE==PE_IO) WRITE(20029) array1

!call gather(THETA_M(:,3),array1,root) !//
!call MPI_Barrier(MPI_COMM_WORLD,mpierr) !//PAR_I/O enforce barrier here
!if (myPE==PE_IO) WRITE(20030) array1


!call gather(X_s(:,2,1),array1,root) !//
!call MPI_Barrier(MPI_COMM_WORLD,mpierr) !//PAR_I/O enforce barrier here
!if (myPE==PE_IO) WRITE(20031) array1


!call gather(X_s(:,2,2),array1,root) !//
!call MPI_Barrier(MPI_COMM_WORLD,mpierr) !//PAR_I/O enforce barrier here
!if (myPE==PE_IO) WRITE(20032) array1



!call MPI_Barrier(MPI_COMM_WORLD,mpierr) !//PAR_I/O enforce barrier here
!call gather(X_g(:,1),array1,root) !//
!call MPI_Barrier(MPI_COMM_WORLD,mpierr) !//PAR_I/O enforce barrier here
!if (myPE==PE_IO) WRITE(909991,100) array1



!IF(Nscalar==1) THEN
! WRITE(20024,100)Scalar(:,1)
!END IF


ELSE IF (Write_Level==2) THEN
! write(9020,100) FREQ1
! write(9021,100) FREQ2

WRITE(20001,100) P_G
WRITE(20002,100) K_TURB_G
WRITE(20003,100)E_TURB_G
WRITE(20004,100) MU_GT
WRITE(20005,100) P_STAR
WRITE(20006,100) EP_G
WRITE(20007,100) RO_G
WRITE(20008,100) ROP_S(:,1)
WRITE(20009,100) ROP_S(:,2)
WRITE(20010,100) MU_S(:,1)
WRITE(20011,100) MU_S(:,2)
WRITE(20012,100) T_G
WRITE(20013,100)T_S(:,1)
WRITE(20014,100) T_S(:,2)
WRITE(20015,100) U_G
WRITE(20016,100) V_G
WRITE(20017,100) U_S(:,1)
WRITE(20018,100) U_S(:,2)
WRITE(20019,100) V_S(:,1)
WRITE(20020,100) V_S(:,2)
WRITE(20021,100) THETA_M(:,1)
WRITE(20022,100) THETA_M(:,2)

WRITE(20023,100) 1.0-EP_G
WRITE(20026,100) X_G(:,1)
WRITE(20027,100) X_G(:,2)

!WRITE(30000,100) TEST_DEN
!IF(Nscalar==1) THEN
!! WRITE(20024,100)Scalar(:,1)
!END IF

WRITE(20030,100) K_s(:,1)
WRITE(20031,100) K_g
WRITE(20032,100) C_pg
END IF





 DO LC = 1, MMAX 
         IF (SPECIES_EQ(LC)) THEN 
            DO N = 1, NMAX(LC) 
               if (myPE == PE_IO) WRITE (UNIT_OUT, 1720) CHAR(12), LC, N, TIME 
!              call MPI_Barrier(MPI_COMM_WORLD,mpierr)  !//PAR_I/O enforce barrier here
               call gather (X_s(:,LC,N),array1,root)    !//
!              call MPI_Barrier(MPI_COMM_WORLD,mpierr)  !//PAR_I/O enforce barrier here
               if (myPE == PE_IO) CALL OUT_ARRAY (array1, 'X_s') 
            END DO 
         ENDIF 
      END DO 
      if (myPE == PE_IO) WRITE (UNIT_OUT, 1800) CHAR(12), TIME 
!     call MPI_Barrier(MPI_COMM_WORLD,mpierr)  !//PAR_I/O enforce barrier here
      call gather (U_g,array1,root)    !//
!     call MPI_Barrier(MPI_COMM_WORLD,mpierr)  !//PAR_I/O enforce barrier here
!//SP - Changed U_G to array1
      if (myPE == PE_IO) CALL OUT_ARRAY (array1, 'U_g') 
!
      if (myPE == PE_IO) WRITE (UNIT_OUT, 1900) CHAR(12), TIME 
!     call MPI_Barrier(MPI_COMM_WORLD,mpierr)  !//PAR_I/O enforce barrier here
      call gather (V_g,array1,root)    !//
!     call MPI_Barrier(MPI_COMM_WORLD,mpierr)  !//PAR_I/O enforce barrier here
!//SP - Changed V_G to array1
      if (myPE == PE_IO) CALL OUT_ARRAY (array1, 'V_g') 
!
      if (myPE == PE_IO) WRITE (UNIT_OUT, 2000) CHAR(12), TIME 
!     call MPI_Barrier(MPI_COMM_WORLD,mpierr)  !//PAR_I/O enforce barrier here
      call gather (W_g,array1,root)    !//
!     call MPI_Barrier(MPI_COMM_WORLD,mpierr)  !//PAR_I/O enforce barrier here
!//SP - Changed W_G to array1
      if (myPE == PE_IO) CALL OUT_ARRAY (array1, 'W_g') 
!
      DO LC = 1, MMAX 
         if (myPE == PE_IO) WRITE (UNIT_OUT, 2100) CHAR(12), LC, TIME 
!        call MPI_Barrier(MPI_COMM_WORLD,mpierr)  !//PAR_I/O enforce barrier here
         call gather (U_s(:,LC),array1,root)    !//
!        call MPI_Barrier(MPI_COMM_WORLD,mpierr)  !//PAR_I/O enforce barrier here
         if (myPE == PE_IO) CALL OUT_ARRAY (array1, 'U_s') 
!
         if (myPE == PE_IO) WRITE (UNIT_OUT, 2200) CHAR(12), LC, TIME 
!        call MPI_Barrier(MPI_COMM_WORLD,mpierr)  !//PAR_I/O enforce barrier here
         call gather (V_s(:,LC),array1,root)    !//
!        call MPI_Barrier(MPI_COMM_WORLD,mpierr)  !//PAR_I/O enforce barrier here
         if (myPE == PE_IO) CALL OUT_ARRAY (array1, 'V_s') 
!
         if (myPE == PE_IO) WRITE (UNIT_OUT, 2300) CHAR(12), LC, TIME 
!        call MPI_Barrier(MPI_COMM_WORLD,mpierr)  !//PAR_I/O enforce barrier here
         call gather (W_s(:,LC),array1,root)    !//
!        call MPI_Barrier(MPI_COMM_WORLD,mpierr)  !//PAR_I/O enforce barrier here
         if (myPE == PE_IO) CALL OUT_ARRAY (array1, 'W_s') 
!
!         IF(GRANULAR_ENERGY)THEN
         if (myPE == PE_IO) WRITE (UNIT_OUT, 2400) CHAR(12), LC, TIME 
!        call MPI_Barrier(MPI_COMM_WORLD,mpierr)  !//PAR_I/O enforce barrier here
         call gather (THETA_m(:,LC),array1,root)    !//
!        call MPI_Barrier(MPI_COMM_WORLD,mpierr)  !//PAR_I/O enforce barrier here
         if (myPE == PE_IO) CALL OUT_ARRAY (array1, 'Theta_m') 
      END DO 
      if (myPE == PE_IO) WRITE (UNIT_OUT, '(/1X,1A1)') CHAR(12) 
      IF (CALL_USR) CALL USR_WRITE_OUT1 
!     call MPI_Barrier(MPI_COMM_WORLD,mpierr)  !//PAR_I/O enforce barrier here

      IF(NScalar /= 0) THEN
        DO LC = 1, NScalar
         if (myPE == PE_IO) WRITE (UNIT_OUT, 2500) CHAR(12), LC, TIME 
!        call MPI_Barrier(MPI_COMM_WORLD,mpierr)  !//PAR_I/O enforce barrier here
         call gather (Scalar(:,LC),array1,root)    !//
!        call MPI_Barrier(MPI_COMM_WORLD,mpierr)  !//PAR_I/O enforce barrier here
         if (myPE == PE_IO) CALL OUT_ARRAY (array1, 'Scalar') 
        END DO
      ENDIF

      IF(K_Epsilon) THEN
         if (myPE == PE_IO) WRITE (UNIT_OUT, 2600) CHAR(12), TIME 
!        call MPI_Barrier(MPI_COMM_WORLD,mpierr)  !//PAR_I/O enforce barrier here
         call gather (K_Turb_G,array1,root)    !//
!        call MPI_Barrier(MPI_COMM_WORLD,mpierr)  !//PAR_I/O enforce barrier here
         if (myPE == PE_IO) CALL OUT_ARRAY (array1, 'K_Turb_G') 
         call gather (E_Turb_G,array1,root)    !//
         if (myPE == PE_IO) CALL OUT_ARRAY (array1, 'E_Turb_G') 
      ENDIF
      IF(nRR /= 0) THEN
        DO LC = 1, nRR
         if (myPE == PE_IO) WRITE (UNIT_OUT, 2500) CHAR(12), LC, TIME 
!        call MPI_Barrier(MPI_COMM_WORLD,mpierr)  !//PAR_I/O enforce barrier here
         call gather (ReactionRates(:,LC),array1,root)    !//
!        call MPI_Barrier(MPI_COMM_WORLD,mpierr)  !//PAR_I/O enforce barrier here
         if (myPE == PE_IO) CALL OUT_ARRAY (array1, 'RRates') 
        END DO
      ENDIF
      
      deallocate(array1)  !//

!
!             form feed character = CHAR(12)
 !     WRITE (UNIT_OUT, '(/1X,1A1)') CHAR(12) 
 !     IF (CALL_USR) CALL USR_WRITE_OUT1 
      RETURN  
 
 100 FORMAT(F30.20)

 1000 FORMAT(1X,A1,/5X,'--- Gas pressure (P_g) at time ',G12.5,' ---',2/) 
 1050 FORMAT(1X,A1,/5X,'--- Solids pressure (P_star) at time ',G12.5,' ---',2/) 
 1100 FORMAT(1X,A1,/5X,'--- Void fraction (EP_g) at time ',G12.5,' ---',2/) 
 1200 FORMAT(1X,A1,/5X,'--- Gas density (RO_g) at time ',G12.5,' ---',2/) 
 1400 FORMAT(1X,A1,/5X,'--- Solids Phase-',I1,' density x volume',&
         ' fraction (ROP_s) at time ',G12.5,' ---',2/) 
 1500 FORMAT(1X,A1,/5X,'--- Gas temperature (T_g) at time ',G12.5,' ---',2/) 
 1600 FORMAT(1X,A1,/5X,'--- Solids Phase-',I1,' temperature (T_s)',' at time ',&
         G12.5,' ---',2/) 
 1710 FORMAT(1X,A1,/5X,'--- Mass fraction of gas species (X_g) ',&
         I2,' at time ',&
         G12.5,' ---',2/) 
 1720 FORMAT(1X,A1,/5X,'--- Mass fraction of solids-',I1,' species (X_s)',I2,&
         ' at time ',G12.5,' ---',2/) 
 1800 FORMAT(1X,A1,/5X,'--- X-component of gas velocity (U_g) at time ',G12.5,&
         ' ---',2/) 
 1900 FORMAT(1X,A1,/5X,'--- Y-component of gas velocity (V_g) at time ',G12.5,&
         ' ---',2/) 
 2000 FORMAT(1X,A1,/5X,'--- Z-component of gas velocity (W_g) at time ',G12.5,&
         ' ---',2/) 
 2100 FORMAT(1X,A1,/5X,'--- X-component of Solids Phase-',I1,&
         ' velocity (U_s) at time ',G12.5,' ---',2/) 
 2200 FORMAT(1X,A1,/5X,'--- Y-component of Solids Phase-',I1,&
         ' velocity (V_s) at time ',G12.5,' ---',2/) 
 2300 FORMAT(1X,A1,/5X,'--- Z-component of Solids Phase-',I1,&
         ' velocity (W_s) at time ',G12.5,' ---',2/) 
 2400 FORMAT(1X,A1,/5X,'--- Granular temperature of Solids Phase-',I1,&
         ' (Theta_m) at time ',G12.5,' ---',2/) 
 2500 FORMAT(1X,A1,/5X,'--- Scalar Field-',I2, ' (Scalar) at time ',G12.5,' ---',2/) 
 2600 FORMAT(1X,A1,/5X,'--- Turbulence Field-', ' (K-Epsilon) at time ',G12.5,' ---',2/)
      END SUBROUTINE WRITE_OUT1 

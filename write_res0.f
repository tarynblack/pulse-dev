!vvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvC
!                                                                      C
!  Module name: WRITE_RES0                                             C
!  Purpose: write out the initial restart records (namelist data)      C
!                                                                      C
!  Author: P. Nicoletti                               Date: 13-DEC-91  C
!  Reviewer: P. Nicoletti, W. Rogers, M. Syamlal      Date: 24-JAN-92  C
!                                                                      C
!  Revision Number:                                                    C
!  Purpose:                                                            C
!  Author:                                            Date: dd-mmm-yy  C
!  Reviewer:                                          Date: dd-mmm-yy  C
!                                                                      C
!  Literature/Document References:                                     C
!                                                                      C
!  Variables referenced: RUN_NAME  ,  ID_MONTH  ,  ID_DAY , ID_YEAR    C
!                        ID_HOUR, ID_MINUTE, ID_SECOND, IMAX, JMAX     C
!                        KMAX, IMAX1, JMAX1, KMAX1, IMAX2, JMAX2,KMAX2 C
!                        IJMAX2, IJKMAX2, MMAX, DT, XLENGTH, YLENGTH   C
!                        ZLENGTH, DX, DY, DZ, RUN_NAME, DESCRIPTION    C
!                        UNITS, RUN_TYPE, CORDINATES, D_p0, RO_s,       C
!                        EP_star, MU_g0, MW_AVG, IC_X_w, IC_X_e, IC_Y_sC
!                        IC_Y_n, IC_Z_b, IC_Z_t, IC_I_w, IC_I_e        C
!                        IC_J_s, IC_J_n, IC_K_b, IC_K_t, IC_EP_g       C
!                        IC_P_g, IC_T_g, IC_T_s,  IC_U_g      C
!                        IC_V_g, IC_W_g, IC_ROP_s, IC_U_s, IC_V_s      C
!                        IC_W_s, BC_X_w, BC_X_e, BC_Y_s, BC_Y_n        C
!                        BC_Z_b, BC_Z_t, BC_I_w, BC_I_e, BC_J_s        C
!                        BC_K_b, BC_K_t, BC_EP_g, BC_P_g, BC_T_g       C
!                        BC_T_s,  BC_U_g, BC_V_g, BC_W_g      C
!                        BC_RO_g, BC_ROP_g, BC_VOLFLOW_g,BC_MASSFLOW_g C
!                        BC_ROP_s, BC_U_s, BC_V_s, BC_VOLFLOW_s        C
!                        BC_MASSFLOW_s, BC_TYPE, FLAG, RO_g0           C
!  Variables modified: None                                            C
!                                                                      C
!  Local variables: LC, L, N, NEXT_RECA, VERSION                       C
!                                                                      C
!^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^C
!
      SUBROUTINE WRITE_RES0 
!...Translated by Pacific-Sierra Research VAST-90 2.06G5  12:17:31  12/09/98  
!...Switches: -xf
!
!-----------------------------------------------
!   M o d u l e s 
!-----------------------------------------------
      USE param 
      USE param1 
      USE geometry
      USE physprop
      USE run
      USE ic
      USE is
      USE bc
      USE constant
      USE funits 
      USE output
      USE scales 
      USE scalars
      USE rxns
      USE ur_facs 
      USE leqsol 
      USE toleranc 
      USE compar           !//
      USE mpi_utility      !// for gather
      USE sendrecv         !// for filling the boundary information
!//       USE tmp_array    !// no longer using these arrays
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
!//  ... temporary arrays
!
      integer, allocatable :: arr1(:)
      integer, allocatable :: arr2(:)
!
!                loop counters
      INTEGER :: LC, L, N
!
!                Pointer to the next record
      INTEGER :: NEXT_RECA 
!
!                file version id
      CHARACTER :: VERSION*512 
!-----------------------------------------------
!
      if (myPE.eq.PE_IO) then
         allocate (arr1(ijkmax3))        !// 
         allocate (arr2(ijkmax2))        !// 
      else
         allocate (arr1(1))              !// 
         allocate (arr2(1))              !// 
         goto 1200
      end if

!
      NEXT_RECA = 5 
!
!     Add new data entries at the end of the file and identify version no.
!------------------------------------------------------------------------
      VERSION = 'RES = 01.6' 
!------------------------------------------------------------------------
!//SP - Temporarily disabled so that the binary files can be diffed.....
!
      WRITE (UNIT_RES, REC=1) VERSION 
      WRITE (UNIT_RES, REC=2) RUN_NAME, ID_MONTH, ID_DAY, ID_YEAR, ID_HOUR, &
         ID_MINUTE, ID_SECOND 
      WRITE (UNIT_RES, REC=3) NEXT_RECA 
      WRITE (UNIT_RES, REC=4) IMIN1, JMIN1, KMIN1, IMAX, JMAX, KMAX, IMAX1, &
         JMAX1, KMAX1, IMAX2, JMAX2, KMAX2, IJMAX2, IJKMAX2, MMAX, DIMENSION_IC&
         , DIMENSION_BC, DIMENSION_C, DIMENSION_IS, DT, XMIN, XLENGTH, YLENGTH&
         , ZLENGTH, C_E, C_F, PHI, PHI_W 

!
!  Add new write statements above this line.  Remember to update NEXT_RECA.
!  Remember to change the version number near begining of this subroutine.
!  Also modify READ_RES0.  The routines such as OUT_BIN_512 etc. writes
!  arrays dimensioned ARRAY(DIM).  So arrays dimensioned ARRAY(DIM1:DIM2)
!  should be passed as ARRAY(DIM1) and array length as DIM2-DIM1+1.
!---------------------------------------------------------------------------
      WRITE (UNIT_RES, REC=3) NEXT_RECA 
      CALL FLUSH (UNIT_RES) 

 1200 continue

      deallocate (arr1)              !//
      deallocate (arr2)              !//

      RETURN  
      END SUBROUTINE WRITE_RES0 

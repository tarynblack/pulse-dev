!vvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvC
!                                                                      C
!  Module name: WRITE_OUT0                                             C
!  Purpose: echo user input                                            C
!                                                                      C
!  Author: P. Nicoletti, M. Syamlal                   Date: 04-DEC-91  C
!  Reviewer: W. Rogers, M. Syamlal, S. Venkatesan     Date: 31-JAN-92  C
!                                                                      C
!  Revision Number: 1                                                  C
!  Purpose: add node, version                                          C
!  Author: P.Nicoletti                                Date: 07-FEB-92  C
!  Reviewer: S. Venkatesan                            Date: 11-DEC-92  C
!                                                                      C
!  Literature/Document References:                                     C
!                                                                      C
!  Variables referenced: ID_MONTH, ID_DAY, ID_YEAR, RUN_NAME, UNITS    C
!                        DESCRIPTION, RUN_TYPE, DX, IMAX1, IMAX, DY    C
!                        JMAX1, YLENGTH, XLENGTH, JMAX, DZ, KMAX1      C
!                        KMAX, ZLENGTH, MMAX, D_p0, RO_s, EP_star, MU_g0C
!                        MW_AVG, IC_DEFINED, IC_X_w, IC_X_e, IC_Y_s    C
!                        IC_Z_b, IC_Z_t, IC_I_w, IC_I_e, IC_J_s        C
!                        IC_J_n, IC_K_b, IC_K_t, IC_EP_g, IC_P_g       C
!                        IC_U_g, IC_V_g, IC_W_g, IC_ROP_s, IC_T_s      C
!                        IC_U_s, IC_V_s, IC_W_s, BC_DEFINED            C
!                        BC_TYPE, BC_X_w, BC_X_e, BC_Y_s, BC_Y_n       C
!                        BC_Z_b, BC_Z_t, BC_I_w, BC_I_e, BC_J_s        C
!                        BC_J_n, BC_Z_b, BC_Z_t, BC_EP_g, BC_P_g       C
!                        BC_T_g, BC_U_g, BC_V_g, BC_W_g, BC_ROP_s      C
!                        BC_T_s, BC_U_s, BC_V_s, BC_W_s                C
!                        ICBC_FLAG, IMIN1, JMIN1, KMIN1, ID_NODE       C
!                        ID_VERSION, RO_g0                             C
!  Variables modified: M                                               C
!                                                                      C
!  Local variables: L, LOC                                             C
!                                                                      C
!^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^C
!
      SUBROUTINE WRITE_OUT0 
!...Translated by Pacific-Sierra Research VAST-90 2.06G5  12:17:31  12/09/98  
!...Switches: -xf
!
!-----------------------------------------------
!   M o d u l e s 
!-----------------------------------------------
      USE param 
      USE param1 
      USE run
      USE output
      USE physprop
      USE geometry
      USE ic
      USE bc
      USE is
      USE fldvar
      USE constant
      USE indices
      USE funits 
      USE toleranc 
      USE scales 
      USE scalars
      USE ur_facs 
      USE leqsol 
      USE compar         !//d
      USE mpi_utility    !//d
      USE sendrecv    !//d
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
      INTEGER :: L, M, N 
      DOUBLE PRECISION, DIMENSION(6) :: LOC 
! 
!                      Coefficient of restitution (old symbol) 
      DOUBLE PRECISION :: E 
      CHARACTER, DIMENSION(3) :: LEGEND*3 
      CHARACTER, DIMENSION(0:8) :: DISCR_NAME*12 
      CHARACTER, DIMENSION(0:8) :: DISCR_NAME1*12 
!-----------------------------------------------
!   E x t e r n a l   F u n c t i o n s
!-----------------------------------------------
      DOUBLE PRECISION , EXTERNAL :: LOCATION 
!-----------------------------------------------

! Output script of constants in mfix.dat for matlab processing
! T. Black, 17 Nov 2015
Open(254698, File = 'mfixconst')
Write(254698,*)'IMAX',IMAX
Write(254698,*)'JMAX',JMAX
Write(254698,*)'KMAX',KMAX
Write(254698,*)'LENGTH',XLENGTH
Write(254698,*)'HEIGHT',YLENGTH
Write(254698,*)'WIDTH',ZLENGTH
Write(254698,*)'RO_S1',RO_s(1)
Write(254698,*)'RO_S2',RO_s(2)
Write(254698,*)'RO_S3',RO_s(3)
Write(254698,*)'NFRAC_S1',PARTICLE_INLET_FRAC(1)
Write(254698,*)'NFRAC_S2',PARTICLE_INLET_FRAC(2)
Write(254698,*)'NFRAC_S3',PARTICLE_INLET_FRAC(3)
Write(254698,*)'PULSING',PULSED
Write(254698,*)'FREQUENCY',FREQUENCY
Write(254698,*)'MIN_GAS',MIN_GAS
Write(254698,*)'MAX_GAS',MAX_GAS
Write(254698,*)'VENT_RADIUS',SETRADIUS
Write(254698,*)'TIME_INTERVAL',TIME_INTERVAL
Write(254698,*)'TSTOP',TSTOP
Write(254698,*)'ATMOSPHERIC',ATMOSPHERIC
Write(254698,*)'TROPOPAUSE',TROPOPAUSE
Write(254698,*)'BC_EPG',BC_EP_g(2)
Write(254698,*)'BC_PG',BC_P_g(2)
Write(254698,*)'BC_TG',BC_T_g(2)
Write(254698,*)'BC_TS1',BC_T_s(2,1)
Write(254698,*)'BC_TS2',BC_T_s(2,2)
Write(254698,*)'BC_TS3',BC_T_s(2,3)
Write(254698,*)'D_S1',D_p0(1)
Write(254698,*)'D_S2',D_p0(2)
Write(254698,*)'D_S3',D_p0(3)
!
!
!
!                      Coefficient of restitution (old symbol)
      DATA DISCR_NAME/'FOUP', 'FOUP', 'Superbee', 'Smart', 'Ultra-Quick', &
         'QUICKEST', 'Muscl', 'VanLeer', 'Minmod'/ 
      DATA DISCR_NAME1/'FOUP', 'FOUP', 'Fourth Order', 'Smart', 'Ultra-Quick', &
         'QUICKEST', 'Muscl', 'VanLeer', 'Minmod'/ 

      if (myPE.ne.PE_IO) return

!
!
      RETURN  
 1000 FORMAT(17X,'MM      MM  FFFFFFFFFF    IIIIII    XX      XX',/17X,&
         'MM      MM  FFFFFFFFFF    IIIIII    XX      XX',/17X,&
         'MMMM  MMMM  FF              II      XX      XX',/17X,&
         'MMMM  MMMM  FF              II      XX      XX',/17X,&
         'MM  MM  MM  FF              II        XX  XX  ',/17X,&
         'MM  MM  MM  FF              II        XX  XX  ',/17X,&
         'MM      MM  FFFFFFFF        II          XX    ',/17X,&
         'MM      MM  FFFFFFFF        II          XX    ',/17X,&
         'MM      MM  FF              II        XX  XX  ',/17X,&
         'MM      MM  FF              II        XX  XX  ',/17X,&
         'MM      MM  FF              II      XX      XX',/17X,&
         'MM      MM  FF              II      XX      XX',/17X,&
         'MM      MM  FF            IIIIII    XX      XX',/17X,&
         'MM      MM  FF            IIIIII    XX      XX',2/20X,&
         'Multiphase Flow with Interphase eXchanges'/34X,'Version: ',A,/20X,&
         'Time: ',I2,':',I2,20X,'Date: ',I2,'-',I2,'-',I4) 
 1010 FORMAT(/7X,'Computer : ',A50,/,1X,79('_')) 
 1100 FORMAT(//,3X,'1. RUN CONTROL',/) 
 1101 FORMAT(/7X,'* Model B momentum equations are solved')
 1102 FORMAT(/7X,'Number of scalars = ', I4,&
             /7X,'Scalar No.        Carrier Phase (Phase4Scalar)')
 1103 FORMAT(/7X, I4,'               ',I4)
 1104 FORMAT(/7X,'* K and Epsilon equations are solved.')
 1105 FORMAT(/7X,'* Simonin model is solved')
 1106 FORMAT(/7X,'* Ahmadi model is solved')
 1107 FORMAT(/7X,'** Note: When Simonin or Ahmadi model is solved, K-Epsilon' &
                  ' and granular energy are automatically solved.')
 1108 FORMAT(/7X,'* Schaeffer frictional model is solved')
 1109 FORMAT(/7X,'* Savage frictional model is solved')
 1110 FORMAT(7X,'Run name(RUN_NAME): ',A60) 
 1120 FORMAT(7X,'Brief description of the run (DESCRIPTION) :',/9X,A60) 
 1130 FORMAT(7X,'Units (UNITS) : ',A16) 
 1135 FORMAT(7X,'Start-time (TIME) = ',G12.5,/7X,'Stop_time (TSTOP) = ',G12.5,/&
         7X,'Time step (DT) = ',G12.5,/7X,'Max time step (DT_MAX) = ',G12.5,/7X&
         ,'Min time step (DT_MIN) = ',G12.5,/7X,&
         'Time step adjustment factor (DT_FAC) = ',G12.5) 
 1136 FORMAT(7X,'* Steady state simulation.') 
 1137 FORMAT(7X,'Type of run (RUN_TYPE) : ',A16) 
 1138 FORMAT(30X,'(Initial conditions from the input (.DAT) file)') 
 1139 FORMAT(30X,'(Initial conditions from the restart (.RES) file)') 
 1140 FORMAT(/7X,'* Gas momentum equation-',A,' is',A,'solved.') 
 1141 FORMAT(/7X,'* Solids-',I1,' momentum equation-',A,' is',A,'solved.') 
 1143 FORMAT(/7X,'* Energy equations are solved.') 
 1144 FORMAT(/7X,'* Energy equations are NOT solved.') 
 1145 FORMAT(/7X,'* Gas Species equations are solved.') 
 1146 FORMAT(/7X,'* Gas Species equations are NOT solved.') 
 1147 FORMAT(/7X,'* Solids-',I1,' Species equations are solved.') 
 1148 FORMAT(/7X,'* Solids-',I1,' Species equations are NOT solved.') 
 1149 FORMAT(/7X,'* User-defined subroutines are',A,'called.') 
!
 1150 FORMAT(//,3X,'2. PHYSICAL AND NUMERICAL PARAMETERS',/) 
 1151 FORMAT(7X,'Coefficient of restitution (C_e) = ',G12.5) 
 1152 FORMAT(7X,'Coefficient of friction (C_f) = ',G12.5) 
 1153 FORMAT(7X,'Angle of internal friction (Phi) = ',G12.5) 
 1154 FORMAT(7X,'Angle of wall_particle friction (Phi_w) = ',G12.5) 
 1155 FORMAT(7X,'Default turbulence length scale (L_scale0) = ',G12.5,/7X,&
         'Maximum turbulent viscosity (MU_gmax) = ',G12.5) 
 1156 FORMAT(7X,'Excluded volume for B-M stress term (V_ex) = ',G12.5) 
 1157 FORMAT(7X,'Reference pressure (P_ref) = ',G12.5,/7X,&
         'Pressure scale-factor (P_scale) = ',G12.5,/7X,&
         'Gravitational acceleration (GRAVITY) = ',G12.5) 
 1158 FORMAT(7X,'Under relaxation (UR_FAC) and',&
         ' Iterations in Leq solver (LEQ_IT):'/,9X,&
         '                        UR_FAC',2X,'LEQ_IT','  LEQ_METHOD',&
         '  LEQ_SWEEP', '  LEQ_TOL', '  DISCRETIZE') 
 1159 FORMAT(9X,&
         'Fluid cont. and P_g   = ',F5.3,2X,I4,5X,I4,9x,A4,4X,G11.4,2X,A12/9X,&
         'Solids cont. and P_s  = ',F5.3,2X,I4,5X,I4,9x,A4,4X,G11.4,2X,A12/9X,&
         'U velocity            = ',F5.3,2X,I4,5X,I4,9x,A4,4X,G11.4,2X,A12/9X,&
         'V velocity            = ',F5.3,2X,I4,5X,I4,9x,A4,4X,G11.4,2X,A12/9X,&
         'W velocity            = ',F5.3,2X,I4,5X,I4,9x,A4,4X,G11.4,2X,A12/9X,&
         'Energy                = ',F5.3,2X,I4,5X,I4,9x,A4,4X,G11.4,2X,A12/9X,&
         'Species               = ',F5.3,2X,I4,5X,I4,9x,A4,4X,G11.4,2X,A12/9X,&
         'Granular Energy       = ',F5.3,2X,I4,5X,I4,9x,A4,4X,G11.4,2X,A12/9X,&
         'User scalar           = ',F5.3,2X,I4,5X,I4,9x,A4,4X,G11.4,2X,A12/) 
 1190 FORMAT(7X,1A20,'- C(',I2,') = ',G12.5) 
!
 1200 FORMAT(//,3X,'3. GEOMETRY AND DISCRETIZATION',/) 
 1201 FORMAT(7X,'Coordinates: ',1A16/) 
 1202 FORMAT(7X,'Cyclic boundary conditions in ',A,' direction',A) 
 1203 FORMAT(7X,'Pressure drop (DELP_',A,') = ',G12.5) 
 1210 FORMAT(7X,'X-direction cell sizes (DX) and East face locations:') 
 1211 FORMAT(7X,'Minimum value of X, or R (XMIN) =',G12.5) 
 1212 FORMAT(7X,'Number of cells in X, or R, direction (IMAX) = ',I4) 
 1213 FORMAT(7X,'Reactor length in X, or R, direction (XLENGTH) =',G12.5//) 
 1220 FORMAT(7X,'Y-direction cell sizes (DY) and North face locations:') 
 1221 FORMAT(7X,'Number of cells in Y direction (JMAX) = ',I4) 
 1222 FORMAT(7X,'Reactor length in Y direction (YLENGTH) =',G12.5//) 
 1230 FORMAT(7X,'Z-direction cell sizes (DZ) and Top face locations:') 
 1231 FORMAT(7X,'Number of cells in Z, or theta, direction (KMAX) = ',I4) 
 1232 FORMAT(7X,'Reactor length in Z, or theta, direction (ZLENGTH) =',G12.5) 
!
 1300 FORMAT(//,3X,'4. GAS PHASE',/) 
 1305 FORMAT(7X,'Gas density (RO_g0) = ',G12.5,&
         '  (A constant value is used everywhere)') 
 1310 FORMAT(7X,'Viscosity (MU_g0) = ',G12.5,&
         '  (A constant value is used everywhere)') 
 1315 FORMAT(7X,'Number of gas species (NMAX(0)) = ',I3) 
 1316 FORMAT(7X,'Gas species',5X,'Molecular weight (MW_g)') 
 1317 FORMAT(7X,3X,I3,15X,G12.5) 
 1320 FORMAT(7X,'Average molecular weight (MW_avg) = ',G12.5,&
         '  (A constant value is used everywhere)') 
!
 1400 FORMAT(//,3X,'5. SOLIDS PHASE',/) 
 1405 FORMAT(7X,'Viscosity (MU_s0) = ',G12.5,&
         '  (A constant value is used everywhere)') 
 1410 FORMAT(7X,'Number of particulate phases (MMAX) = ',I2) 
 1420 FORMAT(/7X,'M',5X,'Diameter (D_p0)',T35,'Density (RO_s)',T50,&
         'Close_Packed') 
 1421 FORMAT(7X,I1,5X,G12.5,T35,G12.5,T55,L1) 
 1422 FORMAT(7X,'Number of solids-',I1,' species (NMAX(',I1,')) = ',I3) 
 1423 FORMAT(7X,'Solids species',5X,'Molecular weight (MW_s)') 
 1424 FORMAT(7X,3X,I3,18X,G12.5) 
 1430 FORMAT(/7X,'Void fraction at maximum packing (EP_star) = ',G12.5) 
!
 1500 FORMAT(//,3X,'6. INITIAL CONDITIONS') 
 1510 FORMAT(/7X,'Initial condition no : ',I4) 
 1520 FORMAT(9X,39X,' Specified  ',5X,' Simulated  ',/9X,&
         'X coordinate of west face   (IC_X_w) = ',G12.5,5X,G12.5/,9X,&
         'X coordinate of east face   (IC_X_e) = ',G12.5,5X,G12.5/,9X,&
         'Y coordinate of south face  (IC_Y_s) = ',G12.5,5X,G12.5/,9X,&
         'Y coordinate of north face  (IC_Y_n) = ',G12.5,5X,G12.5/,9X,&
         'Z coordinate of bottom face (IC_Z_b) = ',G12.5,5X,G12.5/,9X,&
         'Z coordinate of top face    (IC_Z_t) = ',G12.5,5X,G12.5) 
 1530 FORMAT(9X,'I index of cell at west   (IC_I_w) = ',I4,/,9X,&
         'I index of cell at east   (IC_I_e) = ',I4,/,9X,&
         'J index of cell at south  (IC_J_s) = ',I4,/,9X,&
         'J index of cell at north  (IC_J_n) = ',I4,/,9X,&
         'K index of cell at bottom (IC_K_b) = ',I4,/,9X,&
         'K index of cell at top    (IC_K_t) = ',I4) 
 1540 FORMAT(9X,'Void fraction (IC_EP_g) = ',G12.5) 
 1541 FORMAT(9X,'Gas pressure (IC_P_g) = ',G12.5) 
 1542 FORMAT(9X,'Gas temperature (IC_T_g) = ',G12.5) 
 1543 FORMAT(9X,'Gas species',5X,'Mass fraction (IC_X_g)') 
 1544 FORMAT(9X,3X,I3,15X,G12.5) 
 1545 FORMAT(9X,'Gas radiation coefficient   (IC_GAMA_Rg) = ',G12.5,/,9X,&
         'Gas radiation temperature   (IC_T_Rg) = ',G12.5) 
 1550 FORMAT(9X,'X-component of gas velocity (IC_U_g) = ',G12.5,/9X,&
         'Y-component of gas velocity (IC_V_g) = ',G12.5,/9X,&
         'Z-component of gas velocity (IC_W_g) = ',G12.5) 
 1560 FORMAT(9X,'Solids phase-',I1,' Density x Volume fr. (IC_ROP_s) = ',G12.5) 
 1561 FORMAT(9X,'Solids phase-',I1,' temperature (IC_T_s) = ',G12.5) 
 1563 FORMAT(9X,'Solids-',I1,' species',5X,'Mass fraction (IC_X_s)') 
 1564 FORMAT(9X,3X,I3,20X,G12.5) 
 1565 FORMAT(9X,'Solids phase-',I1,' radiation coefficient (IC_GAMA_Rs)',' =',&
         G12.5,/9X,'Solids phase-',I1,' radiation temperature (IC_T_Rs) =',&
         G12.5) 
 1570 FORMAT(9X,'X-component of solids phase-',I1,' velocity (IC_U_s) =',G12.5,&
         /9X,'Y-component of solids phase-',I1,' velocity (IC_V_s) =',G12.5,/9X&
         ,'Z-component of solids phase-',I1,' velocity (IC_W_s) =',G12.5) 
 1574 FORMAT(9X,'Solids pressure (IC_P_star) = ',G12.5) 
 1575 FORMAT(9X,'Turbulence length scale (IC_L_scale) = ',G12.5) 
!
 1600 FORMAT(//,3X,'7. BOUNDARY CONDITIONS') 
 1601 FORMAT(/7X,'Average value of ',A,G12.5) 
 1602 FORMAT(/7X,'Average value of ',A,I2,A,G12.5) 
 1610 FORMAT(/7X,'Boundary condition no : ',I4) 
 1611 FORMAT(9X,'Type of boundary condition : ',A16) 
 1612 FORMAT(11X,'(Inlet with specified gas and solids mass flux)') 
 1613 FORMAT(11X,'(Outlet with specified gas and solids mass flux)') 
 1614 FORMAT(11X,'(Inlet with specified gas pressure)') 
 1615 FORMAT(11X,'(Outlet with specified gas pressure)') 
 1616 FORMAT(11X,'(Gradients of parallel velocity components are zero)') 
 1617 FORMAT(11X,'(Velocity is zero at wall)') 
 1618 FORMAT(11X,'(Partial slip condition at wall)') 
 1619 FORMAT(11X,'(Outflow condition)') 
 1620 FORMAT(9X,39X,' Specified  ',5X,' Simulated  ',/9X,&
         'X coordinate of west face   (BC_X_w) = ',G12.5,5X,G12.5/,9X,&
         'X coordinate of east face   (BC_X_e) = ',G12.5,5X,G12.5/,9X,&
         'Y coordinate of south face  (BC_Y_s) = ',G12.5,5X,G12.5/,9X,&
         'Y coordinate of north face  (BC_Y_n) = ',G12.5,5X,G12.5/,9X,&
         'Z coordinate of bottom face (BC_Z_b) = ',G12.5,5X,G12.5/,9X,&
         'Z coordinate of top face    (BC_Z_t) = ',G12.5,5X,G12.5) 
 1630 FORMAT(9X,'I index of cell at west   (BC_I_w) = ',I4,/,9X,&
         'I index of cell at east   (BC_I_e) = ',I4,/,9X,&
         'J index of cell at south  (BC_J_s) = ',I4,/,9X,&
         'J index of cell at north  (BC_J_n) = ',I4,/,9X,&
         'K index of cell at bottom (BC_K_b) = ',I4,/,9X,&
         'K index of cell at top    (BC_K_t) = ',I4) 
 1640 FORMAT(9X,'Void fraction (BC_EP_g) = ',G12.5) 
 1641 FORMAT(9X,'Gas pressure (BC_P_g) = ',G12.5) 
 1642 FORMAT(9X,'Gas temperature (BC_T_g) = ',G12.5) 
 1643 FORMAT(9X,'Gas species',5X,'Mass fraction (BC_X_g)') 
 1644 FORMAT(9X,3X,I3,15X,G12.5) 
 1648 FORMAT(9X,'Gas mass flow rate (BC_MASSFLOW_g) = ',G12.5) 
 1649 FORMAT(9X,'Gas volumetric flow rate (BC_VOLFLOW_g) = ',G12.5) 
 1650 FORMAT(9X,'X-component of gas velocity (BC_U_g) = ',G12.5) 
 1651 FORMAT(9X,'Y-component of gas velocity (BC_V_g) = ',G12.5) 
 1652 FORMAT(9X,'Z-component of gas velocity (BC_W_g) = ',G12.5) 
 1655 FORMAT(9X,'Initial interval when jet vel= BC_Jet_g0 (BC_DT_0) = ',G12.5,/&
         9X,'Initial jet velocity (BC_Jet_g0) = ',G12.5,/9X,&
         'Interval when jet vel= BC_Jet_gl (BC_DT_l) = ',G12.5,/9X,&
         'Low value of jet velocity (BC_Jet_gl) = ',G12.5,/9X,&
         'Interval when jet vel = BC_Jet_gh (BC_DT_h) = ',G12.5,/9X,&
         'High value of jet velocity (BC_Jet_gh) = ',G12.5) 
 1656 FORMAT(9X,'Interval for averaging outflow rates= (BC_DT_0) = ',G12.5) 
 1660 FORMAT(9X,'Solids phase-',I1,' Density x Volume fr. (BC_ROP_s) = ',G12.5) 
 1661 FORMAT(9X,'Solids phase-',I1,' temperature (BC_T_s) = ',G12.5) 
 1663 FORMAT(9X,'Solids-',I1,' species',5X,'Mass fraction (BC_X_s)') 
 1664 FORMAT(9X,3X,I3,20X,G12.5) 
 1668 FORMAT(9X,'Solids phase-',I1,' mass flow rate (BC_MASSFLOW_s) =',G12.5) 
 1669 FORMAT(9X,'Solids phase-',I1,' volumetric flow rate (BC_VOLFLOW_s) =',&
         G12.5) 
 1670 FORMAT(9X,'X-component of solids phase-',I1,' velocity (BC_U_s) =',G12.5) 
 1671 FORMAT(9X,'Y-component of solids phase-',I1,' velocity (BC_V_s) =',G12.5) 
 1672 FORMAT(9X,'Z-component of solids phase-',I1,' velocity (BC_W_s) =',G12.5) 
 1675 FORMAT(9X,'Partial slip coefficient   (BC_hw_g) = ',G12.5,/,9X,&
         'Slip velociity U at wall   (BC_Uw_g) = ',G12.5,/,9X,&
         'Slip velociity V at wall   (BC_Vw_g) = ',G12.5,/,9X,&
         'Slip velociity W at wall   (BC_Ww_g) = ',G12.5) 
 1676 FORMAT(9X,'Solids phase: ',I1,/,11X,&
         'Partial slip coefficient   (BC_hw_s) = ',G12.5,/,11X,&
         'Slip velociity U at wall   (BC_Uw_s) = ',G12.5,/,11X,&
         'Slip velociity V at wall   (BC_Vw_s) = ',G12.5,/,11X,&
         'Slip velociity W at wall   (BC_Ww_s) = ',G12.5) 
!
 1700 FORMAT(//,3X,'8. INTERNAL SURFACES') 
 1710 FORMAT(/7X,'Internal surface no : ',I4) 
 1711 FORMAT(9X,'Type of internal surface : ',A16) 
 1712 FORMAT(11X,'(No gas or solids flow through the surface)') 
 1713 FORMAT(11X,'(Only gas flows through the surface)') 
 1720 FORMAT(9X,39X,' Specified  ',5X,' Simulated  ',/9X,&
         'X coordinate of west face   (IS_X_w) = ',G12.5,5X,G12.5/,9X,&
         'X coordinate of east face   (IS_X_e) = ',G12.5,5X,G12.5/,9X,&
         'Y coordinate of south face  (IS_Y_s) = ',G12.5,5X,G12.5/,9X,&
         'Y coordinate of north face  (IS_Y_n) = ',G12.5,5X,G12.5/,9X,&
         'Z coordinate of bottom face (IS_Z_b) = ',G12.5,5X,G12.5/,9X,&
         'Z coordinate of top face    (IS_Z_t) = ',G12.5,5X,G12.5) 
 1730 FORMAT(9X,'I index of cell at west   (IS_I_w) = ',I4,/,9X,&
         'I index of cell at east   (IS_I_e) = ',I4,/,9X,&
         'J index of cell at south  (IS_J_s) = ',I4,/,9X,&
         'J index of cell at north  (IS_J_n) = ',I4,/,9X,&
         'K index of cell at bottom (IS_K_b) = ',I4,/,9X,&
         'K index of cell at top    (IS_K_t) = ',I4) 
 1740 FORMAT(9X,'Permeability (IS_PC1) = ',G12.5) 
 1741 FORMAT(9X,'Inertial resistance factor (IS_PC2) = ',G12.5) 
 1742 FORMAT(9X,'Solids phase-',I2,' Velocity (IS_VEL_s) = ',G12.5) 
!
 1800 FORMAT(//,3X,'9. OUTPUT DATA FILES:',/7X,'Extension',T18,'Description',&
         T59,'Interval for writing',/7X,'.OUT',T18,'This file (ASCII)',T61,&
         G12.5,/7X,'.LOG',T18,'Log file containing messages (ASCII)',&
         /7X,'.RES',&
         T18,'Restart file (Binary)',T61,G12.5,/7X,'.SP1',T18,&
         'EP_g (Binary, single precision)',T61,G12.5,/7X,'.SP2',T18,&
         'P_g, P_star (Binary, single precision)',T61,G12.5,/7X,'.SP3',T18,&
         'U_g, V_g, W_g (Binary, single precision)',T61,G12.5,/7X,'.SP4',T18,&
         'U_s, V_s, W_s (Binary, single precision)',T61,G12.5,/7X,'.SP5',T18,&
         'ROP_s (Binary, single precision)',T61,G12.5,/7X,'.SP6',T18,&
         'T_g, T_s (Binary, single precision)',T61,G12.5,/7X,'.SP7',T18,&
         'X_g, X_s (Binary, single precision)',T61,G12.5,/7X,'.SP8',T18,&
         'Theta_m (Binary, single precision)',T61,G12.5,/7X,'.SP9',T18,&
         'User Scalar (Binary, single precision)',T61,G12.5) 
!
 1900 FORMAT(//,3X,'10. TOLERANCES',/7X,&
         'The following values are specified in the file TOLERANCE.INC.') 
 1901 FORMAT(/7X,'Minimum value of EP_s tracked (ZERO_EP_s) = ',G12.5) 
 1904 FORMAT(7X,'Maximum average residual (TOL_RESID) = ',G12.5,/7X,&
         'Maximum average residual (TOL_RESID_T) = ',G12.5,/7X,&
         'Maximum average residual (TOL_RESID_X) = ',G12.5,/7X,&
         'Minimum residual at divergence (TOL_DIVERGE) = ',G12.5) 
 1905 FORMAT(7X,'Tolerance for species and energy balances (TOL_COM) = ',G12.5) 
 1906 FORMAT(7X,'Tolerance for scalar mass balances (TOL_RESID_Scalar) = ',G12.5)  
 1907 FORMAT(7X,'Tolerance for K-Epsilon balances (TOL_RESID_K_Epsilon) = ',G12.5)  
 1908 FORMAT(7X,'Tolerance for Granular Temp.  balances (TOL_RESID_Th) = ',G12.5)
!
      END SUBROUTINE WRITE_OUT0 
      
      SUBROUTINE WRITE_FLAGS
      USE param
      USE param1
      USE funits
      USE geometry
      USE indices
      USE compar         !//d
      USE mpi_utility    !//d
      USE sendrecv    !//d
      IMPLICIT NONE
      integer ijk
!
      character*3, allocatable :: array1(:)   !//d
      character*4, dimension(:), allocatable :: array2, array3
      include 'function.inc'
      
  
      if (myPE .eq. PE_IO) then
         allocate (array1(ijkmax3))
         allocate (array2(dimension_3))
         allocate (array3(ijkmax3))
      else
         allocate (array1(1))
         allocate (array2(dimension_3))
         allocate (array3(1))
      end if

!write(*,*) 'ijkmax3', ijkmax3, dimension_3
      
!//SP Filling the processor ghost layer with the correct values

      call gather (icbc_flag,array1,PE_IO)
      call scatter (icbc_flag,array1,PE_IO)
      
!
!  Superimpose internal surface flags on Initial and boundary condition flags
!
      DO ijk = IJKSTART3, IJKEND3
        array2(ijk) = '    '
        array2(ijk)(1:3) = icbc_flag(ijk)(1:3)
        IF (IP_AT_E(IJK)) THEN 
           array2(IJK)(4:4) = 'E' 
        ELSE IF (SIP_AT_E(IJK)) THEN 
           array2(IJK)(4:4) = 'e' 
        ENDIF 
!
        IF (IP_AT_N(IJK)) THEN 
           array2(IJK)(4:4) = 'N' 
        ELSE IF (SIP_AT_N(IJK)) THEN 
           array2(IJK)(4:4) = 'n' 
        ENDIF 
!
        IF (IP_AT_T(IJK)) THEN 
           array2(IJK)(4:4) = 'T' 
        ELSE IF (SIP_AT_T(IJK)) THEN 
           array2(IJK)(4:4) = 't' 
        ENDIF 
      ENDDO
      call gather (array2,array3,PE_IO)
      
      if(myPE.eq.PE_IO) then
        WRITE (UNIT_OUT, 2000) CHAR(12) 
        CALL OUT_ARRAY_C (array3, 'BC/IC condition flags') 
        WRITE (UNIT_OUT, *)
      ENDIF
      

      deallocate (array1) 
      deallocate (array2) 
      deallocate (array3) 
!
 2000 FORMAT(//,3X,'11. INITIAL AND BOUNDARY CONDITION FLAGS',/7X,&
         'The initial and boundary conditions specified are shown in',/7X,&
         'the following map. Each computational cell is represented',/7X,&
         'by a string of three characters.  The first character',/7X,&
         'represents the type of cell, and the last two characters',/7X,&
         'give a number that identifies a boundary or initial condi-',/7X,&
         'tion.  For example, .02 indicates a cell where Initial',/7X,&
         'Condition No. 2 will be specified. Only the last two digits'/7X,&
         'are written.  Hence, for example, Condition No. 12, 112, 212'/7X,&
         'etc. will be represented only as 12.',/7X,&
         '  First Character       Description'/7X,&
         '       .                Initial condition'/7X,&
         '       W                No slip wall'/7X,&
         '       S                Free-slip wall'/7X,&
         '       s                Partial-slip wall'/7X,&
         '       c                Cyclic boundary'/7X,&
         '       C                Cyclic boundary with pressure drop'/7X,&
         '       I                Specified mass-flux inflow cell'/7X,&
         '       O                Outflow cell'/7X,&
         '       p                Specified pressure inflow cell'/7X,&
         '       P                Specified pressure outflow cell'/7X,&
         '                                                       '/7X,&
         'Internal surfaces at East, North or Top of each cell is',/7X,&
         'is represented by the following letters to the right of the',/7X,&
         'three-character string:',/7X,&
         '  Side          Impermeable           Semipermeable',/7X,&
         '  East             E                       e       ',/7X,&
         '  North            N                       n       ',/7X,&
         '  Top              T                       t       ',/7X,&
         'For cells with internal surfaces on more than one side',/7X,&
         'the characters will be over-written in the above order',/1X,A1) 
	 RETURN
	 END SUBROUTINE WRITE_FLAGS

      

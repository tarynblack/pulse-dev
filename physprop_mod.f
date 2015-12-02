!vvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvC
!                                                                      C
!  Module name: physical_prop.inc                                      C
!  Purpose: Common block containing physical property data             C
!                                                                      C
!  Author: M. Syamlal                                 Date: dd-mmm-yy  C
!  Reviewer:                                          Date: dd-mmm-yy  C
!                                                                      C
!  Revision Number: 1                                                  C
!  Purpose: Add EP_zero, D_p3, oD_p3, MASS_s                     C
!  Author: W. Sams                                    Date: 22-JUL-93  C
!  Reviewer:                                          Date: dd-mmm-yy  C
!                                                                      C
!  Revision Numner: 2                                                  C
!  Purpose: Add K_scale                                                C
!  Author: W. Sams                                    Date: 26-APR-94  C
!  Reviewer:                                                           C
!                                                                      C
!  Literature/Document References:                                     C
!                                                                      C
!  Variables referenced:                                               C
!  Variables modified:                                                 C
!                                                                      C
!  Local variables:                                                    C
!                                                                      C
!^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^C
 
 
      MODULE physprop
 
 
      Use param
      Use param1
 
!DUFEK

DOUBLE PRECISION VOL_INLET_G, VEL_INLET_G, ROP_INLET_P1, ROP_INLET_P2,ROP_INLET_P3



LOGICAL ATMOSPHERIC
DOUBLE PRECISION TROPOPAUSE
DOUBLE PRECISION  PARTICLE_INLET_FRAC(DIM_M)
DOUBLE PRECISION SETRADIUS
DOUBLE PRECISION::MIN_GAS,MAX_GAS,FREQUENCY,LAGRAN_UPDATE,LAG_RAD,LAG_DENSE,TOP_PRESSURE,WATER_K,WATER_C,ICE_RATE,WATER_RATE,WATER_VAPORIZATION,TIME_INTERVAL
DOUBLE PRECISION::DRAGGER_10,SMAG,C_RATE_SUM
DOUBLE PRECISION::SAMPLEWRITEOUT,WATER_VIS,ICE_VIS,MAGMA_VIS,WATER_DENSITY
INTEGER:: SRATE,Write_Level
LOGICAL:: GLACIER, WATER_RUN
LOGICAL:: LES
DOUBLE PRECISION, DIMENSION(:,:), ALLOCATABLE::VENT_CONDITIONS1,VENT_CONDITIONS2,VENT_CONDITIONS3,CRATER_LOC2
DOUBLE PRECISION,DIMENSION(:),ALLOCATABLE::R_HUM,RR_TEMP,COLL_RATE!PARTICLE_INLET_FRAC
!!DOUBLE PRECISION, DIMENSION(:), ALLOCATABLE:: BOUNDARY_CELL,GY,GX,GZ
!!!DOUBLE PRECISION, DIMENSION(180,30)::height_check

!DOUBLE PRECISION, DIMENSION(:), ALLOCATABLE:: leakfac1,leakfac2,leakfac3,leakfac4 
!DOUBLE PRECISION, DIMENSION(:), ALLOCATABLE:: EscapeMax,EscapeMin,FREQ1,FREQ2 

!DOUBLE PRECISION, DIMENSION(:), ALLOCATABLE::LOSS1,LOSS2

!---------LAGRAN Variables--------------------------------------------!

DOUBLE PRECISION::BOT_Z_LAG,TOP_Z_LAG,W_LAG,RATE_DEP_REC
LOGICAL:: WATER_BOUNDARY
!!DOUBLE PRECISION, DIMENSION(:,:),ALLOCATABLE::SUM_WATER_LOSS,SUM_DEPOSIT,WATER_LOSS1,WATER_LOSS2,WATER_LOSS3,WATER_LOSS4
!!DOUBLE PRECISION, DIMENSION(:,:),ALLOCATABLE::DEPOSIT_1,DEPOSIT_2,DEPOSIT_3,DEPOSIT_4,HBB,HEIGHTBB
INTEGER::PARTICLECHECK
!---------LAGRAN Variables--------------------------------------------!

DOUBLE PRECISION, DIMENSION(:), ALLOCATABLE:: LAG_VOL,U_cg,V_cg,W_cg,UOLD,VOLD,WOLD
INTEGER,DIMENSION(:),ALLOCATABLE::PARTICLE_IMPACT

INTEGER random_seed
DOUBLE PRECISION Dtimer,LAGRANWRITEOUT,Dtimer2
LOGICAL LAGRAN, LEAK
LOGICAL FLUCT
DOUBLE PRECISION TIME_START_LAG,TIME_STOP_LAG,DIA_AVG,DIA_VAR
INTEGER DT_LAG,INTRO_PART,INTRO_PART0,NUMPARTICLES
!DOUBLE PRECISION, DIMENSION(:),ALLOCATABLE::P_G_ref,T_G_ref,TEST_DEN

DOUBLE PRECISION, DIMENSION(:),ALLOCATABLE::Pug1,Pvg1,DIAMETER,DENSITYP,PPU,PPV,PPX,PPY,Fgpu,Fgpv,Fspu,Fspv,PPW,PPZ,Fspw,Fgpw
!DOUBLE PRECISION, DIMENSION(:),ALLOCATABLE::P1_T_g,P1_T_s,P1_Theta,P1_P_g,P1_K_Turb_g,P1_U_g,P1_V_g,P1_EP_g
!DOUBLE PRECISION, DIMENSION(:),ALLOCATABLE::DIAMETER2,DENSITYP2,PPU2,PPV2,PPX2,PPY2,Fgpu2,Fgpv2,Fspu2,Fspv2
!DOUBLE PRECISION, DIMENSION(:),ALLOCATABLE::P1_T_g2,P1_T_s2,P1_Theta2,P1_P_g2,P1_K_Turb_g2,P1_U_g2,P1_V_g2,P1_EP_g2
!DOUBLE PRECISION, DIMENSION(:),ALLOCATABLE::DIAMETER3,DENSITYP3,PPU3,PPV3,PPX3,PPY3,Fgpu3,Fgpv3,Fspu3,Fspv3
!DOUBLE PRECISION, DIMENSION(:),ALLOCATABLE::P1_T_g3,P1_T_s3,P1_Theta3,P1_P_g3,P1_K_Turb_g3,P1_U_g3,P1_V_g3,P1_EP_g3
!DOUBLE PRECISION, DIMENSION(:),ALLOCATABLE::DIAMETER4,DENSITYP4,PPU4,PPV4,PPX4,PPY4,Fgpu4,Fgpv4,Fspu4,Fspv4
!DOUBLE PRECISION, DIMENSION(:),ALLOCATABLE::P1_T_g4,P1_T_s4,P1_Theta4,P1_P_g4,P1_K_Turb_g4,P1_U_g4,P1_V_g4,P1_EP_g4
!DOUBLE PRECISION, DIMENSION(:),ALLOCATABLE::DIAMETER5,DENSITYP5,PPU5,PPV5,PPX5,PPY5,Fgpu5,Fgpv5,Fspu5,Fspv5
!DOUBLE PRECISION, DIMENSION(:),ALLOCATABLE::P1_T_g5,P1_T_s5,P1_Theta5,P1_P_g5,P1_K_Turb_g5,P1_U_g5,P1_V_g5,P1_EP_g5
DOUBLE PRECISION U_LAG,V_LAG,TOP_X_LAG,TOP_Y_LAG,BOT_X_LAG,BOT_Y_LAG


      LOGICAL          MASS_INFLUX_TIME,PULSED
!
!                      Number of solids phases
      INTEGER          MMAX
!
!                      Scale factor for gas turbulence length scale
      DOUBLE PRECISION K_scale
!
!                      Particle diameters
      DOUBLE PRECISION D_p0 (DIM_M)
!
!                      index to rearrange particles from coarsest to finest
!                      for use in function CALC_ep_star(IJK,IER)
      INTEGER          M_MAX (DIM_M)
!
!                      Particle densities
      DOUBLE PRECISION RO_s  (DIM_M)
!
!                      Specified constant solids viscosity
      DOUBLE PRECISION MU_s0
!
!                      Flag indicates whether the phase becomes close-packed
!                      at ep_star
      LOGICAL          CLOSE_PACKED (DIM_M)
!
!                      Specified constant gas density
      DOUBLE PRECISION RO_g0
!
!                      Specified constant gas viscosity
      DOUBLE PRECISION MU_g0
!
!                      gas viscosity
      DOUBLE PRECISION, DIMENSION(:), ALLOCATABLE ::  MU_g 
!
!                      average molecular weight of gas
      DOUBLE PRECISION MW_AVG
!
!                      Constant constant-pressure specific heat of gas
      DOUBLE PRECISION C_pg0
!
!                      Constant pressure specific heat of gas
      DOUBLE PRECISION, DIMENSION(:), ALLOCATABLE ::  C_pg 
!
!                      Constant constant-pressure specific heat of solids
      DOUBLE PRECISION C_ps0
!
!                      Constant pressure specific heat of solids
      DOUBLE PRECISION, DIMENSION(:, :), ALLOCATABLE ::  C_ps 
!
!                      Specified constant gas conductivity
      DOUBLE PRECISION K_g0
!
!                      Conductivity of gas
      DOUBLE PRECISION, DIMENSION(:), ALLOCATABLE ::  K_g 
!
!                      Specified constant solids conductivity
      DOUBLE PRECISION K_s0
!
!                      Conductivity of solids
      DOUBLE PRECISION, DIMENSION(:, :), ALLOCATABLE ::  K_s 
!
!		       Granular Temperature Conductivity (associated
!                      with temperature gradient)
      DOUBLE PRECISION, DIMENSION(:, :), ALLOCATABLE ::  Kth_s 
!
!		       Granular Temperature Conductivity (associated
!                      with volume fraction gradient)
      DOUBLE PRECISION, DIMENSION(:, :), ALLOCATABLE ::  Kphi_s 
!
!                      Specified constant gas diffusivity
      DOUBLE PRECISION DIF_g0
!
!                      Diffusivity of gas species N
      DOUBLE PRECISION, DIMENSION(:, :), ALLOCATABLE ::  DIF_g 
!
!                      Specified constant solids diffusivity
      DOUBLE PRECISION DIF_s0
!
!                      Diffusivity of solids species N
      DOUBLE PRECISION, DIMENSION(:, :, :), ALLOCATABLE ::  DIF_s 
!
!                      Total number of gas or solids species
      INTEGER          NMAX(0:DIM_M)
!
!                      Molecular weight of gas species
      DOUBLE PRECISION MW_g (DIM_N_g)
!
!                      Molecular weight of solids species
      DOUBLE PRECISION MW_s (DIM_M, DIM_N_s)
!
!                      Molecular weight of gas mixture
      DOUBLE PRECISION, DIMENSION(:), ALLOCATABLE ::  MW_MIX_g 
!
 
 
!!!HPF$ align MU_g(:) with TT(:)
!!!HPF$ align C_pg(:) with TT(:)
!!!HPF$ align C_ps(:, *) with TT(:)
!!!HPF$ align K_g(:) with TT(:)
!!!HPF$ align K_s(:, *) with TT(:)
!!!HPF$ align Kth_s(:, *) with TT(:)
!!!HPF$ align Kphi_s(:, *) with TT(:)
!!!HPF$ align DIF_g(:, *) with TT(:)
!!!HPF$ align DIF_s(:, *, *) with TT(:)
!!!HPF$ align MW_MIX_g(:) with TT(:)

      END MODULE physprop

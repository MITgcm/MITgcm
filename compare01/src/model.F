C $Id: model.F,v 1.5 1998/06/23 13:36:42 adcroft Exp $
#include "CPP_OPTIONS.h"
#include "CPP_MACROS.h"
C================================================================================
C     Procedure name: MODEL                                                     |
C           Function: Main time-stepping loop for MIT Ocean-Atmosphere model    |
C           Comments:                                                           |
C================================================================================
CStartofinterface
      SUBROUTINE MODEL (
     U                  U, V, W, T, S, PH, PS )
      IMPLICIT NONE
C     /-------------------------------------------------------------------------\
C     | Global variable declarations                                            |
C     \-------------------------------------------------------------------------/
#include "SIZE.h"
#include "OPERATORS.h"
#include "GRID.h"
#include "PARAMS.h"
#include "OLDG.h"
#include "MASKS.h"
#include "CG2DA.h"
#include "CG2DZ.h"
#include "AJAINF.h"
#include "FORCING.h"
#include "EXTERNAL.h"
C     /-------------------------------------------------------------------------\
C     |  Routine argument declarations                                          |
C     |=========================================================================|
C     | U, V, W - X,Y,Z Velocity ( m/s, m/s, Pa/s ).                            |
C     | T       - Potential temperature (oC).                                   |
C     | S       - Salinity (ppt).                                               |
C     | PH      - Hydrostatic pressure perturbation (m).                        |
C     | PS      - Surface pressure (m).                                         |
C     \-------------------------------------------------------------------------/
      REAL    U (_I3(nz,nx,ny))
      REAL    V (_I3(nz,nx,ny))
      REAL    W (_I3(nz,nx,ny))
      REAL    T (_I3(nz,nx,ny))
      REAL    S (_I3(nz,nx,ny))
      REAL    PH(_I3(nz,nx,ny))
      REAL    PS(nx, ny       )
CEndofinteface
C     /-------------------------------------------------------------------------\
C     |  Local variable declarations                                            |
C     |=========================================================================|
C     | gu      - Accumulates du/dt ( m/s/s).                                   |
C     | gv      - Accumulates dv/dt ( m/s/s).                                   |
C     | gt      - Accumulates dT/dt ( oC/s/s).                                  |
C     | gs      - Accumulates ds/dt ( ppt/s/s).                                 |
C     | rPrm    - Density perturbation at "t,s" points.                         |
C     | rPrmBz  - Density perturbation at "w" points                            |
C     | rhs2d   - Right-hand side of elliptic equation.                         |
C     | rhs2d_K - Per level term contributing to rhs2d.                         |
C     | divh    - Vertically integrated divergence.                             |
C     | divh_K  - Per level term contributing to divh.                          |
C     | tmp     - I/O buffer                                                    |
C     | I,J,K  - Loop counters.                                                 |
C     \-------------------------------------------------------------------------/
      REAL gu         (nx,ny,nz)
      REAL gv         (nx,ny,nz)
      REAL gt         (nx,ny,nz)
      SAVE gT
      REAL gs         (nx,ny,nz)
      SAVE gs
      REAL rPrm       (nx,ny,nz)
      REAL rPrmBz     (nx,ny,nz)
      REAL RHS2d      (nx,ny)
      REAL divH       (nx,ny)
      REAL RHS2d_K    (nx,ny)
      REAL divH_K     (nx,ny)
      REAL tmp        (nx,ny)
      INTEGER nIts, I, J, K, KK, KKK, LEV

C     /-------------------------------------------------------------------------\ 
C     | Variables for eddy paramiterisation                                     |
C     \-------------------------------------------------------------------------/ 
      REAL Kredigm(_I3(Nz,Nx,Ny))
      REAL K31(_I3(Nz,Nx,Ny))
      REAL K32(_I3(Nz,Nx,Ny))
      REAL K33(_I3(Nz,Nx,Ny))
      REAL Gwk (_I3(Nz,Nx,Ny))
      REAL Tsurf(_I3(2,Nx,Ny))
      LOGICAL DumpFlg

C     /-------------------------------------------------------------------------\ 
C     | Control variables for multithreaded execution                           |
C     \-------------------------------------------------------------------------/
      INTEGER Gu_Loop_No_Of_Parallel_Blocks
      INTEGER Gu_Loop_Size_Of_SubBlock
      PARAMETER ( Gu_Loop_No_Of_Parallel_Blocks = 1 )
      PARAMETER ( Gu_Loop_Size_Of_SubBlock      = Nz)

C     /-------------------------------------------------------------------------\ 
C     | I/O code                                                                |
C     \-------------------------------------------------------------------------/
      CHARACTER*(MAXFN) fName
      Real*4   TIM1,TARR1(2),TIM2,TARR2(2)

C     /-------------------------------------------------------------------------\ 
C     | Diagnostics                                                             |
C     \-------------------------------------------------------------------------/
      REAL bAveT, bAveS
      REAL depth
C
#ifdef USE_ETIME
      TIM1 = ETIME(TARR1)
#endif
C     IF ( pickupRun ) THEN
       DO K =1,nz
        LEV=K
        CALL CADJ(
     I             LEV,
     U             T, S)
       ENDDO

CMIC$  PARALLEL
CMIC$&  SHARED(T,S,rPrm)
CMIC$&  PRIVATE(LEV,K)
CMIC$  DO PARALLEL
       DO K =1,nz
        LEV=K
        CALL UPDATE_RHO(
     I               T, S, LEV,'LINEAR','LOCAL',
     O               rPrm )
       ENDDO
CMIC$  END DO
CMIC$  END PARALLEL
       rPrmBz(_I3(1,:,:)) = rPrm(_I3(1,:,:))
       DO K = 2, nz
        rPrmBz(_I3(K,:,:)) = ( rPrm(_I3(K,:,:))+rPrm(_I3(K-1,:,:)) )*0.5
       ENDDO
C     ENDIF

C     /----------------------------------------------------------------\
C     | Load wind stress data                                          |
C     \----------------------------------------------------------------/
C     CALL LOAD_TAU
C     /----------------------------------------------------------------\
C     | Load T and S relaxation profile                                |
C     \----------------------------------------------------------------/
C     CALL LOAD_SST
C     /----------------------------------------------------------------\
C     | Initial statistics                                             |
C     \----------------------------------------------------------------/
      depth = -delps(1)/2/G/ronil
      WRITE(0,*) 'Temp     Salt    Depth'
      DO K = 1, Nz
       IF ( K .NE. 1 ) THEN
        depth = depth - delps(K-1)/2/G/ronil - delps(K)/2/G/ronil
       ENDIF
       bAveT = 
     &SUM(T( _I3(K,:,:) )*ZA( _I3(K,:,:) )*delps(K),MASK=PMASK( _I3(K,:,:) ).EQ.WATER)/
     &SUM(ZA( _I3(K,:,:) )*delps(K),MASK=PMASK( _I3(K,:,:) ).EQ.WATER)
       bAveS = 
     &SUM(S( _I3(K,:,:) )*ZA( _I3(K,:,:) )*delps(K),MASK=PMASK( _I3(K,:,:) ).EQ.WATER)/
     &SUM(ZA( _I3(K,:,:) )*delps(K),MASK=PMASK( _I3(K,:,:) ).EQ.WATER)
       WRITE (0,*) bAveT, bAveS, depth
      ENDDO
C     /----------------------------------------------------------------\
C     | Start of main time stepping loop                               |
C     \----------------------------------------------------------------/
      gT = 0.
      gS = 0.
      nIts = numberOfTimeSteps
      DO J =1, nIts
c      write(0,*) 'model: T  ',minval(T),maxval(T)
       nIter   = nIter+1
       RHS2d   = 0.
       divH    = 0.
C      /----------------------------------------------------------------\
C      | Find hydrostatic pressure.                                     |
C      \----------------------------------------------------------------/
       CALL FIND_PH( RHS2D, rPrmBz, divH,
     O            PH , PS )
C      /----------------------------------------------------------------\
C      | Find explicit tendency terms for momentum equation.            |
C      |================================================================|
C      | o Gu =  GuAdvection + GuDiffusion + GuCoriolis + GuMetric      |
C      |       + GuForcing                                              |
C      |                                                                |
C      | o Gv =  GvAdvection + GvDiffusion + GvCoriolis + GvMetric      |
C      |       + GvForcing                                              |
C      |                                                                |
C      | o Gw =  GwAdvection + GwDiffusion + GwCoriolis + GwMetric      |
C      |       + GwForcing                                              |
C      \----------------------------------------------------------------/
CMIC$  PARALLEL
CMIC$&  SHARED(U,V,W,PH,GU,GV,PS,RHS2D,divH),
CMIC$&  PRIVATE(RHS2D_K,K,KK,KKK,LEV,divH_K)
       RHS2D_K   = 0.
       divH_K    = 0.
CMIC$  DO PARALLEL
       DO KKK = 1, Gu_Loop_No_Of_Parallel_Blocks
        DO KK  = 1, Gu_Loop_Size_Of_SubBlock
         K = Gu_Loop_Size_Of_SubBlock*(KKK-1)+KK
         LEV = K
         CALL G_CALC(
     I               U, V, W, PH, PS, LEV,
     U               RHS2d_K, divH_K,
     O               GU, GV)
caja     CALL UPDATE_T(
caja I                U, V, W, LEV, T,
caja O                GT )
        ENDDO
       ENDDO

Caja from 4x4 working model
       CALL RediTensor(T,S,Kredigm,K31,K32,K33,nIter,DumpFlg)
       CALL INC_TRACER (
     I      U,V,W,
     U      T,gWk,GTNM1,
     I      Kredigm,K31,K32,K33,
     I      Heat,.TRUE.,.TRUE.,.TRUE.,
     I      .TRUE.,.TRUE.,
     I      a4TempXY,a2TempXY,a2TempZ)
         GTNM1=gWk
       CALL INC_TRACER (
     I      U,V,W,
     U      S,gWk,GSNM1,
     I      Kredigm,K31,K32,K33,
     I      Ssurf,.TRUE.,.TRUE.,.TRUE.,
     I      .TRUE.,.TRUE.,
     I      a4SaltXY,a2SaltXY,a2SaltZ)
         GsNM1=gWk
Caja end from 4x4 working model

CMIC$  GUARD
       RHS2d = RHS2d + RHS2d_K
       divH  = divH  + divH_K
CMIC$  END GUARD
CMIC$  END DO
CMIC$  END PARALLEL
CcnhDebugStarts
c      WRITE(0,*) ' GUNM1 K=1 at T = ', currentTime
c      CALL PLOT_FIELD(GUNM1(_I3(1,:,:)),Nx,Ny)
c      WRITE(0,*) ' GVNM1 K=1 at T = ', currentTime
c      CALL PLOT_FIELD(GVNM1(_I3(1,:,:)),Nx,Ny)
CcnhDebugEnds
C      /----------------------------------------------------------------\
C      | Find the pressure that keeps flow non-divergent.               |
C      |================================================================|
C      | Surface pressure is found from.                                |
C      | del_xy**2(ps) = div_xy(G)+1/DT*div_xy(u)-del_xy**2(ph)         |
C      | Non-hydrostatic pressure is found from.                        |
C      | del**2(pnh) = div(G)+1/DT*div(u)-del**2(ph)-del_xy**2(ps)      |
C      \----------------------------------------------------------------/
CcnhDebugStarts
c      WRITE(0,*) ' S/R MODEL: CALL PFIND'
CcnhDebugEnds
       CALL PFIND( RHS2D, rPrmBz, divH,
     O            PH , PS )

C      /----------------------------------------------------------------\
C      | Step forward the velocity field.                               |
C      |================================================================|
C      | u <- u + Gu - G*Ddx{ps+ph+pnh}                                 |
C      | v <- v + Gv - G*Ddy{ps+ph+pnh}                                 |
C      | Ddz{w} = -Ddx{u}-Ddy{v}                                        |
C      \----------------------------------------------------------------/
CMIC$  PARALLEL
CMIC$&  SHARED(U,V,W,PH,GU,GV,PS)
CMIC$&  PRIVATE(K,LEV)
CMIC$  DO PARALLEL
       DO K = 1, nZ
        LEV = K
        CALL UPDATE_UV(
     I              PH, PS, GU, GV, LEV,
     U              U, V )
       ENDDO
CMIC$  END DO
CMIC$  END PARALLEL
       CALL UPDATE_W(
     I               U, V, PS,
     O               W )
CcnhDebugStarts
c      WRITE(0,*) ' S/R MODEL: CALL UPDATE_UV'
c      WRITE(0,*) ' U K=1 at t = ', currentTime
c      CALL PLOT_FIELD(U(_I3(1,:,:)),Nx,Ny)
c      WRITE(0,*) ' V K=1 at t = ', currentTime
c      CALL PLOT_FIELD(V(_I3(1,:,:)),Nx,Ny)
c      WRITE(0,*) ' W K=2 at t = ', currentTime
c      CALL PLOT_FIELD(W(_I3(2,:,:))/-9808.038D0,Nx,Ny)
CcnhDebugEnds
C      CALL LOAD_SST
       currentTime = currentTime+DELT
c      WRITE(0,*) currentTime
#ifdef GMGS
C      /----------------------------------------------------------------\
C      | Calculate eddy parameterisation terms.                         |
C      |================================================================|
C      | uTotal = U + uEddy                                             |
C      | vTotal = V + uEddy                                             |
C      | Ddz{wTotal} = -Ddx{uTotal}-Ddy{vTotal}                         |
C      \----------------------------------------------------------------/
C      DumpFlg = .FALSE.
C      if (J.eq.nIts) DumpFlg = .TRUE.
C      CALL RediTensor(T,S,Kredi,K13,K23,K31,K32,K33,Ustar,Vstar,Wstar
C    &    ,nIter,DumpFlg)
C
C      CALL INC_TRACER (
C    I      U,V,W,
C    U      T,gWk,GTNM1,
C    I      Kredi,K13,K23,K31,K32,K33,Ustar,Vstar,Wstar,
C    I      Tsurf,.TRUE.,.TRUE.,.TRUE.,
C    I      .TRUE.,.TRUE.,
C    I      a4TempXY,a2TempZ )
C        GTNM1=gWk
C      CALL INC_TRACER (
C    I      U,V,W,
C    U      S,gWk,GSNM1,
C    I      Kredi,K13,K23,K31,K32,K33,Ustar,Vstar,Wstar,
C    I      Ssurf,.TRUE.,.TRUE.,.TRUE.,
C    I      .TRUE.,.TRUE.,
C    I      a4TempXY,a2TempZ )
C        GSNM1=gWk
#else
C      /----------------------------------------------------------------\
C      | Step forward the temperature field.                            |
C      |================================================================|
C      | Gt = GtAdvection + GtForcing + GtDiffusion                     |
C      | t <- t + Gt*DT                                                 |
C      \----------------------------------------------------------------/
CcnhDebugStarts
C       WRITE(0,*) ' S/R MODEL: UPDATE_T not being called'
CcnhDebugEnds
CMIC$  PARALLEL
CMIC$&  SHARED(U,V,W,T,GT,S,GS)
CMIC$&  PRIVATE(LEV,K)
CMIC$  DO PARALLEL
C      DO K =1,nz
C       LEV=K
C       CALL UPDATE_T(
C    I               U, V, W, LEV, T,
C    O               GT )
C      WRITE(20,*) ' S/R MODEL: UPDATE_T being called'
C       CALL UPDATE_S(
C    I               U, V, W, LEV,S,
C    O               GS )
C      ENDDO
CMIC$  END DO
CMIC$  END PARALLEL
       T = T + GT*DELT*asyncFac*0.
       S = S + GS*DELT*asyncFac*0.
#endif
CcnhDebugStarts
c      WRITE(0,*) ' T K=1 at t = ', currentTime
c      CALL PLOT_FIELD(T(_I3(1,:,:)),Nx,Ny)
CcnhDebugEnds
C      /----------------------------------------------------------------\
C      | Diagnose density field.                                        |
C      |================================================================|
C      | rho = rho{T,S,P}                                               |
C      |       **Note: Vertical mixing by convective adjustment is done |
C      |               in this stage.                                   |
C      \----------------------------------------------------------------/
CMIC$  PARALLEL
CMIC$&  SHARED(T,S,rPrm)
CMIC$&  PRIVATE(LEV,K)
CMIC$  DO PARALLEL
       DO K =1,nz
        LEV=K
        CALL CADJ(
     I             LEV,
     U             T, S)
       ENDDO
CMIC$  END DO
C  CMIC$  DO PARALLEL
C         DO K =2,nz,2
C          LEV=K
C          CALL CADJ(
C       I             LEV,
C       U             T, S)
C         ENDDO
C  CMIC$  END DO
CMIC$  END PARALLEL
CMIC$  PARALLEL
CMIC$&  SHARED(T,S,rPrm)
CMIC$&  PRIVATE(LEV,K)
CMIC$  DO PARALLEL
       DO K =1,nz
        LEV=K
        CALL UPDATE_RHO(
     I               T, S, LEV,'LINEAR','LOCAL',
     O               rPrm )
       ENDDO
CMIC$  END DO
CMIC$  END PARALLEL
CcnhDebugStarts
c      WRITE(0,*) ' RPRM K=1 at t = ', currentTime
c      CALL PLOT_FIELD(rPrm(_I3(1,:,:)),Nx,Ny)
CcnhDebugEnds
       rPrmBz(_I3(1,:,:)) = rPrm(_I3(1,:,:))
       DO K = 2, nz
        rPrmBz(_I3(K,:,:)) = ( rPrm(_I3(K,:,:))+rPrm(_I3(K-1,:,:)) )*0.5
       ENDDO
 
!      write(0,*) 'model: gT ',minval(gTnm1),maxval(gTnm1)
!      write(0,*) 'model: T* ',minval(T),maxval(T)
!      write(0,*) 'model: pH ',minval(pH),maxval(pH)
!      write(0,*) 'model: gU ',minval(gUnm1),maxval(gUnm1)
!      write(0,*) 'model: gV ',minval(gVnm1),maxval(gVnm1)
!      write(0,*) 'model: PS ',minval(PS),maxval(PS)
!      write(0,*) 'model: U  ',minval(U),maxval(U)
!      write(0,*) 'model: V  ',minval(V),maxval(V)
!      write(0,*) 'model: K31',minval(K31),maxval(K31)
!      write(0,*) 'model: K32',minval(K32),maxval(K32)
!      write(0,*) 'model: K33',minval(K33),maxval(K33)

      ENDDO
C     /----------------------------------------------------------------\
C     | End of main time stepping loop                                 |
C     \----------------------------------------------------------------/
C
      END

C $Id: set_defaults.F,v 1.2 1998/06/08 21:42:59 cnh Exp $
#include "CPP_OPTIONS.h"
#include "CPP_MACROS.h"
C================================================================================
C     Procedure name: SET_DEFAULTS                                              |
C           Function: Assign default values to model parameters.                |
C           Comments:                                                           |
C================================================================================

CStartofinterface
      SUBROUTINE SET_DEFAULTS (
     O                         U, V, W, T, S, PH, PS )
      IMPLICIT NONE
C     /-------------------------------------------------------------------------\
C     | Global variable declarations                                            |
C     \-------------------------------------------------------------------------/
#include "SIZE.h"
#include "PARAMS.h"
#include "STRINGS.h"
#include "OPERATORS.h"
#include "GRID.h"
#include "OLDG.h"
#include "MASKS.h"
#include "CG2DA.h"
#include "CG2DZ.h"
#include "AJAINF.h"
#include "EPARAM.h"
#include "FORCING.h"
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
C     | DELTAZ - Layer thickness (m).                                           |
C     | torry  - Reference wind stress (N).                                     |
C     | y      - Distance accumulator (m).                                      |
C     | ySize  - Basin extent in y (m).                                         |
C     | pBeta  - Planetary beta (s^-1.m^-1).                                    |
C     | I,J,K - Loop counters.                                                  |
C     \-------------------------------------------------------------------------/
      REAL    DELTAZ(nz)
      REAL    torry
      REAL    y
      REAL    ySize
      REAL    pBeta
      REAL    Q
      REAL    DELTAF
      REAL    Fref
      REAL    fWall
      REAL    Beta
      REAL    r
      REAL    Kz
      REAL    Dep
      REAL    Rho0
      REAL    dist
      REAL    RMAX
      REAL    RSQ
      REAL    tau
      REAL    XCENTER
      REAL    YCENTER
      REAL    mx
      REAL    mn
      REAL    HEATFLAG
      REAL    HEATSUM
      REAL    RANDN      
      INTEGER I
      INTEGER J
      INTEGER K

      REAL TAUMAX, DISTY, LY
C
C     /-------------------------------------------------------------------------\
C     | Parameter settings for small double gyre beta-plane experiment.         |
C     \=========================================================================|
C
      G           = 9.81D0
      RONIL       = 999.8D0
      CP          = 3900.D0
CENDIGNOREENDIGNOREENDIGNOREENDIGNOREENDIGNOREENDIGNOREENDIGNOREENDIGNORE

       delT       = 1200.D0
      freeSurfFac = 1.D0
       startTime  = 0.
      endTime     = startTime+10.*delT
      dPUFreq     = 300.*delT


CSTARTIGNORESTARTIGNORESTARTIGNORESTARTIGNORESTARTIGNORESTARTIGNORESTARTIGNORE
      phiMin      = -30.
      dPhi        = 1.0
      dTheta      = 1.0
CENDIGNOREENDIGNOREENDIGNOREENDIGNOREENDIGNOREENDIGNOREENDIGNOREENDIGNORE


      a2MomXY     = 4.D2
      a2MomZ      = 1.D-2*G*RONIL*G*RONIL
      a4MomXY     = 0.D0
      a2TempXY    = 4.D2
      a2TempZ     = 1.D-2*G*RONIL*G*RONIL
C     a2TempZ     = 1.D-5*G*RONIL*G*RONIL
      a4TempXy    = 0.D0


CSTARTIGNORESTARTIGNORESTARTIGNORESTARTIGNORESTARTIGNORESTARTIGNORESTARTIGNORE
      WRITE(20,*) ' delT',delT
      WRITE(20,*) ' startTime',startTime
      WRITE(20,*) ' endTime',endTime
      WRITE(20,*) ' dPUFreq',dPUFreq
      WRITE(20,*) ' a2MomXY',a2MomXY
      WRITE(20,*) ' a2MomZ',a2MomZ
      WRITE(20,*) ' a2TempXY',a2TempXY
      WRITE(20,*) ' a2TempZ',a2TempZ
C********* GMGS *******************
C     Ratio of time step
      asyncfac            = 1.0
C     Scale dependent mixing scheme parameters
      difref               =        a2tempXY
      difmin               = 0.1  * a2tempXY
      difhmx               = 2.5  * a2tempXY
      coef                 = 1.8E9
      dbotm                = 1000.
C     NOTE:  MOD( INT(dslpcalcfreq), INT(dkcalcfreq) ) must = 0
      dslpcalcfreq =  2. * delT
      dkcalcfreq   = 12. * delT
C     Threashold for mix layer base
      MixLayrDensGrad = 0.05
C     Slope factors
      slpmax1=5.e-3
      slpmax2=1.e-2
      slpmax3=1.e-3
      small=1.e-30
      dthin=50.
C     Minimum density stratification
      MinSigStrat= -1.e-8
c     epsl = Av/Ah in Redis scheme
      epsl = 0.
      trDifScheme = gmconKh   
      verticalProfKh = .FALSE.
  
      divHmax       = 1.D+300
      divHMin       = 0.
      rDelt         = 1./DELT
      numberOfTimeSteps = NINT((endTime-startTime)/delT)
      WRITE(20,*) ' numberOfTimeSteps',numberOfTimeSteps
      currentTime   = startTime
      nIter         = NINT(startTime/delt)
      WRITE(20,*) ' nIter',nIter
      WRITE(puSuffix,'(I10.10)') nIter
      PickupRun           = .FALSE.
      IF ( startTime .NE. 0. ) pickupRun = .TRUE.
C     State variables
      gsNM1         = 0.
      gtNM1         = 0.
      guNM1         = 0.
      gvNM1         = 0.
      U             = 0.
      V             = 0.
      W             = 0.
      S             = 0.
      PH            = 0.
      PS            = 0.
      PSNM1         = 0.
      PHNM1         = 0.
      uAja          = 0.
      vAja          = 0.
CENDIGNOREENDIGNOREENDIGNOREENDIGNOREENDIGNOREENDIGNOREENDIGNOREENDIGNORE


      THS(1)   = 20.D0
      THS(2)   = 10.D0
      THS(3)   =  8.D0
      THS(4)   =  6.D0
      SSPPT = 10.D0
      T(_I3(1,:,:)) = 20.0D0
      T(_I3(2,:,:)) = 10.0D0
      T(_I3(3,:,:)) =  8.0D0
      T(_I3(4,:,:)) =  6.0D0
C     T(_I3(1,:,:)) = 1.D-12
C     T(_I3(2,:,:)) = 0.
C     T(_I3(3,:,:)) = 0.
C     T(_I3(4,:,:)) = 0.
C     T(_I3(1,:,:)) = 23.0D0
C     T(_I3(2,:,:)) = 22.0D0
C     T(_I3(3,:,:)) = 21.0D0
C     T(_I3(4,:,:)) = 20.0D0
      S = 10.0D0    
      DELTAZ(1) =  500.0D0
      DELTAZ(2) =  500.0D0
      DELTAZ(3) =  500.0D0
      DELTAZ(4) =  500.0D0

C     Geometry
      DM        = 20. D3
      DM        = 1.
      WRITE(20,*) ' DM',DM


CBEGINIGNOREBEGINIGNOREBEGINIGNOREBEGINIGNOREBEGINIGNOREBEGINIGNOREBEGINIGNORE
      rPvolHat  = 0.
      DO K = 1, nz
       DELPS(K)           = G*RONIL*DELTAZ(K)
       XA(_I3(K,:,:))     = DELPS(K)*DM
       YA(_I3(K,:,:))     = DELPS(K)*DM
       rUVol(_I3(K,:,:))  = 1.D0/DM/DM/DELPS(K)
       rVVol(_I3(K,:,:))  = 1.D0/DM/DM/DELPS(K)
       rPVol(_I3(K,:,:))  = 1.D0/DM/DM/DELPS(K)
       rPvolHat           = rPvolHat+DM*DM*DELPS(K)
       IF ( K .EQ. 1 ) THEN
        uDdzOp(_I3(K,:,:)) = 1.D0/DELPS(K)
        vDdzOp(_I3(K,:,:)) = 1.D0/DELPS(K)
        pDdzOp(_I3(K,:,:)) = 1.D0/DELPS(K)
       ELSE
        uDdzOp(_I3(K,:,:)) = 2.D0/(DELPS(K)+DELPS(K-1))
        vDdzOp(_I3(K,:,:)) = 2.D0/(DELPS(K)+DELPS(K-1))
        pDdzOp(_I3(K,:,:)) = 2.D0/(DELPS(K)+DELPS(K-1))
       ENDIF
! begin *** Redi Operators
       rDZatP(K)=1.D0/DELPS(K)
       if (K.eq.1) then
        rDZatW(K)=1.D0/(DELPS(K))
       else
        rDZatW(K)=2.D0/(DELPS(K)+DELPS(K-1))
       endif
! end ***** Redi Operators
      ENDDO
! begin *** Redi Operators
      rDXatU=1.d0/DM
      rDYatV=1.d0/DM
! end ***** Redi Operators

      ZA        = DM*DM
      rPVolHat  = 1.D0/rPvolHat
      uDdxOp    = 1.D0/DM
      uDdyOp    = 1.D0/DM
      vDdxOp    = uDdxOp
      vDdyOp    = uDdyOp
      pDdxOp    = uDdxOp
      pDdyOp    = uDdyOp
C     Physical parameters
      uTphiOp   = 0.
      vTphiOp   = 0.
      fCorW     = 0.
      pBeta     = 1.D-11
CENDIGNOREENDIGNOREENDIGNOREENDIGNOREENDIGNOREENDIGNOREENDIGNOREENDIGNORE


C radial BETA code begins - to use comment out 'NOBETA CODE' following
      XCENTER = (Nx + 1)/2
      WRITE(0,*) ' XCENTER: ', XCENTER
      YCENTER = (Ny + 1)/2
      WRITE(0,*) ' YCENTER: ', YCENTER
      RMAX = 54.0
      WRITE(20,*) ' RMAX: ',RMAX
      WRITE(20,*) ' actual radius (m): ',(RMAX*DM)
      fRef = 4.0
      WRITE(20,*) ' fRef: ',fRef
      fWall = 4.0
      WRITE(20,*) ' fWall: ',fWall
      Beta = (fWall - fRef)/(RMAX)
      WRITE(20,*) ' Beta(s-1/gridpoint): ',Beta
      WRITE(20,*) ' Beta(s-1/m): ',Beta/DM



C start NOBETACODE 

      fCorV(_I3(:,:,1)) = 1.D-4
Cd2
C     pBeta  = 0.
Cd2
      write(20,*) 'NO RADIAL BETA'
 
      fCorU(_I3(:,:,1)) = fCorV(_I3(:,:,1))+0.5*DM*pBeta
      DO J = 2, ny
       fCorV(_I3(:,:,J)) = fCorV(_I3(:,:,J-1))+DM*pBeta
       fCorU(_I3(:,:,J)) = fCorU(_I3(:,:,J-1))+DM*pBeta
      ENDDO

C end NOBETACODE 


CBEGINIGNOREBEGINIGNOREBEGINIGNOREBEGINIGNOREBEGINIGNOREBEGINIGNOREBEGINIGNORE
      sBeta     = 0.
      WRITE(20,*) ' sBeta',sBeta
       tAlpha     = 2.D-4
C      tAlpha     = 0.D-4
      WRITE(20,*) ' tAlpha',tAlpha


C     Numerics parameters
      abEps     = 0.1
      rAja      = 9.930555555555556D-1
C     rAja      = 0.
      dUnit               = 10
      scrUnit1            = 11
      scrUnit2            = 12
      scrUnitError        = 13
      InitialisationError = .FALSE.
      IntegrationError    = .FALSE.

C     Topography
      PMASK     = WATER
C     Enclose in a box
C     Wall at X = Lx
      PMASK(_I3(:,nx,:))  = LAND  ! Land in east most cell
C     Wall at Y = Ly
      PMASK(_I3(:,:,ny))  = LAND  ! Land in northern most cell
C     bathySource = textBathymetry 
C     Island
      PMASK(_I3(Nz,1,24))  = LAND

C

      MAX2DIT       = 1000
      TOLER2D       = 1.D-13
      freqCheckToler2d = 1

C--   Momentum equation forcing terms
      tauMax = 0.1D0
      lY = 0.
      DO j=1,nY-1
       lY = lY + DM
      ENDDO
      distY = 0.
      DO j=1,nY
       distY = distY+DM*0.5D0
       DO i=1,nX
        fv(i,j) = 0.D0
        fu(i,j) = -tauMax*cos(2.D0*PI*distY/lY)
C       fu(i,j) = -tauMax
        fu(i,j) = tauMax*sin(PI*distY/lY)
        fu(i,j) = fu(i,j)/(deltaZ(1)*ronil)
       ENDDO
       distY = distY+DM*0.5D0
      ENDDO
      fu(4,4) = fu(4,4)*0.917D0

C
CcnhDebugStarts
Cdbg  WRITE(0,*) ' lY    = ', lY
Cdbg  WRITE(0,*) ' distY = ', distY
Cdbg  WRITE(0,*) ' tauMax = ', tauMax
      WRITE(0,*) ' fu '
      CALL PLOT_FIELD(fu,Nx,Ny)
Cdbg  STOP
CcnhDebugEnds
C
      END

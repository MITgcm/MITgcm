C $Id: set_defaults.F,v 1.5 1998/06/23 13:36:42 adcroft Exp $
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
      Real*4  tmpXY(Nx,Ny)
      Real*4 tmpXYZ( _I3(Nz,Nx,Ny) )
C
C     /-------------------------------------------------------------------------\
C     | Parameter settings for small double gyre beta-plane experiment.         |
C     \=========================================================================|
C
      G           = 9.81D0
      RONIL       = 999.8D0
      CP          = 3900.D0
CENDIGNOREENDIGNOREENDIGNOREENDIGNOREENDIGNOREENDIGNOREENDIGNOREENDIGNORE

       delT       = 2400.D0
      freeSurfFac = 1.D0
       startTime  = 0.
      endTime     = startTime+10.*delT
      dPUFreq     = 300.*delT


CSTARTIGNORESTARTIGNORESTARTIGNORESTARTIGNORESTARTIGNORESTARTIGNORESTARTIGNORE
      phiMin      = -80.
      dPhi        = 4.0
      dTheta      = 4.0
CENDIGNOREENDIGNOREENDIGNOREENDIGNOREENDIGNOREENDIGNOREENDIGNOREENDIGNORE


      a2MomXY     = 5.0D5
      a2MomZ      = 1.D-3*G*RONIL*G*RONIL
C     a2MomXY     = 0.0D5
C     a2MomZ      = 0.D-3*G*RONIL*G*RONIL
      a4MomXY     = 0.D0
      a2TempXY    = 1.D3
      a2TempZ     = 3.D-5*G*RONIL*G*RONIL
      a2SaltXY    = 1.D3
      a2SaltZ     = 3.D-5*G*RONIL*G*RONIL
C     a2SaltXY    = 0.D3
C     a2SaltZ     = 0.D-5*G*RONIL*G*RONIL
C     a2TempZ     = 1.D-5*G*RONIL*G*RONIL
      a4TempXy    = 0.D0
      a4SaltXy    = 0.D0


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
      asyncfac            = 30.0
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


      THS      =  0.D0
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
      THS( 1)       = 16.0D0
      THS( 2)       = 15.2D0
      THS( 3)       = 14.5D0
      THS( 4)       = 13.9D0
      THS( 5)       = 13.3D0
      THS( 6)       = 12.4D0
      THS( 7)       = 11.3D0
      THS( 8)       =  9.9D0
      THS( 9)       =  8.4D0
      THS(10)       =  6.7D0
      THS(11)       =  5.2D0
      THS(12)       =  3.8D0
      THS(13)       =  2.9D0
      THS(14)       =  2.3D0
      THS(15)       =  1.8D0
      THS(16)       =  1.5D0
      THS(17)       =  1.1D0
      THS(18)       =  0.8D0
      THS(19)       =  0.66D0
      THS(20)       =  0.63D0
      T(_I3( 1,:,:)) = THS( 1)
      T(_I3( 2,:,:)) = THS( 2)
      T(_I3( 3,:,:)) = THS( 3)
      T(_I3( 4,:,:)) = THS( 4)
      T(_I3( 5,:,:)) = THS( 5)
      T(_I3( 6,:,:)) = THS( 6)
      T(_I3( 7,:,:)) = THS( 7)
      T(_I3( 8,:,:)) = THS( 8)
      T(_I3( 9,:,:)) = THS( 9)
      T(_I3(10,:,:)) = THS(10)
      T(_I3(11,:,:)) = THS(11)
      T(_I3(12,:,:)) = THS(12)
      T(_I3(13,:,:)) = THS(13)
      T(_I3(14,:,:)) = THS(14)
      T(_I3(15,:,:)) = THS(15)
      T(_I3(16,:,:)) = THS(16)
      T(_I3(17,:,:)) = THS(17)
      T(_I3(18,:,:)) = THS(18)
      T(_I3(19,:,:)) = THS(19)
      T(_I3(20,:,:)) = THS(20)

      SSPPT( 1)      = 34.65
      SSPPT( 2)      = 34.75
      SSPPT( 3)      = 34.82
      SSPPT( 4)      = 34.87
      SSPPT( 5)      = 34.90
      SSPPT( 6)      = 34.90
      SSPPT( 7)      = 34.86
      SSPPT( 8)      = 34.78
      SSPPT( 9)      = 34.69
      SSPPT(10)      = 34.60
      SSPPT(11)      = 34.58
      SSPPT(12)      = 34.62
      SSPPT(13)      = 34.68
      SSPPT(14)      = 34.72
      SSPPT(15)      = 34.73
      SSPPT(16)      = 34.74
      SSPPT(17)      = 34.73
      SSPPT(18)      = 34.73
      SSPPT(19)      = 34.72
      SSPPT(20)      = 34.72
      S(_I3( 1,:,:)) = SSPPT( 1)
      S(_I3( 2,:,:)) = SSPPT( 2)
      S(_I3( 3,:,:)) = SSPPT( 3)
      S(_I3( 4,:,:)) = SSPPT( 4)
      S(_I3( 5,:,:)) = SSPPT( 5)
      S(_I3( 6,:,:)) = SSPPT( 6)
      S(_I3( 7,:,:)) = SSPPT( 7)
      S(_I3( 8,:,:)) = SSPPT( 8)
      S(_I3( 9,:,:)) = SSPPT( 9)
      S(_I3(10,:,:)) = SSPPT(10)
      S(_I3(11,:,:)) = SSPPT(11)
      S(_I3(12,:,:)) = SSPPT(12)
      S(_I3(13,:,:)) = SSPPT(13)
      S(_I3(14,:,:)) = SSPPT(14)
      S(_I3(15,:,:)) = SSPPT(15)
      S(_I3(16,:,:)) = SSPPT(16)
      S(_I3(17,:,:)) = SSPPT(17)
      S(_I3(18,:,:)) = SSPPT(18)
      S(_I3(19,:,:)) = SSPPT(19)
      S(_I3(20,:,:)) = SSPPT(20)
      DELTAZ( 1) =  50.D0
      DELTAZ( 2) =  50.D0
      DELTAZ( 3) =  55.D0
      DELTAZ( 4) =  60.D0
      DELTAZ( 5) =  65.D0
      DELTAZ( 6) =  70.D0
      DELTAZ( 7) =  80.D0
      DELTAZ( 8) =  95.D0
      DELTAZ( 9) = 120.D0
      DELTAZ(10) = 155.D0
      DELTAZ(11) = 200.D0
      DELTAZ(12) = 260.D0
      DELTAZ(13) = 320.D0
      DELTAZ(14) = 400.D0
      DELTAZ(15) = 480.D0
      DELTAZ(16) = 570.D0
      DELTAZ(17) = 655.D0
      DELTAZ(18) = 725.D0
      DELTAZ(19) = 775.D0
      DELTAZ(20) = 815.D0

C     Geometry
      DM        = 20. D3
      DM        = 4.
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
      sBeta     = 7.4E-4
      WRITE(20,*) ' sBeta',sBeta
       tAlpha     = 2.D-4
C      tAlpha     = 0.D-4
      WRITE(20,*) ' tAlpha',tAlpha


C     Numerics parameters
      abEps     = 0.1
      rAja      = 9.925333200592357E-01
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
      bathySource = binaryBathymetry 
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

      OPEN ( 11, FILE='windx', STATUS='old' , FORM='unformatted')
      READ (11) tmpXY
C     Fu = tmpXY/DELPS(1)*G*0.1*uMask(_I3(1,:,:))           ! Dynes/cm**2 -> m/s**2
      Fu = tmpXY/DELPS(1)*G
C     Fu = 0.1/DELPS(1)*G
      CLOSE(11)

      OPEN ( 11, FILE='windy', STATUS='old' , FORM='unformatted')
      READ (11) tmpXY
C     Fu = tmpXY/DELPS(1)*G*0.1*uMask(_I3(1,:,:))           ! Dynes/cm**2 -> m/s**2
      Fv = tmpXY/DELPS(1)*G
      CLOSE(11)

C
CcnhDebugStarts
Cdbg  WRITE(0,*) ' lY    = ', lY
Cdbg  WRITE(0,*) ' distY = ', distY
Cdbg  WRITE(0,*) ' tauMax = ', tauMax
      WRITE(0,*) ' fu '
      CALL PLOT_FIELD(fu,Nx,Ny)
      WRITE(0,*) ' fv '
      CALL PLOT_FIELD(fv,Nx,Ny)
C     STOP
CcnhDebugEnds

C     T and S profiles
      OPEN ( 11, FILE='LevCli.ptmp.sun.b', STATUS='old' , FORM='unformatted')
      READ (11) tmpXYZ
      Heat = tmpXYZ(_I3(1,:,:))
      CLOSE(11)
C     T and S profiles
      OPEN ( 11, FILE='LevCli.salt.sun.b', STATUS='old' , FORM='unformatted')
      READ (11) tmpXYZ
      SSurf = tmpXYZ(_I3(1,:,:))
      CLOSE(11)
C
      END

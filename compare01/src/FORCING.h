C $Id: FORCING.h,v 1.1 1998/05/25 20:21:06 cnh Exp $
C/-------------------------------------------------------------------\
C|||  External forcing data.                                       |||
C|||===============================================================|||
C|||  In multi-threading mode the I/O associated with loading      |||
C|||  datasets etc... shuld be done outside the parallel threads.  |||
C|||  Using global data structures as illustrated here is a        |||
C|||  practical way to do this.                                    |||
C\-------------------------------------------------------------------/
C     /--------------------------------------------------------------\
C     | FORCING_F                                                    |
C     |==============================================================|
C     | Floating point fields for forcing.                           |
C     | Udsk    - Zonal velocity of disk.                            |
C     | Vdsk    - Meridional velocity of disk.                       |
C     | lambdaVel - Disk to flow relaxation time scale.              |
C     |             Set to zero outside region of stress.            |
C     | Heat  - Surface temperature rate of change.                  |
C     | Ssurf - Surface salinity field ( S is a passive dye ).       |
C     \--------------------------------------------------------------/
      COMMON /FORCING_F/ Udsk, Vdsk, Heat, Ssurf, lambdaVel,
     &                   fu, fv
      REAL Udsk(Nx,Ny)
      REAL Vdsk(Nx,Ny)
      REAL lambdaVel(Nx,Ny)
      REAL Heat (Nx,Ny)
      REAL Ssurf(Nx,Ny)
      REAL fu(Nx,Ny)
      REAL fv(Nx,Ny)

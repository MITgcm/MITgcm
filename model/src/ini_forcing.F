C $Header: /u/gcmpack/MITgcm/model/src/ini_forcing.F,v 1.3 1998/05/20 19:59:13 adcroft Exp $

#include "CPP_EEOPTIONS.h"

CStartOfInterface
      SUBROUTINE INI_FORCING( myThid )
C     /==========================================================\
C     | SUBROUTINE INI_FORCING                                   |
C     | o Set model initial forcing fields.                      |
C     \==========================================================/

C     === Global variables ===
#include "SIZE.h"
#include "EEPARAMS.h"
#include "PARAMS.h"
#include "GRID.h"
#include "FFIELDS.h"

C     == Routine arguments ==
C     myThid -  Number of this instance of INI_FORCING
      INTEGER myThid
CEndOfInterface

C     == Local variables ==
C     iG, jG - Global coordinate index
C     bi,bj  - Loop counters
C     I,J,K
C     distY  - Distance accross domain of point j
C     lY     - Width of the basin ( last point is land )
C     tauMax - Peak zonal wind stress ( N/m^2 )
C     tauX   - Local zonal wind stress
      INTEGER iG, jG
      INTEGER bi, bj
      INTEGER  I,  J, K
      _RL distY
      _RL lY
      _RL tauX, tauMax

C--   Initialise surface bc arrays
C     In cartesian yc, delY and ly are meters.
C     In spherical polar yc, delY and ly are degrees
      tauMax = 0.1 _d 0
      lY = 0. _d 0
      DO j=1,nY-1
       lY = lY + delY(j)
      ENDDO
      DO bj = myByLo(myThid), myByHi(myThid)
       DO bi = myBxLo(myThid), myBxHi(myThid)
        DO j=1-OLy,sNy+OLy
         DO i=1-OLx,sNx+OLx
          fu(i,j,bi,bj) = 0. _d 0
          fv(i,j,bi,bj) = 0. _d 0
         ENDDO
        ENDDO
       ENDDO
      ENDDO
      DO bj = myByLo(myThid), myByHi(myThid)
       DO bi = myBxLo(myThid), myBxHi(myThid)
        DO j=1,sNy
         DO i=1,sNx
          distY = yC(i,j,bi,bj)/lY
          tauX  = -tauMax*cos(2. _d 0*PI*distY)
c         tauX  = tauMax*sin(PI*distY)
          fu(i,j,bi,bj) = tauX/(delZ(1)*rhonil)
C         fu(i,j,bi,bj) = -tauMax
         ENDDO
        ENDDO
       ENDDO
      ENDDO
      fu(Nx/2,Ny/2,1,1) = fu(Nx/2,Ny/2,1,1)*0.917d0
C
      _EXCH_XY_R4(fu  , myThid )
      _EXCH_XY_R4(fv  , myThid )

CcnhDebugStarts
Cdbg  WRITE(0,*) ' distY = ', distY
Cdbg  WRITE(0,*) '    ly = ', lY
Cdbg  WRITE(0,*) ' tauMax= ', tauMax
Cdbg  CALL PLOT_FIELD_XYR8( fu, 'INI_FORCING FU',1,myThid)
Cdbg  STOP 'INI_FORCING'
CcnhDebugEnds

      RETURN
      END

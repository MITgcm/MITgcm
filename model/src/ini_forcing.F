C $Header: /u/gcmpack/MITgcm/model/src/ini_forcing.F,v 1.11 1998/06/22 15:26:25 adcroft Exp $

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

      _BARRIER

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

C--   Initialise surface bc arrays
      IF ( zonalWindFile .EQ. ' ' ) THEN
C      In cartesian yc, delY and ly are meters.
C      In spherical polar yc, delY and ly are degrees
       tauMax = 0.1 _d 0
       lY = 0. _d 0
       DO j=1,nY-1
        lY = lY + delY(j)
       ENDDO
       DO bj = myByLo(myThid), myByHi(myThid)
        DO bi = myBxLo(myThid), myBxHi(myThid)
         DO j=1,sNy
          DO i=1,sNx
           distY = (yC(i,j,bi,bj)-yC0)/lY
C          tauX  = -tauMax*cos(2. _d 0*PI*distY)
           tauX  = tauMax*sin(PI*distY)
           fu(i,j,bi,bj) = tauX/(delZ(1)*rhonil)
          ENDDO
         ENDDO
        ENDDO
       ENDDO
C      fu(4,4,1,1) = fu(4,4,1,1)*0.917d0
      ENDIF
C
      _BARRIER

      IF ( meridWindFile .EQ. ' ' ) THEN
       DO bj = myByLo(myThid), myByHi(myThid)
        DO bi = myBxLo(myThid), myBxHi(myThid)
         DO j=1-OLy,sNy+OLy
          DO i=1-OLx,sNx+OLx
           fv(i,j,bi,bj) = 0./(delZ(1)*rhonil)
          ENDDO
         ENDDO
        ENDDO
       ENDDO
      ENDIF
C
      _EXCH_XY_R4(fu  , myThid )
      _EXCH_XY_R4(fv  , myThid )

      CALL PLOT_FIELD_XYRS( fu, 'S/R INI_FORCING FU',1,myThid)
      CALL PLOT_FIELD_XYRS( fv, 'S/R INI_FORCING FV',1,myThid)

      RETURN
      END

C $Header: /u/gcmpack/MITgcm/model/src/ini_forcing.F,v 1.8 1998/06/12 19:33:33 cnh Exp $

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
           distY = (yC(i,j,bi,bj)-yC0)/lY
           tauX  = -tauMax*cos(2. _d 0*PI*distY)
           tauX  = tauMax*sin(PI*distY)
           fu(i,j,bi,bj) = tauX/(delZ(1)*rhonil)
C          fu(i,j,bi,bj) = -tauMax
          ENDDO
         ENDDO
        ENDDO
       ENDDO
       fu(4,4,1,1) = fu(4,4,1,1)*0.917d0
      ELSE
       _BEGIN_MASTER(myThid)
       CALL READ_FLD_XY_RS( zonalWindFile, ' ', fu, 0, myThid )
       DO bj = myByLo(myThid), myByHi(myThid)
        DO bi = myBxLo(myThid), myBxHi(myThid)
         DO j=1,sNy
          DO i=1,sNx
           fu(i,j,bi,bj) = fu(i,j,bi,bj)/(delZ(1)*rhonil)*0.1
          ENDDO
         ENDDO
        ENDDO
       ENDDO
       _END_MASTER(myThid)
      ENDIF
C
      _BARRIER

      IF ( meridWindFile .EQ. ' ' ) THEN
       DO bj = myByLo(myThid), myByHi(myThid)
        DO bi = myBxLo(myThid), myBxHi(myThid)
         DO j=1-OLy,sNy+OLy
          DO i=1-OLx,sNx+OLx
           fv(i,j,bi,bj) = 0. _d 0
          ENDDO
         ENDDO
        ENDDO
       ENDDO
      ELSE
       _BEGIN_MASTER(myThid)
       CALL READ_FLD_XY_RS( meridWindFile, ' ', fv, 0, myThid )
       DO bj = myByLo(myThid), myByHi(myThid)
        DO bi = myBxLo(myThid), myBxHi(myThid)
         DO j=1,sNy
          DO i=1,sNx
           fv(i,j,bi,bj) = fv(i,j,bi,bj)/(delZ(1)*rhonil)*0.1
          ENDDO
         ENDDO
        ENDDO
       ENDDO
       _END_MASTER(myThid)
      ENDIF
C
      _EXCH_XY_R4(fu  , myThid )
      _EXCH_XY_R4(fv  , myThid )

CcnhDebugStarts
Cdbg  WRITE(0,*) ' distY = ', distY
Cdbg  WRITE(0,*) '    ly = ', lY
Cdbg  WRITE(0,*) ' tauMax= ', tauMax
      CALL PLOT_FIELD_XYR8( fu, 'INI_FORCING FU',1,myThid)
      CALL PLOT_FIELD_XYR8( fv, 'INI_FORCING FV',1,myThid)
Cdbg  STOP 'INI_FORCING'
CcnhDebugEnds

      RETURN
      END

C $Header: /u/gcmpack/MITgcm/model/src/ini_forcing.F,v 1.15 1998/12/15 00:20:34 adcroft Exp $

#include "CPP_OPTIONS.h"

CStartOfInterface
      SUBROUTINE INI_FORCING( myThid )
C     /==========================================================\
C     | SUBROUTINE INI_FORCING                                   |
C     | o Set model initial forcing fields.                      |
C     \==========================================================/
      IMPLICIT NONE

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
      INTEGER iG, jG
      INTEGER bi, bj
      INTEGER  I,  J, K

      _BARRIER

      DO bj = myByLo(myThid), myByHi(myThid)
       DO bi = myBxLo(myThid), myBxHi(myThid)
        DO j=1-OLy,sNy+OLy
         DO i=1-OLx,sNx+OLx
          fu   (i,j,bi,bj) = 0. _d 0
          fv   (i,j,bi,bj) = 0. _d 0
          Qnet (i,j,bi,bj) = 0. _d 0
          EmPmR(i,j,bi,bj) = 0. _d 0
          SST  (i,j,bi,bj) = 0. _d 0
          SSS  (i,j,bi,bj) = 0. _d 0
         ENDDO
        ENDDO
       ENDDO
      ENDDO
C
      _EXCH_XY_R4(fu   , myThid )
      _EXCH_XY_R4(fv   , myThid )
      _EXCH_XY_R4(Qnet , myThid )
      _EXCH_XY_R4(EmPmR, myThid )
      _EXCH_XY_R4(SST  , myThid )
      _EXCH_XY_R4(SSS  , myThid )

C     CALL PLOT_FIELD_XYRS( fu, 'S/R INI_FORCING FU',1,myThid)
C     CALL PLOT_FIELD_XYRS( fv, 'S/R INI_FORCING FV',1,myThid)

      DO bj = myByLo(myThid), myByHi(myThid)
       DO bi = myBxLo(myThid), myBxHi(myThid)
        DO j=1-OLy,sNy+OLy
         DO i=1-OLx,sNx+OLx
          fudot   (i,j,bi,bj) = 0. _d 0
          fvdot   (i,j,bi,bj) = 0. _d 0
          Qnetdot (i,j,bi,bj) = 0. _d 0
          EmPmRdot(i,j,bi,bj) = 0. _d 0
          SSTdot  (i,j,bi,bj) = 0. _d 0
          SSSdot  (i,j,bi,bj) = 0. _d 0
         ENDDO
        ENDDO
       ENDDO
      ENDDO
C
      _EXCH_XY_R4(fudot   , myThid )
      _EXCH_XY_R4(fvdot   , myThid )
      _EXCH_XY_R4(Qnetdot , myThid )
      _EXCH_XY_R4(EmPmRdot, myThid )
      _EXCH_XY_R4(SSTdot  , myThid )
      _EXCH_XY_R4(SSSdot  , myThid )

      RETURN
      END

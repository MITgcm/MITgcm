C $Header:

#include "SEAICE_OPTIONS.h"
 
CStartOfInterface
      SUBROUTINE SEAICE_EXCH ( fld, myThid )
C     /==========================================================\
C     | SUBROUTINE SEAICE_EXCH                                   |
C     | o RL exchange routine with k=3                           |
C     |==========================================================|
C     \==========================================================/
      IMPLICIT NONE

C     == Global variables ===
#include "SIZE.h"
#include "EEPARAMS.h"

C     == Routine arguments ==
C     myThid - Thread number for this instance of the routine.
      _RL fld(1-OLx:sNx+OLx,1-OLy:sNy+OLy,3,nSx,nSy)
      INTEGER myThid
CEndofinterface

#ifdef ALLOW_SEAICE

C     == Local variables ==
      INTEGER i,j,k,bi,bj
      _RL tempVar(1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)

C--   Update overlap regions 
      DO k=1,3
         DO bj=myByLo(myThid),myByHi(myThid)
            DO bi=myBxLo(myThid),myBxHi(myThid)
               DO j=1-Oly,sNy+OLy
                  DO i=1-OLx,sNx+OLx
                     tempVar(i,j,bi,bj)=fld(i,j,k,bi,bj)
                  ENDDO
               ENDDO
            ENDDO
         ENDDO
         _EXCH_XY_R8(tempVar , myThid )
         DO bj=myByLo(myThid),myByHi(myThid)
            DO bi=myBxLo(myThid),myBxHi(myThid)
               DO j=1-Oly,sNy+OLy
                  DO i=1-OLx,sNx+OLx
                     fld(i,j,k,bi,bj)=tempVar(i,j,bi,bj)
                  ENDDO
               ENDDO
            ENDDO
         ENDDO
      ENDDO

#endif /* ALLOW_SEAICE */

      RETURN
      END

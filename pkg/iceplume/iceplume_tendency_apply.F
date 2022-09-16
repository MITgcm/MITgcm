
#include "ICEPLUME_OPTIONS.h"

C---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|
CBOP 0
C !ROUTINE: ICEPLUME_TENDENCY_APPLY_T

C !INTERFACE:
      SUBROUTINE ICEPLUME_TENDENCY_APPLY_T(
     U                    gT_arr,
     I                    k, bi, bj, myTime, myIter, myThid )

C     !DESCRIPTION:
C     As for S/R ICEFRONT_TENDENCY_APPLY_T
C     Add icefront tendency terms to T (theta) tendency.
C     Routine works for one level at a time.

C     !USES:
      IMPLICIT NONE
#include "SIZE.h"
#include "GRID.h"
#include "EEPARAMS.h"
#include "DYNVARS.h"
#include "ICEPLUME.h"


C     !INPUT/OUTPUT PARAMETERS:
C     gT_arr    :: the tendency array
C     k         :: Current vertical level index
C     bi,bj     :: Current tile indices
C     myTime    :: Current time in simulation
C     myIter    :: Current iteration number
C     myThid    :: my Thread Id number
      _RL     gT_arr(1-OLx:sNx+OLx,1-OLy:sNy+OLy)
      INTEGER k, bi, bj
      _RL     myTime
      INTEGER myIter
      INTEGER myThid

C     !LOCAL VARIABLES:
      INTEGER i, j

#ifdef ALLOW_ICEPLUME

      IF ( applyIcefrontTendT ) THEN
       DO j=1,sNy
        DO i=1,sNx
          gT_arr(i,j) = gT_arr(i,j)
     &         + ICEFRONT_TendT(i,j,k,bi,bj)
        ENDDO
       ENDDO
       ENDIF

#endif /* ALLOW_ICEPLUME */

      RETURN
      END

C---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|
CBOP 0
C !ROUTINE: ICEPLUME_TENDENCY_APPLY_S

C !INTERFACE:
      SUBROUTINE ICEPLUME_TENDENCY_APPLY_S(
     U                    gS_arr,
     I                    k, bi, bj, myTime, myIter, myThid )

C     !DESCRIPTION:
C     As for S/R ICEFRONT_TENDENCY_APPLY_S
C     Add icefront tendency terms to S tendency.
C     Routine works for one level at a time.

C     !INPUT PARAMETERS:
      IMPLICIT NONE
#include "SIZE.h"
#include "GRID.h"
#include "EEPARAMS.h"
#include "DYNVARS.h"
#include "ICEPLUME.h"

C     !INPUT/OUTPUT PARAMETERS:
C     gS_arr    :: the tendency array
C     k         :: Current vertical level index
C     bi,bj     :: Current tile indices
C     myTime    :: Current time in simulation
C     myIter    :: Current iteration number
C     myThid    :: my Thread Id number
      _RL     gS_arr(1-OLx:sNx+OLx,1-OLy:sNy+OLy)
      INTEGER k, bi, bj
      _RL     myTime
      INTEGER myIter
      INTEGER myThid
CEOP

C     !LOCAL VARIABLES:
      INTEGER i, j

#ifdef ALLOW_ICEPLUME

      IF ( applyIcefrontTendS ) THEN
       DO j=1,sNy
        DO i=1,sNx
          gS_arr(i,j) = gS_arr(i,j)
     &         + ICEFRONT_TendS(i,j,k,bi,bj)
        ENDDO
       ENDDO
      ENDIF

#endif /* ALLOW_ICEPLUME */

      RETURN
      END

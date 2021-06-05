#include "CPP_OPTIONS.h"

CBOP
C     !ROUTINE: INI_NH_VARS
C     !INTERFACE:
      SUBROUTINE INI_NH_VARS ( myThid )
C     !DESCRIPTION: \bv
C     *==========================================================*
C     | SUBROUTINE INI_NH_VARS
C     | o Initialise to zero all NH_VARS.h arrays
C     *==========================================================*
C     | Sets all the NH State variables & tendencies to zero.
C     *==========================================================*
C     \ev

C     !USES:
      IMPLICIT NONE
C     === Global variables ===
#include "SIZE.h"
#include "EEPARAMS.h"
#include "PARAMS.h"
#include "NH_VARS.h"

C     !INPUT/OUTPUT PARAMETERS:
C     == Routine arguments ==
C     myThid  ::  My Thread Id number
      INTEGER myThid
CEOP

#if (defined ALLOW_NONHYDROSTATIC || defined ALLOW_QHYD_STAGGER_TS )
C     !LOCAL VARIABLES:
C     == Local variables ==
C     bi,bj   :: tile indices
C     i,j,k   :: loop counters
      INTEGER bi, bj
      INTEGER i, j, k
#endif /* ALLOW_NONHYDROSTATIC or ALLOW_QHYD_STAGGER_TS */

#ifdef ALLOW_NONHYDROSTATIC
c     IF ( nonHydrostatic ) THEN
C- Note: comment out this IF so that we are sure that all variables in
C        common blocs (even if never used later) are always initialised
        DO bj = myByLo(myThid), myByHi(myThid)
         DO bi = myBxLo(myThid), myBxHi(myThid)
          DO j=1-OLy,sNy+OLy
            DO i=1-OLx,sNx+OLx
              dPhiNH(i,j,bi,bj)    = 0. _d 0
            ENDDO
          ENDDO
          DO k=1,Nr
           DO j=1-OLy,sNy+OLy
            DO i=1-OLx,sNx+OLx
              phi_nh(i,j,k,bi,bj)  = 0. _d 0
              gW   (i,j,k,bi,bj)   = 0. _d 0
#ifdef ALLOW_ADAMSBASHFORTH_3
              gWnm (i,j,k,bi,bj,1) = 0. _d 0
              gWnm (i,j,k,bi,bj,2) = 0. _d 0
#else /* ALLOW_ADAMSBASHFORTH_3 */
              gWnm1(i,j,k,bi,bj)   = 0. _d 0
#endif /* ALLOW_ADAMSBASHFORTH_3 */
            ENDDO
           ENDDO
          ENDDO
         ENDDO
        ENDDO
c     ENDIF
#endif /* ALLOW_NONHYDROSTATIC */

#ifdef ALLOW_QHYD_STAGGER_TS
c     IF ( staggerTimeStep ) THEN
        DO bj = myByLo(myThid), myByHi(myThid)
         DO bi = myBxLo(myThid), myBxHi(myThid)
          DO k=1,Nr
           DO j=1-OLy,sNy+OLy
            DO i=1-OLx,sNx+OLx
#ifdef ALLOW_ADAMSBASHFORTH_3
              QHydGwNm(i,j,k,bi,bj,1) = 0. _d 0
              QHydGwNm(i,j,k,bi,bj,2) = 0. _d 0
#else /* ALLOW_ADAMSBASHFORTH_3 */
              QHydGwNm(i,j,k,bi,bj)   = 0. _d 0
#endif /* ALLOW_ADAMSBASHFORTH_3 */
            ENDDO
           ENDDO
          ENDDO
         ENDDO
        ENDDO
c     ENDIF
#endif /* ALLOW_QHYD_STAGGER_TS */

      RETURN
      END

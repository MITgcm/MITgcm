#include "BULK_FORCE_OPTIONS.h"

CBOP
C     !ROUTINE: BULKF_INIT_VARIA
C     !INTERFACE:
      SUBROUTINE BULKF_INIT_VARIA( myThid )

C     *==========================================================*
C     | SUBROUTINE BULKF_INIT_VARIA
C     | o Initialize bulk formula arrays
C     *==========================================================*
C     \ev

C     !USES:
      IMPLICIT NONE
C     === Global variables ===
#include "SIZE.h"
#include "EEPARAMS.h"
#include "BULKF_PARAMS.h"
#include "BULKF.h"
#ifdef CONSERV_BULKF
#include "BULKF_CONSERV.h"
#endif

C     !INPUT/OUTPUT PARAMETERS:
C     myThid ::  Number of this instance of BULKF_INIT_VARIA
      INTEGER myThid
CEOP

#ifdef ALLOW_BULK_FORCE
C     == Local variables ==
C     bi,bj  :: tile indices
C     i,j    :: Loop counters
      INTEGER bi, bj
      INTEGER i, j

C     set up bulk formula arrays to zero
      DO bj = myByLo(myThid), myByHi(myThid)
       DO bi = myBxLo(myThid), myBxHi(myThid)
         DO j=1-OLy,sNy+OLy
           DO i=1-OLx,sNx+OLx
             Tair  (i,j,bi,bj) = 0. _d 0
             Qair  (i,j,bi,bj) = 0. _d 0
             Solar (i,j,bi,bj) = 0. _d 0
             flwdwn(i,j,bi,bj) = 0. _d 0
             cloud (i,j,bi,bj) = 0. _d 0
             wspeed(i,j,bi,bj) = 0. _d 0
             uwind (i,j,bi,bj) = 0. _d 0
             vwind (i,j,bi,bj) = 0. _d 0
             runoff(i,j,bi,bj) = 0. _d 0
#ifdef ALLOW_FORMULA_AIM
             thAir (i,j,bi,bj) = 0. _d 0
#endif
             qnetch(i,j,bi,bj) = 0. _d 0
             empch (i,j,bi,bj) = 0. _d 0
           ENDDO
         ENDDO
#ifdef CONSERV_BULKF
         DO j=1-OLy,sNy+OLy
           DO i=1-OLx,sNx+OLx
              CONS_Qnet (i,j,bi,bj) = 0. _d 0
              CONS_EmPmR(i,j,bi,bj) = 0. _d 0
           ENDDO
         ENDDO
#endif
       ENDDO
      ENDDO

#ifdef CONSERV_BULKF
      constim = 0. _d 0
#endif

#endif /* ALLOW_BULK_FORCE */

      RETURN
      END

#include "SALT_PLUME_OPTIONS.h"

CBOP
C     !ROUTINE: SALT_PLUME_DO_EXCH
C     !INTERFACE:
      SUBROUTINE SALT_PLUME_DO_EXCH(
     I                           myTime, myIter, myThid )

C     !DESCRIPTION: \bv
C     *==========================================================*
C     | SUBROUTINE SALT_PLUME_DO_EXCH
C     | o Apply Exchanges to SALT_PLUME variables when necessary
C     *==========================================================*
C     | o If coupled, prior to EXCH call, also apply update
C     |   from coupling interface
C     *==========================================================*
C     \ev

C     !USES:
      IMPLICIT NONE

C     == Global variables ==
#include "SIZE.h"
#include "EEPARAMS.h"
#include "PARAMS.h"
#include "SALT_PLUME.h"
#ifdef ALLOW_OCN_COMPON_INTERF
# include "CPL_PARAMS.h"
# include "OCNCPL.h"
#endif /* ALLOW_OCN_COMPON_INTERF */

C     !INPUT/OUTPUT PARAMETERS:
C     myTime :: Current time in simulation
C     myIter :: Current iteration number in simulation
C     myThid :: my Thread Id. number
      _RL     myTime
      INTEGER myIter
      INTEGER myThid
CEOP

C     !LOCAL VARIABLES:
#ifdef ALLOW_OCN_COMPON_INTERF
      INTEGER bi, bj, i, j
#endif /* ALLOW_OCN_COMPON_INTERF */

#ifdef ALLOW_OCN_COMPON_INTERF
c     IF ( useSALT_PLUME .AND. useCoupler ) THEN
      IF ( useCoupler ) THEN
       IF ( useImportSltPlm ) THEN
C-    Take a fraction (SPsalFRAC) of the coupler-imported salt-flux
C      and apply it to salt-plume array (for vertical redistribution)
        DO bj = myByLo(myThid), myByHi(myThid)
         DO bi = myBxLo(myThid), myBxHi(myThid)
           DO j=1-OLy,sNy+OLy
            DO i=1-OLx,sNx+OLx
              saltPlumeFlux(i,j,bi,bj) = saltPlmFlx_cpl(i,j,bi,bj)
     &                                 * SPsalFRAC
            ENDDO
           ENDDO
         ENDDO
        ENDDO
       ENDIF
      ENDIF
#endif /* ALLOW_OCN_COMPON_INTERF */

      IF ( useSALT_PLUME ) THEN
       _EXCH_XY_RL( saltPlumeFlux, myThid )
      ENDIF

      RETURN
      END

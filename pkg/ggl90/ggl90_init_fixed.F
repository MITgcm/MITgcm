#include "GGL90_OPTIONS.h"

C---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|
CBOP 0
C !ROUTINE: GGL90_INIT_FIXED
C !INTERFACE:
      SUBROUTINE GGL90_INIT_FIXED( myThid )

C     !DESCRIPTION: \bv
C     *================================================================*
C     | S/R GGL90_INIT_FIXED
C     | Initialize GGL90 variables that are kept fixed during the run.
C     *================================================================*
C     \ev

C     !USES:
      IMPLICIT NONE
#include "EEPARAMS.h"
#include "SIZE.h"
#include "PARAMS.h"
#include "GGL90.h"

C     !INPUT/OUTPUT PARAMETERS:
C     myThid ::  my Thread Id number
      INTEGER myThid

C     !LOCAL VARIABLES:
C     === Local variables ===
#ifdef ALLOW_GGL90_SMOOTH
      INTEGER i, j, bi, bj
#endif
CEOP

C#ifdef ALLOW_MNC
C      IF (useMNC) THEN
C        CALL GGL90_MNC_INIT( myThid )
C      ENDIF
C#endif /* ALLOW_MNC */

#ifdef ALLOW_DIAGNOSTICS
      IF ( useDiagnostics ) THEN
        CALL GGL90_DIAGNOSTICS_INIT( myThid )
      ENDIF
#endif

#ifdef ALLOW_GGL90_SMOOTH
      DO bj=myByLo(myThid),myByHi(myThid)
       DO bi=myBxLo(myThid),myBxHi(myThid)
        DO j=1-OLy,sNy+OLy
         DO i=1-OLx,sNx+OLx
          mskCor(i,j,bi,bj) = 1. _d 0
         ENDDO
        ENDDO
C- for now, hide this call from OpenAD:
# if ( !(defined ALLOW_OPENAD) || (defined ALLOW_EXCH2) )
        IF ( useCubedSphereExchange ) THEN
          CALL FILL_CS_CORNER_TR_RL( 0, .FALSE.,
     U                               mskCor(1-OLx,1-OLy,bi,bj),
     I                               bi, bj, myThid )
        ENDIF
# endif
       ENDDO
      ENDDO
#endif /* ALLOW_GGL90_SMOOTH */

      RETURN
      END

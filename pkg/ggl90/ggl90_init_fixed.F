C $Header: /u/gcmpack/MITgcm/pkg/ggl90/ggl90_init_fixed.F,v 1.4 2012/08/30 23:08:02 jmc Exp $
C $Name:  $

#include "GGL90_OPTIONS.h"

C---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|
CBOP 0
C !ROUTINE: GGL90_INIT_FIXED

C !INTERFACE:
      SUBROUTINE GGL90_INIT_FIXED( myThid )

C     !DESCRIPTION:
C     Initialize GGL90 variables that are kept fixed during the run.

C     !USES:
      IMPLICIT NONE
#include "EEPARAMS.h"
#include "SIZE.h"
#include "PARAMS.h"
#include "GGL90.h"

C     !INPUT/OUTPUT PARAMETERS:
C     myThid ::  my Thread Id number
      INTEGER myThid
CEOP

C     !LOCAL VARIABLES:
C     === Local variables ===
#ifdef ALLOW_GGL90_SMOOTH
      INTEGER i, j, bi, bj
#endif

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
        IF ( useCubedSphereExchange ) THEN
          CALL FILL_CS_CORNER_TR_RL( 0, .FALSE.,
     U                               mskCor(1-OLx,1-OLy,bi,bj),
     I                               bi,bj, myThid )
        ENDIF
       ENDDO
      ENDDO
#endif

      RETURN
      END

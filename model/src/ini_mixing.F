#include "CPP_OPTIONS.h"

CBOP
C     !ROUTINE: INI_MIXING
C     !INTERFACE:
      SUBROUTINE INI_MIXING( myThid )
C     !DESCRIPTION: \bv
C     *==========================================================*
C     | SUBROUTINE INI_MIXING
C     | o Initialise diffusivity to default constant value.
C     *==========================================================*
C     \ev

C     !USES:
      IMPLICIT NONE
C     === Global variables ===
#include "SIZE.h"
#include "EEPARAMS.h"
#include "PARAMS.h"
#include "GRID.h"
#include "DYNVARS.h"

C     !INPUT/OUTPUT PARAMETERS:
C     myThid ::  Number of this instance
      INTEGER myThid

C     !LOCAL VARIABLES:
C     bi,bj  :: tile indices
C     i,j,k  :: Loop counters
      INTEGER bi, bj
      INTEGER i, j, k
CEOP

#ifdef ALLOW_3D_DIFFKR
       DO bj = myByLo(myThid), myByHi(myThid)
        DO bi = myBxLo(myThid), myBxHi(myThid)
         DO k=1,Nr
          DO j = 1-OLy, sNy+OLy
           DO i = 1-OLx, sNx+OLx
            diffKr(i,j,k,bi,bj) = diffKrNrS(k)
           ENDDO
          ENDDO
         ENDDO
        ENDDO
       ENDDO
       IF ( diffKrFile .NE. ' ' ) THEN
         CALL READ_FLD_XYZ_RL( diffKrFile, ' ', diffKr, 0, myThid )
         _EXCH_XYZ_RL( diffKr, myThid )
       ENDIF
#endif /* ALLOW_3D_DIFFKR */

#ifdef ALLOW_BL79_LAT_VARY
       DO bj = myByLo(myThid), myByHi(myThid)
        DO bi = myBxLo(myThid), myBxHi(myThid)
         DO j = 1-OLy, sNy+OLy
          DO i = 1-OLx, sNx+OLx
           BL79LatArray(i,j,bi,bj) = 0.0 _d 0
           IF ( ABS(YC(i,j,bi,bj)) .LT. BL79LatVary )
     &          BL79LatArray(i,j,bi,bj) =
     &          ( 1 + COS(YC(i,j,bi,bj)*pi/BL79LatVary) ) / 2
          ENDDO
         ENDDO
        ENDDO
       ENDDO
#endif

      RETURN
      END

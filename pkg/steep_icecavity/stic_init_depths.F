#include "STIC_OPTIONS.h"

CBOP
C     !ROUTINE: STIC_INIT_DEPTHS
C     !INTERFACE:
      SUBROUTINE STIC_INIT_DEPTHS(
     I                    myThid )
C     !DESCRIPTION: \bv
C     *==========================================================*
C     | SUBROUTINE STIC_INIT_DEPTHS
C     | o Modify ocean upper boundary position according to
C     |   ice-shelf topography
C     *==========================================================*
C     \ev

C     !USES:
      IMPLICIT NONE
C     === Global variables ===
#include "SIZE.h"
#include "EEPARAMS.h"
#include "PARAMS.h"
#include "STIC.h"

C     !INPUT/OUTPUT PARAMETERS:
C     == Routine arguments ==
C     myThid    :: my Thread Id number
      INTEGER myThid

C     !LOCAL VARIABLES:
C     == Local variables ==
C     bi, bj    :: tile indices
C     i, j      :: Loop counters
      INTEGER bi, bj
      INTEGER i, j
CEOP

C--   Initialize R_stic
      DO bj = myByLo(myThid), myByHi(myThid)
       DO bi = myBxLo(myThid), myBxHi(myThid)
        DO j=1-OLy,sNy+OLy
         DO i=1-OLx,sNx+OLx
          R_stic     (i,j,bi,bj) = 0. _d 0
          sticfLength(i,j,bi,bj) = 0. _d 0
         ENDDO
        ENDDO
       ENDDO
      ENDDO

C---- ICEFRONT BEGIN
      IF ( STIClengthFile .NE. ' ' ) THEN
       CALL READ_FLD_XY_RS( STIClengthFile, ' ',
     &      sticfLength, 0, myThid )
       _EXCH_XY_RS( sticfLength, myThid )
      ENDIF

      IF ( STICdepthFile .NE. ' ' ) THEN
       CALL READ_FLD_XY_RS( STICdepthFile, ' ',
     &      R_stic, 0, myThid )
       _EXCH_XY_RS( R_stic, myThid )
      ENDIF

C     Make sure that R_stic is positive
      DO bj = myByLo(myThid), myByHi(myThid)
       DO bi = myBxLo(myThid), myBxHi(myThid)
        DO j = 1-OLy, sNy+OLy
         DO i = 1-OLx, sNx+OLx
          R_stic(i,j,bi,bj) = ABS(R_stic(i,j,bi,bj))
         ENDDO
        ENDDO
       ENDDO
      ENDDO

      RETURN
      END

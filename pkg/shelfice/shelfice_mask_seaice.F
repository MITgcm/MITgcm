#include "SHELFICE_OPTIONS.h"

CBOP
C !ROUTINE: SHELFICE_MASK_SEAICE

C !INTERFACE: ==========================================================
      SUBROUTINE SHELFICE_MASK_SEAICE(
     U                    SIfield,
     I                    bi, bj, myIter, myThid )

C !DESCRIPTION:
C  Mask seaice field (mask) to prevent seaice to spread
C         in grid-cells that are occupied by ice-shelf

C !USES: ===============================================================
      IMPLICIT NONE
#include "SIZE.h"
#include "EEPARAMS.h"
#include "PARAMS.h"
#include "SHELFICE.h"

C !INPUT/OUTPUT PARAMETERS: ============================================
C     SIfield     :: seaice field @ grid-cell center
C     bi, bj      :: Current tile indices
C     myIter      :: Current iteration number in simulation
C     myThid      :: my Thread Id number
      _RL SIfield(1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      INTEGER bi, bj
      INTEGER myIter
      INTEGER myThid

C !LOCAL VARIABLES: ====================================================
C     i, j        :: Loop counters
      INTEGER i, j
CEOP
C     msgBuf      :: Informational/error message buffer
c     CHARACTER*(MAX_LEN_MBUF) msgBuf

c     DO bj=myByLo(myThid),myByHi(myThid)
c      DO bi=myBxLo(myThid),myBxHi(myThid)

        DO j=1-OLy,sNy+OLy
         DO i=1-OLx,sNx+OLx
          IF ( kTopC(i,j,bi,bj).NE.0 ) SIfield(i,j,bi,bj) = zeroRL
         ENDDO
        ENDDO

c      ENDDO
c     ENDDO

      RETURN
      END

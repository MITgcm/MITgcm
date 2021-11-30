#include "SHELFICE_OPTIONS.h"

CBOP
C !ROUTINE: SHELFICE_DIAGNOSTICS_DRAG

C !INTERFACE: ==========================================================
      SUBROUTINE SHELFICE_DIAGNOSTICS_DRAG(
     I        uFld, vFld,
     I        bi, bj, myIter, myThid )

C !DESCRIPTION:
C Finish to calculate Ice-Shelf stress and fill-up corresponding diagnostics

C !USES: ===============================================================
      IMPLICIT NONE
#include "SIZE.h"
#include "EEPARAMS.h"
#include "PARAMS.h"
#include "GRID.h"
#include "SHELFICE.h"

C !INPUT PARAMETERS: ===================================================
C  uFld           :: velocity, zonal component
C  vFld           :: velocity, meridional component
C  bi,bj          :: tile indices
C  myIter         :: current iteration number
C  myThid         :: my Thread Id number
      _RL uFld   (1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nr)
      _RL vFld   (1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nr)
      INTEGER bi, bj
      INTEGER myIter, myThid

#ifdef ALLOW_SHELFICE
#ifdef ALLOW_DIAGNOSTICS
C !LOCAL VARIABLES : ====================================================
C  i,j            :: loop indices
C  k              :: vertical level
      INTEGER i, j, k
CEOP

      IF ( selectImplicitDrag.EQ.2 ) THEN
       DO j=1-OLy,sNy+OLy
        DO i=1-OLx+1,sNx+OLx
          k = MAX( 1, kTopC(i-1,j,bi,bj), kTopC(i,j,bi,bj) )
c         IF ( k.GE.1 ) THEN
           shelficeDragU(i,j,bi,bj) = -shelficeDragU(i,j,bi,bj)
     &                              * uFld(i,j,k)
c         ENDIF
        ENDDO
       ENDDO
       DO j=1-OLy+1,sNy+OLy
        DO i=1-OLx,sNx+OLx
          k = MAX( 1, kTopC(i,j-1,bi,bj), kTopC(i,j,bi,bj) )
c         IF ( k.GE.1 ) THEN
           shelficeDragV(i,j,bi,bj) = -shelficeDragV(i,j,bi,bj)
     &                              * vFld(i,j,k)
c         ENDIF
        ENDDO
       ENDDO
      ENDIF

      CALL DIAGNOSTICS_FILL_RS( shelficeDragU, 'SHI_TauX',
     &                          0, 1, 1, bi, bj, myThid )
      CALL DIAGNOSTICS_FILL_RS( shelficeDragV, 'SHI_TauY',
     &                          0, 1, 1, bi, bj, myThid )

#endif /* ALLOW_DIAGNOSTICS */
#endif /* ALLOW_SHELFICE */

      RETURN
      END

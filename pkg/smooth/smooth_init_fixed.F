#include "SMOOTH_OPTIONS.h"

      SUBROUTINE SMOOTH_INIT_FIXED( myThid )

C     *==========================================================*
C     | SUBROUTINE smooth_init_fixed
C     | o Routine that initializes smoothing/correlation operators
C     *==========================================================*

      IMPLICIT NONE
#include "SIZE.h"
#include "EEPARAMS.h"
#include "PARAMS.h"
#include "GRID.h"
#include "SMOOTH.h"

      INTEGER myThid

      INTEGER smoothOpNb
      INTEGER i, j, k, bi, bj

      DO bj=myByLo(myThid),myByHi(myThid)
       DO bi=myBxLo(myThid),myBxHi(myThid)
        DO k=1,Nr
         DO j=1-OLy,sNy+OLy
          DO i=1-OLx,sNx+OLx
           smooth_recip_hFacC(i,j,k,bi,bj) = _recip_hFacC(i,j,k,bi,bj)
           smooth_hFacW(i,j,k,bi,bj) = _hFacW(i,j,k,bi,bj)
           smooth_hFacS(i,j,k,bi,bj) = _hFacS(i,j,k,bi,bj)
          ENDDO
         ENDDO
        ENDDO
       ENDDO
      ENDDO

      DO smoothOpNb=1,smoothOpNbMax
       IF (smooth2Dtype(smoothOpNb).NE.0) THEN
        CALL smooth_init2D( smoothOpNb, myThid )
       ENDIF
      ENDDO

      DO smoothOpNb=1,smoothOpNbMax
       IF (smooth2Dtype(smoothOpNb).NE.0) THEN
        CALL smooth_filtervar2D( smoothOpNb, myThid )
       ENDIF
      ENDDO

      DO smoothOpNb=1,smoothOpNbMax
       IF ( (smooth3DtypeZ(smoothOpNb).NE.0).OR.
     &      (smooth3DtypeH(smoothOpNb).NE.0) ) THEN
        CALL smooth_init3D( smoothOpNb, myThid )
       ENDIF
      ENDDO

      DO smoothOpNb=1,smoothOpNbMax
       IF ( (smooth3DtypeZ(smoothOpNb).NE.0).OR.
     &      (smooth3DtypeH(smoothOpNb).NE.0) ) THEN
        CALL smooth_filtervar3D( smoothOpNb, myThid )
       ENDIF
      ENDDO

      RETURN
      END

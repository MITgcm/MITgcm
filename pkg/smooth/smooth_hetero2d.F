#include "SMOOTH_OPTIONS.h"

      SUBROUTINE SMOOTH_HETERO2D(
     &           fld_in, mask_in, dist_file, nbt_in, myThid )

C     *==========================================================*
C     | SUBROUTINE smooth_hetero2D
C     | o this routine smoothes a 2D field isotropically (except for
C     |   the land mask) to a space variable length scale. It mostly
C     |   calls smooth_diff2D.F.
C     *==========================================================*

      IMPLICIT NONE
#include "SIZE.h"
#include "EEPARAMS.h"
#include "GRID.h"
#include "PARAMS.h"
#include "SMOOTH.h"

      _RL fld_in(1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      _RS mask_in(1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      CHARACTER*(MAX_LEN_FNAM) dist_file
      INTEGER nbt_in
      INTEGER myThid

      _RL dist_in(1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      INTEGER i,j,bi,bj

      smooth2DtotTime = nbt_in*smooth2DdelTime

      CALL READ_REC_3D_RL( dist_file, smoothprec,
     &                     1, dist_in, 1, 1, myThid )

      DO bj=myByLo(myThid),myByHi(myThid)
       DO bi=myBxLo(myThid),myBxHi(myThid)
        DO j=1-OLy,sNy+OLy
         DO i=1-OLx,sNx+OLx
          smooth2D_Lx(i,j,bi,bj) = dist_in(i,j,bi,bj)
          smooth2D_Ly(i,j,bi,bj) = dist_in(i,j,bi,bj)
         ENDDO
        ENDDO
       ENDDO
      ENDDO

      DO bj=myByLo(myThid),myByHi(myThid)
       DO bi=myBxLo(myThid),myBxHi(myThid)
        DO j=1-OLy,sNy+OLy
         DO i=1-OLx,sNx+OLx
          smooth2D_Kux(i,j,bi,bj) = smooth2D_Lx(i,j,bi,bj)
     &            *smooth2D_Lx(i,j,bi,bj)/smooth2DtotTime/2
          smooth2D_Kvy(i,j,bi,bj) = smooth2D_Ly(i,j,bi,bj)
     &            *smooth2D_Ly(i,j,bi,bj)/smooth2DtotTime/2
         ENDDO
        ENDDO
       ENDDO
      ENDDO

      CALL EXCH_XY_RL( smooth2D_Kux, myThid )
      CALL EXCH_XY_RL( smooth2D_Kvy, myThid )

      CALL SMOOTH_DIFF2D( fld_in, mask_in, nbt_in, myThid )

      CALL EXCH_XY_RL( fld_in, myThid )

      RETURN
      END

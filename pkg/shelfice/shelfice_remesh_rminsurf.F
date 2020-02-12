#include "SHELFICE_OPTIONS.h"

CBOP
C     !ROUTINE: SHELFICE_REMESH_RMINSURF
C     !INTERFACE:
      SUBROUTINE SHELFICE_REMESH_RMINSURF(
     I                    myTime, myIter, myThid )
C     !DESCRIPTION: \bv
C     *==========================================================*
C     | SUBROUTINE SHELFICE_REMESH_RMINSURF
C     | to update Rmin_surf after remeshing downward and avoid
C     | surf_adjustments
C     *==========================================================*
C     \ev

C     !USES:
      IMPLICIT NONE
C     === Global variables ===
#include "SIZE.h"
#include "EEPARAMS.h"
#include "PARAMS.h"
#include "GRID.h"
#include "SURFACE.h"

      _RL myTime
      INTEGER myIter
      INTEGER myThid

#ifdef NONLIN_FRSURF
#ifdef ALLOW_SHELFICE_REMESHING
      _RL Rmin_tmp
      _RL hfacInfMom
      INTEGER bi, bj
      INTEGER i, j, ks

      hFacInfMOM = hFacInf

      DO bj=myByLo(myThid), myByHi(myThid)
       DO bi=myBxLo(myThid), myBxHi(myThid)
C-- Compute the mimimum value of r_surf (used for computing hFac_surfC)
         DO j=1,sNy
          DO i=1,sNx
           ks = kSurfC(i,j,bi,bj)
           IF (ks.LE.Nr) THEN
             Rmin_tmp = rF(ks+1)
             IF ( ks.EQ.kSurfW(i,j,bi,bj))
     &          Rmin_tmp = MAX(Rmin_tmp, R_low(i-1,j,bi,bj))
             IF ( ks.EQ.kSurfW(i+1,j,bi,bj))
     &          Rmin_tmp = MAX(Rmin_tmp, R_low(i+1,j,bi,bj))
             IF ( ks.EQ.kSurfS(i,j,bi,bj))
     &          Rmin_tmp = MAX(Rmin_tmp, R_low(i,j-1,bi,bj))
             IF ( ks.EQ.kSurfS(i,j+1,bi,bj))
     &          Rmin_tmp = MAX(Rmin_tmp, R_low(i,j+1,bi,bj))

             Rmin_surf(i,j,bi,bj) =
     &        MAX( MAX(rF(ks+1),R_low(i,j,bi,bj)) + hFacInf*drF(ks),
     &                                Rmin_tmp + hFacInfMOM*drF(ks)
     &           )
           ENDIF
          ENDDO
         ENDDO
C-    end bi,bj loop.
       ENDDO
      ENDDO

      CALL EXCH_XY_RL( Rmin_surf, myThid )

#endif /* ALLOW_SHELFICE_REMESHING */
#endif /* NONLIN_FRSURF */
      RETURN
      END

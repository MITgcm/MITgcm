C $Header: /u/gcmpack/MITgcm/model/src/freesurf_rescale_g.F,v 1.8 2014/08/14 16:49:19 jmc Exp $
C $Name:  $

#include "CPP_OPTIONS.h"

CBOP
C     !ROUTINE: FREESURF_RESCALE_G
C     !INTERFACE:
      SUBROUTINE FREESURF_RESCALE_G(
     I                     bi, bj, k,
     U                     gTracer,
     I                     myThid )
C     !DESCRIPTION: \bv
C     *==========================================================*
C     | S/R FREESURF_RESCALE_G
C     | o Re-scale Gs to account for change in free-surface
C     |   hieght. Only meaningful with non-linear free-surface.
C     *==========================================================*
C     \ev

C     !USES:
      IMPLICIT NONE
C     == Global variables ===
#include "SIZE.h"
#include "EEPARAMS.h"
#include "PARAMS.h"
#include "GRID.h"
#include "SURFACE.h"

C     !INPUT/OUTPUT PARAMETERS:
C     == Routine Arguments ==
      INTEGER bi,bj,k
      _RL  gTracer(1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nr)
      INTEGER myThid
#ifdef NONLIN_FRSURF
C     == Local variables ==
      INTEGER i,j
CEOP

C---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|

      IF ( nonlinFreeSurf.GT.0 ) THEN
       IF ( select_rStar.GT.0 ) THEN
# ifndef DISABLE_RSTAR_CODE
        DO j=1-OLy,sNy+OLy
         DO i=1-OLx,sNx+OLx
           gTracer(i,j,k) = gTracer(i,j,k)
     &                    / rStarExpC(i,j,bi,bj)
         ENDDO
        ENDDO
# endif /* DISABLE_RSTAR_CODE */
       ELSEIF ( selectSigmaCoord.NE.0 ) THEN
# ifndef DISABLE_SIGMA_CODE
        DO j=1-OLy,sNy+OLy
         DO i=1-OLx,sNx+OLx
           gTracer(i,j,k) = gTracer(i,j,k)
     &        /( 1. _d 0 + dEtaHdt(i,j,bi,bj)*deltaTFreeSurf
     &                    *dBHybSigF(k)*recip_drF(k)
     &                    *recip_hFacC(i,j,k,bi,bj)
     &         )
         ENDDO
        ENDDO
# endif /* DISABLE_SIGMA_CODE */
       ELSE
        DO j=1-OLy,sNy+OLy
         DO i=1-OLx,sNx+OLx
          IF (k.EQ.kSurfC(i,j,bi,bj)) THEN
           gTracer(i,j,k) = gTracer(i,j,k)
     &            *_hFacC(i,j,k,bi,bj)/hFac_surfC(i,j,bi,bj)
          ENDIF
         ENDDO
        ENDDO
       ENDIF
      ENDIF

#endif /* NONLIN_FRSURF */

      RETURN
      END

C $Header: /u/gcmpack/MITgcm/model/src/freesurf_rescale_g.F,v 1.5 2005/12/08 15:44:34 heimbach Exp $
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
      _RL  gTracer(1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nr,nSx,nSy)
      INTEGER myThid
#ifdef NONLIN_FRSURF
C     == Local variables ==
      INTEGER i,j
CEOP

C---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|

      IF (nonlinFreeSurf.GT.0) THEN
       IF (select_rStar.GT.0) THEN
#ifndef DISABLE_RSTAR_CODE
        DO j=1-Oly,sNy+Oly
         DO i=1-Olx,sNx+Olx
           gTracer(i,j,k,bi,bj) = gTracer(i,j,k,bi,bj)
     &                          /rStarExpC(i,j,bi,bj)
         ENDDO
        ENDDO
#endif /* DISABLE_RSTAR_CODE */
       ELSE
        DO j=1-Oly,sNy+Oly
         DO i=1-Olx,sNx+Olx
          IF (k.EQ.ksurfC(i,j,bi,bj)) THEN
           gTracer(i,j,k,bi,bj) = gTracer(i,j,k,bi,bj)
     &           *hFacC(i,j,k,bi,bj)/hFac_surfC(i,j,bi,bj) 
          ENDIF
         ENDDO
        ENDDO
       ENDIF
      ENDIF

#endif /* NONLIN_FRSURF */

      RETURN
      END

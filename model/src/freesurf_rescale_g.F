C $Header: /u/gcmpack/MITgcm/model/src/freesurf_rescale_g.F,v 1.2 2001/09/26 18:09:15 cnh Exp $
C $Name:  $

#include "CPP_OPTIONS.h"

CBOP
C     !ROUTINE: FREESURF_RESCALE_G
C     !INTERFACE:
      SUBROUTINE FREESURF_RESCALE_G(
     I                     bi, bj, K,
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
      INTEGER bi,bj,K
      _RL  gTracer(1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nr,nSx,nSy)
      INTEGER myIter, myThid
C     == Local variables ==
      INTEGER i,j
      _RL ab15,ab05
CEOP

C---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|

#ifdef NONLIN_FRSURF
      IF (nonlinFreeSurf.GT.0) THEN
       DO j=1-Oly,sNy+Oly
        DO i=1-Olx,sNx+Olx
         IF (k.EQ.ksurfC(i,j,bi,bj)) THEN
          gTracer(i,j,k,bi,bj) = gTracer(i,j,k,bi,bj)
     &          *hFacC(i,j,k,bi,bj)/hFac_surfC(i,j,bi,bj) 
         ENDIF
        ENDDO
       ENDDO
      ENDIF
#endif /* NONLIN_FRSURF */

      RETURN
      END

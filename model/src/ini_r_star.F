C $Header: /u/gcmpack/MITgcm/model/src/Attic/ini_r_star.F,v 1.3 2006/07/13 20:48:47 jmc Exp $
C $Name:  $

#include "CPP_OPTIONS.h"

CBOP
C     !ROUTINE: INI_R_STAR
C     !INTERFACE:
      SUBROUTINE INI_R_STAR( myThid )
C     !DESCRIPTION: \bv
C     *==========================================================*
C     | SUBROUTINE INI_R_STAR
C     | o Calculate new column thickness & scaling factor for r*
C     |    according to the surface r-position (Non-Lin Free-Surf)
C     *==========================================================*
C     \ev

C     !USES:
      IMPLICIT NONE
C     == Global variables
#include "SIZE.h"
#include "EEPARAMS.h"
#include "PARAMS.h"
#include "GRID.h"
#include "SURFACE.h"

C     !INPUT/OUTPUT PARAMETERS:
C     == Routine arguments ==
C     myThid :: Thread number for this instance of the routine.
      INTEGER myThid

#ifdef NONLIN_FRSURF

C     !LOCAL VARIABLES:
C     Local variables
C     i,j,k,bi,bj  :: loop counter
      INTEGER i,j,k,bi,bj
c     CHARACTER*(MAX_LEN_MBUF) suff
CEOP

C---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|

#ifdef ALLOW_DEBUG
      IF (debugMode) CALL DEBUG_ENTER('INI_R_STAR',myThid)
#endif

      DO bj=myByLo(myThid), myByHi(myThid)
       DO bi=myBxLo(myThid), myBxHi(myThid)
C-    1rst bi,bj loop :

C-- Initialise arrays :
         DO j=1-Oly,sNy+Oly
          DO i=1-Olx,sNx+Olx
            rStarFacC(i,j,bi,bj) = 1.
            rStarFacW(i,j,bi,bj) = 1.
            rStarFacS(i,j,bi,bj) = 1.
            rStarExpC(i,j,bi,bj) = 1.
            rStarExpW(i,j,bi,bj) = 1.
            rStarExpS(i,j,bi,bj) = 1.
            rStarDhCDt(i,j,bi,bj) = 0.
            rStarDhWDt(i,j,bi,bj) = 0.
            rStarDhSDt(i,j,bi,bj) = 0.
            PmEpR(i,j,bi,bj) = 0.
          ENDDO
         ENDDO
         DO k=1,Nr
          DO j=1-Oly,sNy+Oly
           DO i=1-Olx,sNx+Olx
            h0FacC(i,j,k,bi,bj) = _hFacC(i,j,k,bi,bj)
            h0FacW(i,j,k,bi,bj) = _hFacW(i,j,k,bi,bj)
            h0FacS(i,j,k,bi,bj) = _hFacS(i,j,k,bi,bj)
           ENDDO
          ENDDO
         ENDDO

C-    end 1rst bi,bj loop.
       ENDDO
      ENDDO

#ifdef ALLOW_DEBUG
      IF (debugMode) CALL DEBUG_LEAVE('INI_R_STAR',myThid)
#endif

C---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|
#endif /* NONLIN_FRSURF */

      RETURN
      END

C $Header: /u/gcmpack/MITgcm/model/src/timestep_wvel.F,v 1.1 2005/12/15 21:09:00 jmc Exp $
C $Name:  $

#include "PACKAGES_CONFIG.h"
#include "CPP_OPTIONS.h"

CBOP
C     !ROUTINE: TIMESTEP_WVEL
C     !INTERFACE:
      SUBROUTINE TIMESTEP_WVEL(
     I                          myTime, myIter, myThid )
C     !DESCRIPTION: \bv
C     *==========================================================*
C     | S/R TIMESTEP_WVEL
C     | o Step model vertical velocity forward in time
C     *==========================================================*
C     \ev

C     !USES:
      IMPLICIT NONE
C     == Global variables ==
#include "SIZE.h"
#include "EEPARAMS.h"
#include "PARAMS.h"
#include "GRID.h"
#include "DYNVARS.h"
#include "NH_VARS.h"
c #include "SURFACE.h"

C     !INPUT/OUTPUT PARAMETERS:
C     == Routine Arguments ==
      _RL     myTime
      INTEGER myIter, myThid

#ifdef ALLOW_NONHYDROSTATIC
C     !LOCAL VARIABLES:
C     == Local variables ==
      INTEGER k,bi,bj
      INTEGER iMin,iMax,jMin,jMax
      INTEGER i,j
      _RL     gWtmp(1-OLx:sNx+OLx,1-OLy:sNy+OLy)
CEOP

      iMin = 1
      iMax = sNx
      jMin = 1
      jMax = sNy

C---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|

C     tile loops :
      DO bj=myByLo(myThid),myByHi(myThid)
       DO bi=myBxLo(myThid),myBxHi(myThid)

        DO k=2,Nr

         DO j=jMin,jMax
          DO i=iMin,iMax
           gWtmp(i,j) = gW(i,j,k,bi,bj)
     &             *maskC(i,j,k,bi,bj)*maskC(i,j,k-1,bi,bj)
          ENDDO
         ENDDO

C     Step forward vertical velocity (and keep a copy in gW)
         DO j=jMin,jMax
          DO i=iMin,iMax
            gW(i,j,k,bi,bj) = wVel(i,j,k,bi,bj)
            wVel(i,j,k,bi,bj) = wVel(i,j,k,bi,bj)
     &                        + deltaTmom*nh_Am2*gWtmp(i,j)
          ENDDO
         ENDDO

C-    End of k loop
        ENDDO

C---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|

#ifdef ALLOW_OBCS
C--   This call is aesthetic: it makes the W field
C     consistent with the OBs but this has no algorithmic
C     impact. This is purely for diagnostic purposes.
        IF (useOBCS) THEN
         DO k=1,Nr
          CALL OBCS_APPLY_W( bi, bj, k, wVel, myThid )
         ENDDO
        ENDIF
#endif /* ALLOW_OBCS */

C-    End of bi,bj loops
       ENDDO
      ENDDO

#endif /* ALLOW_NONHYDROSTATIC */

      RETURN
      END

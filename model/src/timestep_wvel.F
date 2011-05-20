C $Header: /u/gcmpack/MITgcm/model/src/timestep_wvel.F,v 1.7 2011/05/20 16:20:13 jmc Exp $
C $Name:  $

#include "CPP_OPTIONS.h"

CBOP
C     !ROUTINE: TIMESTEP_WVEL
C     !INTERFACE:
      SUBROUTINE TIMESTEP_WVEL(
     I                          bi,bj, myTime, myIter, myThid )
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
C     bi, bj    :: Tile indices
C     myTime    :: Current time in simulation
C     myIter    :: Current iteration number in simulation
C     myThid    :: my Thread Id. number
      INTEGER bi,bj
      _RL     myTime
      INTEGER myIter, myThid

#ifdef ALLOW_NONHYDROSTATIC
C     !LOCAL VARIABLES:
C     == Local variables ==
C     iMin,iMax :: 1rst loop counter range
C     jMin,jMax :: 2nd  loop counter range
C     i,j,k     :: Loop counters
C     gWtmp     :: temporary array for vertical momentum tendency
      INTEGER iMin,iMax,jMin,jMax
      INTEGER i, j, k, km1
      _RL     gWtmp(1-OLx:sNx+OLx,1-OLy:sNy+OLy)
      _RL     tmpFac, nh_Fac, igwFac
      PARAMETER( iMin = 1 , iMax = sNx )
      PARAMETER( jMin = 1 , jMax = sNy )
CEOP

      igwFac = 0. _d 0
      IF ( implicitIntGravWave ) igwFac = 1. _d 0

C---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|

      IF ( nonHydrostatic ) THEN
        nh_Fac = 0.
        IF ( nh_Am2.NE.0. ) nh_Fac = 1. _d 0 / nh_Am2

        DO k=1,Nr
         km1 = MAX( k-1, 1 )

         IF ( implicitNHPress.NE.1. _d 0 ) THEN
C--   add explicit part of NH pressure gradient:
          tmpFac = pfFacMom*(1. _d 0 - implicitNHPress)
     &           * wUnit2rVel(k)*wUnit2rVel(k)*recip_rhoFacF(k)
          IF ( k.GE.2 ) THEN
           DO j=jMin,jMax
            DO i=iMin,iMax
             gW(i,j,k,bi,bj) = gW(i,j,k,bi,bj)
     &              - tmpFac*rkSign*recip_drC(k)
     &              *( phi_nh(i,j,k,bi,bj) - phi_nh(i,j,k-1,bi,bj) )
            ENDDO
           ENDDO
          ELSEIF ( selectNHfreeSurf.GE.1 ) THEN
           DO j=jMin,jMax
            DO i=iMin,iMax
             gW(i,j,k,bi,bj) = gW(i,j,k,bi,bj)
     &              - tmpFac*rkSign*recip_drC(k)
     &              *( phi_nh(i,j,k,bi,bj) - dPhiNH(i,j,bi,bj) )
            ENDDO
           ENDDO
          ENDIF
         ENDIF
C     apply mask to gW and keep a copy of wVel in gW:
         DO j=1-Oly,sNy+Oly
          DO i=1-Olx,sNx+Olx
            gWtmp(i,j) = gW(i,j,k,bi,bj)
     &             *maskC(i,j,k,bi,bj)*maskC(i,j,km1,bi,bj)
            gW(i,j,k,bi,bj) = wVel(i,j,k,bi,bj)
          ENDDO
         ENDDO
C     Step forward vertical velocity
         tmpFac = nh_Fac + igwFac*wUnit2rVel(k)*wUnit2rVel(k)
     &                           *dBdrRef(k)*deltaTMom*dTtracerLev(k)
         IF (tmpFac.GT.0. ) tmpFac = 1. _d 0 / tmpFac
         DO j=jMin,jMax
          DO i=iMin,iMax
            wVel(i,j,k,bi,bj) = wVel(i,j,k,bi,bj)
     &                        + deltaTmom*tmpFac*gWtmp(i,j)
          ENDDO
         ENDDO

C-    End of k loop
        ENDDO

C---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|

      ELSEIF ( implicitIntGravWave ) THEN
C     keep a copy of wVel in gW:
        DO k=1,Nr
         DO j=1-Oly,sNy+Oly
          DO i=1-Olx,sNx+Olx
            gW(i,j,k,bi,bj) = wVel(i,j,k,bi,bj)
          ENDDO
         ENDDO
        ENDDO

C-    End if nonHydrostatic / elseif implicitIntGravWave
      ENDIF

#endif /* ALLOW_NONHYDROSTATIC */

      RETURN
      END

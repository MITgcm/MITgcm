C $Header: /u/gcmpack/MITgcm/pkg/generic_advdiff/gad_u3c4_impl_r.F,v 1.1 2004/01/07 21:37:59 jmc Exp $
C $Name:  $

#include "GAD_OPTIONS.h"

CBOP
C     !ROUTINE: GAD_FLUXLIMIT_IMPL_R
C     !INTERFACE:
      SUBROUTINE GAD_U3C4_IMPL_R( 
     I           bi,bj,k, iMin,iMax,jMin,jMax, 
     I           advectionScheme, deltaTarg, rTrans,
     O           a5d, b5d, c5d, d5d, e5d,
     I           myThid )

C     !DESCRIPTION: \bv
C     *==========================================================*
C     | S/R GAD_U3C4_IMPL_R
C     | o Compute matrix element to solve vertical advection
C     |   implicitly using 3rd order upwind 
C     |                 or 4th order Centered advection schemes
C     *==========================================================*
C     | o contribution of vertical transport at interface k
C     |   is added to matrix lines k & k-1 
C     *==========================================================*
C     \ev

C     !USES:
      IMPLICIT NONE

C     == Global variables ===
#include "SIZE.h"
#include "GRID.h"
#include "EEPARAMS.h"
#include "PARAMS.h"
#include "GAD.h"

C     !INPUT/OUTPUT PARAMETERS:
C     == Routine Arguments ==
C     bi,bj                :: tile indices
C     k                    :: vertical level
C     iMin,iMax,jMin,jMax  :: computation domain
C     advectionScheme      :: advection scheme to use
C     deltaTarg            :: time step
C     rTrans               :: vertical volume transport
C     tFld                 :: tracer field
C     a5d                  :: 2nd  lower diagonal of the pentadiagonal matrix 
C     b5d                  :: 1rst lower diagonal of the pentadiagonal matrix 
C     c5d                  :: main diagonal       of the pentadiagonal matrix 
C     d5d                  :: 1rst upper diagonal of the pentadiagonal matrix 
C     e5d                  :: 2nd  upper diagonal of the pentadiagonal matrix 
C     myThid               :: thread number
      INTEGER bi,bj,k
      INTEGER iMin,iMax,jMin,jMax
      INTEGER advectionScheme
      _RL deltaTarg
      _RL rTrans(1-OLx:sNx+OLx,1-OLy:sNy+OLy)
      _RL a5d   (1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nr)
      _RL b5d   (1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nr)
      _RL c5d   (1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nr)
      _RL d5d   (1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nr)
      _RL e5d   (1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nr)
      INTEGER myThid

C     == Local Variables ==
C     i,j                  :: loop indices
C     kp1                  :: =min( k+1 , Nr )
C     km2                  :: =max( k-2 , 1 )
C     rCenter              :: centered contribution
C     rUpwind              :: upwind   contribution
      LOGICAL flagC4
      INTEGER i,j,kp1,km2
      _RL rCenter, rUpwind
      _RL rC4km, rC4kp, rU1k, rU3km, rU3kp
      _RL mskM, mskP, maskM2, maskP1
CEOP

      IF ( k.GT.Nr .OR. k.LT.2 ) RETURN

      km2=MAX(1,k-2)
      kp1=MIN(Nr,k+1)
      maskP1 = 1. _d 0 
      maskM2 = 1. _d 0 
      IF ( k.LE.2 ) maskM2 = 0. _d 0
      IF ( k.GE.Nr) maskP1 = 0. _d 0
      flagC4 = advectionScheme.EQ.ENUM_CENTERED_4TH 
     &         .AND. k.GT.2 .AND. k.LT.Nr

C--    Add centered & upwind contributions
       DO j=jMin,jMax
         DO i=iMin,iMax
           rCenter= 0.5 _d 0 *deltaTtracer*rTrans(i,j)
     &                        *recip_rA(i,j,bi,bj)*rkFac
           mskM   = maskC(i,j,km2,bi,bj)*maskM2
           mskP   = maskC(i,j,kp1,bi,bj)*maskP1
           rC4km  = oneSixth*rCenter*mskM
           rC4kp  = oneSixth*rCenter*mskP
           IF ( flagC4 .AND. mskM*mskP.GT.0. _d 0 ) THEN
            rUpwind= 0. _d 0
            rU3km  = 0. _d 0
            rU3kp  = 0. _d 0
           ELSE
            rU1k   = oneSixth*abs(rCenter)
            rUpwind= rU1k+rU1k
            rU3km  = rU1k*mskM
            rU3kp  = rU1k*mskP
           ENDIF
           a5d(i,j,k)   = a5d(i,j,k)
     &                  - (rC4km - rU3km)
     &                   *recip_hFacC(i,j,k,bi,bj)*recip_drF(k)
           b5d(i,j,k)   = b5d(i,j,k)
     &                  + (rCenter + rC4km - rUpwind - rU3km)
     &                   *recip_hFacC(i,j,k,bi,bj)*recip_drF(k)
           c5d(i,j,k)   = c5d(i,j,k)
     &                  + (rCenter + rC4kp + rUpwind + rU3kp)
     &                    *recip_hFacC(i,j,k,bi,bj)*recip_drF(k)
           d5d(i,j,k)   = d5d(i,j,k)
     &                  - (rC4kp + rU3kp)
     &                   *recip_hFacC(i,j,k,bi,bj)*recip_drF(k)
           b5d(i,j,k-1) = b5d(i,j,k-1)
     &                  + (rC4km - rU3km)
     &                   *recip_hFacC(i,j,k-1,bi,bj)*recip_drF(k-1)
           c5d(i,j,k-1) = c5d(i,j,k-1)
     &                  - (rCenter + rC4km - rUpwind - rU3km)
     &                   *recip_hFacC(i,j,k-1,bi,bj)*recip_drF(k-1)
           d5d(i,j,k-1) = d5d(i,j,k-1)
     &                  - (rCenter + rC4kp + rUpwind + rU3kp)
     &                   *recip_hFacC(i,j,k-1,bi,bj)*recip_drF(k-1)
           e5d(i,j,k-1) = e5d(i,j,k-1)
     &                  + (rC4kp + rU3kp)
     &                   *recip_hFacC(i,j,k-1,bi,bj)*recip_drF(k-1)
         ENDDO
       ENDDO

      RETURN
      END

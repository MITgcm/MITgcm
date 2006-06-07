C $Header: /u/gcmpack/MITgcm/pkg/generic_advdiff/gad_u3c4_impl_r.F,v 1.8 2006/06/07 01:55:14 heimbach Exp $
C $Name:  $

#include "GAD_OPTIONS.h"

CBOP
C     !ROUTINE: GAD_U3C4_IMPL_R
C     !INTERFACE:
      SUBROUTINE GAD_U3C4_IMPL_R(
     I           bi,bj,k, iMin,iMax,jMin,jMax,
     I           advectionScheme, deltaTarg, rTrans,
     O           a5d, b5d, c5d, d5d, e5d,
     I           myThid )

C     !DESCRIPTION:
C     Compute matrix element to solve vertical advection implicitly
C      using 3rd order upwind advection scheme,
C         or 3rd order Direct Space and Time advection scheme,
C         or 4th order Centered advection scheme.
C     Method:
C      contribution of vertical transport at interface k is added
C      to matrix lines k and k-1

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
C     bi,bj           :: tile indices
C     k               :: vertical level
C     iMin,iMax       :: computation domain
C     jMin,jMax       :: computation domain
C     advectionScheme :: advection scheme to use
C     deltaTarg       :: time step
C     rTrans          :: vertical volume transport
C     a5d             :: 2nd  lower diag of pentadiagonal matrix 
C     b5d             :: 1rst lower diag of pentadiagonal matrix 
C     c5d             :: main diag       of pentadiagonal matrix 
C     d5d             :: 1rst upper diag of pentadiagonal matrix 
C     e5d             :: 2nd  upper diag of pentadiagonal matrix 
C     myThid          :: thread number
      INTEGER bi,bj,k
      INTEGER iMin,iMax,jMin,jMax
      INTEGER advectionScheme
      _RL deltaTarg(Nr)
      _RL rTrans(1-OLx:sNx+OLx,1-OLy:sNy+OLy)
      _RL a5d   (1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nr)
      _RL b5d   (1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nr)
      _RL c5d   (1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nr)
      _RL d5d   (1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nr)
      _RL e5d   (1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nr)
      INTEGER myThid

C     == Local Variables ==
C     i,j             :: loop indices
C     kp1             :: =min( k+1 , Nr )
C     km2             :: =max( k-2 , 1 )
C     rCenter         :: centered contribution
C     rUpwind         :: upwind   contribution
C     rC4km, rC4kp    :: high order contribution
C     rHigh           :: high order term factor
      LOGICAL flagC4
      INTEGER i,j,kp1,km2
      _RL wCFL, rCenter, rUpwind
      _RL rC4km, rC4kp, rHigh
      _RL mskM, mskP, maskM2, maskP1
      _RL deltaTcfl
CEOP

C--   process interior interface only:
      IF ( k.GT.1 .AND. k.LE.Nr ) THEN

      km2=MAX(1,k-2)
      kp1=MIN(Nr,k+1)
      maskP1 = 1. _d 0
      maskM2 = 1. _d 0
      IF ( k.LE.2 ) maskM2 = 0. _d 0
      IF ( k.GE.Nr) maskP1 = 0. _d 0
      flagC4 = advectionScheme.EQ.ENUM_CENTERED_4TH
     &         .AND. k.GT.2 .AND. k.LT.Nr

C--    Add centered, upwind and high-order contributions
       deltaTcfl = deltaTarg(k)
       DO j=jMin,jMax
         DO i=iMin,iMax
           rCenter= 0.5 _d 0 *rTrans(i,j)*recip_rA(i,j,bi,bj)*rkSign
           mskM   = maskC(i,j,km2,bi,bj)*maskM2
           mskP   = maskC(i,j,kp1,bi,bj)*maskP1
           IF ( flagC4 .AND. mskM*mskP.GT.0. _d 0 ) THEN
            rUpwind= 0. _d 0
            rC4km  = oneSixth*rCenter*mskM
            rC4kp  = oneSixth*rCenter*mskP
           ELSEIF ( advectionScheme.EQ.ENUM_DST3 ) THEN
            wCFL = deltaTcfl*ABS(rTrans(i,j))
     &            *recip_rA(i,j,bi,bj)*recip_drC(k)
            rHigh  = (1. _d 0 -wCFL*wCFL)*oneSixth
c           rUpwind= (2. _d 0*rHigh - wCFL)*ABS(rCenter)
            rUpwind= (2. _d 0*rHigh )*ABS(rCenter)
            rC4km  =  rHigh * (rCenter+ABS(rCenter))*mskM
            rC4kp  =  rHigh * (rCenter-ABS(rCenter))*mskP
           ELSE
            rUpwind=  2. _d 0*oneSixth*ABS(rCenter)
            rC4km  = oneSixth*(rCenter+ABS(rCenter))*mskM
            rC4kp  = oneSixth*(rCenter-ABS(rCenter))*mskP
           ENDIF
           a5d(i,j,k)   = a5d(i,j,k)
     &                  + rC4km
     &                   *deltaTarg(k)
     &                   *_recip_hFacC(i,j,k,bi,bj)*recip_drF(k)
           b5d(i,j,k)   = b5d(i,j,k)
     &                  - ( (rCenter+rUpwind) + rC4km )
     &                   *deltaTarg(k)
     &                   *_recip_hFacC(i,j,k,bi,bj)*recip_drF(k)
           c5d(i,j,k)   = c5d(i,j,k)
     &                  - ( (rCenter-rUpwind) + rC4kp )
     &                   *deltaTarg(k)
     &                    *_recip_hFacC(i,j,k,bi,bj)*recip_drF(k)
           d5d(i,j,k)   = d5d(i,j,k)
     &                  + rC4kp
     &                   *deltaTarg(k)
     &                   *_recip_hFacC(i,j,k,bi,bj)*recip_drF(k)
           b5d(i,j,k-1) = b5d(i,j,k-1)
     &                  - rC4km
     &                   *deltaTarg(k-1)
     &                   *_recip_hFacC(i,j,k-1,bi,bj)*recip_drF(k-1)
           c5d(i,j,k-1) = c5d(i,j,k-1)
     &                  + ( (rCenter+rUpwind) + rC4km )
     &                   *deltaTarg(k-1)
     &                   *_recip_hFacC(i,j,k-1,bi,bj)*recip_drF(k-1)
           d5d(i,j,k-1) = d5d(i,j,k-1)
     &                  + ( (rCenter-rUpwind) + rC4kp )
     &                   *deltaTarg(k-1)
     &                   *_recip_hFacC(i,j,k-1,bi,bj)*recip_drF(k-1)
           e5d(i,j,k-1) = e5d(i,j,k-1)
     &                  - rC4kp
     &                   *deltaTarg(k-1)
     &                   *_recip_hFacC(i,j,k-1,bi,bj)*recip_drF(k-1)
         ENDDO
       ENDDO

C--   process interior interface only: end
      ENDIF

      RETURN
      END

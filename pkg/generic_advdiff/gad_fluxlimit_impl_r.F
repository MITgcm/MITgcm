C $Header: /u/gcmpack/MITgcm/pkg/generic_advdiff/gad_fluxlimit_impl_r.F,v 1.9 2011/12/01 14:14:44 jmc Exp $
C $Name:  $

#include "GAD_OPTIONS.h"

CBOP
C     !ROUTINE: GAD_FLUXLIMIT_IMPL_R
C     !INTERFACE:
      SUBROUTINE GAD_FLUXLIMIT_IMPL_R(
     I           bi,bj,k, iMin,iMax,jMin,jMax,
     I           deltaTarg, rTrans, recip_hFac, tFld,
     O           a3d, b3d, c3d,
     I           myThid )

C     !DESCRIPTION:
C     Compute matrix element to solve vertical advection implicitly
C      using flux--limiter advection scheme.
C     Method:
C      contribution of vertical transport at interface k is added
C      to matrix lines k and k-1.

C     !USES:
      IMPLICIT NONE

C     == Global variables ===
#include "SIZE.h"
#include "GRID.h"
#include "EEPARAMS.h"
#include "PARAMS.h"

C     !INPUT/OUTPUT PARAMETERS:
C     == Routine Arguments ==
C     bi,bj        :: tile indices
C     k            :: vertical level
C     iMin,iMax    :: computation domain
C     jMin,jMax    :: computation domain
C     deltaTarg    :: time step
C     rTrans       :: vertical volume transport
C     recip_hFac   :: inverse of cell open-depth factor
C     tFld         :: tracer field
C     a3d          :: lower diagonal of the tridiagonal matrix
C     b3d          :: main  diagonal of the tridiagonal matrix
C     c3d          :: upper diagonal of the tridiagonal matrix
C     myThid       :: thread number
      INTEGER bi,bj,k
      INTEGER iMin,iMax,jMin,jMax
      _RL     deltaTarg(Nr)
      _RL     rTrans(1-OLx:sNx+OLx,1-OLy:sNy+OLy)
      _RS recip_hFac(1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nr)
      _RL     tFld  (1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nr)
      _RL     a3d   (1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nr)
      _RL     b3d   (1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nr)
      _RL     c3d   (1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nr)
      INTEGER myThid

C     == Local Variables ==
C     i,j          :: loop indices
C     kp1          :: =min( k+1 , Nr )
C     km1          :: =max( k-1 , 1 )
C     km2          :: =max( k-2 , 1 )
C     Cr           :: slope ratio
C     Rjm,Rj,Rjp   :: differences at i-1,i,i+1
C     w_CFL        :: Courant-Friedrich-Levy number
C     upwindFac    :: upwind factor
C     rCenter      :: centered contribution
C     rUpwind      :: upwind   contribution
      INTEGER i,j,kp1,km1,km2
      _RL Cr,Rjm,Rj,Rjp, w_CFL
      _RL upwindFac(1-OLx:sNx+OLx,1-OLy:sNy+OLy)
      _RL rCenter, rUpwind
      _RL deltaTcfl

C Statement function provides Limiter(Cr)
#include "GAD_FLUX_LIMITER.h"
CEOP

      km2=MAX(1,k-2)
      km1=MAX(1,k-1)
      kp1=MIN(Nr,k+1)

C--   process interior interface only:
      IF ( k.GT.1 .AND. k.LE.Nr ) THEN

C--   Compute the upwind fraction:
       deltaTcfl = deltaTarg(k)
       DO j=jMin,jMax
        DO i=iMin,iMax
         w_CFL = deltaTcfl*rTrans(i,j)*recip_rA(i,j,bi,bj)*recip_drC(k)
         Rjp=(tFld(i,j,kp1)-tFld(i,j,k)  )*maskC(i,j,kp1,bi,bj)
         Rj =(tFld(i,j,k)  -tFld(i,j,km1))
         Rjm=(tFld(i,j,km1)-tFld(i,j,km2))*maskC(i,j,km2,bi,bj)

         IF ( Rj.NE.0. _d 0) THEN
          IF (rTrans(i,j).LT.0. _d 0) THEN
            Cr=Rjm/Rj
          ELSE
            Cr=Rjp/Rj
          ENDIF
          upwindFac(i,j) = 1. _d 0
     &                   - Limiter(Cr) * ( 1. _d 0 + ABS(w_CFL) )
          upwindFac(i,j) = MAX( -1. _d 0, upwindFac(i,j) )
         ELSE
          upwindFac(i,j) = 0. _d 0
         ENDIF
        ENDDO
       ENDDO

C--    Add centered & upwind contributions
       DO j=jMin,jMax
         DO i=iMin,iMax
           rCenter = 0.5 _d 0 *rTrans(i,j)*recip_rA(i,j,bi,bj)*rkSign
           rUpwind = ABS(rCenter)*upwindFac(i,j)
           a3d(i,j,k)   = a3d(i,j,k)
     &                  - (rCenter+rUpwind)*deltaTarg(k)
     &                   *recip_hFac(i,j,k)*recip_drF(k)
           b3d(i,j,k)   = b3d(i,j,k)
     &                  - (rCenter-rUpwind)*deltaTarg(k)
     &                   *recip_hFac(i,j,k)*recip_drF(k)
           b3d(i,j,k-1) = b3d(i,j,k-1)
     &                  + (rCenter+rUpwind)*deltaTarg(k-1)
     &                   *recip_hFac(i,j,k-1)*recip_drF(k-1)
           c3d(i,j,k-1) = c3d(i,j,k-1)
     &                  + (rCenter-rUpwind)*deltaTarg(k-1)
     &                   *recip_hFac(i,j,k-1)*recip_drF(k-1)
         ENDDO
       ENDDO

C--   process interior interface only: end
      ENDIF

      RETURN
      END

#include "GAD_OPTIONS.h"
#ifdef ALLOW_AUTODIFF
# include "AUTODIFF_OPTIONS.h"
#endif

CBOP
C     !ROUTINE: GAD_DST3FL_IMPL_R
C     !INTERFACE:
      SUBROUTINE GAD_DST3FL_IMPL_R(
     I           bi,bj,k, iMin,iMax,jMin,jMax,
     I           deltaTarg, rTrans, recip_hFac, tFld,
     O           a5d, b5d, c5d, d5d, e5d,
     I           myThid )

C     !DESCRIPTION:

C     Compute matrix element to solve vertical advection implicitly
C     using 3rd order Direct Space and Time (DST) advection scheme
C           with Flux-Limiter.
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
C     bi,bj        :: tile indices
C     k            :: vertical level
C     iMin,iMax    :: computation domain
C     jMin,jMax    :: computation domain
C     deltaTarg    :: time step
C     rTrans       :: vertical volume transport
C     recip_hFac   :: inverse of cell open-depth factor
C     tFld         :: tracer field
C     a5d          :: 2nd  lower diag of pentadiagonal matrix
C     b5d          :: 1rst lower diag of pentadiagonal matrix
C     c5d          :: main diag       of pentadiagonal matrix
C     d5d          :: 1rst upper diag of pentadiagonal matrix
C     e5d          :: 2nd  upper diag of pentadiagonal matrix
C     myThid       :: thread number
      INTEGER bi,bj,k
      INTEGER iMin,iMax,jMin,jMax
      _RL     deltaTarg(Nr)
      _RL     rTrans(1-OLx:sNx+OLx,1-OLy:sNy+OLy)
      _RS recip_hFac(1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nr)
      _RL     tFld  (1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nr)
      _RL     a5d   (1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nr)
      _RL     b5d   (1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nr)
      _RL     c5d   (1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nr)
      _RL     d5d   (1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nr)
      _RL     e5d   (1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nr)
      INTEGER myThid

C     == Local Variables ==
C     i,j          :: loop indices
C     kp1          :: =min( k+1 , Nr )
C     km2          :: =max( k-2 , 1 )
C     wCFL         :: Courant-Friedrich-Levy number
C     lowFac       :: low  order term factor
C     highFac      :: high order term factor
C     rCenter      :: centered contribution
C     rUpwind      :: upwind   contribution
C     rC4km, rC4kp :: high order contributions
      INTEGER i,j,kp1,km2
      _RL wCFL, rCenter, rUpwind
      _RL lowFac (1-OLx:sNx+OLx,1-OLy:sNy+OLy)
      _RL highFac(1-OLx:sNx+OLx,1-OLy:sNy+OLy)
      _RL rC4km, rC4kp
      _RL mskM, mskP, maskM2, maskP1
      _RL Rj, Rjh, cL1, cH3, cM2, th1, th2
      _RL deltaTcfl
#ifdef ALLOW_AUTODIFF_TAMC
      INTEGER ikey
#endif
CEOP

#ifdef ALLOW_AUTODIFF_TAMC
CADJ INIT loctape_gad = COMMON, (sNx+2*OLx)*(sNy+2*OLy)
#endif
C--   process interior interface only:
      IF ( k.GT.1 .AND. k.LE.Nr ) THEN

       km2=MAX(1,k-2)
       kp1=MIN(Nr,k+1)
       maskP1 = 1. _d 0
       maskM2 = 1. _d 0
       IF ( k.LE.2 ) maskM2 = 0. _d 0
       IF ( k.GE.Nr) maskP1 = 0. _d 0

C--   Compute the low-order term & high-order term fractions :
       deltaTcfl = deltaTarg(k)
C     DST-3 Flux-Limiter Advection Scheme:
C-    Limiter: Psi=max(0,min(1,cL1+theta*cH1,theta*(1-cfl)/cfl) )
C              with theta=Rjh/Rj ;
C       is linearize arround the current value of theta(tFld) & cfl:
C       lowFac & highFac are set such as Psi*Rj = lowFac*Rj + highFac*Rjh
       DO j=jMin,jMax
         DO i=iMin,iMax
#ifdef ALLOW_AUTODIFF_TAMC
           ikey = i + (sNx+2*OLx)*(j-1)
#endif
           wCFL = deltaTcfl*ABS(rTrans(i,j))
     &           *recip_rA(i,j,bi,bj)*recip_drC(k)
     &           *recip_deepFac2F(k)*recip_rhoFacF(k)
           cL1 = (2. _d 0 -wCFL)*(1. _d 0 -wCFL)*oneSixth
           cH3 = (1. _d 0 -wCFL*wCFL)*oneSixth
c          cM2 = (1. _d 0 - wCFL)/( wCFL +1. _d -20)
           cM2 = (1. _d 0 + wCFL)/( wCFL +1. _d -20)

           Rj =(tFld(i,j,k)  -tFld(i,j,k-1))
           IF ( rTrans(i,j).GT.0. _d 0 ) THEN
             Rjh = (tFld(i,j,k-1)-tFld(i,j,km2))*maskC(i,j,km2,bi,bj)
           ELSE
             Rjh = (tFld(i,j,kp1)-tFld(i,j,k)  )*maskC(i,j,kp1,bi,bj)
           ENDIF
           IF ( Rj*Rjh.LE.0. _d 0 ) THEN
C-         1rst case: theta < 0 (Rj & Rjh opposite sign) => Psi = 0
             lowFac(i,j) = 0. _d 0
             highFac(i,j)= 0. _d 0
           ELSE
#ifdef ALLOW_AUTODIFF_TAMC
C     These stores to local tape avoid recomputations and associated warnings
CADJ STORE Rjh, Rj = loctape_gad, key = ikey
#endif
             Rj  = ABS(Rj)
             Rjh = ABS(Rjh)
             th1 = cL1*Rj+cH3*Rjh
             th2 = cM2*Rjh
            IF     ( th1.LE.th2 .AND. th1.LE.Rj ) THEN
C-          2nd case: cL1+theta*cH3 = min of the three = Psi
             lowFac(i,j) = cL1
             highFac(i,j)= cH3
            ELSEIF ( th2.LT.th1 .AND. th2.LE.Rj ) THEN
C-          3rd case: theta*cM2 = min of the three = Psi
             lowFac(i,j) = 0. _d 0
             highFac(i,j)= cM2
            ELSE
C-          4th case (Rj < th1 & Rj < th2) : 1 = min of the three = Psi
             lowFac(i,j) = 1. _d 0
             highFac(i,j)= 0. _d 0
            ENDIF
           ENDIF
         ENDDO
       ENDDO

C--    Add centered & upwind contributions
       DO j=jMin,jMax
         DO i=iMin,iMax
           rCenter= 0.5 _d 0 *rTrans(i,j)*recip_rA(i,j,bi,bj)*rkSign
           mskM   = maskC(i,j,km2,bi,bj)*maskM2
           mskP   = maskC(i,j,kp1,bi,bj)*maskP1
           rUpwind= (0.5 _d 0 -lowFac(i,j))*ABS(rCenter)*2. _d 0
           rC4km  = highFac(i,j)*(rCenter+ABS(rCenter))*mskM
           rC4kp  = highFac(i,j)*(rCenter-ABS(rCenter))*mskP

           a5d(i,j,k)   = a5d(i,j,k)
     &                  + rC4km
     &                   *deltaTarg(k)
     &                   *recip_hFac(i,j,k)*recip_drF(k)
     &                   *recip_deepFac2C(k)*recip_rhoFacC(k)
           b5d(i,j,k)   = b5d(i,j,k)
     &                  - ( (rCenter+rUpwind) + rC4km )
     &                   *deltaTarg(k)
     &                   *recip_hFac(i,j,k)*recip_drF(k)
     &                   *recip_deepFac2C(k)*recip_rhoFacC(k)
           c5d(i,j,k)   = c5d(i,j,k)
     &                  - ( (rCenter-rUpwind) + rC4kp )
     &                   *deltaTarg(k)
     &                   *recip_hFac(i,j,k)*recip_drF(k)
     &                   *recip_deepFac2C(k)*recip_rhoFacC(k)
           d5d(i,j,k)   = d5d(i,j,k)
     &                  + rC4kp
     &                   *deltaTarg(k)
     &                   *recip_hFac(i,j,k)*recip_drF(k)
     &                   *recip_deepFac2C(k)*recip_rhoFacC(k)
           b5d(i,j,k-1) = b5d(i,j,k-1)
     &                  - rC4km
     &                   *deltaTarg(k-1)
     &                   *recip_hFac(i,j,k-1)*recip_drF(k-1)
     &                   *recip_deepFac2C(k-1)*recip_rhoFacC(k-1)
           c5d(i,j,k-1) = c5d(i,j,k-1)
     &                  + ( (rCenter+rUpwind) + rC4km )
     &                   *deltaTarg(k-1)
     &                   *recip_hFac(i,j,k-1)*recip_drF(k-1)
     &                   *recip_deepFac2C(k-1)*recip_rhoFacC(k-1)
           d5d(i,j,k-1) = d5d(i,j,k-1)
     &                  + ( (rCenter-rUpwind) + rC4kp )
     &                   *deltaTarg(k-1)
     &                   *recip_hFac(i,j,k-1)*recip_drF(k-1)
     &                   *recip_deepFac2C(k-1)*recip_rhoFacC(k-1)
           e5d(i,j,k-1) = e5d(i,j,k-1)
     &                  - rC4kp
     &                   *deltaTarg(k-1)
     &                   *recip_hFac(i,j,k-1)*recip_drF(k-1)
     &                   *recip_deepFac2C(k-1)*recip_rhoFacC(k-1)
         ENDDO
       ENDDO

C--   process interior interface only: end
      ENDIF

      RETURN
      END

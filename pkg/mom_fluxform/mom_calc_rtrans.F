C $Header: /u/gcmpack/MITgcm/pkg/mom_fluxform/mom_calc_rtrans.F,v 1.7 2007/10/15 15:28:58 jmc Exp $
C $Name:  $

#include "MOM_FLUXFORM_OPTIONS.h"

CBOP
C     !ROUTINE: MOM_CALC_RTRANS
C     !INTERFACE:
      SUBROUTINE MOM_CALC_RTRANS(
     I                          k, bi, bj,
     O                          rTransU, rTransV,
     I                          myTime, myIter, myThid)
C     !DESCRIPTION: \bv
C     *==========================================================*
C     | SUBROUTINE MOM_CALC_RTRANS
C     | o Calculate vertical transports at interface k
C     |   above U & V points (West & South face)
C     *==========================================================*
C     | r coordinate (z or p):
C     |  is simply half of the 2 vert. Transp at Center location
C     | r* coordinate: less simple since
C     |  d.eta/dt / H has to be evaluated locally at U & V points
C     *==========================================================*
C     \ev

C     !USES:
      IMPLICIT NONE
C     == GLobal variables ==
#include "SIZE.h"
#include "DYNVARS.h"
#include "EEPARAMS.h"
#include "PARAMS.h"
#include "GRID.h"
#include "SURFACE.h"
#include "MOM_FLUXFORM.h"

C     !INPUT/OUTPUT PARAMETERS:
C     == Routine arguments ==
C     k       :: vertical level
C     bi,bj   :: tile indices
C     rTransU :: vertical transport (above U point)
C     rTransV :: vertical transport (above V point)
C     myTime  :: current time
C     myIter  :: current iteration number
C     myThid  :: thread number

      INTEGER k, bi, bj, myIter, myThid
      _RL myTime
      _RL rTransU(1-OLx:sNx+OLx,1-OLy:sNy+OLy)
      _RL rTransV(1-OLx:sNx+OLx,1-OLy:sNy+OLy)

C     !LOCAL VARIABLES:
#ifdef NONLIN_FRSURF
C     == Local variables in common block ==
C     dWtransC :: vertical transp. difference between r & r* coordinates
C     dWtransU :: same but above u.point location (West  face)
C     dWtransV :: same but above v.point location (South face)
cph need this in a header for the adjoint
cph      COMMON /LOCAL_MOM_CALC_RTRANS/
cph     &       dWtransC, dWtransU, dWtransV
cph      _RL dWtransC(1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
cph      _RL dWtransU(1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
cph      _RL dWtransV(1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
#endif /* NONLIN_FRSURF */
C     == Local variables ==
C     I, J :: Loop counters
      INTEGER i,j
CEOP

#ifdef NONLIN_FRSURF
      IF ( k.EQ.Nr+1 .AND.
     &     useRealFreshWaterFlux .AND. usingPCoords ) THEN
C note: deep-model not implemented for P-coordinate + realFreshWaterFlux ;
C       anelastic: always assumes that rhoFacF(1) = 1
        DO j=1-OLy+1,sNy+OLy
         DO i=1-OLx+1,sNx+OLx
          rTransU(i,j) = mass2rUnit*
     &      0.5 _d 0*( PmEpR( i ,j,bi,bj)*rA( i ,j,bi,bj)
     &                +PmEpR(i-1,j,bi,bj)*rA(i-1,j,bi,bj) )
          rTransV(i,j) = mass2rUnit*
     &      0.5 _d 0*( PmEpR(i, j ,bi,bj)*rA(i, j ,bi,bj)
     &                +PmEpR(i,j-1,bi,bj)*rA(i,j-1,bi,bj) )
         ENDDO
        ENDDO
      ELSEIF ( k.GT.Nr ) THEN
#else /* NONLIN_FRSURF */
       IF ( k.GT.Nr ) THEN
#endif /* NONLIN_FRSURF */
        DO j=1-OLy+1,sNy+OLy
         DO i=1-OLx+1,sNx+OLx
          rTransU(i,j) = 0.
          rTransV(i,j) = 0.
         ENDDO
        ENDDO
      ELSE
C-    Calculate vertical transports above U & V points (West & South face):
        DO j=1-OLy+1,sNy+OLy
         DO i=1-OLx+1,sNx+OLx
          rTransU(i,j) =
     &         0.5 _d 0*( wVel(i-1,j,k,bi,bj)*rA(i-1,j,bi,bj)
     &                   +wVel( i ,j,k,bi,bj)*rA( i ,j,bi,bj)
     &                  )*deepFac2F(k)*rhoFacF(k)
          rTransV(i,j) =
     &         0.5 _d 0*( wVel(i,j-1,k,bi,bj)*rA(i,j-1,bi,bj)
     &                   +wVel(i, j ,k,bi,bj)*rA(i, j ,bi,bj)
     &                  )*deepFac2F(k)*rhoFacF(k)
         ENDDO
        ENDDO
      ENDIF

#ifdef NONLIN_FRSURF
C---  Modify rTransU & rTransV when using r* coordinate:
C     note: not implemented neither for anelastic nor deep-model.
      IF ( select_rStar.NE.0 ) THEN
# ifndef DISABLE_RSTAR_CODE

       IF ( k.EQ.1) THEN
C-    Initialise dWtrans :
        DO j=1-OLy,sNy+OLy
         DO i=1-OLx,sNx+OLx
          dWtransC(i,j,bi,bj) = rStarDhCDt(i,j,bi,bj)
     &         *(Ro_surf(i,j,bi,bj)-R_low(i,j,bi,bj))
     &         *rA(i,j,bi,bj)
         ENDDO
        ENDDO
        DO j=1-OLy+1,sNy+OLy
         DO i=1-OLx+1,sNx+OLx
          dWtransU(i,j,bi,bj) =
     &          0.5 _d 0*(dWtransC(i-1,j,bi,bj)+dWtransC(i,j,bi,bj))
          dWtransV(i,j,bi,bj) =
     &          0.5 _d 0*(dWtransC(i,j-1,bi,bj)+dWtransC(i,j,bi,bj))
         ENDDO
        ENDDO

       ELSEIF (k.LE.Nr) THEN
C-    Update dWtrans from previous value (interface k-1):
        DO j=1-OLy,sNy+OLy
         DO i=1-OLx,sNx+OLx
          dWtransC(i,j,bi,bj) = dWtransC(i,j,bi,bj)
     &     - rStarDhCDt(i,j,bi,bj)*drF(k-1)*h0FacC(i,j,k-1,bi,bj)
     &                            *rA(i,j,bi,bj)
         ENDDO
        ENDDO
        DO j=1-OLy+1,sNy+OLy
         DO i=1-OLx+1,sNx+OLx
          dWtransU(i,j,bi,bj) = dWtransU(i,j,bi,bj)
     &     - rStarDhWDt(i,j,bi,bj)*drF(k-1)*h0FacW(i,j,k-1,bi,bj)
     &                            *rAw(i,j,bi,bj)
          dWtransV(i,j,bi,bj) = dWtransV(i,j,bi,bj)
     &     - rStarDhSDt(i,j,bi,bj)*drF(k-1)*h0FacS(i,j,k-1,bi,bj)
     &                            *rAs(i,j,bi,bj)
         ENDDO
        ENDDO
C-    Modify rTransU & rTransV :
        DO j=1-OLy+1,sNy+OLy
         DO i=1-OLx+1,sNx+OLx
          rTransU(i,j) = rTransU(i,j)-dWtransU(i,j,bi,bj)
     &       + (dWtransC(i-1,j,bi,bj)+dWtransC(i,j,bi,bj))*0.5 _d 0
          rTransV(i,j) = rTransV(i,j)-dWtransV(i,j,bi,bj)
     &       + (dWtransC(i,j-1,bi,bj)+dWtransC(i,j,bi,bj))*0.5 _d 0
         ENDDO
        ENDDO

       ENDIF

# endif /* DISABLE_RSTAR_CODE */
      ENDIF

#endif /* NONLIN_FRSURF */

      RETURN
      END

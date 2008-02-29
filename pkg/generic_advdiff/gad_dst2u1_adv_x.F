C $Header: /u/gcmpack/MITgcm/pkg/generic_advdiff/gad_dst2u1_adv_x.F,v 1.8 2008/02/29 01:30:59 mlosch Exp $
C $Name:  $

#include "GAD_OPTIONS.h"

CBOP
C !ROUTINE: GAD_DST2U1_ADV_X

C !INTERFACE: ==========================================================
      SUBROUTINE GAD_DST2U1_ADV_X(
     I           bi,bj,k, advectionScheme, calcCFL,
     I           deltaTloc, uTrans, uFld,
     I           tracer,
     O           uT,
     I           myThid )

C !DESCRIPTION:
C  Calculates the area integrated zonal flux due to advection
C  of a tracer using second-order Direct Space and Time (DST-2)
C  interpolation (=Lax-Wendroff) or simple 1rst order upwind scheme.

C !USES: ===============================================================
      IMPLICIT NONE
#include "SIZE.h"
#include "GRID.h"
#include "GAD.h"

C !INPUT PARAMETERS: ===================================================
C  bi,bj             :: tile indices
C  k                 :: vertical level
C  advectionScheme   :: advection scheme to use: either 2nd Order DST
C                                                or 1rst Order Upwind
C  calcCFL           :: =T: calculate CFL number ; =F: take uFld as CFL
C  deltaTloc         :: local time-step (s)
C  uTrans            :: zonal volume transport
C  uFld              :: zonal flow / CFL number
C  tracer            :: tracer field
C  myThid            :: thread number
      INTEGER bi,bj,k
      INTEGER advectionScheme
      LOGICAL calcCFL
      _RL deltaTloc
      _RL uTrans(1-OLx:sNx+OLx,1-OLy:sNy+OLy)
      _RL uFld  (1-OLx:sNx+OLx,1-OLy:sNy+OLy)
      _RL tracer(1-OLx:sNx+OLx,1-OLy:sNy+OLy)
      INTEGER myThid

C !OUTPUT PARAMETERS: ==================================================
C  uT                :: zonal advective flux
      _RL uT    (1-OLx:sNx+OLx,1-OLy:sNy+OLy)

C !LOCAL VARIABLES: ====================================================
C  i,j               :: loop indices
C  rLimit            :: centered (vs upwind) fraction
C  uCFL              :: Courant-Friedrich-Levy number
      INTEGER i,j
      _RL uCFL, xLimit, uAbs
CEOP

      xLimit = 0. _d 0
      IF ( advectionScheme.EQ.ENUM_DST2 ) xLimit = 1. _d 0

      DO j=1-Oly,sNy+Oly
       uT(1-Olx,j)=0.
      ENDDO
      DO j=1-Oly,sNy+Oly
       DO i=1-Olx+1,sNx+Olx

        uCFL = uFld(i,j)
        IF ( calcCFL ) uCFL = ABS( uFld(i,j)*deltaTloc
     &                  *recip_dxC(i,j,bi,bj)*recip_deepFacC(k) )

c       uT(i,j) =
c    &     uTrans(i,j)*(tracer(i-1,j)+tracer(i,j))*0.5 _d 0
c    &   + ( 1. _d 0 - xLimit*(1. _d 0 - uCFL) )*ABS(uTrans(i,j))
c    &                *(tracer(i-1,j)-tracer(i,j))*0.5 _d 0
C-- above formulation produces large truncation error when:
C    1rst.O upWind and   u > 0 & |tracer(i-1,j)| << |tracer(i,j)|
C                   or   u < 0 & |tracer(i-1,j)| >> |tracer(i,j)|
C-- change to a more robust expression:
        uAbs = ABS(uTrans(i,j))
     &       *( 1. _d 0 - xLimit*(1. _d 0 - uCFL) )
        uT(i,j) = ( uTrans(i,j)+uAbs )* 0.5 _d 0 * tracer(i-1,j)
     &          + ( uTrans(i,j)-uAbs )* 0.5 _d 0 * tracer(i,j)
       ENDDO
      ENDDO

      RETURN
      END

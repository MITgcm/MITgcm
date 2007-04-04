C $Header: /u/gcmpack/MITgcm/pkg/generic_advdiff/gad_dst2u1_adv_y.F,v 1.7 2007/04/04 01:39:06 jmc Exp $
C $Name:  $

#include "GAD_OPTIONS.h"

CBOP
C !ROUTINE: GAD_DST2U1_ADV_Y

C !INTERFACE: ==========================================================
      SUBROUTINE GAD_DST2U1_ADV_Y(
     I           bi,bj,k, advectionScheme, calcCFL,
     I           deltaTloc, vTrans, vFld,
     I           tracer,
     O           vT,
     I           myThid )

C !DESCRIPTION:
C  Calculates the area integrated meridional flux due to advection
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
C  calcCFL           :: =T: calculate CFL number ; =F: take vFld as CFL
C  deltaTloc         :: local time-step (s)
C  vTrans            :: meridional volume transport
C  vFld              :: meridional flow / CFL number
C  tracer            :: tracer field
C  myThid            :: thread number
      INTEGER bi,bj, k, advectionScheme
      LOGICAL calcCFL
      _RL deltaTloc
      _RL vTrans(1-OLx:sNx+OLx,1-OLy:sNy+OLy)
      _RL vFld  (1-OLx:sNx+OLx,1-OLy:sNy+OLy)
      _RL tracer(1-OLx:sNx+OLx,1-OLy:sNy+OLy)
      INTEGER myThid

C !OUTPUT PARAMETERS: ==================================================
C  vT                :: meridional advective flux
      _RL vT    (1-OLx:sNx+OLx,1-OLy:sNy+OLy)

C !LOCAL VARIABLES: ====================================================
C  i,j               :: loop indices
C  yLimit            :: centered (vs upwind) fraction
C  vCFL              :: Courant-Friedrich-Levy number
      INTEGER i,j
      _RL vCFL, yLimit, vAbs
CEOP

      yLimit = 0. _d 0
      IF ( advectionScheme.EQ.ENUM_DST2 ) yLimit = 1. _d 0

      DO i=1-Olx,sNx+Olx
       vT(i,1-Oly)=0.
      ENDDO
      DO j=1-Oly+1,sNy+Oly
       DO i=1-Olx,sNx+Olx

        vCFL = vFld(i,j)
        IF ( calcCFL ) vCFL = ABS( vFld(i,j)*deltaTloc
     &                  *recip_dyC(i,j,bi,bj)*recip_deepFacC(k) )

c       vT(i,j) =
c    &     vTrans(i,j)*(tracer(i,j-1)+tracer(i,j))*0.5 _d 0
c    &   + ( 1. _d 0 - yLimit*(1. _d 0 - vCFL) )*ABS(vTrans(i,j))
c    &                *(tracer(i,j-1)-tracer(i,j))*0.5 _d 0
C-- above formulation produces large truncation error when:
C    1rst.O upWind and   v > 0 & |tracer(i,j-1)| << |tracer(i,j)|
C                   or   v < 0 & |tracer(i,j-1)| >> |tracer(i,j)|
C-- change to a more robust expression:
        vAbs = ABS(vTrans(i,j))
     &       *( 1. _d 0 - yLimit*(1. _d 0 - vCFL) )
        vT(i,j) = ( vTrans(i,j)+vAbs )* 0.5 _d 0 * tracer(i,j-1)
     &          + ( vTrans(i,j)-vAbs )* 0.5 _d 0 * tracer(i,j)

       ENDDO
      ENDDO

      RETURN
      END

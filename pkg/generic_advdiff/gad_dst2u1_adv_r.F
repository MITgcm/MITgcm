C $Header: /u/gcmpack/MITgcm/pkg/generic_advdiff/gad_dst2u1_adv_r.F,v 1.1 2005/10/22 19:56:32 jmc Exp $
C $Name:  $

#include "GAD_OPTIONS.h"

CBOP
C !ROUTINE: GAD_DST2U1_ADV_R

C !INTERFACE: ==========================================================
      SUBROUTINE GAD_DST2U1_ADV_R( 
     I           bi,bj,k, advectionScheme,
     I           deltaTloc, rTrans, wVel,
     I           tracer,
     O           wT,
     I           myThid )

C !DESCRIPTION:
C  Calculates the area integrated vertical flux due to advection
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
C  deltaTloc         :: local time-step (s)
C  rTrans            :: vertical volume transport
C  wVel              :: vertical flow
C  tracer            :: tracer field
C  myThid            :: thread number
      INTEGER bi,bj,k
      INTEGER advectionScheme
      _RL deltaTloc
      _RL rTrans(1-OLx:sNx+OLx,1-OLy:sNy+OLy)
      _RL wVel  (1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nr,nSx,nSy)
      _RL tracer(1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nr)
      INTEGER myThid

C !OUTPUT PARAMETERS: ==================================================
C  wT                :: vertical advective flux
      _RL wT    (1-OLx:sNx+OLx,1-OLy:sNy+OLy)

C !LOCAL VARIABLES: ====================================================
C  i,j               :: loop indices
C  km1               :: =max( k-1 , 1 )
C  rLimit            :: centered (vs upwind) fraction
C  wFld              :: velocity, vertical component
C  wCFL              :: Courant-Friedrich-Levy number
      INTEGER i,j,km1
      _RL wFld, wCFL, rLimit
CEOP

      rLimit = 0. _d 0
      IF ( advectionScheme.EQ.ENUM_DST2 ) rLimit = 1. _d 0

      km1=MAX(1,k-1)

      IF ( k.LE.1 .OR. k.GT.Nr) THEN
       DO j=1-Oly,sNy+Oly
        DO i=1-Olx,sNx+Olx
         wT(i,j) = 0.
        ENDDO
       ENDDO
      ELSE
       DO j=1-Oly,sNy+Oly
        DO i=1-Olx,sNx+Olx

c        wFld = wVel(i,j,k,bi,bj)
         wFld = rTrans(i,j)*recip_rA(i,j,bi,bj)
         wCFL = ABS(wFld*deltaTloc*recip_drC(k))

         wT(i,j) = maskC(i,j,kM1,bi,bj)*(
     &     rTrans(i,j)*(Tracer(i,j,km1)+Tracer(i,j,k))*0.5 _d 0
     &   + ( 1. _d 0 - rLimit*(1. _d 0 - wCFL) )*ABS(rTrans(i,j))
     &                *(tracer(i,j,km1)-tracer(i,j,k))*0.5 _d 0*rkSign
     &                                  )
        ENDDO
       ENDDO
      ENDIF

      RETURN
      END

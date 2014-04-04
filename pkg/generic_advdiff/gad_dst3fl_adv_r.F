C $Header: /u/gcmpack/MITgcm/pkg/generic_advdiff/gad_dst3fl_adv_r.F,v 1.11 2014/04/04 20:29:08 jmc Exp $
C $Name:  $

#include "GAD_OPTIONS.h"

CBOP
C !ROUTINE: GAD_DST3FL_ADV_R

C !INTERFACE: ==========================================================
      SUBROUTINE GAD_DST3FL_ADV_R(
     I           bi,bj,k,dTarg,
     I           rTrans, wFld,
     I           tracer,
     O           wT,
     I           myThid )

C !DESCRIPTION:
C  Calculates the area integrated vertical flux due to advection of a tracer
C  using 3rd Order DST Scheme with flux limiting

C !USES: ===============================================================
      IMPLICIT NONE

C     == GLobal variables ==
#include "SIZE.h"
#include "GRID.h"
#include "GAD.h"

C     == Routine arguments ==
C !INPUT PARAMETERS: ===================================================
C  bi,bj             :: tile indices
C  k                 :: vertical level
C  deltaTloc         :: local time-step (s)
C  rTrans            :: vertical volume transport
C  wFld              :: vertical flow
C  tracer            :: tracer field
C  myThid            :: thread number
      INTEGER bi,bj,k
      _RL dTarg
      _RL rTrans(1-OLx:sNx+OLx,1-OLy:sNy+OLy)
      _RL wFld  (1-OLx:sNx+OLx,1-OLy:sNy+OLy)
      _RL tracer(1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nr)
      INTEGER myThid

C !OUTPUT PARAMETERS: ==================================================
C  wT                :: vertical advective flux
      _RL wT    (1-OLx:sNx+OLx,1-OLy:sNy+OLy)

C     == Local variables ==
C !LOCAL VARIABLES: ====================================================
C  i,j               :: loop indices
C  km1               :: =max( k-1 , 1 )
C  wLoc              :: velocity, vertical component
C  wCFL              :: Courant-Friedrich-Levy number
      INTEGER i,j,kp1,km1,km2
      _RL Rjm,Rj,Rjp,wCFL,d0,d1
      _RL psiP,psiM,thetaP,thetaM
      _RL wLoc
      _RL thetaMax
      PARAMETER( thetaMax = 1.D+20 )

      km2=MAX(1,k-2)
      km1=MAX(1,k-1)
      kp1=MIN(Nr,k+1)

      DO j=1-OLy,sNy+OLy
       DO i=1-OLx,sNx+OLx
#if (defined ALLOW_AUTODIFF && defined TARGET_NEC_SX)
C     These lines make TAF create vectorizable code
        thetaP = 0. _d 0
        thetaM = 0. _d 0
#endif
        Rjp=(tracer(i,j,k)-tracer(i,j,kp1))
     &         *maskC(i,j,kp1,bi,bj)
        Rj =(tracer(i,j,km1)-tracer(i,j,k))
     &         *maskC(i,j,k,bi,bj)*maskC(i,j,km1,bi,bj)
        Rjm=(tracer(i,j,km2)-tracer(i,j,km1))
     &         *maskC(i,j,km1,bi,bj)

        wLoc = wFld(i,j)
        wCFL = ABS(wLoc*dTarg*recip_drC(k))
        d0=(2. _d 0 -wCFL)*(1. _d 0 -wCFL)*oneSixth
        d1=(1. _d 0 -wCFL*wCFL)*oneSixth

C-      the old version: can produce overflow, division by zero,
C       and is wrong for tracer with low concentration:
c       thetaP=Rjm/(1.D-20+Rj)
c       thetaM=Rjp/(1.D-20+Rj)
C-      the right expression, but not bounded:
c       thetaP=0.D0
c       thetaM=0.D0
c       IF (Rj.NE.0.D0) thetaP=Rjm/Rj
c       IF (Rj.NE.0.D0) thetaM=Rjp/Rj
C-      prevent |thetaP,M| to reach too big value:
        IF ( ABS(Rj)*thetaMax .LE. ABS(Rjm) ) THEN
          thetaP=SIGN(thetaMax,Rjm*Rj)
        ELSE
          thetaP=Rjm/Rj
        ENDIF
        IF ( ABS(Rj)*thetaMax .LE. ABS(Rjp) ) THEN
          thetaM=SIGN(thetaMax,Rjp*Rj)
        ELSE
          thetaM=Rjp/Rj
        ENDIF

        psiP=d0+d1*thetaP
        psiP=MAX(0. _d 0,MIN(MIN(1. _d 0,psiP),
     &                       thetaP*(1. _d 0 -wCFL)/(wCFL+1. _d -20) ))
        psiM=d0+d1*thetaM
        psiM=MAX(0. _d 0,MIN(MIN(1. _d 0,psiM),
     &                       thetaM*(1. _d 0 -wCFL)/(wCFL+1. _d -20) ))

        wT(i,j)=
     &   0.5*(rTrans(i,j)+ABS(rTrans(i,j)))
     &      *( tracer(i,j, k ) + psiM*Rj )
     &  +0.5*(rTrans(i,j)-ABS(rTrans(i,j)))
     &      *( tracer(i,j,km1) - psiP*Rj )

       ENDDO
      ENDDO

      RETURN
      END

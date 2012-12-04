C $Header: /u/gcmpack/MITgcm/pkg/cheapaml/cheapaml_gad_dst3fl_adv_r.F,v 1.3 2012/12/04 17:26:10 jmc Exp $
C $Name:  $

#include "GAD_OPTIONS.h"

CBOP
C !ROUTINE: GAD_DST3FL_ADV_R

C !INTERFACE: ==========================================================
      SUBROUTINE cheapaml_GAD_DST3FL_ADV_R(
     I           bi,bj,dTarg,
     I           rTrans,
     I           tracer,
     O           wT,
     I           myThid )

C !DESCRIPTION:
C  Calculates the area integrated vertical flux due to advection of a tracer
C  using 3rd Order DST Scheme with flux limiting
c  modified for use in Cheapaml

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
C  tracer            :: tracer field
C  myThid            :: thread number
      INTEGER bi,bj
      _RL dTarg
      _RL rTrans(1-OLx:sNx+OLx,1-OLy:sNy+OLy)
      _RL tracer(1-OLx:sNx+OLx,1-OLy:sNy+OLy)
      INTEGER myThid

C !OUTPUT PARAMETERS: ==================================================
C  wT                :: vertical advective flux
      _RL wT    (1-OLx:sNx+OLx,1-OLy:sNy+OLy)

C     == Local variables ==
C !LOCAL VARIABLES: ====================================================
C  i,j               :: loop indices
C  wLoc              :: velocity, vertical component
C  wCFL              :: Courant-Friedrich-Levy number
      INTEGER i,j
      _RL Rjm,Rj,Rjp,wCFL,d0,d1
      _RL psiP,psiM,thetaP,thetaM
      _RL wLoc
      _RL thetaMax
      PARAMETER( thetaMax = 1.D+20 )

      Rjp=  0. _d 0
      Rj =  0. _d 0
      Rjm = 0. _d 0

c     DO j=1-OLy,sNy+OLy
c      DO i=1-OLx,sNx+OLx
      DO j=1,sNy
       DO i=1,sNx

        wLoc = rTrans(i,j)
        wCFL = ABS(wLoc*dTarg*recip_drC(1))
        d0=(2. _d 0 -wCFL)*(1. _d 0 -wCFL)*oneSixth
        d1=(1. _d 0 -wCFL*wCFL)*oneSixth

C-      prevent |thetaP,M| to reach too big value:
          thetaP=SIGN(thetaMax,Rjm*Rj)
          thetaM=SIGN(thetaMax,Rjp*Rj)

        psiP=d0+d1*thetaP
        psiP=MAX(0. _d 0,MIN(MIN(1. _d 0,psiP),
     &                       thetaP*(1. _d 0 -wCFL)/(wCFL+1. _d -20) ))
        psiM=d0+d1*thetaM
        psiM=MAX(0. _d 0,MIN(MIN(1. _d 0,psiM),
     &                       thetaM*(1. _d 0 -wCFL)/(wCFL+1. _d -20) ))

        wT(i,j)=
     &   0.5*(rTrans(i,j)+ABS(rTrans(i,j)))
     &      *( tracer(i,j ) + psiM*Rj )
     &  +0.5*(rTrans(i,j)-ABS(rTrans(i,j)))
     &      *( tracer(i,j) - psiP*Rj )

       ENDDO
      ENDDO

      RETURN
      END

#include "GAD_OPTIONS.h"

CBOP
C !ROUTINE: GAD_DST3_ADV_R

C !INTERFACE: ==========================================================
      SUBROUTINE GAD_DST3_ADV_R(
     I           bi,bj,k,dTarg,
     I           rTrans, wFld,
     I           tracer,
     O           wT,
     I           myThid )

C !DESCRIPTION:
C  Calculates the area integrated vertical flux due to advection of a tracer
C  using 3rd-order Direct Space and Time (DST-3) Advection Scheme

C !USES: ===============================================================
      IMPLICIT NONE

C     == GLobal variables ==
#include "SIZE.h"
#ifdef OLD_DST3_FORMULATION
#include "EEPARAMS.h"
#include "PARAMS.h"
#endif
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
      _RL wLoc
      _RL Rjm,Rj,Rjp,cfl,d0,d1
#ifdef OLD_DST3_FORMULATION
      _RL psiP,psiM,thetaP,thetaM
      _RL smallNo

c     IF (inAdMode .AND. useApproxAdvectionInAdMode) THEN
c      smallNo = 1.0D-20
c     ELSE
       smallNo = 1.0D-20
c     ENDIF
#endif

      km2=MAX(1,k-2)
      km1=MAX(1,k-1)
      kp1=MIN(Nr,k+1)

      DO j=1-OLy,sNy+OLy
       DO i=1-OLx,sNx+OLx
        Rjp=(tracer(i,j,k)-tracer(i,j,kp1))
     &         *maskC(i,j,kp1,bi,bj)
        Rj =(tracer(i,j,km1)-tracer(i,j,k))
     &         *maskC(i,j,k,bi,bj)*maskC(i,j,km1,bi,bj)
        Rjm=(tracer(i,j,km2)-tracer(i,j,km1))
     &         *maskC(i,j,km1,bi,bj)

        wLoc = wFld(i,j)
c       wLoc = rTrans(i,j)*recip_rA(i,j,bi,bj)
        cfl=ABS(wLoc*dTarg*recip_drC(k))
        d0=(2.-cfl)*(1.-cfl)*oneSixth
        d1=(1.-cfl*cfl)*oneSixth
#ifdef OLD_DST3_FORMULATION
        IF ( ABS(Rj).LT.smallNo .OR.
     &       ABS(Rjm).LT.smallNo ) THEN
         thetaP=0.
         psiP=0.
        ELSE
         thetaP=(Rjm+smallNo)/(smallNo+Rj)
         psiP=d0+d1*thetaP
        ENDIF
        IF ( ABS(Rj).LT.smallNo .OR.
     &       ABS(Rjp).LT.smallNo ) THEN
         thetaM=0.
         psiM=0.
        ELSE
         thetaM=(Rjp+smallNo)/(smallNo+Rj)
         psiM=d0+d1*thetaM
        ENDIF
         wT(i,j)=
     &    0.5*(rTrans(i,j)+ABS(rTrans(i,j)))
     &       *( tracer(i,j, k ) + psiM*Rj )
     &   +0.5*(rTrans(i,j)-ABS(rTrans(i,j)))
     &       *( tracer(i,j,km1) - psiP*Rj )
#else /* OLD_DST3_FORMULATION */
        wT(i,j)=
     &    0.5*(rTrans(i,j)+ABS(rTrans(i,j)))
     &       *( tracer(i,j, k ) + (d0*Rj+d1*Rjp) )
     &   +0.5*(rTrans(i,j)-ABS(rTrans(i,j)))
     &       *( tracer(i,j,km1) - (d0*Rj+d1*Rjm) )
#endif /* OLD_DST3_FORMULATION */

       ENDDO
      ENDDO

      RETURN
      END

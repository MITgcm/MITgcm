C $Header: /u/gcmpack/MITgcm/pkg/generic_advdiff/gad_dst3_adv_x.F,v 1.10 2006/10/22 01:08:04 jmc Exp $
C $Name:  $

#include "GAD_OPTIONS.h"

CBOP
C !ROUTINE: GAD_DST3_ADV_X

C !INTERFACE: ==========================================================
      SUBROUTINE GAD_DST3_ADV_X(
     I           bi,bj,k,deltaTloc,
     I           uTrans, uFld,
     I           maskLocW, tracer,
     O           uT,
     I           myThid )

C !DESCRIPTION:
C  Calculates the area integrated zonal flux due to advection of a
C  tracer using 3rd-order Direct Space and Time (DST-3) Advection Scheme

C !USES: ===============================================================
      IMPLICIT NONE

C     == GLobal variables ==
#include "SIZE.h"
#include "GRID.h"
#include "EEPARAMS.h"
#include "PARAMS.h"
#include "GAD.h"

C     == Routine arguments ==
C !INPUT PARAMETERS: ===================================================
C  bi,bj             :: tile indices
C  k                 :: vertical level
C  deltaTloc         :: local time-step (s)
C  uTrans            :: zonal volume transport
C  uFld              :: zonal flow
C  tracer            :: tracer field
C  myThid            :: thread number
      INTEGER bi,bj,k
      _RL deltaTloc
      _RL uTrans(1-OLx:sNx+OLx,1-OLy:sNy+OLy)
      _RL uFld  (1-OLx:sNx+OLx,1-OLy:sNy+OLy)
      _RS maskLocW(1-OLx:sNx+OLx,1-OLy:sNy+OLy)
      _RL tracer(1-OLx:sNx+OLx,1-OLy:sNy+OLy)
      INTEGER myThid

C !OUTPUT PARAMETERS: ==================================================
C  uT                :: zonal advective flux
      _RL uT    (1-OLx:sNx+OLx,1-OLy:sNy+OLy)

C     == Local variables ==
C !LOCAL VARIABLES: ====================================================
C  i,j               :: loop indices
C  uLoc              :: velocity [m/s], zonal component
C  cfl               :: Courant-Friedrich-Levy number
      INTEGER i,j
      _RL uLoc
      _RL Rjm,Rj,Rjp,cfl,d0,d1
#ifdef OLD_DST3_FORMULATION
      _RL psiP,psiM,thetaP,thetaM
      _RL smallNo
c     _RL Rjjm,Rjjp

      IF (inAdMode) THEN
       smallNo = 1.0D-20
      ELSE
       smallNo = 1.0D-20
      ENDIF
#endif

      DO j=1-Oly,sNy+Oly
       uT(1-Olx,j)=0.
       uT(2-Olx,j)=0.
       uT(sNx+Olx,j)=0.
       DO i=1-Olx+2,sNx+Olx-1
        Rjp=(tracer(i+1,j)-tracer( i ,j))*maskLocW(i+1,j)
        Rj =(tracer( i ,j)-tracer(i-1,j))*maskLocW( i ,j)
        Rjm=(tracer(i-1,j)-tracer(i-2,j))*maskLocW(i-1,j)

        uLoc = uFld(i,j)
c       uLoc = uTrans(i,j)*recip_dyG(i,j,bi,bj)
c    &       *recip_drF(k)*_recip_hFacW(i,j,k,bi,bj)
        cfl=ABS(uLoc*deltaTloc*recip_dxC(i,j,bi,bj))
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
        uT(i,j)=
     &   0.5*(uTrans(i,j)+ABS(uTrans(i,j)))
     &      *( Tracer(i-1,j) + psiP*Rj )
     &  +0.5*(uTrans(i,j)-ABS(uTrans(i,j)))
     &      *( Tracer( i ,j) - psiM*Rj )
#else /* OLD_DST3_FORMULATION */
        uT(i,j)=
     &   0.5*(uTrans(i,j)+ABS(uTrans(i,j)))
     &      *( Tracer(i-1,j) + (d0*Rj+d1*Rjm) )
     &  +0.5*(uTrans(i,j)-ABS(uTrans(i,j)))
     &      *( Tracer( i ,j) - (d0*Rj+d1*Rjp) )
#endif /* OLD_DST3_FORMULATION */

       ENDDO
      ENDDO

      RETURN
      END

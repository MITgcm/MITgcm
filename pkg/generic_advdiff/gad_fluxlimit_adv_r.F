C $Header: /u/gcmpack/MITgcm/pkg/generic_advdiff/gad_fluxlimit_adv_r.F,v 1.3 2001/09/10 01:22:48 adcroft Exp $
C $Name:  $

#include "GAD_OPTIONS.h"

      SUBROUTINE GAD_FLUXLIMIT_ADV_R( 
     I           bi_arg,bj_arg,k,dTarg,
     I           rTrans, wVel,
     I           tracer,
     O           wT,
     I           myThid )
C     /==========================================================\
C     | SUBROUTINE GAD_FLUXLIMIT_ADV_R                           |
C     | o Compute vertical advective Flux of Tracer using        |
C     |   Flux Limiter Scheme                                    |
C     |==========================================================|
      IMPLICIT NONE

C     == GLobal variables ==
#include "SIZE.h"
#include "GRID.h"
#include "EEPARAMS.h"
#include "PARAMS.h"

C     == Routine arguments ==
      INTEGER bi_arg,bj_arg,k
      _RL dTarg
      _RL rTrans(1-OLx:sNx+OLx,1-OLy:sNy+OLy)
      _RL wVel  (1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nr,nSx,nSy)
      _RL tracer(1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nr,nSx,nSy)
      _RL wT    (1-OLx:sNx+OLx,1-OLy:sNy+OLy)
      INTEGER myThid

C     == Local variables ==
      INTEGER i,j,kp1,km1,km2,bi,bj
      _RL Cr,Rjm,Rj,Rjp
#include "GAD_FLUX_LIMITER.h"

      IF (.NOT. multiDimAdvection) THEN
C      If using the standard time-stepping/advection schemes (ie. AB-II)
C      then the data-structures are all global arrays
       bi=bi_arg
       bj=bj_arg
      ELSE
C      otherwise if using the multi-dimensional advection schemes
C      then the data-structures are all local arrays except
C      for maskC(...) and wVel(...)
       bi=1
       bj=1
      ENDIF

      km2=MAX(1,k-2)
      km1=MAX(1,k-1)
      kp1=MIN(Nr,k+1)

      IF ( k.GT.Nr) THEN
       DO j=1-Oly,sNy+Oly
        DO i=1-Olx,sNx+Olx
         wT(i,j) = 0.
        ENDDO
       ENDDO
      ELSE
       DO j=1-Oly,sNy+Oly
        DO i=1-Olx,sNx+Olx
         Rjp=(tracer(i,j,kp1,bi,bj)-tracer(i,j,k,bi,bj))
     &        *maskC(i,j,kp1,bi,bj)
         Rj=(tracer(i,j,k,bi,bj)-tracer(i,j,kM1,bi,bj))
         Rjm=(tracer(i,j,km1,bi,bj)-tracer(i,j,kM2,bi,bj))
     &        *maskC(i,j,km2,bi,bj)
         IF (Rj.NE.0.) THEN
          IF (rTrans(i,j).LT.0) THEN
            Cr=Rjm/Rj
          ELSE
            Cr=Rjp/Rj
          ENDIF
         ELSE
          IF (rTrans(i,j).LT.0) THEN
            Cr=Rjm*1.E20
          ELSE
            Cr=Rjp*1.E20
          ENDIF
         ENDIF
         Cr=Limiter(Cr)
         wT(i,j) = maskC(i,j,kM1,bi_arg,bj_arg)*(
     &     rTrans(i,j)*
     &        (Tracer(i,j,k,bi,bj)+Tracer(i,j,kM1,bi,bj))*0.5 _d 0
     &    +(ABS(rTrans(i,j))*(1-Cr)
     &      +rTrans(i,j)*wVel(i,j,k,bi_arg,bj_arg)*dTarg*recip_drC(k)
     &                  *Cr
     &     )*Rj*0.5 _d 0                )
        ENDDO
       ENDDO
      ENDIF

      RETURN
      END

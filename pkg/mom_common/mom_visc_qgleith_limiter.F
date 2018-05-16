#include "MOM_COMMON_OPTIONS.h"

C     !ROUTINE: MOM_VISC_QGLEITH_LIMITER
C     !INTERFACE:
      SUBROUTINE MOM_VISC_QGLEITH_LIMITER(
     I                           bi, bj, k,
     O                           stretching,
     I                           Nsq, uFld, vFld, vort3, myThid )

      IMPLICIT NONE

C     == Global variables ==
#include "SIZE.h"
#include "GRID.h"
#include "EEPARAMS.h"
#include "PARAMS.h"
#ifdef ALLOW_AUTODIFF_TAMC
#include "tamc.h"
#include "tamc_keys.h"
#endif /* ALLOW_AUTODIFF_TAMC */
#include "MOM_VISC.h"

C#include "PACKAGES_CONFIG.h"

C     == Routine arguments ==
      INTEGER bi,bj,k
      _RL stretching(1-OLx:sNx+OLx,1-OLy:sNy+OLy)
      _RL Nsq(1-OLx:sNx+OLx,1-OLy:sNy+OLy)
      _RL uFld(1-OLx:sNx+OLx,1-OLy:sNy+OLy)
      _RL vFld(1-OLx:sNx+OLx,1-OLy:sNy+OLy)
      _RL vort3(1-OLx:sNx+OLx,1-OLy:sNy+OLy)

      INTEGER myThid

C     == Local variables ==
      INTEGER i,j

      _RL U_scale(1-OLx:sNx+OLx,1-OLy:sNy+OLy)
      _RL Ro_g2(1-OLx:sNx+OLx,1-OLy:sNy+OLy)
      _RL Bu_g(1-OLx:sNx+OLx,1-OLy:sNy+OLy)
      _RL Fr_g(1-OLx:sNx+OLx,1-OLy:sNy+OLy)
      _RL stretching_hold(1-OLx:sNx+OLx,1-OLy:sNy+OLy)


C Put a cap on the stretching term.

       DO j=2-OLy,sNy+OLy-1
        DO I=2-OLx,sNx+OLx-1
         U_scale(i,j) = 0.5* SQRT(
     &           ( uFld( i , j )*uFld( i , j )
     &            +uFld(i+1, j )*uFld(i+1, j ) )
     &         + ( vFld( i , j )*vFld( i , j )
     &            +vFld( i ,j+1)*vFld( i ,j+1) )
     &                  )

C        Grid scale Rossby number, squared: U^2 / (fL)
         Ro_g2(i,j) = U_scale(i,j)**2 * recip_rA(i,j,bi,bj) / (f0**2)
C        Grid scale Burger number: N^2 H^2 / (f^2 L^2)
C         Bu_g(i,j) = Nsq(i,j) * drf(k)**2 * recip_rA(i,j,bi,bj) /
C     &                 (f0**2 * PI)
C        Grid scale Froude number: U pi /(N^2 H)
C          Include a small number to prevent division by zero
         Fr_g(i,j) = U_scale(i,j) * PI /
     &                 ((Nsq(i,j) + 1. _d -12) * drf(k))

C        Implement eqn. (55) from Bachman et al. (2017) JGR-Oceans
C         stretching_hold(i,j) = MIN( ABS(stretching(i,j)),
C     &    ABS(vort3(i,j) / MAX(Bu_g(i,j),Ro_g2(i,j)) ) )

C        Implement eqn. (56) from Bachman et al. (2017) JGR-Oceans
C          This limiter goes to 2D Leith as stratification -> 0
         stretching_hold(i,j) = min( ABS(stretching(i,j)),
     &    ABS(vort3(i,j) * Fr_g(i,j)**2 / (Ro_g2(i,j) + Fr_g(i,j)**4)))

         stretching(i,j) = SIGN(stretching_hold(i,j), stretching(i,j))

        ENDDO
       ENDDO


      RETURN
      END

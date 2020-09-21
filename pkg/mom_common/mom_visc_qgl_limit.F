#include "MOM_COMMON_OPTIONS.h"

CBOP
C !ROUTINE: MOM_VISC_QGL_LIMIT

C !INTERFACE: ==========================================================
      SUBROUTINE MOM_VISC_QGL_LIMIT(
     I                           bi, bj, k,
     O                           stretching,
     I                           Nsquare,
     I                           uFld, vFld, vort3,
     I                           myTime, myIter, myThid )

C     !DESCRIPTION:
C     *==========================================================*
C     | SUBROUTINE MOM_VISC_QGL_LIMIT
C     | Limit the contribution of the vortex stretching term.
C     | When the flow is unstratified, the algorithm tries to
C     | divide by zero. This subroutine ensures that the result
C     | remains finite.
C     *==========================================================*

C !USES: ===============================================================
      IMPLICIT NONE

C     == Global variables ==
#include "SIZE.h"
#include "EEPARAMS.h"
#include "PARAMS.h"
#include "GRID.h"
c#include "DYNVARS.h"
c#include "MOM_VISC.h"

C !INPUT PARAMETERS: ===================================================
C  bi,bj                :: tile indices
C  k                    :: vertical level
C  Nsquare              :: buoyancy frequency
C  uFld                 :: U velocity (east flow on spherical grid)
C  vFld                 :: V velocity (north flow on spherical grid)
C  vort3                :: Relative vorticity
C  myTime               :: current time of simulation ( s )
C  myIter               :: current iteration number of simulation
C  myThid               :: my Thread Id number
      INTEGER bi,bj,k
      _RL Nsquare(1-OLx:sNx+OLx,1-OLy:sNy+OLy)
      _RL uFld(1-OLx:sNx+OLx,1-OLy:sNy+OLy)
      _RL vFld(1-OLx:sNx+OLx,1-OLy:sNy+OLy)
      _RL vort3(1-OLx:sNx+OLx,1-OLy:sNy+OLy)

      _RL     myTime
      INTEGER myIter
      INTEGER myThid

C !OUTPUT PARAMETERS: ==================================================
C  stretching           :: vortex stretching term (to be limited)
      _RL stretching(1-OLx:sNx+OLx,1-OLy:sNy+OLy)

#ifdef ALLOW_LEITH_QG

C !LOCAL VARIABLES: ====================================================
      INTEGER i,j
      _RL U_scale_sq(1-OLx:sNx+OLx,1-OLy:sNy+OLy)
      _RL Ro_g_sq(1-OLx:sNx+OLx,1-OLy:sNy+OLy)
C      _RL Bu_g(1-OLx:sNx+OLx,1-OLy:sNy+OLy)
      _RL Fr_g_sq(1-OLx:sNx+OLx,1-OLy:sNy+OLy)
      _RL stretching_hold(1-OLx:sNx+OLx,1-OLy:sNy+OLy)
      _RL QGL_lim_epsil
      _RL vort3C

CEOP

C Initialise local variables
      DO j=1-OLy,sNy+OLy
       DO i=1-OLx,sNx+OLx
C       square of the velocity scale
        U_scale_sq(i,j)         = 0. _d 0
C       square of the gridscale Rossby number
        Ro_g_sq(i,j)           = 0. _d 0
C       Gridscale Burger number
C        Bu_g(i,j)            = 0. _d 0
C       Square of the gridscale Froude number
        Fr_g_sq(i,j)            = 0. _d 0
        stretching_hold(i,j) = 0. _d 0
       ENDDO
      ENDDO

      QGL_lim_epsil = 1. _d -24

C Put a cap on the stretching term.

       DO j=2-OLy,sNy+OLy-1
        DO I=2-OLx,sNx+OLx-1
          U_scale_sq(i,j) = 0.5* (
     &             ( uFld(i,j)*uFld(i,j)
     &              + uFld(i+1,j)*uFld(i+1,j) )
     &           + ( vFld(i,j)*vFld(i,j)
     &              + vFld(i,j+1)*vFld(i,j+1) )
     &                             )

C        Grid scale Rossby number, squared: U^2 / (fL)^2
          Ro_g_sq(i,j) = U_scale_sq(i,j) * recip_rA(i,j,bi,bj) /
     &                  MAX(QGL_lim_epsil, fCori(i,j,bi,bj)**2)
C        Grid scale Burger number: N^2 H^2 / (f^2 L^2)
C         Bu_g(i,j) = Nsquare(i,j) * drf(k)**2 * recip_rA(i,j,bi,bj) /
C     &                 (fCori(i,j,bi,bj)**2 * PI)
C        Grid scale Froude number, squared: U^2 pi^2 /(N^2 H)^2
C          Include a small number to prevent division by zero
          Fr_g_sq(i,j) = U_scale_sq(i,j) * PI * PI /
     &                 (MAX((Nsquare(i,j) * drF(k))**2,
     &                      QGL_lim_epsil))

C     Make the scheme gracefully transition to something else
C     as stratification goes to zero
C
C        Implement eqn. (55) from Bachman et al. (2017) JGR-Oceans
C         stretching_hold(i,j) = MIN( ABS(stretching(i,j)),
C     &    ABS(vort3(i,j) / MAX(Bu_g(i,j),Ro_g_sq(i,j)) ) )

C        Implement eqn. (56) from Bachman et al. (2017) JGR-Oceans
C          This limiter goes to 2D Leith as stratification -> 0
          vort3C = halfRL*halfRL*(vort3(i,j) + vort3(i+1,j)
     &                            + vort3(i,j+1) + vort3(i+1,j+1))

          stretching_hold(i,j) = min( ABS(stretching(i,j)),
     &                  ABS(vort3C * Fr_g_sq(i,j) /
     &                       (Ro_g_sq(i,j) + Fr_g_sq(i,j)**2
     &                         + QGL_lim_epsil) )
     &                               )

          stretching(i,j) = SIGN(stretching_hold(i,j), stretching(i,j))

        ENDDO
       ENDDO

#endif /* ALLOW_LEITH_QG */

      RETURN
      END

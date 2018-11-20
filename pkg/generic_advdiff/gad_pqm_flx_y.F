#     include "GAD_OPTIONS.h"

      SUBROUTINE GAD_PQM_FLX_Y(bi,bj,kk,ix,
     &           calc_CFL,delT,vvel,
     &           vfac,fhat,flux,myThid)
C     |================================================================|
C     | PQM_FLX_Y: evaluate PQM flux on grid-cell edges.               |
C     |================================================================|

          implicit none

C     =============================================== global variables
#         include "SIZE.h"
#         include "GRID.h"
#         include "GAD.h"

C     ================================================================
C       bi,bj     :: tile indexing.
C       kk        :: r-index.
C       ix        :: x-index.
C       calc_CFL  :: TRUE to calc. CFL from vel.
C       delT      :: time-step.
C       vvel      :: vel.-comp in y-direction.
C       vfac      :: vel.-flux in y-direction.
C       fhat      :: row of poly. coeff.
C       flux      :: adv.-flux in y-direction.
C       myThid    :: thread number.
C     ================================================================
          integer bi,bj,kk,ix
          logical calc_CFL
          _RL delT
          _RL vvel(1-OLx:sNx+OLx,
     &             1-OLy:sNy+OLy)
          _RL vfac(1-OLx:sNx+OLx,
     &             1-OLy:sNy+OLy)
          _RL fhat(1:5,
     &             1-OLy:sNy+OLy)
          _RL flux(1-OLx:sNx+OLx,
     &             1-OLy:sNy+OLy)
          integer myThid

C     ================================================================
C       iy        :: y-indexing.
C       vCFL      :: CFL number.
C       intF      :: upwind tracer edge-value.
C       ss11,ss22 :: int. endpoints.
C       ivec      :: int. basis vec.
C     ================================================================
          integer iy
          _RL vCFL,intF
          _RL ss11,ss22
          _RL ivec(1:5)

C     ================================================================
C       (1): calc. "departure-points" for each grid-cell edge by int-
C            egrating edge position backward in time over one single
C            time-step. This is a "single-cell" implementation: requ-
C            ires CFL <= 1.0.
C       (2): calc. flux as the integral of the upwind grid-cell poly-
C            nomial over the deformation interval found in (1).
C     ================================================================

          do  iy = 1-OLy+4, sNy+OLy-3

              if (vvel(ix,iy) .eq. 0. _d 0) then

                  flux(ix,iy)    = 0. _d 0

              else

              if (vvel(ix,iy) .gt. 0. _d 0) then

C     ==================== integrate PQM profile over upwind cell IY-1
              if ( calc_CFL ) then
              vCFL = vvel(ix,iy) * delT
     &             * recip_dyF(ix,iy-1,bi,bj)
     &             * recip_deepFacC(kk)
              else
              vCFL = vvel(ix,iy)
              end if

              ss11 = +1. _d 0 - 2. _d 0 * vCFL
              ss22 = +1. _d 0

C     ==================== integrate profile over region swept by face
              ivec(1) = ss22 - ss11
              ivec(2) =(ss22 ** 2
     &                - ss11 ** 2)*(1. _d 0 / 2. _d 0)
              ivec(3) =(ss22 ** 3
     &                - ss11 ** 3)*(1. _d 0 / 3. _d 0)
              ivec(4) =(ss22 ** 4
     &                - ss11 ** 4)*(1. _d 0 / 4. _d 0)
              ivec(5) =(ss22 ** 5
     &                - ss11 ** 5)*(1. _d 0 / 5. _d 0)

              intF = ivec(1) * fhat(1,iy-1)
     &             + ivec(2) * fhat(2,iy-1)
     &             + ivec(3) * fhat(3,iy-1)
     &             + ivec(4) * fhat(4,iy-1)
     &             + ivec(5) * fhat(5,iy-1)

CML              intF = intF / (ss22 - ss11)

              else

C     ==================== integrate PQM profile over upwind cell IY+0
              if ( calc_CFL ) then
              vCFL = vvel(ix,iy) * delT
     &             * recip_dyF(ix,iy-0,bi,bj)
     &             * recip_deepFacC(kk)
              else
              vCFL = vvel(ix,iy)
              end if

              ss11 = -1. _d 0 - 2. _d 0 * vCFL
              ss22 = -1. _d 0

C     ==================== integrate profile over region swept by face
              ivec(1) = ss22 - ss11
              ivec(2) =(ss22 ** 2
     &                - ss11 ** 2)*(1. _d 0 / 2. _d 0)
              ivec(3) =(ss22 ** 3
     &                - ss11 ** 3)*(1. _d 0 / 3. _d 0)
              ivec(4) =(ss22 ** 4
     &                - ss11 ** 4)*(1. _d 0 / 4. _d 0)
              ivec(5) =(ss22 ** 5
     &                - ss11 ** 5)*(1. _d 0 / 5. _d 0)

              intF = ivec(1) * fhat(1,iy-0)
     &             + ivec(2) * fhat(2,iy-0)
     &             + ivec(3) * fhat(3,iy-0)
     &             + ivec(4) * fhat(4,iy-0)
     &             + ivec(5) * fhat(5,iy-0)

CML              intF = intF / (ss22 - ss11)

              end if

c             intF = 0.5 _d 0 * intF / vCFL
C-    to avoid potential underflow:
              intF = 0.5 _d 0 * intF / sign(max(abs(vCFL),1.d-20),vCFL)

C     ==================== calc. flux = upwind tracer * face-transport
              flux(ix,iy) = + vfac(ix,iy) * intF

              end if

          end do

          return

c     end subroutine GAD_PQM_FLX_Y
      end

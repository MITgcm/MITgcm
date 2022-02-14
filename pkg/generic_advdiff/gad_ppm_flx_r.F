#     include "GAD_OPTIONS.h"

      SUBROUTINE GAD_PPM_FLX_R(bi,bj,ix,iy,
     &           delT,wvel,wfac,fhat,
     &           flux,myThid )
C     |================================================================|
C     | PPM_FLX_R: evaluate PPM flux on grid-cell edges.               |
C     |================================================================|

          implicit none

C     =============================================== global variables
#         include "SIZE.h"
#         include "GRID.h"
#         include "GAD.h"

C     ================================================================
C       bi,bj     :: tile indexing.
C       ix        :: x-index.
C       iy        :: y-index.
C       delT      :: time-step.
C       wvel      :: vel.-comp in r-direction.
C       wfac      :: vel.-flux in r-direction.
C       fhat      :: row of poly. coeff.
C       flux      :: adv.-flux in r-direction.
C       myThid    :: thread number.
C     ================================================================
          integer bi,bj,ix,iy
          _RL delT(1:Nr)
          _RL wvel(1-OLx:sNx+OLx,
     &             1-OLy:sNy+OLy,
     &             1:Nr)
          _RL wfac(1-OLx:sNx+OLx,
     &             1-OLy:sNy+OLy,
     &             1:Nr)
          _RL fhat(1:3 ,
     &             1:Nr)
          _RL flux(1-OLx:sNx+OLx,
     &             1-OLy:sNy+OLy,
     &             1:Nr)
          integer myThid

C     ================================================================
C       ir        :: r-indexing.
C       wCFL      :: CFL number.
C       intF      :: upwind tracer edge-value.
C       ss11,ss22 :: int. endpoints.
C       ivec      :: int. basis vec.
C     ================================================================
          integer ir
          _RL wCFL,intF
          _RL ss11,ss22
          _RL ivec(1:3)

C     ================================================================
C       (1): calc. "departure-points" for each grid-cell edge by int-
C            egrating edge position backward in time over one single
C            time-step. This is a "single-cell" implementation: requ-
C            ires CFL <= 1.0.
C       (2): calc. flux as the integral of the upwind grid-cell poly-
C            nomial over the deformation interval found in (1).
C     ================================================================

          do  ir = +2, Nr

              if (wvel(ix,iy,ir) .eq. 0. _d 0) then

                  flux(ix,iy,ir)    = 0. _d 0

              else

              if (wvel(ix,iy,ir) .lt. 0. _d 0) then

C     ==================== integrate PQM profile over upwind cell IR-1
              wCFL = wvel(ix,iy,ir)
     &             * delT(ir-1)*recip_drF(ir-1)

              ss11 = +1. _d 0 + 2. _d 0 * wCFL
              ss22 = +1. _d 0

C     ==================== integrate profile over region swept by face
              ivec(1) = ss22 - ss11
              ivec(2) =(ss22 ** 2
     &                - ss11 ** 2)*(1. _d 0 / 2. _d 0)
              ivec(3) =(ss22 ** 3
     &                - ss11 ** 3)*(1. _d 0 / 3. _d 0)

              intF = ivec(1) * fhat(1,ir-1)
     &             + ivec(2) * fhat(2,ir-1)
     &             + ivec(3) * fhat(3,ir-1)

CML              intF = intF / (ss22 - ss11)

              else

C     ==================== integrate PQM profile over upwind cell IR+0
              wCFL = wvel(ix,iy,ir)
     &             * delT(ir-0)*recip_drF(ir-0)

              ss11 = -1. _d 0 + 2. _d 0 * wCFL
              ss22 = -1. _d 0

C     ==================== integrate profile over region swept by face
              ivec(1) = ss22 - ss11
              ivec(2) =(ss22 ** 2
     &                - ss11 ** 2)*(1. _d 0 / 2. _d 0)
              ivec(3) =(ss22 ** 3
     &                - ss11 ** 3)*(1. _d 0 / 3. _d 0)

              intF = ivec(1) * fhat(1,ir-0)
     &             + ivec(2) * fhat(2,ir-0)
     &             + ivec(3) * fhat(3,ir-0)

CML              intF = intF / (ss22 - ss11)

              end if

c             intF = -0.5 _d 0 * intF / wCFL
C-    to avoid potential underflow:
              intF = -0.5 _d 0 * intF/sign(max(abs(wCFL),1.d-20),wCFL)

C     ==================== calc. flux = upwind tracer * face-transport
              flux(ix,iy,ir) = + wfac(ix,iy,ir) * intF

              end if

          end do

          return

c     end subroutine GAD_PPM_FLX_R
      end

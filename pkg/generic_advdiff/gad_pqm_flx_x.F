#     include "GAD_OPTIONS.h"

      SUBROUTINE GAD_PQM_FLX_X(bi,bj,kk,iy,
     &           calc_CFL,delT,uvel,
     &           ufac,fhat,flux,myThid)
C     |================================================================|
C     | PQM_FLX_X: evaluate PQM flux on grid-cell edges.               |
C     |================================================================|

          implicit none

C     =============================================== global variables
#         include "SIZE.h"
#         include "GRID.h"
#         include "GAD.h"

C     ================================================================
C       bi,bj     :: tile indexing.
C       kk        :: r-index.
C       iy        :: y-index.
C       calc_CFL  :: TRUE to calc. CFL from vel.
C       delT      :: time-step.
C       uvel      :: vel.-comp in x-direction.
C       ufac      :: vel.-flux in x-direction.
C       fhat      :: row of poly. coeff.
C       flux      :: adv.-flux in x-direction.
C       myThid    :: thread number.
C     ================================================================
          integer bi,bj,kk,iy
          logical calc_CFL
          _RL delT
          _RL uvel(1-OLx:sNx+OLx,
     &             1-OLy:sNy+OLy)
          _RL ufac(1-OLx:sNx+OLx,
     &             1-OLy:sNy+OLy)
          _RL fhat(1:5,
     &             1-OLx:sNx+OLx)
          _RL flux(1-OLx:sNx+OLx,
     &             1-OLy:sNy+OLy)
          integer myThid

C     ================================================================
C       ix        :: x-indexing.
C       uCFL      :: CFL number.
C       intF      :: upwind tracer edge-value.
C       ss11,ss22 :: int. endpoints.
C       ivec      :: int. basis vec.
C     ================================================================
          integer ix
          _RL uCFL,intF
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

          do  ix = 1-OLx+4, sNx+OLx-3

              if (uvel(ix,iy) .eq. 0. _d 0) then

                  flux(ix,iy)    = 0. _d 0

              else

              if (uvel(ix,iy) .gt. 0. _d 0) then

C     ==================== integrate PQM profile over upwind cell IX-1
              if ( calc_CFL ) then
              uCFL = uvel(ix,iy) * delT
     &             * recip_dxF(ix-1,iy,bi,bj)
     &             * recip_deepFacC(kk)
              else
              uCFL = uvel(ix,iy)
              end if

              ss11 = +1. _d 0 - 2. _d 0 * uCFL
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

              intF = ivec(1) * fhat(1,ix-1)
     &             + ivec(2) * fhat(2,ix-1)
     &             + ivec(3) * fhat(3,ix-1)
     &             + ivec(4) * fhat(4,ix-1)
     &             + ivec(5) * fhat(5,ix-1)

CML              intF = intF / (ss22 - ss11)

              else

C     ==================== integrate PQM profile over upwind cell IX+0
              if ( calc_CFL ) then
              uCFL = uvel(ix,iy) * delT
     &             * recip_dxF(ix-0,iy,bi,bj)
     &             * recip_deepFacC(kk)
              else
              uCFL = uvel(ix,iy)
              end if

              ss11 = -1. _d 0 - 2. _d 0 * uCFL
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

              intF = ivec(1) * fhat(1,ix-0)
     &             + ivec(2) * fhat(2,ix-0)
     &             + ivec(3) * fhat(3,ix-0)
     &             + ivec(4) * fhat(4,ix-0)
     &             + ivec(5) * fhat(5,ix-0)

CML              intF = intF / (ss22 - ss11)

              end if

c             intF = 0.5 _d 0 * intF / uCFL
C-    to avoid potential underflow:
              intF = 0.5 _d 0 * intF / sign(max(abs(uCFL),1.d-20),uCFL)

C     ==================== calc. flux = upwind tracer * face-transport
              flux(ix,iy) = + ufac(ix,iy) * intF

              end if

          end do

          return

c     end subroutine GAD_PQM_FLX_X
      end

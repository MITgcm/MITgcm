#     include "GAD_OPTIONS.h"

C--  File gad_osc_hat_y.F: Routines ???
C--   Contents
C--   o GAD_OSC_LOC_Y
C--   o GAD_OSC_HAT_Y

C---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|

      SUBROUTINE GAD_OSC_LOC_Y(iy,mask,fbar,ohat)

          implicit none

C     =============================================== global variables
#         include "SIZE.h"

C     ====================================================== arguments
          integer iy
          _RL mask(1-OLy:sNy+OLy)
          _RL fbar(1-OLy:sNy+OLy)
          _RL ohat(1:2,
     &             1-OLy:sNy+OLy)

C     ====================================================== variables
          _RL floc(-2:+2)

          if (iy.gt. +1-OLy .and.
     &        iy.lt.sNy+OLy) then

C     ================ mask local stencil: expand from centre outwards

          floc(+0) = fbar(+0+iy)

          floc(-1) = floc(+0) +
     &      mask(iy-1)*(fbar(iy-1)-floc(+0))
          floc(+1) = floc(+0) +
     &      mask(iy+1)*(fbar(iy+1)-floc(+0))

C     ================ calc. 1st & 2nd derivatives over masked stencil

          ohat(+1,iy) = floc(+1)*0.25 _d 0
     &                - floc(-1)*0.25 _d 0

          ohat(+2,iy) = floc(+1)*0.25 _d 0
     &                - floc(+0)*0.50 _d 0
     &                + floc(-1)*0.25 _d 0

          else

          if (iy.eq. +1-OLy) then

C     ================ mask local stencil: expand from centre outwards

          floc(+0) = fbar(+0+iy)

          floc(+1) = floc(+0) +
     &      mask(iy+1)*(fbar(iy+1)-floc(+0))
          floc(+2) = floc(+1) +
     &      mask(iy+2)*(fbar(iy+2)-floc(+1))

C     ================ calc. 1st & 2nd derivatives over masked stencil

          ohat(+1,iy) = floc(+1)*0.50 _d 0
     &                - floc(+0)*0.50 _d 0

          ohat(+2,iy) = floc(+2)*0.25 _d 0
     &                - floc(+1)*0.50 _d 0
     &                + floc(+0)*0.25 _d 0

          end if

          if (iy.eq.sNy+OLy) then

C     ================ mask local stencil: expand from centre outwards

          floc(+0) = fbar(+0+iy)

          floc(-1) = floc(+0) +
     &      mask(iy-1)*(fbar(iy-1)-floc(+0))
          floc(-2) = floc(-1) +
     &      mask(iy-2)*(fbar(iy-2)-floc(-1))

C     ================ calc. 1st & 2nd derivatives over masked stencil

          ohat(+1,iy) = floc(+0)*0.50 _d 0
     &                - floc(-1)*0.50 _d 0

          ohat(+2,iy) = floc(+0)*0.25 _d 0
     &                - floc(-1)*0.50 _d 0
     &                + floc(-2)*0.25 _d 0

          end if

          end if

          return

c     end subroutine GAD_OSC_LOC_Y
      end

C---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|

      SUBROUTINE GAD_OSC_HAT_Y(bi,bj,kk,ix,
     &           mask,fbar,ohat,myThid)
C     |================================================================|
C     | OSC_HAT_Y: compute WENO oscillation derivatives in Y.          |
C     |================================================================|

          implicit none

C     =============================================== global variables
#         include "SIZE.h"

C     ====================================================== arguments
          integer bi,bj,kk,ix
          _RL mask(1-OLy:sNy+OLy)
          _RL fbar(1-OLy:sNy+OLy)
          _RL ohat(1:2,
     &             1-OLy:sNy+OLy)
          integer myThid

C     ====================================================== variables
          integer iy

C     ================================ derivatives for WENO indicators
          do  iy = 1-OLy+0, sNy+OLy-0

              CALL GAD_OSC_LOC_Y(iy,mask,fbar,ohat)

          end do

          return

c     end subroutine GAD_OSC_HAT_Y
      end

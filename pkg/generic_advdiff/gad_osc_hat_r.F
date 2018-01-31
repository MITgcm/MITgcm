#     include "GAD_OPTIONS.h"

C--  File gad_osc_hat_r.F: Routines ???
C--   Contents
C--   o GAD_OSC_LOC_R
C--   o GAD_OSC_HAT_R

C---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|

      SUBROUTINE GAD_OSC_LOC_R(ir,mask,fbar,ohat)

          implicit none

C     =============================================== global variables
#         include "SIZE.h"

C     ====================================================== arguments
          integer ir
          _RL mask(1-3:Nr+3)
          _RL fbar(1-3:Nr+3)
          _RL ohat(1:2,
     &             1-3:Nr+3)

C     ====================================================== variables
          _RL floc(-1:+1)

          if (ir.gt.+1-3 .and.
     &        ir.lt.Nr+3) then

C     ================ mask local stencil: expand from centre outwards

          floc(+0) = fbar(+0+ir)

          floc(-1) = floc(+0) +
     &      mask(ir-1)*(fbar(ir-1)-floc(+0))
          floc(+1) = floc(+0) +
     &      mask(ir+1)*(fbar(ir+1)-floc(+0))

C     ================ calc. 1st & 2nd derivatives over masked stencil

          ohat(+1,ir) = floc(+1)*0.25 _d 0
     &                - floc(-1)*0.25 _d 0

          ohat(+2,ir) = floc(+1)*0.25 _d 0
     &                - floc(+0)*0.50 _d 0
     &                + floc(-1)*0.25 _d 0

          else

          if (ir.eq.+1-3) then

C     ================ mask local stencil: expand from centre outwards

          floc(+0) = fbar(+0+ir)

          floc(+1) = floc(+0) +
     &      mask(ir+1)*(fbar(ir+1)-floc(+0))

C     ================ calc. 1st & 2nd derivatives over masked stencil

          ohat(+1,ir) = floc(+1)*0.50 _d 0
     &                - floc(+0)*0.50 _d 0

          ohat(+2,ir) = 0. _d 0

          end if

          if (ir.eq.Nr+3) then

C     ================ mask local stencil: expand from centre outwards

          floc(+0) = fbar(+0+ir)

          floc(-1) = floc(+0) +
     &      mask(ir-1)*(fbar(ir-1)-floc(+0))

C     ================ calc. 1st & 2nd derivatives over masked stencil

          ohat(+1,ir) = floc(+0)*0.50 _d 0
     &                - floc(-1)*0.50 _d 0

          ohat(+2,ir) = 0. _d 0

          end if

          end if

          return

c     end subroutine GAD_OSC_LOC_R
      end

C---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|

      SUBROUTINE GAD_OSC_HAT_R(bi,bj,ix,iy,
     &           mask,fbar,ohat,myThid)
C     |================================================================|
C     | OSC_HAT_R: compute WENO oscillation derivatives in R.          |
C     |================================================================|

          implicit none

C     =============================================== global variables
#         include "SIZE.h"

C     ====================================================== arguments
          integer bi,bj,ix,iy
          _RL mask(1-3:Nr+3)
          _RL fbar(1-3:Nr+3)
          _RL ohat(1:2,
     &             1-3:Nr+3)
          integer myThid

C     ====================================================== variables
          integer ir

C     ================================ derivatives for WENO indicators
          do  ir = +1-3, Nr+3

              CALL GAD_OSC_LOC_R(ir,mask,fbar,ohat)

          end do

          return

c     end subroutine GAD_OSC_HAT_R
      end

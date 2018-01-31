#     include "GAD_OPTIONS.h"

      SUBROUTINE GAD_PPM_P3E_R(bi,bj,ix,iy,
     &           mask,fbar,edge,myThid)
C     |================================================================|
C     | PPM_P3E_R: approximate edge values with degree-3 polynomials.  |
C     | Fixed grid-spacing variant in R.                               |
C     |================================================================|

          implicit none

C     =============================================== global variables
#         include "SIZE.h"
#         include "GRID.h"
#         include "GAD.h"

C     ====================================================== arguments
          integer bi,bj,ix,iy
          _RL mask(1-3:Nr+3)
          _RL fbar(1-3:Nr+3)
          _RL edge(1-0:Nr+1)
          integer myThid

C     ====================================================== variables
          integer ir
          _RL mloc(-2:+1)
          _RL floc(-2:+1)
          _RL ftmp

          do  ir = +1, Nr+1

C     ================ mask local stencil: expand from centre outwards
              mloc(-1) = mask(ir-1)
              mloc(+0) = mask(ir+0)

              floc(-1) = fbar(ir+0)
     &          + mloc(-1)*(fbar(ir-1)-fbar(ir+0))
              floc(+0) = fbar(ir-1)
     &          + mloc(+0)*(fbar(ir+0)-fbar(ir-1))

              mloc(-2) = mask(ir-2) * mloc(-1)

              ftmp = 2. _d 0 * floc(-1) - floc(+0)
              floc(-2) = ftmp
     &          + mloc(-2)*(fbar(ir-2)-ftmp)

              mloc(+1) = mask(ir+1) * mloc(+0)

              ftmp = 2. _d 0 * floc(+0) - floc(-1)
              floc(+1) = ftmp
     &          + mloc(+1)*(fbar(ir+1)-ftmp)

C     ================ centred, 3rd-order interpolation for edge value
              edge(ir) =
     &      -(1. _d 0 / 12. _d 0)*(floc(-2)+floc(+1))
     &      +(7. _d 0 / 12. _d 0)*(floc(-1)+floc(+0))

          end do

          return

c     end subroutine GAD_PPM_P3E_R
      end

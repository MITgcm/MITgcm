#     include "GAD_OPTIONS.h"

      SUBROUTINE GAD_PQM_P5E_R(bi,bj,ix,iy,
     &           mask,fbar,edge,myThid)
C     |================================================================|
C     | PQM_P5E_R: approximate edge values with degree-5 polynomials.  |
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
          _RL edge(1:2,
     &             1-0:Nr+1)
          integer myThid

C     ====================================================== variables
          integer ir
          _RL mloc(-3:+2)
          _RL floc(-3:+2)
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
              mloc(-3) = mask(ir-3) * mloc(-2)

              ftmp = 2. _d 0 * floc(-1) - floc(+0)
              floc(-2) = ftmp
     &          + mloc(-2)*(fbar(ir-2)-ftmp)
              ftmp = 2. _d 0 * floc(-2) - floc(-1)
              floc(-3) = ftmp
     &          + mloc(-3)*(fbar(ir-3)-ftmp)

              mloc(+1) = mask(ir+1) * mloc(+0)
              mloc(+2) = mask(ir+2) * mloc(+1)

              ftmp = 2. _d 0 * floc(+0) - floc(-1)
              floc(+1) = ftmp
     &          + mloc(+1)*(fbar(ir+1)-ftmp)
              ftmp = 2. _d 0 * floc(+1) - floc(+0)
              floc(+2) = ftmp
     &          + mloc(+2)*(fbar(ir+2)-ftmp)

C     ================ centred, 5th-order interpolation for edge value
              edge(1,ir) =
     &    +( 1. _d 0/60. _d 0)*(floc(-3)+floc(+2))
     &    -( 8. _d 0/60. _d 0)*(floc(-2)+floc(+1))
     &    +(37. _d 0/60. _d 0)*(floc(-1)+floc(+0))

              edge(2,ir) = (
     &    -( 1. _d 0/90. _d 0)*(floc(-3)-floc(+2))
     &    +( 5. _d 0/36. _d 0)*(floc(-2)-floc(+1))
     &    -(49. _d 0/36. _d 0)*(floc(-1)-floc(+0))
     &                      ) * recip_drC(ir)

          end do

          return

c     end subroutine GAD_PQM_P5E_R
      end

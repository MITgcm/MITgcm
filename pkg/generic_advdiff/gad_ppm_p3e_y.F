#     include "GAD_OPTIONS.h"

      SUBROUTINE GAD_PPM_P3E_Y(bi,bj,kk,ix,
     &           mask,fbar,edge,myThid)
C     |================================================================|
C     | PPM_P3E_Y: approximate edge values with degree-3 polynomials.  |
C     | Fixed grid-spacing variant in Y.                               |
C     |================================================================|

          implicit none

C     =============================================== global variables
#         include "SIZE.h"
#         include "GRID.h"
#         include "GAD.h"

C     ====================================================== arguments
          integer bi,bj,kk,ix
          _RL mask(1-OLy:sNy+OLy)
          _RL fbar(1-OLy:sNy+OLy)
          _RL edge(1-OLy:sNy+OLy)
          integer myThid

C     ====================================================== variables
          integer iy
          _RL mloc(-2:+1)
          _RL floc(-2:+1)
          _RL ftmp

C     ==================== reconstruct 3rd--order accurate edge values
          do  iy = 1-OLy+2, sNy+OLy-1

C     ================ mask local stencil: expand from centre outwards
              mloc(-1) = mask(iy-1)
              mloc(+0) = mask(iy+0)

              floc(-1) = fbar(iy+0)
     &          + mloc(-1)*(fbar(iy-1)-fbar(iy+0))
              floc(+0) = fbar(iy-1)
     &          + mloc(+0)*(fbar(iy+0)-fbar(iy-1))

              mloc(-2) = mask(iy-2) * mloc(-1)

              ftmp = 2. _d 0 * floc(-1) - floc(+0)
              floc(-2) = ftmp
     &          + mloc(-2)*(fbar(iy-2)-ftmp)

              mloc(+1) = mask(iy+1) * mloc(+0)

              ftmp = 2. _d 0 * floc(+0) - floc(-1)
              floc(+1) = ftmp
     &          + mloc(+1)*(fbar(iy+1)-ftmp)

C     ================ centred, 5th-order interpolation for edge value
              edge(iy) =
     &      -(1. _d 0 / 12. _d 0)*(floc(-2)+floc(+1))
     &      +(7. _d 0 / 12. _d 0)*(floc(-1)+floc(+0))

          end do

          return

c     end subroutine GAD_PPM_P3E_Y
      end

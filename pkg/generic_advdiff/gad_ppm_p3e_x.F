#     include "GAD_OPTIONS.h"

      SUBROUTINE GAD_PPM_P3E_X(bi,bj,kk,iy,
     &           mask,fbar,edge,myThid)
C     |================================================================|
C     | PPM_P3E_X: approximate edge values with degree-3 polynomials.  |
C     | Fixed grid-spacing variant in X.                               |
C     |================================================================|

          implicit none

C     =============================================== global variables
#         include "SIZE.h"
#         include "GRID.h"
#         include "GAD.h"

C     ====================================================== arguments
          integer bi,bj,kk,iy
          _RL mask(1-OLx:sNx+OLx)
          _RL fbar(1-OLx:sNx+OLx)
          _RL edge(1-OLx:sNx+OLx)
          integer myThid

C     ====================================================== variables
          integer ix
          _RL mloc(-2:+1)
          _RL floc(-2:+1)
          _RL ftmp

          do  ix = 1-OLx+2, sNx+OLx-1

C     ================ mask local stencil: expand from centre outwards
              mloc(-1) = mask(ix-1)
              mloc(+0) = mask(ix+0)

              floc(-1) = fbar(ix+0)
     &          + mloc(-1)*(fbar(ix-1)-fbar(ix+0))
              floc(+0) = fbar(ix-1)
     &          + mloc(+0)*(fbar(ix+0)-fbar(ix-1))

              mloc(-2) = mask(ix-2) * mloc(-1)

              ftmp = 2. _d 0 * floc(-1) - floc(+0)
              floc(-2) = ftmp
     &          + mloc(-2)*(fbar(ix-2)-ftmp)

              mloc(+1) = mask(ix+1) * mloc(+0)

              ftmp = 2. _d 0 * floc(+0) - floc(-1)
              floc(+1) = ftmp
     &          + mloc(+1)*(fbar(ix+1)-ftmp)

C     ================ centred, 3rd-order interpolation for edge value
              edge(ix) =
     &      -(1. _d 0 / 12. _d 0)*(floc(-2)+floc(+1))
     &      +(7. _d 0 / 12. _d 0)*(floc(-1)+floc(+0))

          end do

          return

c     end subroutine GAD_PPM_P3E_X
      end

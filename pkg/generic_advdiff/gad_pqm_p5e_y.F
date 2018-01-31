#     include "GAD_OPTIONS.h"

      SUBROUTINE GAD_PQM_P5E_Y(bi,bj,kk,ix,
     &           mask,fbar,edge,myThid)
C     |================================================================|
C     | PQM_P5E_Y: approximate edge values with degree-5 polynomials.  |
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
          _RL edge(1:2,
     &             1-OLy:sNy+OLy)
          integer myThid

C     ====================================================== variables
          integer iy
          _RL mloc(-3:+2)
          _RL floc(-3:+2)
          _RL ftmp

C     ==================== reconstruct 5th--order accurate edge values
          do  iy = 1-OLy+3, sNy+OLy-2

C     ================ mask local stencil: expand from centre outwards
              mloc(-1) = mask(iy-1)
              mloc(+0) = mask(iy+0)

              floc(-1) = fbar(iy+0)
     &          + mloc(-1)*(fbar(iy-1)-fbar(iy+0))
              floc(+0) = fbar(iy-1)
     &          + mloc(+0)*(fbar(iy+0)-fbar(iy-1))

              mloc(-2) = mask(iy-2) * mloc(-1)
              mloc(-3) = mask(iy-3) * mloc(-2)

              ftmp = 2. _d 0 * floc(-1) - floc(+0)
              floc(-2) = ftmp
     &          + mloc(-2)*(fbar(iy-2)-ftmp)
              ftmp = 2. _d 0 * floc(-2) - floc(-1)
              floc(-3) = ftmp
     &          + mloc(-3)*(fbar(iy-3)-ftmp)

              mloc(+1) = mask(iy+1) * mloc(+0)
              mloc(+2) = mask(iy+2) * mloc(+1)

              ftmp = 2. _d 0 * floc(+0) - floc(-1)
              floc(+1) = ftmp
     &          + mloc(+1)*(fbar(iy+1)-ftmp)
              ftmp = 2. _d 0 * floc(+1) - floc(+0)
              floc(+2) = ftmp
     &          + mloc(+2)*(fbar(iy+2)-ftmp)

C     ================ centred, 5th-order interpolation for edge value
              edge(1,iy) =
     &    +( 1. _d 0/60. _d 0)*(floc(-3)+floc(+2))
     &    -( 8. _d 0/60. _d 0)*(floc(-2)+floc(+1))
     &    +(37. _d 0/60. _d 0)*(floc(-1)+floc(+0))

              edge(2,iy) = (
     &    -( 1. _d 0/90. _d 0)*(floc(-3)-floc(+2))
     &    +( 5. _d 0/36. _d 0)*(floc(-2)-floc(+1))
     &    -(49. _d 0/36. _d 0)*(floc(-1)-floc(+0))
     &                ) * recip_dyC(ix,iy,bi,bj)

          end do

          return

c     end subroutine GAD_PQM_P5E_Y
      end

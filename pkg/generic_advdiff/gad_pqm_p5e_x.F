#     include "GAD_OPTIONS.h"

      SUBROUTINE GAD_PQM_P5E_X(bi,bj,kk,iy,
     &           mask,fbar,edge,myThid)
C     |================================================================|
C     | PQM_P5E_X: approximate edge values with degree-5 polynomials.  |
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
          _RL edge(1:2,
     &             1-OLx:sNx+OLx)
          integer myThid

C     ====================================================== variables
          integer ix
          _RL mloc(-3:+2)
          _RL floc(-3:+2)
          _RL ftmp

          do  ix = 1-OLx+3, sNx+OLx-2

C     ================ mask local stencil: expand from centre outwards
              mloc(-1) = mask(ix-1)
              mloc(+0) = mask(ix+0)

              floc(-1) = fbar(ix+0)
     &          + mloc(-1)*(fbar(ix-1)-fbar(ix+0))
              floc(+0) = fbar(ix-1)
     &          + mloc(+0)*(fbar(ix+0)-fbar(ix-1))

              mloc(-2) = mask(ix-2) * mloc(-1)
              mloc(-3) = mask(ix-3) * mloc(-2)

              ftmp = 2. _d 0 * floc(-1) - floc(+0)
              floc(-2) = ftmp
     &          + mloc(-2)*(fbar(ix-2)-ftmp)
              ftmp = 2. _d 0 * floc(-2) - floc(-1)
              floc(-3) = ftmp
     &          + mloc(-3)*(fbar(ix-3)-ftmp)

              mloc(+1) = mask(ix+1) * mloc(+0)
              mloc(+2) = mask(ix+2) * mloc(+1)

              ftmp = 2. _d 0 * floc(+0) - floc(-1)
              floc(+1) = ftmp
     &          + mloc(+1)*(fbar(ix+1)-ftmp)
              ftmp = 2. _d 0 * floc(+1) - floc(+0)
              floc(+2) = ftmp
     &          + mloc(+2)*(fbar(ix+2)-ftmp)

C     ================ centred, 5th-order interpolation for edge value
              edge(1,ix) =
     &    +( 1. _d 0/60. _d 0)*(floc(-3)+floc(+2))
     &    -( 8. _d 0/60. _d 0)*(floc(-2)+floc(+1))
     &    +(37. _d 0/60. _d 0)*(floc(-1)+floc(+0))

              edge(2,ix) = (
     &    -( 1. _d 0/90. _d 0)*(floc(-3)-floc(+2))
     &    +( 5. _d 0/36. _d 0)*(floc(-2)-floc(+1))
     &    -(49. _d 0/36. _d 0)*(floc(-1)-floc(+0))
     &                ) * recip_dxC(ix,iy,bi,bj)

          end do

          return

c     end subroutine GAD_PQM_P5E_X
      end

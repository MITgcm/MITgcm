#     include "GAD_OPTIONS.h"

      SUBROUTINE GAD_OSC_MUL_Y(iy,hh,mask,ohat,scal)
C     |================================================================|
C     | OSC_MUL_Y: evaluate WENO oscillation weights in Y.             |
C     |================================================================|

          implicit none

C     =============================================== global variables
#         include "SIZE.h"

          integer iy,hh
          _RL mask(1-OLy:sNy+OLy)
          _RL ohat(1:2,
     &             1-OLy:sNy+OLy)
          _RL scal(1:2)

          integer ii
          _RL dels,dfs1,dfs2
          _RL osum,zero,mval
          _RL oval,omin,omax

C     =============================== calc. WENO oscillation weighting
c         omin = +huge(+1. _d 0)
c         omax = -huge(+1. _d 0)
          omin = +1. _d 99
          omax = -1. _d 99

          zero = 1. _d -20
          mval = 1. _d + 0

          do  ii = iy-hh, iy+hh

C     =============================== calc. derivatives centred on II.
              dels = (ii - iy) * 2. _d 0

              dfs1 = ohat(1,ii)
              dfs2 = ohat(2,ii)

              dfs1 = dfs1 + dfs2 * dels

C     =============================== oscl. = NORM(H^N * D^N/DY^N(F)).
              oval = (2. _d 0 * dfs1)**2
     &             + (4. _d 0 * dfs2)**2

              if (oval.lt.omin) omin = oval
              if (oval.gt.omax) omax = oval

C     =============================== any mask across oscil. stencil
              mval = mval * mask(ii)

          end do

          if (mval .gt. 0. _d 0) then

C     =============================== calc. WENO-style profile weights
              scal(1) = 1. _d 5
     &             / (omax + zero)**3
              scal(2) = 1. _d 0
     &             / (omin + zero)**3

              osum = scal(1) + scal(2)
              scal(1) = scal(1) / osum
              scal(2) = scal(2) / osum

          else

C     =============================== default to MONO. profile weights
              scal(1) = 0. _d 0
              scal(2) = 1. _d 0

          end if

          return

c     end subroutine GAD_OSC_MUL_Y
      end

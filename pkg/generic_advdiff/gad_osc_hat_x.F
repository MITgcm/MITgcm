#     include "GAD_OPTIONS.h"

C--  File gad_osc_hat_x.F: Routines ???
C--   Contents
C--   o GAD_OSC_LOC_X
C--   o GAD_OSC_HAT_X

C---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|

      SUBROUTINE GAD_OSC_LOC_X(ix,mask,fbar,ohat)

          implicit none

C     =============================================== global variables
#         include "SIZE.h"

C     ====================================================== arguments
          integer ix
          _RL mask(1-OLx:sNx+OLx)
          _RL fbar(1-OLx:sNx+OLx)
          _RL ohat(1:2,
     &             1-OLx:sNx+OLx)

C     ====================================================== variables
          _RL floc(-2:+2)

          if (ix.gt. +1-OLx .and.
     &        ix.lt.sNx+OLx) then

C     ================ mask local stencil: expand from centre outwards

          floc(+0) = fbar(+0+ix)

          floc(-1) = floc(+0) +
     &      mask(ix-1)*(fbar(ix-1)-floc(+0))
          floc(+1) = floc(+0) +
     &      mask(ix+1)*(fbar(ix+1)-floc(+0))

C     ================ calc. 1st & 2nd derivatives over masked stencil

          ohat(+1,ix) = floc(+1)*0.25 _d 0
     &                - floc(-1)*0.25 _d 0

          ohat(+2,ix) = floc(+1)*0.25 _d 0
     &                - floc(+0)*0.50 _d 0
     &                + floc(-1)*0.25 _d 0

          else

          if (ix.eq. +1-OLx) then

C     ================ mask local stencil: expand from centre outwards

          floc(+0) = fbar(+0+ix)

          floc(+1) = floc(+0) +
     &      mask(ix+1)*(fbar(ix+1)-floc(+0))
          floc(+2) = floc(+1) +
     &      mask(ix+2)*(fbar(ix+2)-floc(+1))

C     ================ calc. 1st & 2nd derivatives over masked stencil

          ohat(+1,ix) = floc(+1)*0.50 _d 0
     &                - floc(+0)*0.50 _d 0

          ohat(+2,ix) = floc(+2)*0.25 _d 0
     &                - floc(+1)*0.50 _d 0
     &                + floc(+0)*0.25 _d 0

          end if

          if (ix.eq.sNx+OLx) then

C     ================ mask local stencil: expand from centre outwards

          floc(+0) = fbar(+0+ix)

          floc(-1) = floc(+0) +
     &      mask(ix-1)*(fbar(ix-1)-floc(+0))
          floc(-2) = floc(-1) +
     &      mask(ix-2)*(fbar(ix-2)-floc(-1))

C     ================ calc. 1st & 2nd derivatives over masked stencil

          ohat(+1,ix) = floc(+0)*0.50 _d 0
     &                - floc(-1)*0.50 _d 0

          ohat(+2,ix) = floc(+0)*0.25 _d 0
     &                - floc(-1)*0.50 _d 0
     &                + floc(-2)*0.25 _d 0

          end if

          end if

          return

c     end subroutine GAD_OSC_LOC_X
      end

C---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|

      SUBROUTINE GAD_OSC_HAT_X(bi,bj,kk,iy,
     &           mask,fbar,ohat,myThid)
C     |================================================================|
C     | OSC_HAT_X: compute WENO oscillation derivatives in X.          |
C     |================================================================|

          implicit none

C     =============================================== global variables
#         include "SIZE.h"

C     ====================================================== arguments
          integer bi,bj,kk,iy
          _RL mask(1-OLx:sNx+OLx)
          _RL fbar(1-OLx:sNx+OLx)
          _RL ohat(1:2,
     &             1-OLx:sNx+OLx)
          integer myThid

C     ====================================================== variables
          integer ix

C     ================================ derivatives for WENO indicators
          do ix = 1-OLx+0, sNx+OLx-0

              CALL GAD_OSC_LOC_X(ix,mask,fbar,ohat)

          end do

          return

c     end subroutine GAD_OSC_HAT_X
      end

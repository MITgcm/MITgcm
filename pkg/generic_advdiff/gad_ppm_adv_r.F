#     include "GAD_OPTIONS.h"

      SUBROUTINE GAD_PPM_ADV_R(meth,bi,bj,
     I           delT,velR,facR,fbar,
     O           flux,myThid )
C     |================================================================|
C     | PPM_ADV_R: evaluate grid-cell advective flux in R.             |
C     | Lagrangian-type Piecewise Parabolic Method (PPM).              |
C     |================================================================|

          implicit none

C     =============================================== global variables
#         include "SIZE.h"
#         include "GRID.h"
#         include "GAD.h"

C     ================================================================
C       meth     :: advection method.
C       bi,bj    :: tile indexing
C       delT     :: level-wise time-steps.
C       velR     :: vel. field in r-direction.
C       facR     :: grid-areas in r-direction.
C       fbar     :: grid-cell values.
C       flux     :: trac.-flux in r-direction.
C       myThid   :: thread number.
C     ================================================================
          integer meth
          integer bi,bj
          _RL delT(1:Nr)
          _RL velR(1-OLx:sNx+OLx,
     &             1-OLy:sNy+OLy,
     &             1:Nr)
          _RL facR(1-OLx:sNx+OLx,
     &             1-OLy:sNy+OLy,
     &             1:Nr)
          _RL fbar(1-OLx:sNx+OLx,
     &             1-OLy:sNy+OLy,
     &             1:Nr)
          _RL flux(1-OLx:sNx+OLx,
     &             1-OLy:sNy+OLy,
     &             1:Nr)
          integer myThid

C     ================================================================
C       ix,iy,ir :: grid indexing.
C       floc     :: col. of grid-cell values.
C       mloc     :: col. of grid-cell mask values.
C       fhat     :: col. of poly. coeff.
C                    - FHAT(:,I) = PQM coeff.
C       edge     :: col. of edge-wise values/slopes.
C                    - EDGE(1,:) = VALUE.
C                    - EDGE(2,:) = DF/DR.
C       ohat     :: col. of oscl. coeff.
C                    - OHAT(1,:) = D^1F/DS^1.
C                    - OHAT(2,:) = D^2F/DS^2.
C     ================================================================
          integer ix,iy,ir
          _RL floc(    1-3:Nr+3)
          _RL mloc(    1-3:Nr+3)
          _RL fhat(1:3,1-0:Nr+0)
          _RL edge(    1-0:Nr+1)
          _RL ohat(1:2,1-3:Nr+3)
          _RL vsum

C     ======================================= mask boundary conditions
          mloc(  -2) = 0.0 _d 0
          mloc(  -1) = 0.0 _d 0
          mloc(  +0) = 0.0 _d 0
          mloc(Nr+1) = 0.0 _d 0
          mloc(Nr+2) = 0.0 _d 0
          mloc(Nr+3) = 0.0 _d 0

C     ================================================================
C       (1): copy a single row of data onto contiguous storage, treat
C            as a set of one-dimensional problems.
C       (2): calc. "oscillation-indicators" for each grid-cell if ad-
C            vection scheme is WENO-class.
C       (3): calc. edge-centred values/slopes by high-order interpol-
C            ation.
C       (4): calc. cell-centred polynomial profiles with appropriate
C            slope-limiting.
C       (5): calc. fluxes using a local, semi-lagrangian integration.
C     ================================================================

          do iy = 1-OLy+0, sNy+OLy-0
          do ix = 1-OLx+0, sNx+OLx-0
C     ======================================= no flux through surf. bc
          flux(ix,iy,+1) = 0.0 _d +0
          end do
          end do

C     ==================== calculate transport for interior grid-cells
          do iy = 1-OLy+0, sNy+OLy-0
          do ix = 1-OLx+0, sNx+OLx-0

          vsum = 0.0 _d 0
          do ir = 2, Nr
C     ================================== quick break on zero transport
              vsum = vsum
     &             + abs(velR(ix,iy,ir))
          end do

          if (vsum .gt. 0. _d 0) then

          do ir = 1, Nr
C     ================================== make local unit-stride copies
              floc(ir) = fbar(ix,iy,ir)
              mloc(ir) =
     &            maskC(ix,iy,ir,bi,bj)
          end do

C     ================================== make mask boundary conditions
          floc(  -2) = floc(+1)
          floc(  -1) = floc(+1)
          floc(  +0) = floc(+1)
          floc(Nr+1) = floc(Nr)
          floc(Nr+2) = floc(Nr)
          floc(Nr+3) = floc(Nr)

C     ==================== reconstruct derivatives for WENO indicators
          if (meth.eq.ENUM_PPM_WENO_LIMIT) then
          CALL GAD_OSC_HAT_R(bi,bj,ix,iy,
     &                   mloc,floc,
     &                   ohat,myThid)
          end if

C     ==================== reconstruct 3rd--order accurate edge values
          CALL GAD_PPM_P3E_R(bi,bj,ix,iy,
     &                   mloc,floc,
     &                   edge,myThid)

C     ==================== reconstruct coeff. for grid-cell poynomials
          CALL GAD_PPM_HAT_R(bi,bj,ix,iy,
     &                   meth,
     &                   mloc,floc,
     &                   edge,ohat,
     &                   fhat,myThid)

C     ==================== evaluate integral fluxes on grid-cell edges
          CALL GAD_PPM_FLX_R(bi,bj,ix,iy,
     &                   delT,velR,
     &                   facR,fhat,
     &                   flux,myThid)

          else

          do ir = 2, Nr
C     ================================== "null" flux on zero transport
              flux(ix,iy,ir) = 0.0 _d +0
          end do

          end if

          end do
          end do

          return

c     end subroutine GAD_PPM_ADV_R
      end

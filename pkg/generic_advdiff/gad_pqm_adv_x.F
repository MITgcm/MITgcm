#     include "GAD_OPTIONS.h"

      SUBROUTINE GAD_PQM_ADV_X(meth,bi,bj,kk,
     I           calc_CFL,delT,uvel,ufac,fbar,
     O           flux,myThid )
C     |================================================================|
C     | PQM_ADV_X: evaluate grid-cell advective flux in X.             |
C     | Lagrangian-type Piecewise Quartic Method (PQM).                |
C     |================================================================|

          implicit none

C     =============================================== global variables
#         include "SIZE.h"
#         include "GRID.h"
#         include "GAD.h"

C     ================================================================
C       meth     :: advection method.
C       bi,bj    :: tile indexing.
C       kk       :: r-index.
C       calc_CFL :: TRUE to calc. CFL from vel.
C       delT     :: time-step.
C       uvel     :: vel.-comp in x-direction.
C       ufac     :: vel.-flux in x-direction.
C       fbar     :: grid-cell values.
C       flux     :: adv.-flux in x-direction.
C       myThid   :: thread number.
C     ================================================================
          integer meth
          integer bi,bj,kk
          logical calc_CFL
          _RL delT
          _RL uvel(1-OLx:sNx+OLx,
     &             1-OLy:sNy+OLy)
          _RL ufac(1-OLx:sNx+OLx,
     &             1-OLy:sNy+OLy)
          _RL fbar(1-OLx:sNx+OLx,
     &             1-OLy:sNy+OLy)
          _RL flux(1-OLx:sNx+OLx,
     &             1-OLy:sNy+OLy)
          integer myThid

C     ================================================================
C       ix,iy,ir :: grid indexing.
C       floc     :: row of grid-cell values.
C       mloc     :: row of grid-cell mask values.
C       fhat     :: row of poly. coeff.
C                    - FHAT(:,I) = PQM coeff.
C       edge     :: row of edge-wise values/slopes.
C                    - EDGE(1,:) = VALUE.
C                    - EDGE(2,:) = DF/DY.
C       ohat     :: row of oscl. coeff.
C                    - OHAT(1,:) = D^1F/DS^1.
C                    - OHAT(2,:) = D^2F/DS^2.
C     ================================================================
          integer ix,iy
          _RL mloc(1-OLx:sNx+OLx)
          _RL floc(1-OLx:sNx+OLx)
          _RL fhat(1:5,
     &             1-OLx:sNx+OLx)
          _RL edge(1:2,
     &             1-OLx:sNx+OLx)
          _RL ohat(1:2,
     &             1-OLx:sNx+OLx)
          _RL vsum

          do iy = 1-OLy+0, sNy+OLy-0
C     ==================== zero stencil "ghost" cells along boundaries
              flux( +1-OLx+0,iy) = 0. _d 0
              flux( +1-OLx+1,iy) = 0. _d 0
              flux( +1-OLx+2,iy) = 0. _d 0
              flux( +1-OLx+3,iy) = 0. _d 0
              flux(sNx+OLx-0,iy) = 0. _d 0
              flux(sNx+OLx-1,iy) = 0. _d 0
              flux(sNx+OLx-2,iy) = 0. _d 0
          end do

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

          vsum = 0.0 _d 0
          do ix = 1-OLx+0, sNx+OLx-0
C     ================================== quick break on zero transport
              vsum = vsum
     &             + abs(ufac(ix,iy))
C     ================================== make local unit-stride copies
              floc(ix) = fbar (ix,iy)
              mloc(ix) =
     &          maskC(ix,iy,kk,bi,bj)
          end do

          if (vsum .gt. 0. _d 0) then

C     ==================== reconstruct derivatives for WENO indicators
          if (meth.eq.ENUM_PQM_WENO_LIMIT) then
          CALL GAD_OSC_HAT_X(bi,bj,kk,iy,
     &                   mloc,floc,
     &                   ohat,myThid)
          end if

C     ==================== reconstruct 5th--order accurate edge values
          CALL GAD_PQM_P5E_X(bi,bj,kk,iy,
     &                   mloc,floc,
     &                   edge,myThid)

C     ==================== reconstruct coeff. for grid-cell poynomials
          CALL GAD_PQM_HAT_X(bi,bj,kk,iy,
     &                   meth,
     &                   mloc,floc,
     &                   edge,ohat,
     &                   fhat,myThid)

C     ==================== evaluate integral fluxes on grid-cell edges
          CALL GAD_PQM_FLX_X(bi,bj,kk,iy,
     &                   calc_CFL,
     &                   delT,uvel,
     &                   ufac,fhat,
     &                   flux,myThid)

          else

          do ix = 1-OLx+4, sNx+OLx-3
C     ================================== "null" flux on zero transport
              flux(ix,iy) = 0.0 _d 0
          end do

          end if

          end do

          return

c     end subroutine GAD_PQM_ADV_X
      end

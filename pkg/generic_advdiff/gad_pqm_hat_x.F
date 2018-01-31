#     include "GAD_OPTIONS.h"

      SUBROUTINE GAD_PQM_HAT_X(bi,bj,kk,iy,
     &           method,mask,fbar,edge,
     &           ohat,fhat,myThid)
C     |================================================================|
C     | PQM_HAT_X: reconstruct grid-cell PQM polynomials.              |
C     |================================================================|

          implicit none

C     =============================================== global variables
#         include "SIZE.h"
#         include "GRID.h"
#         include "GAD.h"

C     ====================================================== arguments
C       bi,bj     :: tile indices.
C       kk        :: r-index.
C       iy        :: y-index.
C       method    :: advection scheme.
C       mask      :: row of cell-wise mask values.
C       fbar      :: row of cell-wise values.
C       edge      :: row of edge-wise values/slopes.
C                    - EDGE(1,:) = VALUE.
C                    - EDGE(2,:) = DF/DX.
C       ohat      :: row of oscl. coeff.
C                    - OHAT(1,:) = D^1F/DS^1.
C                    - OHAT(2,:) = D^2F/DS^2.
C       fhat      :: row of poly. coeff.
C                    - FHAT(:,I) = PQM coeff.
C       myThid    :: thread number.
C     ================================================================
          integer bi,bj,kk,iy
          integer method
          _RL mask(1-OLx:sNx+OLx)
          _RL fbar(1-OLx:sNx+OLx)
          _RL edge(1:2,
     &             1-OLx:sNx+OLx)
          _RL ohat(1:2,
     &             1-OLx:sNx+OLx)
          _RL fhat(1:5,
     &             1-OLx:sNx+OLx)
          integer myThid

C     ====================================================== variables
C       ii,ix     :: local x-indexing.
C       ff00      :: centre-biased cell value.
C       ffll,ffrr :: left-, and right-biased cell values.
C       xhat      :: local coord. scaling.
C       fell,ferr :: left-, and right-biased edge values.
C       dell,derr :: left-, and right-biased edge slopes.
C       dfds      :: linear slope estimates.
C       uhat      :: "NULL" limited poly. coeff.
C       lhat      :: "MONO" limited poly. coeff.
C       scal      :: "WENO" weights.
C       fmag,fdel :: local perturbation indicators.
C     ================================================================
          integer ii,ix
          _RL ff00
          _RL ffll,ffrr
          _RL xhat
          _RL fell,ferr
          _RL dell,derr
          _RL dfds(-1:+1)
          _RL uhat(+1:+5)
          _RL lhat(+1:+5)
          _RL scal(+1:+2)
          _RL fmag,fdel
          integer  mono

          do  ix = 1-OLx+3, sNx+OLx-3

              if (mask(ix) .gt. 0. _d 0) then

C     =============================== scale to local grid-cell co-ords
              xhat = dxF(ix,iy,bi,bj) * 0.5 _d 0

C     =============================== assemble cell mean + edge values
              ff00 = fbar(ix+0)
              ffll = ff00
     &          + mask(ix-1)*(fbar(ix-1)-ff00)
              ffrr = ff00
     &          + mask(ix+1)*(fbar(ix+1)-ff00)

              fell = edge(+1,ix-0)
              ferr = edge(+1,ix+1)

              dell = edge(+2,ix-0)
              derr = edge(+2,ix+1)

              dell = dell * xhat
              derr = derr * xhat

c             select case(method)
c             case(ENUM_PQM_NULL_LIMIT)
              if ( method.eq.ENUM_PQM_NULL_LIMIT ) then
C     =============================== "NULL" limited grid-cell profile
              CALL GAD_PQM_FUN_NULL ( ff00,
     &             fell,ferr,dell,derr,lhat,mono)

c             case(ENUM_PQM_MONO_LIMIT)
              elseif ( method.eq.ENUM_PQM_MONO_LIMIT ) then
C     =============================== "MONO" limited grid-cell profile
              CALL GAD_PLM_FUN_U(ffll,ff00,ffrr,dfds)

              CALL GAD_PQM_FUN_MONO ( ff00,ffll,ffrr,
     &             fell,ferr,dell,derr,dfds,lhat,
     &             mono)

c             case(ENUM_PQM_WENO_LIMIT)
              elseif ( method.eq.ENUM_PQM_WENO_LIMIT ) then
C     =============================== "WENO" limited grid-cell profile
              CALL GAD_PLM_FUN_U(ffll,ff00,ffrr,dfds)

              CALL GAD_PQM_FUN_NULL ( ff00,
     &             fell,ferr,dell,derr,uhat,mono)

              CALL GAD_PQM_FUN_MONO ( ff00,ffll,ffrr,
     &             fell,ferr,dell,derr,dfds,lhat,
     &             mono)

              if ( mono .gt. 0) then

C     =============================== only apply WENO if it is worth it
              fdel = abs(ffrr-ff00)+abs(ff00-ffll)
              fmag = abs(ffll)+abs(ff00)+abs(ffrr)

              if (fdel .gt. 1. _d -6 * fmag) then

C     =============================== calc. WENO oscillation weighting
                  CALL GAD_OSC_MUL_X(ix,+2,mask,
     &                 ohat,scal)

                  do  ii = +1, +5
C     =============================== blend limited/un-limited profile
                  lhat(ii) = scal(1) * uhat(ii)
     &                     + scal(2) * lhat(ii)
                  end do

              end if

              end if

c             end select
              endif

              do  ii = +1, +5
C     =============================== copy polynomial onto output data
                  fhat(ii,ix) = lhat(ii)
              end do

              else

              do  ii = +1, +5
                  fhat(ii,ix) = 0.0 _d 0
              end do

              end if

          end do

          return

c     end subroutine GAD_PQM_HAT_X
      end

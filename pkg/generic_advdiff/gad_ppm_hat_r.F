#     include "GAD_OPTIONS.h"

      SUBROUTINE GAD_PPM_HAT_R(bi,bj,ix,iy,
     &           method,mask,fbar,edge,
     &           ohat,fhat,myThid)
C     |================================================================|
C     | PPM_HAT_R: reconstruct grid-cell PPM polynomials.              |
C     |================================================================|

          implicit none

C     =============================================== global variables
#         include "SIZE.h"
#         include "GRID.h"
#         include "GAD.h"

C     ====================================================== arguments
C       bi,bj     :: tile indexing.
C       ix,iy     :: x-,y-indexing.
C       method    :: advection scheme.
C       mask      :: row of cell-wise masking values.
C       fbar      :: row of cell-wise values.
C       edge      :: row of edge-wise values.
C       ohat      :: row of oscl. coeff.
C                    - OHAT(1,:) = D^1F/DS^1.
C                    - OHAT(2,:) = D^2F/DS^2.
C       fhat      :: row of poly. coeff.
C                    - FHAT(:,I) = PPM coeff.
C       myThid    :: thread number.
C     ================================================================
          integer bi,bj,ix,iy
          integer method
          _RL mask(1-3:Nr+3)
          _RL fbar(1-3:Nr+3)
          _RL edge(1-0:Nr+1)
          _RL ohat(1:2,
     &             1-3:Nr+3)
          _RL fhat(1:3,
     &             1-0:Nr+0)
          integer myThid

C     ====================================================== variables
C       ii,ir     :: local r-indexing.
C       ff00      :: centre-biased cell mean.
C       ffll,ffrr :: left-, and right-biased cell values.
C       fell,ferr :: left-, and right-biased edge values.
C       dfds      :: linear slope estimates.
C       uhat      :: "NULL" limited poly. coeff.
C       lhat      :: "MONO" limited poly. coeff.
C       scal      :: "WENO" weights.
C       fmag,fdel :: local perturbation indicators.
C     ================================================================
          integer ii,ir
          _RL ff00,ffll,ffrr
          _RL fell,ferr
          _RL dfds(-1:+1)
          _RL uhat(+1:+5)
          _RL lhat(+1:+5)
          _RL scal(+1:+2)
          _RL fmag,fdel
          integer  mono

          do  ir = +1, Nr

C     =============================== assemble cell mean + edge values
              ff00 = fbar(ir+0)
              ffll = ff00
     &             + mask(ir-1)*(fbar(ir-1)-ff00)
              ffrr = ff00
     &             + mask(ir+1)*(fbar(ir+1)-ff00)

              fell = edge(ir-0)
              ferr = edge(ir+1)

c             select case(method)
c             case(ENUM_PPM_NULL_LIMIT)
              if ( method.eq.ENUM_PPM_NULL_LIMIT ) then
C     =============================== "NULL" limited grid-cell profile
              CALL GAD_PPM_FUN_NULL ( ff00,fell,ferr,
     &             lhat,mono)

c             case(ENUM_PPM_MONO_LIMIT)
              elseif ( method.eq.ENUM_PPM_MONO_LIMIT ) then
C     =============================== "MONO" limited grid-cell profile
              CALL GAD_PLM_FUN_U(ffll,ff00,ffrr,dfds)

              CALL GAD_PPM_FUN_MONO ( ff00,ffll,ffrr,
     &             fell,ferr,dfds,lhat,mono)

c             case(ENUM_PPM_WENO_LIMIT)
              elseif ( method.eq.ENUM_PPM_WENO_LIMIT ) then
C     =============================== "WENO" limited grid-cell profile
              CALL GAD_PLM_FUN_U(ffll,ff00,ffrr,dfds)

              CALL GAD_PPM_FUN_NULL ( ff00,fell,ferr,
     &             uhat,mono)

              CALL GAD_PPM_FUN_MONO ( ff00,ffll,ffrr,
     &             fell,ferr,dfds,lhat,mono)

              if ( mono .gt. 0) then

C     =============================== only apply WENO if it is worth it
              fdel = abs(ffrr-ff00)+abs(ff00-ffll)
              fmag = abs(ffll)+abs(ff00)+abs(ffrr)

              if (fdel .gt. 1. _d -6 * fmag) then

C     =============================== calc. WENO oscillation weighting
                  CALL GAD_OSC_MUL_R(ir,+2,mask,
     &                 ohat,scal)

                  do  ii = +1, +3
C     =============================== blend limited/un-limited profile
                  lhat(ii) = scal(1) * uhat(ii)
     &                     + scal(2) * lhat(ii)
                  end do

              end if

              end if

c             end select
              endif

              do  ii = +1, +3
C     =============================== copy polynomial onto output data
                  fhat(ii,ir) = lhat(ii)
              end do

          end do

          return

c     end subroutine GAD_PPM_HAT_R
      end

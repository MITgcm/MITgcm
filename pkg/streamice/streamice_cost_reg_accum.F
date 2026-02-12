#include "STREAMICE_OPTIONS.h"
#ifdef ALLOW_AUTODIFF
# include "AUTODIFF_OPTIONS.h"
#endif
#ifdef ALLOW_COST
# include "COST_OPTIONS.h"
#endif

      subroutine streamice_cost_reg_accum ( myIter, myThid )
C     *==========================================================*
C     | subroutine streamice_cost_reg_accum                      |
C     | o this routine calculates the regularisation contri-     |
C     |   bution to the per-timestep STREAMICE cost              |
C     *==========================================================*
C     |                                                          |
C     | Notes                                                    |
C     | =====                                                    |
C     *==========================================================*
      IMPLICIT NONE

C     == Global variables ===
#include "SIZE.h"
#include "GRID.h"
#include "EEPARAMS.h"
#include "PARAMS.h"
#include "DYNVARS.h"
#ifdef ALLOW_STREAMICE
# include "STREAMICE.h"
#endif

#ifdef ALLOW_COST
# include "cost.h"
#endif
#ifdef ALLOW_AUTODIFF_TAMC
# include "tamc.h"
#endif

C     == Routine arguments ==
C     myThid - Thread number for this instance of the routine.
      integer myIter, myThid

#ifdef ALLOW_STREAMICE_TC_COST
C     == Local variables
      _RL HAF
      integer i, j, k, bi, bj
      integer ig, jg
      integer itlo,ithi
      integer jtlo,jthi
      integer il
      logical calc_prior_cost_bglen

      _RL i_numcells, dCdx, dCdy, dBdx, dBdy, gridtimfac, dMdx, dMdy
      _RL rhoi, rhow, r, i_r, surf_err, h, hf
      _RL cfricval, bglenval, cfricvalp1, bglenvalp1

      if (STREAMICEBglenCostMaskFile .ne. ' ') then
       calc_prior_cost_bglen = .true.
      else
       calc_prior_cost_bglen = .false.
      endif

      rhoi = streamice_density
      rhow = streamice_density_ocean_avg
      r=rhoi/rhow
      i_r = 1./r
      i_numcells = 1.0/(Nx*Ny)
      gridtimfac = i_numcells / nTimeSteps

C--   Calculate cost function on tile of this instance
      DO bj=myByLo(myThid),myByHi(myThid)
        DO bi=myBxLo(myThid),myBxHi(myThid)
          do j=1,sNy
            do i=1,sNx

             cfricval = C_basal_friction(i,j,bi,bj)
             bglenval = B_glen(i,j,bi,bj)

             cfricvalp1 = C_basal_friction(i+1,j,bi,bj)
             dCdx = (Cfricvalp1-cfricval)/
     &              (dxC(i+1,j,bi,bj))
             cfricvalp1 = C_basal_friction(i,j+1,bi,bj)
             dCdy = (Cfricvalp1-cfricval)/
     &              (dxC(i,j+1,bi,bj))

             bglenvalp1 = B_glen(i+1,j,bi,bj)
             dBdx = (bglenvalp1-
     &               bglenval) /
     &              (dxC(i+1,j,bi,bj))
             bglenvalp1 = B_glen(i,j+1,bi,bj)
             dBdy = (bglenvalp1-
     &               bglenval) /
     &               (dyC(i,j+1,bi,bj))
             dMdx = (streamice_bdot_maxmelt_v(i+1,j,bi,bj)-
     &               streamice_bdot_maxmelt_v(i,j,bi,bj)) /
     &              (dxC(i+1,j,bi,bj))
             dMdy = (streamice_bdot_maxmelt_v(i,j+1,bi,bj)-
     &               streamice_bdot_maxmelt_v(i,j,bi,bj)) /
     &              (dyC(i+1,j,bi,bj))

             if (streamice_hmask(i,j,bi,bj).eq.1.0) then

              cost_func1_streamice(bi,bj) =
     &         cost_func1_streamice(bi,bj) +
     &         streamice_wgt_tikh_beta * (dCdx**2+dCdy**2) * gridtimfac
              cost_smooth_fric_streamice(bi,bj) =
     &         cost_smooth_fric_streamice(bi,bj) +
     &         streamice_wgt_tikh_beta * (dCdx**2+dCdy**2) * gridtimfac

              cost_func1_streamice(bi,bj) =
     &         cost_func1_streamice(bi,bj) +
     &         streamice_wgt_tikh_bglen * (dBdx**2+dBdy**2) * gridtimfac
              cost_smooth_glen_streamice(bi,bj) =
     &         cost_smooth_glen_streamice(bi,bj) +
     &         streamice_wgt_tikh_bglen * (dBdx**2+dBdy**2) * gridtimfac

              cost_func1_streamice(bi,bj) =
     &         cost_func1_streamice(bi,bj) +
     &         streamice_wgt_tikh_gen * (dMdx**2+dMdy**2) * gridtimfac
              cost_smooth_bdotmelt_streamice(bi,bj) =
     &         cost_smooth_bdotmelt_streamice(bi,bj) +
     &         streamice_wgt_tikh_gen * (dMdx**2+dMdy**2) * gridtimfac

              h = H_streamice(i,j,bi,bj)
              hf = -1.0 * i_r * R_low_si (i,j,bi,bj)

              IF ((h-hf) .gt. 5. .AND. B_glen0(i,j,bi,bj).gt.0.0 .and.
     &            calc_prior_cost_bglen) then

                 cost_func1_streamice(bi,bj) =
     &            cost_func1_streamice(bi,bj) +
     &            streamice_wgt_prior_bglen * gridtimfac *
     &            (B_glen(i,j,bi,bj)-B_glen0(i,j,bi,bj))**2
                 cost_prior_streamice(bi,bj) =
     &            cost_prior_streamice(bi,bj) +
     &            streamice_wgt_prior_bglen * gridtimfac *
     &            (B_glen(i,j,bi,bj)-B_glen0(i,j,bi,bj))**2

              ENDIF

             endif

            end do
          end do
        end do
      end do

#endif

      RETURN
      END

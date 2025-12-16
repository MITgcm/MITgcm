#include "STREAMICE_OPTIONS.h"
#ifdef ALLOW_COST
#include "COST_OPTIONS.h"
#endif

      subroutine streamice_cost_final( myThid )
C     /==========================================================\
C     | subroutine streamice_cost_final                          |
C     | o this routine computes the cost function contribution   |
C     |   from the STREAMICE package for the tiles of this       |
C     |   processor. Called from cost_final                      |
C     |==========================================================|
C     |                                                          |
C     | Notes                                                    |
C     | =====                                                    |
C     \==========================================================/
      IMPLICIT NONE

C     == Global variables ===
#include "SIZE.h"
#include "EEPARAMS.h"
#include "PARAMS.h"
#include "DYNVARS.h"
#include "GRID.h"
#ifdef ALLOW_STREAMICE
# include "STREAMICE.h"
#endif

#ifdef ALLOW_COST
#include "cost.h"
#endif

C     == Routine arguments ==
C     myThid - Thread number for this instance of the routine.
      integer myThid

#ifdef ALLOW_COST_STREAMICE
C     == Local variables
      integer bi, bj
      integer i, j, k
      integer ig, jg
      integer itlo,ithi
      integer jtlo,jthi
      _RL i_numcells, dCdx, dCdy, dBdx, dBdy, dmdx, dmdy, idt
      _RL utmp, vtmp, uotmp, votmp, cotmp, dhdt, dhdt_fac
      _RL dRdx, dRdy, cfricval, bglenval, cfricvalp1, bglenvalp1
      INTEGER  ILNBLNK
      EXTERNAL ILNBLNK
      CHARACTER*(MAX_LEN_FNAM) STREAMICExvelOptimFile
      CHARACTER*(MAX_LEN_FNAM) STREAMICEyvelOptimFile
      CHARACTER*(MAX_LEN_FNAM) STREAMICEerrvelOptimFile
      LOGICAL calc_prior_cost_bglen
      _RL U_obs (1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      _RL V_obs (1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      _RL U_err (1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      _RL R0 (1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)

      _RL smooth_cost_fric  (nSx,nSy)
      _RL smooth_cost_maxmelt (nSx,nSy)
      _RL smooth_cost_prior (nSx,nSy)
      _RL smooth_cost_bglen (nSx,nSy)
      _RL cost_misfit (nSx,nSy)
      _RL cost_misfit_norm (nSx,nSy)

      _RL smooth_cost_fric_gl, smooth_cost_prior_gl,
     &    smooth_cost_bglen_gl, cost_misfit_gl,
     &    cost_thin_gl, cost_misfit_norm_gl,
     &    cost_vel_td_gl, smooth_cost_maxmelt_gl
      _RL rhoi, rhow, r, i_r, h, hf, r_low_tmp, haf

      jtlo = myByLo(myThid)
      jthi = myByHi(myThid)
      itlo = myBxLo(myThid)
      ithi = myBxHi(myThid)

      i_numcells = 1.0/(Nx*Ny)
      idt = 1.0/deltaT*365.0*86400.0

      _BARRIER

       STREAMICExvelOptimFile=
     &  STREAMICEvelOptimSnapBasename(1:
     &  ILNBLNK(STREAMICEvelOptimSnapBasename))//"u.bin"
       STREAMICEyvelOptimFile=
     &  STREAMICEvelOptimSnapBasename(1:
     &  ILNBLNK(STREAMICEvelOptimSnapBasename))//"v.bin"
       STREAMICEerrvelOptimFile=
     &  STREAMICEvelOptimSnapBasename(1:
     &  ILNBLNK(STREAMICEvelOptimSnapBasename))//"err.bin"

      if (streamice_do_snapshot_cost) then
       CALL READ_FLD_XY_RL( STREAMICExvelOptimFile, ' ',
     &      U_obs, 0, myThid )
       print *, 'GOT HERE read velobsu'
       CALL READ_FLD_XY_RL( STREAMICEyvelOptimFile, ' ',
     &      V_obs, 0, myThid )
       print *, 'GOT HERE read velobsv'
       CALL READ_FLD_XY_RL( STREAMICEerrvelOptimFile, ' ',
     &      U_err, 0, myThid )
       print *, 'GOT HERE read uerr'
      endif
      if (STREAMICEBglenCostMaskFile .ne. ' ') then
       calc_prior_cost_bglen = .true.
      else
       calc_prior_cost_bglen = .false.
      endif
      CALL READ_FLD_XY_RL(STREAMICEtopogFile, ' ',
     &      R0, 0, myThid )
      print *, 'GOT HERE read topo file'

      _EXCH_XY_RL (R0, myThid)

C--   Calculate mask for tracer cells  (0 => land, 1 => water)
c       k=1

C--   Calculate cost function on tile of this instance

      rhoi = streamice_density
      rhow = streamice_density_ocean_avg
      r=rhoi/rhow
      i_r = 1/r

      do bj = jtlo,jthi
        do bi = itlo,ithi

          smooth_cost_fric (bi,bj) = 0.0
          smooth_cost_maxmelt (bi,bj) = 0.0
          smooth_cost_bglen (bi,bj) = 0.0
          smooth_cost_prior (bi,bj) = 0.0
          cost_misfit (bi,bj) = 0.0
          cost_misfit_norm (bi,bj) = 0.0

          do j=1,sNy
            do i=1,sNx

#ifdef ALLOW_STREAMICE_TC_COST
             IF ( .not. STREAMICE_do_timedep_cost ) THEN
#endif
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

              dRdx = (R_low_si(i+1,j,bi,bj)-
     &               R_low_si(i,j,bi,bj)) /
     &              (dxC(i+1,j,bi,bj))
              dRdy = (R_low_si(i,j+1,bi,bj)-
     &               R_low_si(i,j,bi,bj)) /
     &               (dyC(i,j+1,bi,bj))

#ifdef ALLOW_STREAMICE_TC_COST
             endif
#endif

             if (STREAMICE_do_snapshot_cost) then

              utmp = streamice_u_surf(i,j,bi,bj)
              vtmp = streamice_v_surf(i,j,bi,bj)
              uotmp = U_obs(i,j,bi,bj)
              votmp = V_obs(i,j,bi,bj)
              cotmp = sqrt(uotmp**2+votmp**2)

              IF((cotmp.ne.0.0) .and.
     &         streamice_hmask(i,j,bi,bj).eq.1.0) THEN

               tile_fc (bi,bj) = tile_fc (bi,bj) +
     &         streamice_wgt_vel *
     &         (
     &         0.5 * (streamice_u_surf(i,j,bi,bj)-
     &                U_obs(i,j,bi,bj))**2 +
     &         0.5 * (streamice_v_surf(i,j,bi,bj)-
     &                V_obs(i,j,bi,bj))**2
     &         ) * I_numcells
     &         / (1.0+U_err(i,j,bi,bj)**2)

               cost_misfit (bi,bj) = cost_misfit (bi,bj) +
     &         streamice_wgt_vel *
     &         (
     &         0.5 * (streamice_u_surf(i,j,bi,bj)-
     &                U_obs(i,j,bi,bj))**2 +
     &         0.5 * (streamice_v_surf(i,j,bi,bj)-
     &                V_obs(i,j,bi,bj))**2
     &         ) * I_numcells
     &         / (1.0+U_err(i,j,bi,bj)**2)

               tile_fc (bi,bj) = tile_fc (bi,bj) +
     &         streamice_wgt_vel_norm *
     &         (
     &         0.5 * (streamice_u_surf(i,j,bi,bj)-
     &                U_obs(i,j,bi,bj))**2 +
     &         0.5 * (streamice_v_surf(i,j,bi,bj)-
     &                V_obs(i,j,bi,bj))**2
     &         ) * I_numcells
     &         / (U_err(i,j,bi,bj)**2+
     &            U_obs(i,j,bi,bj)**2+V_obs(i,j,bi,bj)**2)

               cost_misfit_norm (bi,bj) = cost_misfit_norm (bi,bj) +
     &         streamice_wgt_vel_norm *
     &         (
     &         0.5 * (streamice_u_surf(i,j,bi,bj)-
     &                U_obs(i,j,bi,bj))**2 +
     &         0.5 * (streamice_v_surf(i,j,bi,bj)-
     &                V_obs(i,j,bi,bj))**2
     &         ) * I_numcells
     &         / (U_err(i,j,bi,bj)**2+
     &            U_obs(i,j,bi,bj)**2+V_obs(i,j,bi,bj)**2)

              ENDIF
             endif

#ifdef ALLOW_STREAMICE_TC_COST
             IF ( .not. STREAMICE_do_timedep_cost ) THEN
#endif

              if(streamice_hmask(i,j,bi,bj).eq.1.0) then
               tile_fc (bi,bj) = tile_fc (bi,bj) +
     &         streamice_wgt_tikh_beta * (dCdx**2+dCdy**2) * I_numcells
               smooth_cost_fric (bi,bj) = smooth_cost_fric (bi,bj) +
     &         streamice_wgt_tikh_beta * (dCdx**2+dCdy**2) * I_numcells

               tile_fc (bi,bj) = tile_fc (bi,bj) +
     &         streamice_wgt_tikh_bglen * (dBdx**2+dBdy**2) * I_numcells
               smooth_cost_bglen (bi,bj) = smooth_cost_bglen (bi,bj) +
     &         streamice_wgt_tikh_bglen * (dBdx**2+dBdy**2) * I_numcells

               tile_fc (bi,bj) = tile_fc (bi,bj) +
     &         0.e5 * (dRdx**2+dRdy**2) * I_numcells

               h = H_streamice(i,j,bi,bj)
               hf = -1.0 * i_r * R_low_si (i,j,bi,bj)

               IF ((h-hf) .gt. 5. .AND. B_glen0(i,j,bi,bj).gt.0.0 .and.
     &             calc_prior_cost_bglen) then

                 tile_fc (bi,bj) = tile_fc (bi,bj) +
     &            streamice_wgt_prior_bglen * I_numcells *
     &            (B_glen(i,j,bi,bj)-B_glen0(i,j,bi,bj))**2

                 smooth_cost_prior (bi,bj) = smooth_cost_prior (bi,bj) +
     &            streamice_wgt_prior_bglen * I_numcells *
     &            (B_glen(i,j,bi,bj)-B_glen0(i,j,bi,bj))**2

               ENDIF
              endif

#ifdef ALLOW_STREAMICE_TC_COST
             ENDIF
#endif

             if (streamice_do_verification_cost) then
              if (streamice_hmask(i,j,bi,bj).eq.1.0) then
               tile_fc (bi,bj) = tile_fc (bi,bj) +
     &         u_streamice(i,j,bi,bj)**2+v_streamice(i,j,bi,bj)**2+
     &         h_streamice(i,j,bi,bj)**2
              endif
             endif

             if (streamice_do_vaf_cost) then
              if (streamice_hmask(i,j,bi,bj).eq.1.0) then

               r_low_tmp = R_low_si (i,j,bi,bj)

               if (r_low_tmp .ge. 0.0) then
                HAF = h_streamice (i,j,bi,bj)
               else
                HAF = h_streamice (i,j,bi,bj) +
     &           (streamice_density_ocean_avg / streamice_density
     &           * r_low_tmp)
               endif

               if (HAF .gt. 0.0) then
                tile_fc(bi,bj) = tile_fc(bi,bj) +
     &          HAF * rA (i,j,bi,bj)
               endif

              endif
             endif

            end do
          end do

          tile_fc (bi,bj) =
     &       tile_fc (bi,bj) + cost_func1_streamice(bi,bj)

#ifdef ALLOW_STREAMICE_TC_COST
          if ( STREAMICE_do_timedep_cost ) then
           smooth_cost_prior(bi,bj) = cost_prior_streamice(bi,bj)
           smooth_cost_fric(bi,bj) = cost_smooth_fric_streamice(bi,bj)
           smooth_cost_bglen(bi,bj) = cost_smooth_glen_streamice(bi,bj)
           smooth_cost_maxmelt(bi,bj) =
     &                        cost_smooth_bdotmelt_streamice(bi,bj)
          endif
#endif

        end do
      end do

      CALL GLOBAL_SUM_TILE_RL
     &   ( smooth_cost_fric, smooth_cost_fric_gl, myThid )
      CALL GLOBAL_SUM_TILE_RL
     &   ( smooth_cost_bglen, smooth_cost_bglen_gl, myThid )
      CALL GLOBAL_SUM_TILE_RL
     &   ( smooth_cost_prior, smooth_cost_prior_gl, myThid )
      CALL GLOBAL_SUM_TILE_RL
     &   ( cost_misfit, cost_misfit_gl, myThid )
      CALL GLOBAL_SUM_TILE_RL
     &   ( cost_misfit_norm, cost_misfit_norm_gl, myThid )
      CALL GLOBAL_SUM_TILE_RL
     &   ( cost_surf_streamice, cost_thin_gl, myThid )
      CALL GLOBAL_SUM_TILE_RL
     &   ( cost_vel_streamice, cost_vel_td_gl, myThid )
      CALL GLOBAL_SUM_TILE_RL
     &   ( smooth_cost_maxmelt, smooth_cost_maxmelt_gl, myThid )
       write(standardMessageUnit,'(A,D22.15)') 'maxmlt smooth contr = ',
     &   smooth_cost_maxmelt_gl

      if (STREAMICE_do_timedep_cost .or.
     &    STREAMICE_do_snapshot_cost) then

       write(standardMessageUnit,'(A,D22.15)') 'fric smooth contr = ',
     &   smooth_cost_fric_gl
       write(standardMessageUnit,'(A,D22.15)') 'bglen smooth contr = ',
     &   smooth_cost_bglen_gl
       write(standardMessageUnit,'(A,D22.15)') 'prior smooth contr = ',
     &   smooth_cost_prior_gl
       write(standardMessageUnit,'(A,D22.15)') 'vel misfit = ',
     &   cost_misfit_gl
       write(standardMessageUnit,'(A,D22.15)') 'vel misfit norm = ',
     &   cost_misfit_norm_gl

      endif

      if (STREAMICE_do_timedep_cost) then

       write(standardMessageUnit,'(A,D22.15)') 'thinning contr = ',
     &   cost_thin_gl
       write(standardMessageUnit,'(A,D22.15)') 'td vel misfit = ',
     &   cost_vel_td_gl

      endif

#endif

      RETURN
      END

C $Header: /u/gcmpack/MITgcm_contrib/dgoldberg/code_ad_flowline/cost_test.F,v 1.1 2012/07/20 13:42:08 heimbach Exp $
C $Name:  $

#include "STREAMICE_OPTIONS.h"
#ifdef ALLOW_COST
#include "COST_OPTIONS.h"
#endif

      subroutine streamice_cost_final( myThid )
C     /==========================================================\
C     | subroutine cost_test                                     |
C     | o this routine computes the cost function for the tiles  |
C     |   of this processor                                      |
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
      integer bi, bj
      integer myThid

#ifdef ALLOW_COST_STREAMICE
C     == Local variables
      _RL thetaRef, HAF

      integer i, j, k
      integer ig, jg
      integer itlo,ithi
      integer jtlo,jthi
      _RL i_numcells, dCdx, dCdy, dBdx, dBdy, dmdx, dmdy, idt
      _RL utmp, vtmp, uotmp, votmp, cotmp, dhdt, dhdt_fac
      _RL dRdx, dRdy
      INTEGER  ILNBLNK
      EXTERNAL ILNBLNK
      CHARACTER*(MAX_LEN_FNAM) STREAMICExvelOptimFile
      CHARACTER*(MAX_LEN_FNAM) STREAMICEyvelOptimFile
      CHARACTER*(MAX_LEN_FNAM) STREAMICEerrvelOptimFile
      LOGICAL calc_prior_cost_bglen
      _RL U_obs (1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      _RL V_obs (1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      _RL U_err (1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      _RL Bglen0 (1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      _RL BglenMask (1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      _RL R0 (1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)

      _RL smooth_cost_fric  (nSx,nSy)
      _RL smooth_cost_prior (nSx,nSy)
      _RL smooth_cost_bglen (nSx,nSy)
      _RL cost_misfit (nSx,nSy)
      _RL cost_misfit_norm (nSx,nSy)


      _RL smooth_cost_fric_gl, smooth_cost_prior_gl,
     &    smooth_cost_bglen_gl, cost_misfit_gl,
     &    cost_thin_gl, cost_misfit_norm_gl,
     &    cost_vel_td_gl
      _RL rhoi, rhow, r, i_r, h, hf


      jtlo = mybylo(mythid)
      jthi = mybyhi(mythid)
      itlo = mybxlo(mythid)
      ithi = mybxhi(mythid)


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
       CALL READ_FLD_XY_RL(STREAMICEBGlenCostMaskFile, ' ',
     &      Bglen0, 0, myThid )
       print *, 'GOT HERE read Bglen file'
       calc_prior_cost_bglen = .true.
      else
       calc_prior_cost_bglen = .false.
      endif
      CALL READ_FLD_XY_RL(STREAMICEtopogFile, ' ',
     &      R0, 0, myThid )
      print *, 'GOT HERE read topo file'

      _EXCH_XY_RL (R0, mythid)


C--   Calculate mask for tracer cells  (0 => land, 1 => water)
!       k=1

C--   Calculate cost function on tile of this instance

      rhoi = streamice_density
      rhow = streamice_density_ocean_avg
      r=rhoi/rhow
      i_r = 1/r

      do bj = jtlo,jthi
        do bi = itlo,ithi

          smooth_cost_fric (bi,bj) = 0.0
          smooth_cost_bglen (bi,bj) = 0.0
          smooth_cost_prior (bi,bj) = 0.0
          cost_misfit (bi,bj) = 0.0
          cost_misfit_norm (bi,bj) = 0.0

          do j=1,sNy
            do i=1,sNx

             dCdx = (C_basal_friction(i+1,j,bi,bj)-
     &               C_basal_friction(i,j,bi,bj)) /
     &              (dxC(i+1,j,bi,bj))
             dCdy = (C_basal_friction(i,j+1,bi,bj)-
     &               C_basal_friction(i,j,bi,bj)) /
     &               (dyC(i,j+1,bi,bj))

             dBdx = (B_glen(i+1,j,bi,bj)-
     &               B_glen(i,j,bi,bj)) /
     &              (dxC(i+1,j,bi,bj))
             dBdy = (B_glen(i,j+1,bi,bj)-
     &               B_glen(i,j,bi,bj)) /
     &               (dyC(i,j+1,bi,bj))

             dRdx = (R_low_si(i+1,j,bi,bj)-
     &               R_low_si(i,j,bi,bj)) /
     &              (dxC(i+1,j,bi,bj))
             dRdy = (R_low_si(i,j+1,bi,bj)-
     &               R_low_si(i,j,bi,bj)) /
     &               (dyC(i,j+1,bi,bj))

             if (STREAMICE_do_snapshot_cost) then
   
              utmp = streamice_u_surf(i,j,bi,bj)
              vtmp = streamice_v_surf(i,j,bi,bj)
              uotmp = U_obs(i,j,bi,bj)
              votmp = V_obs(i,j,bi,bj)
              cotmp = sqrt(uotmp**2+votmp**2)

              IF((cotmp.ne.0.0) .and. 
     &         streamice_hmask(i,j,bi,bj).eq.1.0) THEN


               tile_fc (bi,bj) = tile_fc (bi,bj) + 
     &         ( 
     &         streamice_wgt_vel *
     &         0.5 * (streamice_u_surf(i,j,bi,bj)-
     &                U_obs(i,j,bi,bj))**2 +
     &         0.5 * (streamice_v_surf(i,j,bi,bj)-
     &                V_obs(i,j,bi,bj))**2) * I_numcells
     &         / (1.0+U_err(i,j,bi,bj))**2

               cost_misfit (bi,bj) = cost_misfit (bi,bj) + 
     &         (
     &         streamice_wgt_vel *
     &         0.5 * (streamice_u_surf(i,j,bi,bj)-
     &                U_obs(i,j,bi,bj))**2 +
     &         0.5 * (streamice_v_surf(i,j,bi,bj)-
     &                V_obs(i,j,bi,bj))**2) * I_numcells
     &         / (1.0+U_err(i,j,bi,bj))**2

               tile_fc (bi,bj) = tile_fc (bi,bj) + 
     &         ( 
     &         streamice_wgt_vel_norm *
     &         0.5 * (streamice_u_surf(i,j,bi,bj)-
     &                U_obs(i,j,bi,bj))**2 +
     &         0.5 * (streamice_v_surf(i,j,bi,bj)-
     &                V_obs(i,j,bi,bj))**2) * I_numcells
     &         / (1.0+U_obs(i,j,bi,bj)**2+V_obs(i,j,bi,bj)**2)

               cost_misfit_norm (bi,bj) = cost_misfit_norm (bi,bj) + 
     &         (
     &         streamice_wgt_vel_norm *
     &         0.5 * (streamice_u_surf(i,j,bi,bj)-
     &                U_obs(i,j,bi,bj))**2 +
     &         0.5 * (streamice_v_surf(i,j,bi,bj)-
     &                V_obs(i,j,bi,bj))**2) * I_numcells
     &         / (1.0+U_obs(i,j,bi,bj)**2+V_obs(i,j,bi,bj)**2)

              ENDIF
             endif


             if(streamice_hmask(i,j,bi,bj).eq.1.0) then
              tile_fc (bi,bj) = tile_fc (bi,bj) +
     &        streamice_wgt_tikh_beta * (dCdx**2+dCdy**2) * I_numcells 
              smooth_cost_fric (bi,bj) = smooth_cost_fric (bi,bj) +
     &        streamice_wgt_tikh_beta * (dCdx**2+dCdy**2) * I_numcells 

              tile_fc (bi,bj) = tile_fc (bi,bj) +
     &        streamice_wgt_tikh_bglen * (dBdx**2+dBdy**2) * I_numcells
              smooth_cost_bglen (bi,bj) = smooth_cost_bglen (bi,bj) +
     &        streamice_wgt_tikh_bglen * (dBdx**2+dBdy**2) * I_numcells

              tile_fc (bi,bj) = tile_fc (bi,bj) +
     &        0.e5 * (dRdx**2+dRdy**2) * I_numcells

              h = H_streamice(i,j,bi,bj)
              hf = -1.0 * i_r * R_low_si (i,j,bi,bj)

              IF ((h-hf) .gt. 5. .AND. Bglen0(i,j,bi,bj).gt.0.0 .and.
     &            calc_prior_cost_bglen) then

                tile_fc (bi,bj) = tile_fc (bi,bj) + 
     &           streamice_wgt_prior_bglen * I_numcells *  
     &           (B_glen(i,j,bi,bj)-Bglen0(i,j,bi,bj))**2

                smooth_cost_prior (bi,bj) = smooth_cost_prior (bi,bj) + 
     &           streamice_wgt_prior_bglen * I_numcells *  
     &           (B_glen(i,j,bi,bj)-Bglen0(i,j,bi,bj))**2

              ENDIF
             endif

            end do
          end do

          tile_fc (bi,bj) =
     &       tile_fc (bi,bj) + cost_func1_streamice(bi,bj)

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

      write(standardmessageunit,'(A,D22.15)') 'fric smooth contr = ', 
     &   smooth_cost_fric_gl
      write(standardmessageunit,'(A,D22.15)') 'bglen smooth contr = ', 
     &   smooth_cost_bglen_gl
      write(standardmessageunit,'(A,D22.15)') 'prior smooth contr = ', 
     &   smooth_cost_prior_gl
      write(standardmessageunit,'(A,D22.15)') 'vel misfit = ', 
     &   cost_misfit_gl
      write(standardmessageunit,'(A,D22.15)') 'vel misfit norm = ', 
     &   cost_misfit_norm_gl
      write(standardmessageunit,'(A,D22.15)') 'thinning contr = ', 
     &   cost_thin_gl
      write(standardmessageunit,'(A,D22.15)') 'td vel misfit = ', 
     &   cost_vel_td_gl

#endif

      RETURN
      END

C $Header: /u/gcmpack/MITgcm/pkg/streamice/streamice_cost_surf_accum.F,v 1.4 2014/09/09 23:09:21 jmc Exp $
C $Name:  $

#include "STREAMICE_OPTIONS.h"
#ifdef ALLOW_AUTODIFF
# include "AUTODIFF_OPTIONS.h"
#endif
#ifdef ALLOW_COST
# include "COST_OPTIONS.h"
#endif

      subroutine streamice_cost_surf_accum ( myThid, myIter )
C     *==========================================================*
C     | subroutine cost_test                                     |
C     | o this routine computes the cost function for the tiles  |
C     |   of this processor                                      |
C     *==========================================================*
C     |                                                          |
C     | Notes                                                    |
C     | =====                                                    |
C     *==========================================================*
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
# include "cost.h"
#endif
#ifdef ALLOW_AUTODIFF_TAMC
# include "tamc.h"
#endif

C     == Routine arguments ==
C     myThid - Thread number for this instance of the routine.
      integer myThid, myIter

#ifdef ALLOW_COST_TEST
C     == Local variables
      _RL thetaRef, HAF

      integer i, j, k, bi, bj
      integer ig, jg
      integer itlo,ithi
      integer jtlo,jthi
      integer il
      INTEGER  ILNBLNK
      EXTERNAL ILNBLNK
      CHARACTER*(MAX_LEN_FNAM) suff
      CHARACTER*(MAX_LEN_FNAM) STREAMICEsurfOptimFile
      _RL S_obs (1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)

      jtlo = mybylo(myThid)
      jthi = mybyhi(myThid)
      itlo = mybxlo(myThid)
      ithi = mybxhi(myThid)

      WRITE(suff,'(a,I10.10,a)') 'land_ice_surf.',myIter, '.bin'

      CALL READ_FLD_XY_RL( suff, ' ',
     &      S_obs, 0, myThid )

C--   Calculate mask for tracer cells  (0 => land, 1 => water)
!       k=1

#ifdef ALLOW_AUTODIFF_TAMC
CADJ STORE surf_el_streamice  = comlev1, key = ikey_dynamics,
CADJ &     kind = isbyte
CADJ STORE H_streamice_prev  = comlev1, key = ikey_dynamics,
CADJ &     kind = isbyte
CADJ STORE H_streamice  = comlev1, key = ikey_dynamics,
CADJ &     kind = isbyte
#endif

C--   Calculate cost function on tile of this instance
      do bj = jtlo,jthi
        do bi = itlo,ithi
          do j=1,sNy
            do i=1,sNx

!             S_obs(i,j,bi,bj) = 0.0
             if (streamice_cost_mask(i,j,bi,bj).eq.1.0) THEN
              cost_func1_streamice (bi,bj) =
     &        cost_func1_streamice (bi,bj) +
     &        streamice_wgt_surf * (S_obs(i,j,bi,bj)-
     &               surf_el_streamice(i,j,bi,bj))**2 / Nx / Ny +
     &        streamice_wgt_drift * (H_streamice(i,j,bi,bj)-
     &               H_streamice_prev(i,j,bi,bj))**2 / Nx / Ny
             endif

            end do
          end do
        end do
      end do

#endif

      RETURN
      END

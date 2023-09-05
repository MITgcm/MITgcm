#include "STREAMICE_OPTIONS.h"
c#ifdef ALLOW_AUTODIFF
c# include "AUTODIFF_OPTIONS.h"
c#endif
#ifdef ALLOW_COST
# include "COST_OPTIONS.h"
#endif

      subroutine streamice_cost_surf_accum ( myIter, myThid )
C     *==========================================================*
C     | subroutine streamice_cost_surf_accum                     |
C     | o this routine calculates the surface misfit contri-     |
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
c#ifdef ALLOW_AUTODIFF_TAMC
c# include "tamc.h"
c#endif

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
      CHARACTER*(10) myIterStr
      INTEGER  ILNBLNK
      EXTERNAL ILNBLNK
      CHARACTER*(MAX_LEN_FNAM) surffilename, errsurffilename
      _RL surfobs (1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      _RL errsurfobs (1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      _RL rhoi, rhow, r, i_r, i_numcells, surf_err, h, hf
      WRITE(myIterStr,'(I10.10)') myIter
      surffilename=STREAMICEsurfOptimTCBasename(1:
     & ILNBLNK(STREAMICEsurfOptimTCBasename))//myIterStr//".bin"
      errsurffilename=STREAMICEsurfOptimTCBasename(1:
     & ILNBLNK(STREAMICEsurfOptimTCBasename))//myIterStr//"err.bin"
      CALL READ_FLD_XY_RL( surffilename, ' ', surfobs,
     &  0, myThid )
      CALL READ_FLD_XY_RL( errsurffilename, ' ', errsurfobs,
     &  0, myThid )

c#ifdef ALLOW_AUTODIFF_TAMC
cCADJ STORE surf_el_streamice  = comlev1, key = ikey_dynamics,
cCADJ &     kind = isbyte
cCADJ STORE H_streamice_prev  = comlev1, key = ikey_dynamics,
cCADJ &     kind = isbyte
cCADJ STORE H_streamice  = comlev1, key = ikey_dynamics,
cCADJ &     kind = isbyte
c#endif

      rhoi = streamice_density
      rhow = streamice_density_ocean_avg
      r=rhoi/rhow
      i_r = 1./r
      i_numcells = 1.0/(Nx*Ny)

C--   Calculate cost function on tile of this instance
      DO bj=myByLo(myThid),myByHi(myThid)
        DO bi=myBxLo(myThid),myBxHi(myThid)
          do j=1,sNy
            do i=1,sNx
             if (SURFOBS(i,j,bi,bj).ge.-999990. .and.
     &           streamice_hmask(i,j,bi,bj).eq.1.0) THEN

             h = H_streamice(i,j,bi,bj)
             hf = -1.0 * i_r * R_low_si (i,j,bi,bj)

              if (((h-hf) .gt. 5.) .OR.
     &             STREAMICE_shelf_dhdt_ctrl) then

               surf_err = streamice_wgt_surf * i_numcells *
     &         (surf_el_streamice(i,j,bi,bj) -
     &          surfobs(i,j,bi,bj))**2 /
     &          (.001 + errSurfObs(i,j,bi,bj)**2)

               cost_func1_streamice (bi,bj) =
     &         cost_func1_streamice (bi,bj) +
     &              surf_err
               cost_surf_streamice (bi,bj) =
     &         cost_surf_streamice (bi,bj) +
     &              surf_err

              endif

             endif

            end do
          end do
        end do
      end do

#endif

      RETURN
      END

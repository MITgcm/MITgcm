#include "STREAMICE_OPTIONS.h"
c#ifdef ALLOW_AUTODIFF
c# include "AUTODIFF_OPTIONS.h"
c#endif
#ifdef ALLOW_COST
# include "COST_OPTIONS.h"
#endif

      subroutine streamice_cost_vel_accum ( myIter, myThid )
C     *==========================================================*
C     | subroutine streamice_cost_vel_accum                      |
C     | o this routine calculates the velocity misfit contri-    |
C     |   bution to the per-timestep STREAMICE cost              |
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
      INTEGER  ILNBLNK
      EXTERNAL ILNBLNK
      CHARACTER*(MAX_LEN_FNAM) uvelfilename
      CHARACTER*(MAX_LEN_FNAM) vvelfilename
      CHARACTER*(MAX_LEN_FNAM) errvelfilename
      CHARACTER*(10) myIterStr
      _RL uvelobs (1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      _RL vvelobs (1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      _RL errvelobs (1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      _RL i_numcells, vel_err, uobs, vobs, verr

      WRITE(myIterStr,'(I10.10)') myIter

      uvelfilename=STREAMICEvelOptimTCBasename(1:
     & ILNBLNK(STREAMICEvelOptimTCBasename))//myIterStr//"u.bin"
      vvelfilename=STREAMICEvelOptimTCBasename(1:
     & ILNBLNK(STREAMICEvelOptimTCBasename))//myIterStr//"v.bin"
      errvelfilename=STREAMICEvelOptimTCBasename(1:
     & ILNBLNK(STREAMICEvelOptimTCBasename))//myIterStr//"err.bin"
      CALL READ_FLD_XY_RL( uvelfilename, ' ', uvelobs,
     &      0, myThid )
      CALL READ_FLD_XY_RL( vvelfilename, ' ', vvelobs,
     &      0, myThid )
      CALL READ_FLD_XY_RL( errvelfilename, ' ', errvelobs,
     &      0, myThid )

c#ifdef ALLOW_AUTODIFF_TAMC
cCADJ STORE surf_el_streamice  = comlev1, key = ikey_dynamics,
cCADJ &     kind = isbyte
cCADJ STORE H_streamice_prev  = comlev1, key = ikey_dynamics,
cCADJ &     kind = isbyte
cCADJ STORE H_streamice  = comlev1, key = ikey_dynamics,
cCADJ &     kind = isbyte
c#endif

      i_numcells = 1.0/(Nx*Ny)

C--   Calculate cost function on tile of this instance
      DO bj=myByLo(myThid),myByHi(myThid)
        DO bi=myBxLo(myThid),myBxHi(myThid)
          do j=1,sNy
            do i=1,sNx

             uobs = uvelobs(i,j,bi,bj)
             vobs = vvelobs(i,j,bi,bj)
             verr = errvelobs(i,j,bi,bj)

             if (uobs.gt.-999990. .and. vobs.gt.-999990. .and.
     &           verr.gt.-999990.) then

              vel_err = streamice_wgt_vel * i_numcells * (
     &         (streamice_u_surf(i,j,bi,bj)-uobs)**2
     &        +(streamice_v_surf(i,j,bi,bj)-vobs)**2)
     &        /(1. + verr**2)

              cost_func1_streamice (bi,bj) =
     &        cost_func1_streamice (bi,bj) +
     &              vel_err
              cost_vel_streamice (bi,bj) =
     &        cost_vel_streamice (bi,bj) +
     &              vel_err

             end if

            end do
          end do
        end do
      end do

#endif

      RETURN
      END

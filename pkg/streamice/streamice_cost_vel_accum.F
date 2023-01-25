#include "STREAMICE_OPTIONS.h"
#ifdef ALLOW_AUTODIFF
# include "AUTODIFF_OPTIONS.h"
#endif
#ifdef ALLOW_COST
# include "COST_OPTIONS.h"
#endif

      subroutine streamice_cost_vel_accum ( myThid, myIter )
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
      _RL uvelobs (1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      _RL vvelobs (1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      _RL errvelobs (1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      WRITE(uvelfilename,'(a,I10.10,a)') STREAMICEvelOptimTCBasename,
     &  myIter,'u.bin'
      WRITE(vvelfilename,'(a,I10.10,a)') STREAMICEvelOptimTCBasename,
     &  myIter,'v.bin'
      WRITE(errvelfilename,'(a,I10.10,a)') STREAMICEvelOptimTCBasename,
     &  myIter,'err.bin'
      CALL READ_FLD_XY_RL( uvelfilename, ' ', uvelobs,
     &      0, myThid )
      CALL READ_FLD_XY_RL( vvelfilename, ' ', vvelobs,
     &      0, myThid )
      CALL READ_FLD_XY_RL( errvelfilename, ' ', errvelobs,
     &      0, myThid )



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


            end do
          end do
        end do
      end do

#endif

      RETURN
      END

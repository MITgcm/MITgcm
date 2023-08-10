#include "COST_OPTIONS.h"
#ifdef ALLOW_STREAMICE
# include "STREAMICE_OPTIONS.h"
#endif

      subroutine cost_test( myThid )
C     *==========================================================*
C     | subroutine cost_test                                     |
C     | o this routine computes the cost function for the tiles  |
C     |   of this processor                                      |
C     *==========================================================*
C     | Notes                                                    |
C     *==========================================================*
      IMPLICIT NONE

C     == Global variables ===
#include "SIZE.h"
#include "EEPARAMS.h"
#include "PARAMS.h"
#include "DYNVARS.h"
#include "GRID.h"
#include "cost.h"
#ifdef ALLOW_STREAMICE
# include "STREAMICE.h"
#endif

C     == Routine arguments ==
C     myThid - Thread number for this instance of the routine.
      integer bi, bj
      integer myThid

#ifdef ALLOW_COST_TEST
C     == Local variables
      integer i, j
c      _RL i_numcells
      INTEGER  ILNBLNK
      EXTERNAL ILNBLNK
c      CHARACTER*(MAX_LEN_FNAM) STREAMICExvelOptimFile
c      CHARACTER*(MAX_LEN_FNAM) STREAMICEyvelOptimFile
c      _RL U_obs (1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
c      _RL V_obs (1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)

c      i_numcells = 1.0/(Nx*Ny)

c      STREAMICExvelOptimFile=
c     & STREAMICEvelOptimFile(1:
c     & ILNBLNK(STREAMICEvelOptimFile))//"u.bin"
c      STREAMICEyvelOptimFile=
c     & STREAMICEvelOptimFile(1:
c     & ILNBLNK(STREAMICEvelOptimFile))//"v.bin"

c      _BARRIER

c      CALL READ_FLD_XY_RS( STREAMICExvelOptimFile, ' ',
c     &      U_obs, 0, myThid )
c      CALL READ_FLD_XY_RS( STREAMICEyvelOptimFile, ' ',
c     &      V_obs, 0, myThid )

C--   Calculate mask for tracer cells  (0 => land, 1 => water)
c       k=1

C--   Calculate cost function on tile of this instance
      do bj = myByLo(myThid),myByHi(myThid)
        do bi = myBxLo(myThid),myBxHi(myThid)
          do j=1,sNy
            do i=1,sNx

            if (streamice_hmask(i,j,bi,bj).eq.1.0) then
             objf_test (bi,bj) = objf_test (bi,bj) +
     &       u_streamice(i,j,bi,bj)**2+v_streamice(i,j,bi,bj)**2+
     &       h_streamice(i,j,bi,bj)**2
            endif

            enddo
          enddo
        enddo
      enddo

#endif /* ALLOW_COST_TEST */

      RETURN
      END

#include "STREAMICE_OPTIONS.h"

      subroutine cost_test( myThid )
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

#ifdef ALLOW_COST_TEST
C     == Local variables
      _RL thetaRef, HAF

      integer i, j, k
      integer ig, jg
      integer itlo,ithi
      integer jtlo,jthi
      _RL i_numcells, dCdx, dCdy, dBdx, dBdy, dTdx, dTdy
      _RL utmp, vtmp, uotmp, votmp, cotmp
      INTEGER  ILNBLNK
      EXTERNAL ILNBLNK
      CHARACTER*(MAX_LEN_FNAM) STREAMICExvelOptimFile
      CHARACTER*(MAX_LEN_FNAM) STREAMICEyvelOptimFile
      _RL U_obs (1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      _RL V_obs (1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)

      jtlo = mybylo(mythid)
      jthi = mybyhi(mythid)
      itlo = mybxlo(mythid)
      ithi = mybxhi(mythid)

      i_numcells = 1.0/(Nx*Ny)

!      STREAMICExvelOptimFile=
!     & STREAMICEvelOptimFile(1:
!     & ILNBLNK(STREAMICEvelOptimFile))//"u.bin"
!      STREAMICEyvelOptimFile=
!     & STREAMICEvelOptimFile(1:
!     & ILNBLNK(STREAMICEvelOptimFile))//"v.bin"

!      _BARRIER

!      CALL READ_FLD_XY_RS( STREAMICExvelOptimFile, ' ', 
!     &      U_obs, 0, myThid )
!      CALL READ_FLD_XY_RS( STREAMICEyvelOptimFile, ' ', 
!     &      V_obs, 0, myThid )

C--   Calculate mask for tracer cells  (0 => land, 1 => water)
!       k=1

C--   Calculate cost function on tile of this instance
      do bj = jtlo,jthi
        do bi = itlo,ithi
          do j=1,sNy
            do i=1,sNx


            if (streamice_hmask(i,j,bi,bj).eq.1.0) then
             objf_test (bi,bj) = objf_test (bi,bj) +
     &       u_streamice(i,j,bi,bj)**2+v_streamice(i,j,bi,bj)**2+
     &       h_streamice(i,j,bi,bj)**2
            endif


            end do
          end do
        end do
      end do

#endif

      RETURN
      END

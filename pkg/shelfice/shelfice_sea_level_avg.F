C $Header: /u/gcmpack/MITgcm_contrib/verification_other/shelfice_remeshing/code/shelfice_sea_level_avg.F,v 1.2 2016/05/05 18:16:04 dgoldberg Exp $
C $Name:  $

#include "SHELFICE_OPTIONS.h"

CBOP
C    KS16, add sum_eta to subroutine fields
      SUBROUTINE SHELFICE_SEA_LEVEL_AVG( avgSL, sum_eta, myThid )
C     *============================================================*
C     | SUBROUTINE SHELFICE_INIT_VARIA
C     | o Routine to initialize SHELFICE variables.
C     *============================================================*
C     | Initialize SHELFICE parameters and variables.
C     *============================================================*
      IMPLICIT NONE

C     === Global variables ===
#include "SIZE.h"
#include "EEPARAMS.h"
#include "PARAMS.h"
#include "GRID.h"
#include "SHELFICE.h"
#include "DYNVARS.h"
#ifdef ALLOW_COST
# include "SHELFICE_COST.h"
#endif /* ALLOW_COST */

C     === Routine arguments ===
C     myThid -  Number of this instance 
C     
C     This subroutine averages eta over all cells where
C     here is open ocean, assessed by
C     - maskInC = 1
C     - kTopC = 0
C     - shelficemass = 0

      _RL avgSL
C   KS16, put sum_eta here
      _RL sum_eta
      INTEGER myThid

CEndOfInterface

#ifdef ALLOW_SHELFICE
C     === Local variables ===
C     i,j,bi,bj - Loop counters
      INTEGER i, j, bi, bj
      _RL locsum_eta (nSx,nSy)
      _RL locsum_area (nSx,nSy)
      _RL eta_loc, area_loc
C    KS16 comment sum_eta here
C      _RL sum_eta
      _RL sum_area
CEOP

C     KS16, initilized sum_eta to 0
      sum_eta = 0. _d 0
      avgSL = 0. _d 0

      DO bj = myByLo(myThid), myByHi(myThid)
       DO bi = myBxLo(myThid), myBxHi(myThid)
        locsum_eta (bi,bj) = 0. _d 0
        locsum_area (bi,bj) = 0. _d 0
       ENDDO
      ENDDO

      DO bj = myByLo(myThid), myByHi(myThid)
       DO bi = myBxLo(myThid), myBxHi(myThid)
        DO j = 1, sNy
         DO i = 1, sNx

          IF ((maskInC(i,j,bi,bj) .eq. 1) .and.
     &        (kTopC(i,j,bi,bj) .eq. 0) .and.
     &        (shelficeMass(i,j,bi,bj) .lt. .01)) then

           

           eta_loc = etaN (i,j,bi,bj)
           area_loc = rA (i,j,bi,bj)  
!            print *, "GOT HERE ", i,j, eta_loc
           locsum_eta(bi,bj) = locsum_eta(bi,bj) + 
     &      eta_loc * area_loc
           locsum_area(bi,bj) = locsum_area(bi,bj) + area_loc

          ENDIF
         ENDDO
        ENDDO
       ENDDO
      ENDDO

      CALL GLOBAL_SUM_TILE_RL( locsum_eta, sum_eta, myThid )
      CALL GLOBAL_SUM_TILE_RL( locsum_area, sum_area, myThid )


      IF (sum_area .le. 0.0) THEN
       avgSL = 0. _d 0
      ELSE
       avgSL = sum_eta / sum_area
      ENDIF


#endif /* ALLOW_SHELFICE */

      RETURN
      END

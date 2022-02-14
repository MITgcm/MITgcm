#include "SEAICE_OPTIONS.h"

CBOP
C !ROUTINE: SEAICE_ITD_SUM

C !INTERFACE: ==========================================================
      SUBROUTINE SEAICE_ITD_SUM(
     I                  bi, bj, myTime, myIter, myThid )

C !DESCRIPTION: \bv
C     *===========================================================*
C     | SUBROUTINE SEAICE_ITD_SUM
C     | o sum ice area and volume over all ITD categories
C     |   and write into AREA and HEFF
C     |
C     | Torge Martin, Mar. 2012, torge@mit.edu
C     *===========================================================*
C \ev

C !USES: ===============================================================
      IMPLICIT NONE

C     === Global variables ===
C     nITD      :: number of seaice thickness categories
C     AREA      :: sea ice area   total
C     AREAITD   :: sea ice area   by category
C     HEFF      :: sea ice volume total
C     HEFFITD   :: sea ice volume by category
C     HSNOW     :: snow    volume total
C     HSNOWITD  :: snow    volume by category
#include "SIZE.h"
#include "EEPARAMS.h"
c#include "PARAMS.h"
#include "SEAICE_SIZE.h"
c#include "SEAICE_PARAMS.h"
#include "SEAICE.h"

C !INPUT PARAMETERS: ===================================================
C     bi,bj     :: Current tile indices
C     myTime    :: Current time in simulation
C     myIter    :: Current iteration number
C     myThid    :: my Thread Id number
      _RL myTime
      INTEGER bi, bj
      INTEGER myIter
      INTEGER myThid
CEOP

#ifdef SEAICE_ITD
C !LOCAL VARIABLES: ====================================================
C     i,j,k     :: inner loop counters
      INTEGER i, j, k

C---+-|--1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|

c      DO bj=myByLo(myThid),myByHi(myThid)
c       DO bi=myBxLo(myThid),myBxHi(myThid)
C must now be called within bi,bj loop

        DO j=1-OLy,sNy+OLy
         DO i=1-OLx,sNx+OLx
           AREA (i,j,bi,bj) = AREAITD (i,j,1,bi,bj)
           HEFF (i,j,bi,bj) = HEFFITD (i,j,1,bi,bj)
           HSNOW(i,j,bi,bj) = HSNOWITD(i,j,1,bi,bj)
         ENDDO
        ENDDO

        DO k=2,nITD
         DO j=1-OLy,sNy+OLy
          DO i=1-OLx,sNx+OLx
           AREA (i,j,bi,bj) = AREA (i,j,bi,bj) + AREAITD (i,j,k,bi,bj)
           HEFF (i,j,bi,bj) = HEFF (i,j,bi,bj) + HEFFITD (i,j,k,bi,bj)
           HSNOW(i,j,bi,bj) = HSNOW(i,j,bi,bj) + HSNOWITD(i,j,k,bi,bj)
          ENDDO
         ENDDO
        ENDDO

c       ENDDO
c      ENDDO

C---+-|--1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|
#endif /* SEAICE_ITD */
      RETURN
      END

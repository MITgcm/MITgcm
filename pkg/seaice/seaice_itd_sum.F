C $Header: /u/gcmpack/MITgcm/pkg/seaice/seaice_itd_sum.F,v 1.3 2014/10/20 03:20:57 gforget Exp $
C $Name:  $

#include "SEAICE_OPTIONS.h"
#ifdef ALLOW_AUTODIFF
# include "AUTODIFF_OPTIONS.h"
#endif

C !ROUTINE: SEAICE_ITD_SUM

C !INTERFACE: ==========================================================
      SUBROUTINE SEAICE_ITD_SUM(
     I     bi, bj, myTime, myIter, myThid )

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
C     AREA      :: sea ice area   total
C     AREAITD   :: sea ice area   by category
C     HEFF      :: sea ice volume total
C     HEFFITD   :: sea ice volume by category
C     HSNOW     :: snow    volume total
C     HSNOWITD  :: snow    volume by category
C
#include "SIZE.h"
#include "EEPARAMS.h"
#include "PARAMS.h"
#include "GRID.h"
#include "SEAICE_SIZE.h"
#include "SEAICE_PARAMS.h"
#include "SEAICE.h"

#ifdef ALLOW_AUTODIFF_TAMC
# include "tamc.h"
#endif

C !INPUT PARAMETERS: ===================================================
C     === Routine arguments ===
C     bi,bj     :: outer loop counters
C     myTime    :: current time
C     myIter    :: iteration number
C     myThid    :: Thread no. that called this routine.
      _RL myTime
      INTEGER bi, bj
      INTEGER myIter
      INTEGER myThid
CEndOfInterface

#ifdef SEAICE_ITD

C !LOCAL VARIABLES: ====================================================
C     === Local variables ===
C     i,j,k :: inner loop counters
C     nITD        :: number of sea ice thickness categories
C
      INTEGER i, j, k
#ifdef ALLOW_AUTODIFF_TAMC
      INTEGER itmpkey
#endif /* ALLOW_AUTODIFF_TAMC */
#ifdef SEAICE_AGE
      INTEGER iTracer
#endif

C---+-|--1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|

c      DO bj=myByLo(myThid),myByHi(myThid)
c       DO bi=myBxLo(myThid),myBxHi(myThid)
C must now be called within bi,bj loop

        DO j=1-OLy,sNy+OLy
         DO i=1-OLx,sNx+OLx
          AREA (i,j,bi,bj)=AREAITD (i,j,1,bi,bj)
          HEFF (i,j,bi,bj)=HEFFITD (i,j,1,bi,bj)
          HSNOW(i,j,bi,bj)=HSNOWITD(i,j,1,bi,bj)
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

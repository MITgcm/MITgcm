C $Header: /u/gcmpack/MITgcm/pkg/bulk_force/bulkf_flux_adjust.F,v 1.1 2006/03/14 15:57:31 jmc Exp $
C $Name:  $

#include "BULK_FORCE_OPTIONS.h"

CBOP
C     !ROUTINE: BULKF_FLUX_ADJUST
C     !INTERFACE:
      SUBROUTINE BULKF_FLUX_ADJUST(
     I                              bi, bj, iMin, iMax, jMin, jMax,
     I                              adjustMask,
     I                              myTime, myIter, myThid )
C     !DESCRIPTION: \bv
C     *==========================================================*
C     | S/R BULKF_FLUX_ADJUST
c     | o Modify fluxes (Qnet,EmPmR) where adjustMask=True
C     *==========================================================*
C     | o presently implemented:
C     | replace computed Qnet,EmPmR by prescribed Qnetch,EmPch
C     *==========================================================*
C     \ev

C     !USES:
      IMPLICIT NONE

C     == Global data ==
#include "SIZE.h"
#include "EEPARAMS.h"
#include "FFIELDS.h"
#include "BULKF_PARAMS.h"
#include "BULKF.h"

C     !INPUT/OUTPUT PARAMETERS:
C     === Routine arguments ===
C     bi, bj      :: tile indices
C     iMin,iMax   :: computation domain: 1rst index range
C     jMin,jMax   :: computation domain: 2nd  index range
C     adjustMask  :: True where flux adjustment applies
C     myTime      :: Current time in simulation
C     myIter      :: Current iteration number in simulation
C     myThid      :: My Thread number Id

      INTEGER bi,bj
      INTEGER iMin, iMax
      INTEGER jMin, jMax
      LOGICAL adjustMask(1-OLx:sNx+OLx,1-OLy:sNy+OLy)
      _RL     myTime
      INTEGER myIter
      INTEGER myThid
CEOP

C     == Local variables ==
      INTEGER i,j

      IF ( useQnetch ) THEN
        DO j = jMin, jMax
         DO i = iMin, iMax
           IF ( adjustMask(i,j) ) Qnet(i,j,bi,bj) = Qnetch(i,j,bi,bj)
         ENDDO
        ENDDO
      ENDIF

      IF ( useEmPch ) THEN
        DO j = jMin, jMax
         DO i = iMin, iMax
           IF ( adjustMask(i,j) ) EmPmR(i,j,bi,bj) = EmPch(i,j,bi,bj)
         ENDDO
        ENDDO
      ENDIF

      RETURN
      END

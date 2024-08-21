#include "BATS_OPTIONS.h"

C---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|
CBOP
C     !ROUTINE: BATS_INIT_FIXED

C     !INTERFACE:
      SUBROUTINE BATS_INIT_FIXED( myThid )

C     !DESCRIPTION:
C     Initialize fixed quantities

C     !USES:
      IMPLICIT NONE
#include "SIZE.h"
#include "EEPARAMS.h"
#include "PARAMS.h"
#include "GRID.h"
#include "BATS_VARS.h"

C     !INPUT PARAMETERS:
C     myThid       :: my Thread Id number
      INTEGER myThid
CEOP

      INTEGER k, nparams
      INTEGER iUnit
      CHARACTER*(MAX_LEN_MBUF) msgBuf
C      double precision :: bioparams(161)

C---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|

      _BEGIN_MASTER(myThid)

C--   Set other constant/flag

C       CALL MDSFINDUNIT( iUnit, myThid )
C       OPEN(UNIT=iUnit,FILE='bioparams.dat',STATUS='old')
C       DO k=1,nparams
C           READ(iUnit,*) bioparams(k)
C           WRITE(standardMessageUnit,*) 'bioparam',bioparams(k)
C       ENDDO
C       CLOSE(iUnit)
      print('init_fixed not used')

      _END_MASTER(myThid)

      RETURN
      END

C $Header: /u/gcmpack/MITgcm/pkg/regrid/regrid_check.F,v 1.1 2006/08/15 04:05:48 edhill Exp $
C $Name:  $

#include "REGRID_OPTIONS.h"

C---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|
CBOP 0
C     !ROUTINE: REGRID_CHECK

C     !INTERFACE:
      SUBROUTINE REGRID_CHECK( myThid )

C     !DESCRIPTION:
C     Check dependances with other packages

C     !USES:
      IMPLICIT NONE
#include "SIZE.h"
#include "EEPARAMS.h"
#include "PARAMS.h"
#include "REGRID_SIZE.h"
#include "REGRID.h"

C     !INPUT PARAMETERS:
C     myThid -  Number of this instances
      INTEGER myThid
CEOP

C     !LOCAL VARIABLES:
C     msgBuf      - Informational/error meesage buffer
      CHARACTER*(MAX_LEN_MBUF) msgBuf
      

#ifdef ALLOW_REGRID
      
      WRITE(msgBuf,'(A)') 'REGRID_CHECK: #define REGRID'
      CALL PRINT_MESSAGE( msgBuf, standardMessageUnit,
     &     SQUEEZE_RIGHT , 1)

      _BEGIN_MASTER(myThid)
      
      IF (.NOT. useMNC) THEN
        WRITE(msgBuf,'(A)') 'Regrid needs useMNC=.true.'
        CALL PRINT_ERROR( msgBuf , 1)
        STOP 'ABNORMAL END: S/R REGRID_CHECK'
      ENDIF
      
      _END_MASTER(myThid)
      
#endif /* ALLOW_REGRID */
      
      RETURN
      END

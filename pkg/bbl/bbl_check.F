C $Header: /u/gcmpack/MITgcm/pkg/bbl/bbl_check.F,v 1.3 2013/08/27 21:34:21 dimitri Exp $
C $Name:  $

#include "BBL_OPTIONS.h"

C---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|
CBOP 0
C !ROUTINE: BBL_CHECK

C !INTERFACE:
      SUBROUTINE BBL_CHECK( myThid )

C     !DESCRIPTION:
C     Check dependances with other packages

C     !USES:
      IMPLICIT NONE
#include "SIZE.h"
#include "EEPARAMS.h"
#include "PARAMS.h"
#include "BBL.h"

C     !INPUT PARAMETERS:
C     myThid   :: my Thread Id number
      INTEGER myThid
CEOP

#ifdef ALLOW_BBL
C     !LOCAL VARIABLES:
C     msgBuf   :: Informational/error message buffer
      CHARACTER*(MAX_LEN_MBUF) msgBuf

      _BEGIN_MASTER(myThid)

       WRITE(msgBuf,'(A)') 'BBL_CHECK: #define BBL'
       CALL PRINT_MESSAGE( msgBuf, standardMessageUnit,
     &                     SQUEEZE_RIGHT , 1)

C--  Print out some key parameters :
       CALL WRITE_0D_RL( bbl_wvel, INDEX_NONE,
     &  'bbl_wvel =', ' /* vertical entrainment (m/s) */')
       CALL WRITE_0D_RL( bbl_hvel, INDEX_NONE,
     &  'bbl_hvel =', ' /* horizontal velocity (m/s) */')
       CALL WRITE_0D_RL( bbl_initEta, INDEX_NONE,
     &  'bbl_initEta =', ' /* default thickness (m) */')

C--  Check parameters:

      _END_MASTER(myThid)

#endif /* ALLOW_BBL */

      RETURN
      END

C $Header: /u/gcmpack/MITgcm/pkg/bbl/bbl_check.F,v 1.1 2011/08/06 03:13:22 dimitri Exp $
C $Name:  $

#include "BBL_OPTIONS.h"

C---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|
CBOP 0
C !ROUTINE: BBL_CHECK

C !INTERFACE:
      SUBROUTINE MYPACKAGE_CHECK( myThid )

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
       CALL WRITE_0D_L( bbl_RelaxH, INDEX_NONE,
     &  'bbl_RelaxH =', ' /* horizontal transfer coefficient */')
       CALL WRITE_0D_L( bbl_RelaxR, INDEX_NONE,
     &  'bbl_RelaxR =', ' /* vertical transfer coefficient */')
       CALL WRITE_0D_L( bbl_InitEta, INDEX_NONE,
     &  'bbl_InitEta =', ' /* default bbl thickness */')

C--  Check parameters:

      _END_MASTER(myThid)

#endif /* ALLOW_BBL */

      RETURN
      END

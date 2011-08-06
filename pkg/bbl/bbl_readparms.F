C $Header: /u/gcmpack/MITgcm/pkg/bbl/bbl_readparms.F,v 1.1 2011/08/06 03:13:22 dimitri Exp $
C $Name:  $

#include "BBL_OPTIONS.h"

C---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|
CBOP 0
C !ROUTINE: BBL_READPARMS

C !INTERFACE:
      SUBROUTINE BBL_READPARMS( myThid )

C     !DESCRIPTION:
C     Initialize BBL variables and constants.

C     !USES:
      IMPLICIT NONE
#include "SIZE.h"
#include "EEPARAMS.h"
#include "PARAMS.h"
#include "BBL.h"

C     !INPUT PARAMETERS:
      INTEGER myThid
CEOP

#ifdef ALLOW_BBL

C     !LOCAL VARIABLES:
C     msgBuf     :: Informational/error message buffer
C     iUnit      :: Work variable for IO unit number
      CHARACTER*(MAX_LEN_MBUF) msgBuf
      INTEGER iUnit

      NAMELIST /BBL_PARM01/
     &     bbl_RelaxH, bbl_RelaxR, bbl_InitEta,
     &     bbl_thetaFile, bbl_saltFile, bbl_etaFile

      _BEGIN_MASTER(myThid)

C--   Default values for BBL
      bbl_RelaxH    =   86400. _d 0
      bbl_RelaxR    = 2592000. _d 0
      bbl_InitEta   =      20. _d 0

C-    file names for initial conditions:
      bbl_thetaFile = ' '
      bbl_saltFile  = ' '
      bbl_etaFile   = ' '

      WRITE(msgBuf,'(A)') 'BBL_READPARMS: opening data.bbl'
      CALL PRINT_MESSAGE( msgBuf, standardMessageUnit,
     &                    SQUEEZE_RIGHT , 1)
      CALL OPEN_COPY_DATA_FILE(
     I                     'data.bbl', 'BBL_READPARMS',
     O                     iUnit,
     I                     myThid )

C     Read parameters from open data file
      READ(UNIT=iUnit,NML=BBL_PARM01)
      WRITE(msgBuf,'(A)')
     &    'BBL_READPARMS: finished reading data.bbl'
      CALL PRINT_MESSAGE( msgBuf, standardMessageUnit,
     &    SQUEEZE_RIGHT , 1)
C     Close the open data file
      CLOSE(iUnit)

      _END_MASTER(myThid)

C--   Everyone else must wait for the parameters to be loaded
      _BARRIER

#endif /* ALLOW_BBL */

      RETURN
      END

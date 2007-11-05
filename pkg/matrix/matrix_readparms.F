C $Header: /u/gcmpack/MITgcm/pkg/matrix/matrix_readparms.F,v 1.3 2007/11/05 18:58:00 jmc Exp $
C $Name:  $

#include "PACKAGES_CONFIG.h"
#include "CPP_OPTIONS.h"

C     !ROUTINE: MATRIX_READPARMS

      SUBROUTINE MATRIX_READPARMS( myThid )

C     !DESCRIPTION:
C     Initialize MATRIX parameters, read in data.matrix

      IMPLICIT NONE
#include "SIZE.h"
#include "EEPARAMS.h"
#include "PTRACERS_SIZE.h"
#include "MATRIX.h"

C     !INPUT PARAMETERS:
C     myThid            :: thread number
      INTEGER myThid

C     !OUTPUT PARAMETERS:
C     none
C     !LOCAL VARIABLES:
C     iUnit  :: unit number for I/O
C     msgBuf :: message buffer
      INTEGER iUnit
      CHARACTER*(MAX_LEN_MBUF) msgBuf

#ifdef ALLOW_MATRIX
C     Open and read the data.matrix file

      NAMELIST /MATRIX_PARM01/ expMatrixWriteTime, impMatrixWriteTime

      expMatrixWriteTime = 10. _d 10
      impMatrixWriteTime = 10. _d 10

      _BEGIN_MASTER(myThid)
      WRITE(msgBuf,'(A)') ' MATRIX_READPARMS: opening data.matrix'
      CALL PRINT_MESSAGE(msgBuf, standardMessageUnit,
     &                   SQUEEZE_RIGHT , 1)
      CALL OPEN_COPY_DATA_FILE(
     I                   'data.matrix', 'MATRIX_READPARMS',
     O                   iUnit,
     I                   myThid )
      READ(UNIT=iUnit,NML=MATRIX_PARM01)
      WRITE(msgBuf,'(A)')
     &  ' MATRIX_READPARMS: finished reading data.matrix'
      CALL PRINT_MESSAGE(msgBuf, standardMessageUnit,
     &                   SQUEEZE_RIGHT , 1)

C Close the open data file
      CLOSE(iUnit)
      _END_MASTER(myThid)

C Everyone else must wait for the parameters to be loaded
      _BARRIER

#endif /* ALLOW_MATRIX */
      RETURN
      END

#include "STIC_OPTIONS.h"

C---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|
CBOP
C !ROUTINE: STIC_CHECK

C !INTERFACE:
      SUBROUTINE STIC_CHECK( myThid )

C     !DESCRIPTION:
C     Check dependances with other packages

C     !USES:
      IMPLICIT NONE
C     == Global variables ===
#include "SIZE.h"
#include "EEPARAMS.h"
#include "PARAMS.h"
#include "STIC.h"
#ifdef ALLOW_SHELFICE
# include "SHELFICE.h"
#endif

C     !INPUT PARAMETERS:
C     myThid   :: my Thread Id number
      INTEGER myThid
CEOP

C     !FUNCTIONS:
      INTEGER  ILNBLNK
      EXTERNAL ILNBLNK

C     !LOCAL VARIABLES:
C     msgBuf   :: Informational/error message buffer
      CHARACTER*(MAX_LEN_MBUF) msgBuf
      INTEGER errCount, iL, jL

      _BEGIN_MASTER(myThid)
      errCount = 0

       WRITE(msgBuf,'(A)') 'STIC_CHECK: #define STEEP_ICECAVITY'
       CALL PRINT_MESSAGE( msgBuf, standardMessageUnit,
     &                     SQUEEZE_RIGHT, myThid )

C--  Print out some kee parameters :
       CALL WRITE_0D_C( STIClengthFile,-1,INDEX_NONE,'STIClengthFile =',
     &  ' /* file name of ice front lengths */')
       CALL WRITE_0D_C( STICdepthFile,-1,INDEX_NONE,'STICdepthFile =',
     &  ' /* file name of ice front depths */')

C--  Check parameters:

       IF ( .NOT. useSHELFICE ) THEN
#ifndef ALLOW_SHELFICE
        WRITE(msgBuf,'(A)')
     &      'pkg/steep_icecavity requires pkg/shelfice,'
        CALL PRINT_ERROR( msgBuf, myThid )
        WRITE(msgBuf,'(A)')
     &      'please add shelfice to packages.conf'
        CALL PRINT_ERROR( msgBuf, myThid )
        errCount = errCount + 1
#endif
        WRITE(msgBuf,'(A)')
     &       'pkg/steep_icecavity requires useShelfIce = T, '//
     &       'set useShelfIce = .TRUE., in data.pkg'
        CALL PRINT_ERROR( msgBuf, myThid )
        errCount = errCount + 1
       ELSE
#ifdef ALLOW_SHELFICE
        IF ( STICdepthFile .NE. SHELFICEtopoFile ) THEN
         iL = ILNBLNK(STICdepthFile)
         jL = ILNBLNK(SHELFICEtopoFile)
         WRITE(msgBuf,'(6A)') '** WARNING ** STIC_CHECK: ',
     &        'STICdepthFile = ''', STICdepthFile(1:iL),
     &        ''' .NE. SHELFICEtopoFile = ''',
     &        SHELFICEtopoFile(1:jL), ''''
         CALL PRINT_MESSAGE( msgBuf, errorMessageUnit,
     &                       SQUEEZE_RIGHT, myThid )
        ENDIF
        IF ( SHELFICEboundaryLayer ) THEN
         WRITE(msgBuf,'(A,A)')
     &     'pkg/steep_icecavity requires SHELFICEboundaryLayer = F, ',
     &     'remove SHELFICEboundaryLayer = .TRUE., in data.shelfice'
         CALL PRINT_ERROR( msgBuf, myThid )
         errCount = errCount + 1
        ENDIF
#endif
       ENDIF

#ifndef ALLOW_ADDFLUID
       WRITE(msgBuf,'(A,A)')
     &      'pkg/steep_icecavity requires'
     &      'CPP flag ALLOW_ADDFLUID to be defined'
       CALL PRINT_ERROR( msgBuf, myThid )
       errCount = errCount + 1
#endif

      IF ( errCount.GE.1 ) THEN
        WRITE(msgBuf,'(A,I3,A)')
     &       'STIC_CHECK: detected', errCount,' fatal error(s)'
        CALL PRINT_ERROR( msgBuf, myThid )
        CALL ALL_PROC_DIE( 0 )
        STOP 'ABNORMAL END: S/R STIC_CHECK'
      ENDIF

      _END_MASTER(myThid)

      RETURN
      END

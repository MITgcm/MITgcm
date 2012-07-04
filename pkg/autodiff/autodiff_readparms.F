C $Header: /u/gcmpack/MITgcm/pkg/autodiff/autodiff_readparms.F,v 1.1 2012/07/04 20:16:43 jmc Exp $
C $Name:  $

#include "AUTODIFF_OPTIONS.h"

C---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|
CBOP 0
C !ROUTINE: AUTODIFF_READPARMS

C !INTERFACE:
      SUBROUTINE AUTODIFF_READPARMS( myThid )

C     !DESCRIPTION:
C     Initialize AUTODIFF variables and constants.

C     !USES:
      IMPLICIT NONE
#include "SIZE.h"
#include "EEPARAMS.h"
#include "PARAMS.h"
#include "AUTODIFF_PARAMS.h"

C     !INPUT PARAMETERS:
      INTEGER myThid
CEOP

#ifdef ALLOW_AUTODIFF

C     !LOCAL VARIABLES:
C     msgBuf     :: Informational/error message buffer
C     iUnit      :: Work variable for IO unit number
      CHARACTER*(MAX_LEN_MBUF) msgBuf
      INTEGER iUnit

      NAMELIST /AUTODIFF_PARM01/
     &       useKPPinAdMode,
     &       useGMRediInAdMode,
     &       useSEAICEinAdMode,
     &       inAdExact

      _BEGIN_MASTER(myThid)

C--   Default values for AUTODIFF
      useKPPinAdMode     = .TRUE.
      useGMRediInAdMode  = .TRUE.
      useSEAICEinAdMode  = .TRUE.
      inAdExact          = .TRUE.

      WRITE(msgBuf,'(A)') 'AUTODIFF_READPARMS: opening data.autodiff'
      CALL PRINT_MESSAGE( msgBuf, standardMessageUnit,
     &                    SQUEEZE_RIGHT, myThid )
      CALL OPEN_COPY_DATA_FILE(
     I                     'data.autodiff', 'AUTODIFF_READPARMS',
     O                     iUnit,
     I                     myThid )

C     Read parameters from open data file
      READ(UNIT=iUnit,NML=AUTODIFF_PARM01)
      WRITE(msgBuf,'(A)')
     &    'AUTODIFF_READPARMS: finished reading data.autodiff'
      CALL PRINT_MESSAGE( msgBuf, standardMessageUnit,
     &                    SQUEEZE_RIGHT, myThid )
C     Close the open data file
      CLOSE(iUnit)

C--   Flag for approximate adjoint
      IF ( inAdExact ) THEN
       inAdTrue  = .FALSE.
       inAdFalse = .FALSE.
      ELSE
       inAdTrue  = .TRUE.
       inAdFalse = .FALSE.
      ENDIF
      inAdMode   = .FALSE.

C--   packages which can be switched off in adjoint mode (approximate adjoint):
C     store value of usePKG as it will be reset in S/R ADAUTODIFF_INADMODE_UNSET
      useKPPinFwdMode    = useKPP
      useGMRediInFwdMode = useGMRedi
      useSEAICEinFwdMode = useSEAICE

C     store value of usePKG as it will be set in S/R ADAUTODIFF_INADMODE_SET
      useKPPinAdMode    = useKPPinAdMode    .AND. useKPP
      useGMRediInAdMode = useGMRediInAdMode .AND. useGMRedi
      useSEAICEinAdMode = useSEAICEinAdMode .AND. useSEAICE

      _END_MASTER(myThid)

C--   Everyone else must wait for the parameters to be loaded
      _BARRIER

#endif /* ALLOW_AUTODIFF */

      RETURN
      END

#include "CPP_OPTIONS.h"

CBOP 0
C !ROUTINE: CPL_READ_PARAMS

C !INTERFACE:
      SUBROUTINE CPL_READ_PARAMS( msgUnit )

C !DESCRIPTION:
C     *==========================================================*
C     | SUBROUTINE CPL_READ_PARAMS
C     | o Read coupler and mapping parameters
C     *==========================================================*

C !USES:
      IMPLICIT NONE
C     == Global variables ==
#include "CPL_PARAMS.h"

C !INPUT/OUTPUT PARAMETERS:
C     msgUnit          :: log-file I/O unit
      INTEGER msgUnit

C !LOCAL VARIABLES:
      INTEGER ioUnit
      LOGICAL fileExist
CEOP

      NAMELIST /COUPLER_PARAMS/
     &     cpl_sequential,
     &     cpl_exchange_RunOff,
     &     cpl_exchange1W_sIce, cpl_exchange2W_sIce,
     &     cpl_exchange_SaltPl, cpl_exchange_DIC,
     &     runOffMapSize, runOffMapFile

C--   Default values for Coupler parameters
      cpl_sequential = 0
C     default coupling-exch selector value: fit a simple warm
C     aqua-planet (no land, no seaice, no CO2/DIC coupling)
      cpl_exchange_RunOff = 0
      cpl_exchange1W_sIce = 0
      cpl_exchange2W_sIce = 0
      cpl_exchange_SaltPl = 0
      cpl_exchange_DIC    = 0
      runOffMapSize = 0
      runOffMapFile = ' '
C-    other parameters:
      cplErrorCount = 0
      fileExist = .FALSE.

C--   Read-in parameter file:
      INQUIRE( FILE='data.cpl', EXIST=fileExist )
      IF ( fileExist ) THEN
        WRITE(msgUnit,'(2A)') 'CPL_READ_PARAMS: ',
     &                        'Reading parameter file "data.cpl"'
        ioUnit = 88
        OPEN( ioUnit, FILE='data.cpl',STATUS='old')
        READ( ioUnit, COUPLER_PARAMS )
        CLOSE(ioUnit )
      ELSE
        cplErrorCount = 1
        WRITE(msgUnit,'(2A)') ' *** ERROR *** CPL_READ_PARAMS: ',
     &                'parameter file "data.cpl" not found'
        WRITE(msgUnit,'(2A,I4,A)') ' *** ERROR *** CPL_READ_PARAMS: ',
     &                '==> set ErrorCount=', cplErrorCount, ' (Fatal)'
      ENDIF

C--   Check parameter consistency:
      IF ( cpl_exchange2W_sIce.EQ.3 .AND. cpl_sequential.EQ.0 ) THEN
        cplErrorCount = cplErrorCount + 1
        WRITE(msgUnit,'(2A)') ' *** ERROR *** CPL_READ_PARAMS: ',
     &       'needs "cpl_sequential=1" to use: cpl_exchange2W_sIce=3'
        WRITE(msgUnit,'(2A,I4,A)') ' *** ERROR *** CPL_READ_PARAMS: ',
     &                '==> set ErrorCount=', cplErrorCount, ' (Fatal)'
      ENDIF
      IF ( cpl_exchange2W_sIce.EQ.1 .AND. cpl_sequential.EQ.1 ) THEN
        cplErrorCount = cplErrorCount + 1
        WRITE(msgUnit,'(2A)') ' *** ERROR *** CPL_READ_PARAMS: ',
     &       'needs "cpl_sequential=0" to use: cpl_exchange2W_sIce=1'
        WRITE(msgUnit,'(2A,I4,A)') ' *** ERROR *** CPL_READ_PARAMS: ',
     &                '==> set ErrorCount=', cplErrorCount, ' (Fatal)'
      ENDIF

C--   Print summary of Coupler parameters value
      IF ( cplErrorCount.EQ.0 ) THEN
        WRITE(msgUnit,'(2A)') 'CPL_READ_PARAMS: ',
     &   '------  Coupler parameter Summary:  -----------------'

C-    main coupling time-stepping selector:
        WRITE(msgUnit,'(2A,I4)') 'CPL_READ_PARAMS: ',
     &           ' cpl_sequential = ', cpl_sequential

C-    cpl_exchange field selector:
        WRITE(msgUnit,'(2A,I4)') 'CPL_READ_PARAMS: ',
     &           ' cpl_exchange_RunOff =', cpl_exchange_RunOff
        WRITE(msgUnit,'(2A,I4)') 'CPL_READ_PARAMS: ',
     &           ' cpl_exchange1W_sIce =', cpl_exchange1W_sIce
        WRITE(msgUnit,'(2A,I4)') 'CPL_READ_PARAMS: ',
     &           ' cpl_exchange2W_sIce =', cpl_exchange2W_sIce
        WRITE(msgUnit,'(2A,I4)') 'CPL_READ_PARAMS: ',
     &           ' cpl_exchange_SaltPl =', cpl_exchange_SaltPl
        WRITE(msgUnit,'(2A,I4)') 'CPL_READ_PARAMS: ',
     &           ' cpl_exchange_DIC    =', cpl_exchange_DIC

C-    run-off mapping parameters:
        WRITE(msgUnit,'(2A,I8)') 'CPL_READ_PARAMS: ',
     &                ' runOffMapSize=', runOffMapSize
        WRITE(msgUnit,'(3A)') 'CPL_READ_PARAMS: ',
     &                ' runOffMapFile=', runOffMapFile

        WRITE(msgUnit,'(2A)') 'CPL_READ_PARAMS: ',
     &   '------  End of Coupler parameter Summary ------------'
      ENDIF

      RETURN
      END

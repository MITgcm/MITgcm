C $Header: /u/gcmpack/MITgcm/pkg/ggl90/ggl90_readparms.F,v 1.4 2006/06/06 19:47:36 mlosch Exp $
C $Name:  $
#include "GGL90_OPTIONS.h"

      SUBROUTINE GGL90_READPARMS( myThid )
C     /==========================================================\
C     | SUBROUTINE GGL90_READPARMS                               |
C     | o Routine to read in file data.ggl90                     |
C     \==========================================================/
      IMPLICIT NONE

C     === Global variables ===
#include "SIZE.h"
#include "EEPARAMS.h"
#include "PARAMS.h"
#include "GRID.h"
#include "GGL90.h"

C     === Routine arguments ===
C     myThid -  Number of this instance of GGL90_READPARMS
      INTEGER myThid

#ifdef ALLOW_GGL90

C     === Local variables ===
C     msgBuf      - Informational/error meesage buffer
C     errIO       - IO error flag
C     iUnit       - Work variable for IO unit number

      CHARACTER*(MAX_LEN_MBUF) msgBuf
      INTEGER errIO, iUnit

C--   GGL90 vertical mixing parameters
      NAMELIST /GGL90_PARM01/
     &     GGL90dumpFreq, GGL90taveFreq,
     &     GGL90diffTKEh,
     &     GGL90mixingMaps, GGL90writeState,
     &     GGL90ck, GGL90ceps, GGL90alpha, GGL90m2,
     &     GGL90TKEmin, GGL90TKEsurfMin, GGL90TKEbottom,
     &     GGL90mixingLengthMin, GGL90viscMax, GGL90diffMax,
     &     GGL90TKEFile
      _BEGIN_MASTER(myThid)

      WRITE(msgBuf,'(A)') ' GGL90_READPARMS: opening data.ggl90'
      CALL PRINT_MESSAGE( msgBuf, standardMessageUnit,
     &                    SQUEEZE_RIGHT , 1)


      CALL OPEN_COPY_DATA_FILE(
     I                          'data.ggl90', 'GGL90_READPARMS',
     O                          iUnit,
     I                          myThid )

C--   set default TKE vertical mixing parameters
      GGL90dumpFreq        = dumpFreq
      GGL90taveFreq        = taveFreq
      GGL90mixingMaps      = .FALSE.
      GGL90writeState      = .FALSE.
      GGL90ck              = 0.1 _d 0
      GGL90ceps            = 0.7 _d 0
      GGL90alpha           = 1.0 _d 0
C     Blanke and Delecluse (1993, JPO) use
      GGL90m2              = 3.75 _d 0
      GGL90TKEmin          = 1.0 _d -11
C     Blanke and Delecluse (1993, JPO) use
      GGL90TKEsurfMin      = 1.0 _d -04
      GGL90TKEbottom       = UNSET_RL
      GGL90viscMax         = 1.0 _d 0
      GGL90diffMax         = 1.0 _d 1
      GGL90diffTKEh        = 0.0 _d 0
      GGL90mixingLengthMin = 1.0 _d -08
      GGL90TKEFile         = ' '

C-----------------------------------------------------------------------
C define some non-dimensional constants and
C the vertical mixing coefficients in m-k-s units
C-----------------------------------------------------------------------
 
C--   Read settings from model parameter file "data.ggl90".
      READ(UNIT=iUnit,NML=GGL90_PARM01,IOSTAT=errIO)
      IF ( errIO .LT. 0 ) THEN
       WRITE(msgBuf,'(A)')
     &  'S/R INI_PARMS'
       CALL PRINT_ERROR( msgBuf , 1)
       WRITE(msgBuf,'(A)')
     &  'Error reading numerical model '
       CALL PRINT_ERROR( msgBuf , 1)
       WRITE(msgBuf,'(A)')
     &  'parameter file "data.ggl90"'
       CALL PRINT_ERROR( msgBuf , 1)
       WRITE(msgBuf,'(A)')
     &  'Problem in namelist GGL90_PARM01'
       CALL PRINT_ERROR( msgBuf , 1)
C      CALL MODELDATA_EXAMPLE( myThid )
       STOP 'ABNORMAL END: S/R GGL90_READPARMS'
      ENDIF

      CLOSE(iUnit)

      WRITE(msgBuf,'(A)')
     &     ' GGL90_READPARMS: finished reading data.ggl90'
      CALL PRINT_MESSAGE( msgBuf, standardMessageUnit,
     &                    SQUEEZE_RIGHT , 1)

C Now set-up any remaining parameters that result from the input parameters 
      IF ( GGL90TKEbottom .EQ. UNSET_RL ) THEN
       GGL90TKEbottom = GGL90TKEmin
      ENDIF
      IF ( GGL90TKEmin .LE. 0. ) THEN
       WRITE(msgBuf,'(A)') 
     &      'GGL90TKEmin must be greater than zero'
       CALL PRINT_ERROR( msgBuf , 1)
       STOP 'ABNORMAL END: S/R GGL90_READPARMS'
      ENDIF
      IF ( GGL90TKEbottom .LT. 0. ) THEN
       WRITE(msgBuf,'(A)') 
     &      'GGL90TKEbottom must not be less than zero'
       CALL PRINT_ERROR( msgBuf , 1)
       STOP 'ABNORMAL END: S/R GGL90_READPARMS'
      ENDIF
      IF ( GGL90mixingLengthMin .LE. 0. ) THEN
       WRITE(msgBuf,'(A)') 
     &      'GGL90mixingLengthMin must be greater than zero'
       CALL PRINT_ERROR( msgBuf , 1)
       STOP 'ABNORMAL END: S/R GGL90_READPARMS'
      ENDIF
      IF ( GGL90viscMax .LE. 0. ) THEN
       WRITE(msgBuf,'(A)') 'GGL90viscMax must be greater than zero'
       CALL PRINT_ERROR( msgBuf , 1)
       STOP 'ABNORMAL END: S/R GGL90_READPARMS'
      ENDIF
      IF ( GGL90diffMax .LE. 0. ) THEN
       WRITE(msgBuf,'(A)') 'GGL90diffMax must be greater than zero'
       CALL PRINT_ERROR( msgBuf , 1)
       STOP 'ABNORMAL END: S/R GGL90_READPARMS'
      ENDIF

C--   print TKE vertical mixing parameters to stdout for better debugging
      WRITE(msgBuf,*) 'GGL90: GGL90dumpFreq        = ', GGL90dumpFreq
      CALL PRINT_MESSAGE( msgBuf, standardMessageUnit,
     &                    SQUEEZE_RIGHT , 1)
      WRITE(msgBuf,*) 'GGL90: GGL90taveFreq        = ', GGL90taveFreq
      CALL PRINT_MESSAGE( msgBuf, standardMessageUnit,
     &                    SQUEEZE_RIGHT , 1)
      WRITE(msgBuf,*) 'GGL90: GGL90mixingMaps      = ', GGL90mixingMaps
      CALL PRINT_MESSAGE( msgBuf, standardMessageUnit,
     &                    SQUEEZE_RIGHT , 1)
      WRITE(msgBuf,*) 'GGL90: GGL90writeState      = ', GGL90writeState
      CALL PRINT_MESSAGE( msgBuf, standardMessageUnit,
     &                    SQUEEZE_RIGHT , 1)
      WRITE(msgBuf,*) 'GGL90: GGL90ck              = ', GGL90ck
      CALL PRINT_MESSAGE( msgBuf, standardMessageUnit,
     &                    SQUEEZE_RIGHT , 1)
      WRITE(msgBuf,*) 'GGL90: GGL90ceps            = ', GGL90ceps
      CALL PRINT_MESSAGE( msgBuf, standardMessageUnit,
     &                    SQUEEZE_RIGHT , 1)
      WRITE(msgBuf,*) 'GGL90: GGL90alpha           = ', GGL90alpha
      CALL PRINT_MESSAGE( msgBuf, standardMessageUnit,
     &                    SQUEEZE_RIGHT , 1)
      WRITE(msgBuf,*) 'GGL90: GGL90m2              = ', GGL90m2
      CALL PRINT_MESSAGE( msgBuf, standardMessageUnit,
     &                    SQUEEZE_RIGHT , 1)
      WRITE(msgBuf,*) 'GGL90: GGL90TKEmin          = ', GGL90TKEmin
      CALL PRINT_MESSAGE( msgBuf, standardMessageUnit,
     &                    SQUEEZE_RIGHT , 1)
      WRITE(msgBuf,*) 'GGL90: GGL90TKEsurfMin      = ', GGL90TKEsurfMin
      CALL PRINT_MESSAGE( msgBuf, standardMessageUnit,
     &                    SQUEEZE_RIGHT , 1)
      WRITE(msgBuf,*) 'GGL90: GGL90TKEbottom       = ', GGL90TKEbottom
      CALL PRINT_MESSAGE( msgBuf, standardMessageUnit,
     &                    SQUEEZE_RIGHT , 1)
      WRITE(msgBuf,*) 'GGL90: GGL90viscMax         = ', GGL90viscMax
      CALL PRINT_MESSAGE( msgBuf, standardMessageUnit,
     &                    SQUEEZE_RIGHT , 1)
      WRITE(msgBuf,*) 'GGL90: GGL90diffMax         = ', GGL90diffMax
      CALL PRINT_MESSAGE( msgBuf, standardMessageUnit,
     &                    SQUEEZE_RIGHT , 1)
      WRITE(msgBuf,*) 'GGL90: GGL90diffTKEh        = ', GGL90diffTKEh
      CALL PRINT_MESSAGE( msgBuf, standardMessageUnit,
     &                    SQUEEZE_RIGHT , 1)
      WRITE(msgBuf,*) 'GGL90: GGL90mixingLengthMin = ', 
     &                                           GGL90mixingLengthMin
      CALL PRINT_MESSAGE( msgBuf, standardMessageUnit,
     &                    SQUEEZE_RIGHT , 1)
      WRITE(msgBuf,*) 'GGL90: GGL90TKEFile         = ', GGL90TKEFile
      CALL PRINT_MESSAGE( msgBuf, standardMessageUnit,
     &                    SQUEEZE_RIGHT , 1)

      _END_MASTER(myThid)

C--   Everyone else must wait for the parameters to be loaded
      _BARRIER

#endif /* ALLOW_GGL90 */

      return
      end

C $Header: /u/gcmpack/MITgcm/pkg/ggl90/ggl90_readparms.F,v 1.13 2014/05/27 23:41:29 jmc Exp $
C $Name:  $

#include "GGL90_OPTIONS.h"

      SUBROUTINE GGL90_READPARMS( myThid )
C     *==========================================================*
C     | SUBROUTINE GGL90_READPARMS                               |
C     | o Routine to read in file data.ggl90                     |
C     *==========================================================*
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

C     === Functions ===
      INTEGER  ILNBLNK
      EXTERNAL ILNBLNK

C     === Local variables ===
C     msgBuf      - Informational/error message buffer
C     errIO       - IO error flag
C     iUnit       - Work variable for IO unit number
      CHARACTER*(MAX_LEN_MBUF) msgBuf
      INTEGER errIO, iUnit, iL

C--   GGL90 vertical mixing parameters
      NAMELIST /GGL90_PARM01/
     &     GGL90dumpFreq, GGL90taveFreq,
     &     GGL90diffTKEh,
     &     GGL90mixingMaps, GGL90writeState,
     &     GGL90ck, GGL90ceps, GGL90alpha, GGL90m2,
     &     GGL90TKEmin, GGL90TKEsurfMin, GGL90TKEbottom,
     &     GGL90mixingLengthMin, mxlMaxFlag, mxlSurfFlag,
     &     GGL90viscMax, GGL90diffMax, GGL90TKEFile

C---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|

      IF ( .NOT.useGGL90 ) THEN
C-    pkg GGL90 is not used
        _BEGIN_MASTER(myThid)
C-    Track pkg activation status:
C     print a (weak) warning if data.ggl90 is found
         CALL PACKAGES_UNUSED_MSG( 'useGGL90', ' ', ' ' )
        _END_MASTER(myThid)
        RETURN
      ENDIF

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
      GGL90viscMax         = 1. _d 2
      GGL90diffMax         = 1. _d 2
      GGL90diffTKEh        = 0.0 _d 0
      GGL90mixingLengthMin = 1.0 _d -08
      mxlMaxFlag           = 0
      mxlSurfFlag    = .FALSE.
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
      WRITE(msgBuf,'(A)')
     &'// ======================================================='
      CALL PRINT_MESSAGE( msgBuf, standardMessageUnit,
     &                    SQUEEZE_RIGHT , 1)
      WRITE(msgBuf,'(A)') '// GGL90 configuration'
      CALL PRINT_MESSAGE( msgBuf, standardMessageUnit,
     &                    SQUEEZE_RIGHT , 1)
      WRITE(msgBuf,'(A)')
     &'// ======================================================='
      CALL PRINT_MESSAGE( msgBuf, standardMessageUnit,
     &  SQUEEZE_RIGHT , 1)

      CALL WRITE_0D_RL( GGL90dumpFreq, INDEX_NONE,'GGL90dumpFreq =',
     &'   /* GGL90 state write out interval ( s ). */')
      CALL WRITE_0D_RL( GGL90taveFreq, INDEX_NONE,'GGL90taveFreq =',
     &'   /* GGL90 averaging interval ( s ). */')
      CALL WRITE_0D_L(GGL90mixingMaps,INDEX_NONE,
     &     'GGL90mixingMAPS =', '   /* GGL90 IO flag. */')
      CALL WRITE_0D_L(GGL90writeState,INDEX_NONE,
     &     'GGL90writeState =', '   /* GGL90 IO flag. */')
      CALL WRITE_0D_RL( GGL90ck, INDEX_NONE,'GGL90ck =',
     &'   /* GGL90 viscosity parameter. */')
      CALL WRITE_0D_RL( GGL90ceps, INDEX_NONE,'GGL90ceps =',
     &'   /* GGL90 dissipation parameter. */')
      CALL WRITE_0D_RL( GGL90alpha, INDEX_NONE,'GGL90alpha =',
     &'   /* GGL90 TKE diffusivity parameter. */')
      CALL WRITE_0D_RL( GGL90m2, INDEX_NONE,'GGL90m2 =',
     &'   /* GGL90 wind stress to vertical stress ratio. */')
      CALL WRITE_0D_RL( GGL90TKEmin, INDEX_NONE,'GGL90TKEmin =',
     &'   /* GGL90 minimum kinetic energy ( m^2/s^2 ). */')
      CALL WRITE_0D_RL( GGL90TKEsurfMin, INDEX_NONE,
     &     'GGL90TKEsurfMin =',
     &'   /* GGL90 minimum surface kinetic energy ( m^2/s^2 ). */')
      CALL WRITE_0D_RL( GGL90TKEbottom, INDEX_NONE,
     &     'GGL90TKEbottom =',
     &     '   /* GGL90 bottom kinetic energy ( m^2/s^2 ). */')
      CALL WRITE_0D_RL( GGL90viscMax, INDEX_NONE,'GGL90viscMax =',
     &     '   /* GGL90 upper limit for viscosity ( m^2/s ). */')
      CALL WRITE_0D_RL( GGL90diffMax, INDEX_NONE,'GGL90diffMax =',
     &     '   /* GGL90 upper limit for diffusivity ( m^2/s ). */')
      CALL WRITE_0D_RL( GGL90diffTKEh, INDEX_NONE,'GGL90diffTKEh =',
     &     '   /* GGL90 horizontal diffusivity for TKE ( m^2/s ). */')
      CALL WRITE_0D_RL( GGL90mixingLengthMin, INDEX_NONE,
     &     'GGL90mixingLengthMin =',
     &     '   /* GGL90 minimum mixing length ( m ). */')
      CALL WRITE_0D_I(mxlMaxFlag, INDEX_NONE, 'mxlMaxFlag =',
     &     '   /* Flag for limiting mixing-length method */')
      CALL WRITE_0D_L(mxlSurfFlag,INDEX_NONE,
     &     'mxlSurfFlag =',
     &     '   /* GGL90 flag for near surface mixing. */')
      iL = MAX_LEN_MBUF - 22
      iL = MIN( iL, MAX(ILNBLNK(GGL90TKEFile),1) )
      WRITE(msgBuf,'(A,A)')'GGL90: GGL90TKEFile = ',GGL90TKEFile(1:iL)
      CALL PRINT_MESSAGE( msgBuf, standardMessageUnit,
     &                    SQUEEZE_RIGHT , 1)

      WRITE(msgBuf,'(A)')
     &'// ======================================================='
      CALL PRINT_MESSAGE( msgBuf, standardMessageUnit,
     &                    SQUEEZE_RIGHT , 1)
      WRITE(msgBuf,'(A)') '// End of GGL90 config. summary'
      CALL PRINT_MESSAGE( msgBuf, standardMessageUnit,
     &                    SQUEEZE_RIGHT , 1)
      WRITE(msgBuf,'(A)')
     &'// ======================================================='
      CALL PRINT_MESSAGE( msgBuf, standardMessageUnit,
     &  SQUEEZE_RIGHT , 1)
      WRITE(msgBuf,'(A)') ' '
      CALL PRINT_MESSAGE( msgBuf, standardMessageUnit,
     &  SQUEEZE_RIGHT , 1)

      _END_MASTER(myThid)

C--   Everyone else must wait for the parameters to be loaded
      _BARRIER

#endif /* ALLOW_GGL90 */

      RETURN
      END

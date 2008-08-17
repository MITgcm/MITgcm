C $Header: /u/gcmpack/MITgcm/pkg/down_slope/dwnslp_readparms.F,v 1.1 2008/08/17 02:01:27 jmc Exp $
C $Name:  $

#include "DWNSLP_OPTIONS.h"

CBOP
C     !ROUTINE: DWNSLP_READPARMS
C     !INTERFACE:
      SUBROUTINE DWNSLP_READPARMS( myThid )
C     !DESCRIPTION: \bv
C     *==========================================================*
C     | SUBROUTINE DWNSLP_READPARMS
C     | o Routine to initialize Down-Sloping Parameters
C     *==========================================================*
C     \ev

C     !USES:
      IMPLICIT NONE

C     === Global variables ===
#include "SIZE.h"
#include "EEPARAMS.h"
#include "PARAMS.h"
#include "DWNSLP_PARAMS.h"

C     !INPUT/OUTPUT PARAMETERS:
C     === Routine arguments ===
      INTEGER myThid

#ifdef ALLOW_DOWN_SLOPE

      NAMELIST /DWNSLP_PARM01/
     &          DWNSLP_slope, DWNSLP_rec_mu, DWNSLP_drFlow,
     &          temp_useDWNSLP, salt_useDWNSLP

C     !LOCAL VARIABLES:
C     === Local variables ===
C     msgBuf      - Informational/error meesage buffer
C     iUnit       - Work variable for IO unit number
      CHARACTER*(MAX_LEN_MBUF) msgBuf
      INTEGER iUnit
CEOP

      _BEGIN_MASTER(myThid)

      WRITE(msgBuf,'(A)')' DWNSLP_READPARMS: opening data.down_slope'
      CALL PRINT_MESSAGE( msgBuf, standardMessageUnit,
     &                    SQUEEZE_RIGHT , 1)

      CALL OPEN_COPY_DATA_FILE(
     I                         'data.down_slope', 'DWNSLP_READPARMS',
     O                         iUnit,
     I                         myThid )

C--   Default flags and values for DownSlope Parameterisation
      temp_useDWNSLP = tempStepping
      salt_useDWNSLP = saltStepping
      DWNSLP_slope  = 0.
      DWNSLP_rec_mu = 0.
      DWNSLP_drFlow = 0.

C--   Read parameters from open data file
      READ(UNIT=iUnit,NML=DWNSLP_PARM01)

      WRITE(msgBuf,'(A)')
     &   ' DWNSLP_READPARMS: finished reading data.downslp'
      CALL PRINT_MESSAGE( msgBuf, standardMessageUnit,
     &                    SQUEEZE_RIGHT , 1)

C--   Close the open data file
      CLOSE(iUnit)
      _END_MASTER(myThid)

C--   Everyone else must wait for the parameters to be loaded
      _BARRIER

C--   Check the parameters :
      IF ( temp_useDWNSLP .AND. .NOT. tempStepping ) THEN
        WRITE(msgBuf,'(A)')
     &  'need tempStepping=T to apply DWNSLP to Temp (temp_useDWNSLP=T)'
        CALL PRINT_ERROR( msgBuf , myThid )
        STOP 'ABNORMAL END: S/R DWNSLP_READPARMS'
      ENDIF
      IF ( salt_useDWNSLP .AND. .NOT. saltStepping ) THEN
        WRITE(msgBuf,'(A)')
     &  'need saltStepping=T to apply DWNSLP to Salt (salt_useDWNSLP=T)'
        CALL PRINT_ERROR( msgBuf , myThid )
        STOP 'ABNORMAL END: S/R DWNSLP_READPARMS'
      ENDIF

C- print out some kee parameters :
      _BEGIN_MASTER(myThid)

       CALL WRITE_0D_R8( DWNSLP_slope, INDEX_NONE,'DWNSLP_slope =',
     &  '   /* DOWNSLP fixed slope (=0 => use local slope) */')
       CALL WRITE_0D_R8( DWNSLP_rec_mu,INDEX_NONE,'DWNSLP_rec_mu =',
     &  '   /* DOWNSLP recip. friction parameter (time, s ) */')
       CALL WRITE_0D_R8( DWNSLP_drFlow,INDEX_NONE,'DWNSLP_drFlow =',
     &  '   /* DOWNSLP effective layer thickness ( m ) */')

      _END_MASTER(myThid)

#endif /* ALLOW_DOWN_SLOPE */
      RETURN
      END

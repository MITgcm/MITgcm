C $Header: /u/gcmpack/MITgcm/pkg/gmredi/gmredi_readparms.F,v 1.4 2001/02/04 14:38:49 cnh Exp $
C $Name:  $

#include "GMREDI_OPTIONS.h"

      SUBROUTINE GMREDI_READPARMS( myThid )
C     /==========================================================\
C     | SUBROUTINE GMREDI_READPARMS                              |
C     | o Routine to initialize GM/Redi variables and constants. |
C     |==========================================================|
C     | Initialize GM/Redi parameters, read in data.gmredi       |
C     \==========================================================/
      IMPLICIT NONE

C     === Global variables ===
#include "SIZE.h"
#include "EEPARAMS.h"
#include "PARAMS.h"
#include "GRID.h"
#include "GMREDI.h"
#include "GMREDI_DIAGS.h"

C     === Routine arguments ===
      INTEGER myThid

#ifdef ALLOW_GMREDI

C--   GM/Redi parameter
      NAMELIST /GM_PARM01/
     &          GM_background_K,
     &          GM_taper_scheme,
     &          GM_maxSlope,
#ifdef GM_VISBECK_VARIABLE_K
     &          GM_Visbeck_alpha,
     &          GM_Visbeck_length,
     &          GM_Visbeck_depth,
     &          GM_Visbeck_maxval_K,
#endif
     &          GM_Scrit,
     &          GM_Sd
c     &          GM_dumpFreq,
c     &          GM_taveFreq

C     === Local variables ===
C     msgBuf      - Informational/error meesage buffer
C     iUnit       - Work variable for IO unit number
      CHARACTER*(MAX_LEN_MBUF) msgBuf
      INTEGER iUnit

C--   GMREDI_READPARMS has been called so we know that
C     the package is active.
      GMRediIsOn=.TRUE.


      _BEGIN_MASTER(myThid)

      WRITE(msgBuf,'(A)') ' GM_READPARMS: opening data.gmredi'
      CALL PRINT_MESSAGE( msgBuf, standardMessageUnit,
     &                    SQUEEZE_RIGHT , 1)
    

      CALL OPEN_COPY_DATA_FILE(
     I                          'data.gmredi', 'GM_READPARMS',
     O                          iUnit,
     I                          myThid )

C--   Default values GM/Redi
      GM_background_K     = 0.
      GM_maxslope         = 1.0D-2
      GM_taper_scheme     = ' '
      GM_Scrit            = 0.004
      GM_Sd               = 0.001

C--   Default values GM/Redi I/O control
c      GM_dumpFreq         = -1.
c      GM_taveFreq         = -1.

#ifdef GM_VISBECK_VARIABLE_K
C--   Default values Visbeck
      GM_Visbeck_alpha    = 0.
      GM_Visbeck_length   = 200.D3
      GM_Visbeck_depth    = 1000.D0
      GM_Visbeck_maxval_K = 2500.D0
#endif


C--   Read parameters from open data file
      READ(UNIT=iUnit,NML=GM_PARM01)

C     Some constants
      GM_rMaxSlope=0.
      if (GM_maxSlope.ne.0.) GM_rMaxSlope=1. / GM_maxSlope

C     Unspecified I/O control parameters default to model pars
c      if (GM_dumpFreq.EQ.-1.) GM_dumpFreq=dumpFreq
c      if (GM_taveFreq.EQ.-1.) GM_taveFreq=taveFreq

#ifndef GM_VISBECK_VARIABLE_K
C     Make sure we are not trying to use something that is unavailable
      IF (GM_Visbeck_alpha .NE. 0.) THEN
       WRITE(msgBuf,'(A)')
     &   ' GM_READPARMS: Visbeck variables used in data.gmredi'
       CALL PRINT_ERROR( msgBuf, 1 )
       WRITE(msgBuf,'(A)')
     &   ' GM_READPARMS: without #define GM_VISBECK_VARIABLE_K'
       CALL PRINT_ERROR( msgBuf, 1 )
       STOP 'ABNORMAL END: S/R GM_READPARMS'
      ENDIF
#endif

      WRITE(msgBuf,'(A)') ' GM_READPARMS: finished reading data.gmredi'
      CALL PRINT_MESSAGE( msgBuf, standardMessageUnit,
     &                    SQUEEZE_RIGHT , 1)

C--   Close the open data file
      CLOSE(iUnit)
      _END_MASTER(myThid)

C--   Everyone else must wait for the parameters to be loaded
      _BARRIER


#endif /* ALLOW_GMREDI */

      return
      end

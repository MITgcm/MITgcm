C $Header: /u/gcmpack/MITgcm/pkg/gmredi/Attic/gmredi_init.F,v 1.1 2000/06/21 19:42:54 adcroft Exp $

#include "GMREDI_OPTIONS.h"

      SUBROUTINE GMREDI_INIT( myThid )
C     /==========================================================\
C     | SUBROUTINE GMREDI_INIT                                   |
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
C     myThid -  Number of this instance of KPP_INIT
      INTEGER myThid

#ifdef ALLOW_GMREDI

C--   GM/Redi parameter
      NAMELIST /GM_PARM01/
     &          GM_background_K,
     &          GM_taper_scheme,
     &          GM_maxSlope,
     &          GM_Visbeck_alpha,
     &          GM_Visbeck_length,
     &          GM_Visbeck_depth,
     &          GM_Visbeck_maxval_K,
     &          GM_Scrit,
     &          GM_Sd

C     === Local variables ===
C     msgBuf      - Informational/error meesage buffer
C     iUnit       - Work variable for IO unit number
      CHARACTER*(MAX_LEN_MBUF) msgBuf
      INTEGER iUnit
      INTEGER k,bi,bj

      _BEGIN_MASTER(myThid)

      WRITE(msgBuf,'(A)') ' GM_INIT: opening data.gmredi'
      CALL PRINT_MESSAGE( msgBuf, standardMessageUnit,
     &                    SQUEEZE_RIGHT , 1)
    

      CALL OPEN_COPY_DATA_FILE(
     I                          'data.gmredi', 'GM_INIT',
     O                          iUnit,
     I                          myThid )

C--   Default values GM/Redi
      GM_background_K     = 0.
      GM_maxslope         = 1.0D-2
      GM_taper_scheme     = ' '
      GM_Scrit=0.004
      GM_Sd=0.001

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

#ifndef GM_VISBECK_VARIABLE_K
C     Make sure we are not trying to use something that is unavailable
      IF (GM_Visbeck_alpha .NE. 0.) THEN
       WRITE(msgBuf,'(A)')
     &   ' GM_INIT: Visbeck variables used in data.gmredi'
       CALL PRINT_ERROR( msgBuf, 1 )
       WRITE(msgBuf,'(A)')
     &   ' GM_INIT: without #define GM_VISBECK_VARIABLE_K'
       CALL PRINT_ERROR( msgBuf, 1 )
       STOP 'ABNORMAL END: S/R GM_INIT'
      ENDIF
#endif

      WRITE(msgBuf,'(A)') ' GM_INIT: finished reading data.gmredi'
      CALL PRINT_MESSAGE( msgBuf, standardMessageUnit,
     &                    SQUEEZE_RIGHT , 1)

C--   Close the open data file
      CLOSE(iUnit)
      _END_MASTER(myThid)

C--   Everyone else must wait for the parameters to be loaded
      _BARRIER


#ifdef INCLUDE_DIAGNOSTICS_INTERFACE_CODE
C     Initialize averages to zero
      DO bj = myByLo(myThid), myByHi(myThid)
       DO bi = myBxLo(myThid), myBxHi(myThid)
        DO k=1,Nr
         CALL TIMEAVER_INI_XYZ(GM_Kwx_T,bi,bj,K,myThid)
         CALL TIMEAVER_INI_XYZ(GM_Kwy_T,bi,bj,K,myThid)
         CALL TIMEAVER_INI_XYZ(GM_Kwz_T,bi,bj,K,myThid)
         GM_TimeAve(k,bi,bj)=0.
        ENDDO
#ifdef GM_VISBECK_VARIABLE_K
        CALL TIMEAVER_INI_XY(Visbeck_K_T, bi,bj,myThid)
#endif
       ENDDO
      ENDDO
#endif /* INCLUDE_DIAGNOSTICS_INTERFACE_CODE */

#endif /* ALLOW_GMREDI */

      return
      end

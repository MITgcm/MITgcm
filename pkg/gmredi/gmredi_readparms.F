C $Header: /u/gcmpack/MITgcm/pkg/gmredi/gmredi_readparms.F,v 1.14 2006/10/17 19:00:44 jmc Exp $
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

C     === Routine arguments ===
      INTEGER myThid

#ifdef ALLOW_GMREDI

C--   GM/Redi parameter
C     GM_Small_Number :: epsilon used in computing the slope
C     GM_slopeSqCutoff :: slope^2 cut-off value
      NAMELIST /GM_PARM01/
     &          GM_AdvForm, GM_AdvSeparate,
     &          GM_isopycK,
     &          GM_background_K,
     &          GM_taper_scheme,
     &          GM_maxSlope,
     &          GM_Kmin_horiz,
     &          GM_Small_Number, GM_slopeSqCutoff,
     &          GM_Visbeck_alpha,
     &          GM_Visbeck_length,
     &          GM_Visbeck_depth,
     &          GM_Visbeck_maxval_K,
     &          GM_Scrit,
     &          GM_Sd,
     &          GM_MNC
c     &          GM_dumpFreq,
c     &          GM_taveFreq

C     === Local variables ===
C     msgBuf      - Informational/error meesage buffer
C     iUnit       - Work variable for IO unit number
      CHARACTER*(MAX_LEN_MBUF) msgBuf
      INTEGER iUnit

      _BEGIN_MASTER(myThid)

C--   GMREDI_READPARMS has been called so we know that
C     the package is active.
      GMRediIsOn = .TRUE.

C--   Default values GM/Redi
      GM_AdvForm          = .FALSE.
      GM_AdvSeparate      = .FALSE.
      GM_isopycK          = -999.
      GM_background_K     = 0.
      GM_maxslope         = 1.0D-2
      GM_Kmin_horiz       = 0.
      GM_Small_Number     = 1. _d -12
      GM_slopeSqCutoff    = 1. _d +48
      GM_taper_scheme     = ' '
      GM_Scrit            = 0.004
      GM_Sd               = 0.001
      GM_MNC              = useMNC

C--   Default values GM/Redi I/O control
c     GM_dumpFreq         = -1.
c     GM_taveFreq         = -1.

C--   Default values Visbeck
      GM_Visbeck_alpha    = 0.
      GM_Visbeck_length   = 200.D3
      GM_Visbeck_depth    = 1000.D0
      GM_Visbeck_maxval_K = 2500.D0

      WRITE(msgBuf,'(A)') ' GM_READPARMS: opening data.gmredi'
      CALL PRINT_MESSAGE( msgBuf, standardMessageUnit,
     &                    SQUEEZE_RIGHT , 1)
      CALL OPEN_COPY_DATA_FILE(
     I                          'data.gmredi', 'GM_READPARMS',
     O                          iUnit,
     I                          myThid )

C     Read parameters from open data file
      READ(UNIT=iUnit,NML=GM_PARM01)
      WRITE(msgBuf,'(A)') ' GM_READPARMS: finished reading data.gmredi'
      CALL PRINT_MESSAGE( msgBuf, standardMessageUnit,
     &                    SQUEEZE_RIGHT , 1)
C     Close the open data file
      CLOSE(iUnit)

C     Default value for GM_isopycK is equal to GM_background_K :
      IF (GM_isopycK.EQ.-999.) GM_isopycK = GM_background_K

C     Some constants
      GM_rMaxSlope=0.
      if (GM_maxSlope.ne.0.) GM_rMaxSlope=1. / GM_maxSlope

      IF (GM_AdvForm) THEN
        GM_skewflx = 0.
        GM_advect  = 1.
        GM_ExtraDiag = GM_Visbeck_alpha.NE.0. .OR. GM_isopycK.NE.0.
      ELSE
        GM_skewflx = 1.
        GM_advect  = 0.
        GM_ExtraDiag = GM_isopycK.NE.GM_background_K
      ENDIF

C     Make sure that we locally honor the global MNC on/off flag
      GM_MNC = GM_MNC .AND. useMNC
#ifndef ALLOW_MNC
C     Fix to avoid running without getting any output:
      GM_MNC = .FALSE.
#endif
      GM_MDSIO = (.NOT. GM_MNC) .OR. outputTypesInclusive

      _END_MASTER(myThid)

C--   Everyone else must wait for the parameters to be loaded
      _BARRIER

#endif /* ALLOW_GMREDI */

      RETURN
      END

#include "GMREDI_OPTIONS.h"

CBOP
C     !ROUTINE: GMREDI_READPARMS
C     !INTERFACE:
      SUBROUTINE GMREDI_READPARMS( myThid )

C     !DESCRIPTION: \bv
C     *==========================================================*
C     | SUBROUTINE GMREDI_READPARMS
C     | o Routine to initialize GM/Redi variables and constants.
C     *==========================================================*
C     | Initialize GM/Redi parameters, read in data.gmredi
C     *==========================================================*
C     \ev

C     !USES:
      IMPLICIT NONE

C     === Global variables ===
#include "SIZE.h"
#include "EEPARAMS.h"
#include "PARAMS.h"
#include "GRID.h"
#include "GMREDI.h"

C     !INPUT/OUTPUT PARAMETERS:
C     === Routine arguments ===
      INTEGER myThid

#ifdef ALLOW_GMREDI
C     !LOCAL VARIABLES:
C     === Local variables ===
C     msgBuf     :: Informational/error message buffer
C     iUnit      :: Work variable for IO unit number
C     nRetired   :: Counter used to trap "retired" parameters in namelist.
C     GM_isopycK3dFile      :: input file for 3.D GM_isopycK
C     GM_background_K3dFile :: input file for 3.D GM_background_K
      CHARACTER*(MAX_LEN_MBUF) msgBuf
      INTEGER iUnit
      INTEGER nRetired
      CHARACTER*(MAX_LEN_FNAM) GM_isopycK3dFile
      CHARACTER*(MAX_LEN_FNAM) GM_background_K3dFile
CEOP

C--   GM/Redi parameter
C     GM_Small_Number  :: epsilon used in computing the slope
C     GM_slopeSqCutoff :: slope^2 cut-off value
      NAMELIST /GM_PARM01/
     &          GM_AdvForm, GM_AdvSeparate,
     &          GM_InMomAsStress,
     &          GM_isopycK,
     &          GM_background_K,
     &          GM_iso2dFile, GM_iso1dFile,
     &          GM_bol2dFile, GM_bol1dFile,
     &          GM_K3dRediFile, GM_K3dGMFile,
     &          GM_background_K3dFile, GM_isopycK3dFile,
     &          GM_taper_scheme,
     &          GM_maxSlope,
     &          GM_Kmin_horiz,
     &          GM_Small_Number, GM_slopeSqCutoff,
     &          GM_Scrit, GM_Sd,
     &          GM_facTrL2dz, GM_facTrL2ML, GM_maxTransLay,
     &          GM_UseBVP, GM_BVP_cMin, GM_BVP_ModeNumber,
     &          GM_useSubMeso, subMeso_Ceff, subMeso_invTau,
     &          subMeso_LfMin, subMeso_Lmax,
     &          GM_Visbeck_alpha, GM_Visbeck_length,
     &          GM_Visbeck_depth,
     &          GM_Visbeck_minDepth, GM_Visbeck_maxSlope,
     &          GM_Visbeck_minVal_K, GM_Visbeck_maxVal_K,
     &          GM_useBatesK3d, GM_Bates_smooth, GM_Bates_use_constK,
     &          GM_Bates_beta_eq_0, GM_Bates_ThickSheet,
     &          GM_Bates_surfK, GM_Bates_constRedi,
     &          GM_Bates_gamma, GM_Bates_b1,
     &          GM_Bates_EadyMinDepth, GM_Bates_EadyMaxDepth,
     &          GM_Bates_Lambda, GM_Bates_smallK, GM_Bates_maxK,
     &          GM_Bates_constK, GM_Bates_maxC,
     &          GM_Bates_Rmax, GM_Bates_Rmin,
     &          GM_Bates_minCori, GM_Bates_minN2,
     &          GM_Bates_surfMinDepth, GM_Bates_vecFreq,
     &          GM_Bates_minRenorm, GM_Bates_maxRenorm,
     &          GM_useLeithQG,
     &          GM_MNC

C---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|

      IF ( .NOT.useGMRedi ) THEN
C-    pkg GMREDI is not used
        _BEGIN_MASTER(myThid)
C-    Track pkg activation status:
C     print a (weak) warning if data.gmredi is found
         CALL PACKAGES_UNUSED_MSG( 'useGMRedi', ' ', ' ' )
        _END_MASTER(myThid)
        RETURN
      ENDIF

      _BEGIN_MASTER(myThid)

C--   Default values GM/Redi
      GM_AdvForm          = .FALSE.
      GM_AdvSeparate      = .FALSE.
      GM_InMomAsStress    = .FALSE.
      GM_isopycK          = -999.
      GM_background_K     = 0. _d 0
      GM_maxSlope         = 1. _d -2
      GM_Kmin_horiz       = 0. _d 0
      GM_Small_Number     = 1. _d -20
      GM_slopeSqCutoff    = 1. _d +48
      GM_taper_scheme     = ' '
      GM_facTrL2dz        = 1.
      GM_facTrL2ML        = 5.
      GM_maxTransLay      = 500.
      GM_Scrit            = 0.004 _d 0
      GM_Sd               = 0.001 _d 0
      GM_MNC              = useMNC
      GM_iso2dFile        = ' '
      GM_iso1dFile        = ' '
      GM_bol2dFile        = ' '
      GM_bol1dFile        = ' '
      GM_K3dRediFile      = ' '
      GM_K3dGMFile        = ' '
      GM_useLeithQG       = .FALSE.

C--   Default values GM/Redi I/O control
c     GM_dumpFreq         = -1.
c     GM_taveFreq         = -1.

C--   Default values BVP
      GM_UseBVP           = .FALSE.
      GM_BVP_ModeNumber   = 1
      GM_BVP_cMin         = 1. _d -1

C--   Default values Sub-Meso (corresponding tau = 5.8 days):
      GM_useSubMeso = .FALSE.
      subMeso_invTau =  2.0 _d -6
      subMeso_LfMin  =  1.0 _d +3
      subMeso_Ceff   =  7.0 _d -2
      subMeso_Lmax   = 110. _d +3

C--   Default values Visbeck
      GM_Visbeck_alpha    =    0. _d 0
      GM_Visbeck_length   =  200. _d 3
      GM_Visbeck_depth    = 1000. _d 0
      GM_Visbeck_minDepth =    0. _d 0
      GM_Visbeck_maxSlope = UNSET_RL
      GM_Visbeck_minVal_K =    0. _d 0
      GM_Visbeck_maxVal_K = 2500. _d 0

C--   Default values for BatesK3d
      GM_useBatesK3d     = .FALSE.
      GM_Bates_smooth    = .TRUE.
      GM_Bates_use_constK= .FALSE.
      GM_Bates_beta_eq_0 = .TRUE.
      GM_Bates_ThickSheet= .FALSE.
      GM_Bates_surfK     = .FALSE.
      GM_Bates_constRedi = .FALSE.
      GM_Bates_gamma     = 1.0
      GM_Bates_b1        = 4.0
      GM_Bates_EadyMinDepth = 50.0
      GM_Bates_EadyMaxDepth = 1000.
      GM_Bates_Lambda    = 1.0
      GM_Bates_smallK    = 100.
      GM_Bates_maxK      = 20. _d 3
      GM_Bates_constK    = 1000.
      GM_Bates_maxC      = 0.15 _d 0
      GM_Bates_Rmax      = 75. _d 3
      GM_Bates_Rmin      = 30. _d 3
      GM_Bates_minCori   = zeroRL
      GM_Bates_minN2     = 1. _d -8
      GM_Bates_surfMinDepth = 100.
      GM_Bates_vecFreq   = 2592000.
      GM_Bates_minRenorm = oneRL
      GM_Bates_maxRenorm = 20.

C--   Initialise retired parameters to unlikely value
      nRetired = 0
      GM_background_K3dFile = ' '
      GM_isopycK3dFile    = ' '

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
#ifdef SINGLE_DISK_IO
      CLOSE(iUnit)
#else
      CLOSE(iUnit,STATUS='DELETE')
#endif /* SINGLE_DISK_IO */

C     Default value for GM_isopycK is equal to GM_background_K :
      IF (GM_isopycK.EQ.-999.) GM_isopycK = GM_background_K

C     Default value for GM_Visbeck_maxSlope is equal to GM_maxSlope :
      IF ( GM_Visbeck_maxSlope .EQ. UNSET_RL )
     &     GM_Visbeck_maxSlope = GM_maxSlope

C     Some constants
      GM_rMaxSlope = 0.
      if (GM_maxSlope.NE.0.) GM_rMaxSlope = 1. _d 0 / GM_maxSlope

      IF (GM_AdvForm) THEN
        GM_skewflx = 0.
        GM_advect  = 1.
        GM_ExtraDiag = GM_Visbeck_alpha.NE.0. .OR. GM_isopycK.NE.0.
      ELSE
        GM_skewflx = 1.
        GM_advect  = 0.
        GM_ExtraDiag = GM_isopycK.NE.GM_background_K
      ENDIF
      IF ( GM_iso2dFile .NE. GM_bol2dFile .OR.
     &     GM_iso1dFile .NE. GM_bol1dFile ) THEN
        GM_ExtraDiag = .TRUE.
      ENDIF

      IF (GM_UseBVP) THEN
        GM_BVP_rModeNumber = 1. _d 0/DBLE(GM_BVP_ModeNumber)
        GM_BVP_cHat2Min = GM_BVP_cMin**2*rhoConst/gravity
      ENDIF

C     Make sure that we locally honor the global MNC on/off flag
      GM_MNC = GM_MNC .AND. useMNC
#ifndef ALLOW_MNC
C     Fix to avoid running without getting any output:
      GM_MNC = .FALSE.
#endif
      GM_MDSIO = (.NOT. GM_MNC) .OR. outputTypesInclusive

C     Check for retired parameters
      IF ( GM_background_K3dFile .NE. ' ' ) THEN
        nRetired = nRetired+1
        WRITE(msgBuf,'(2A)') 'S/R GMREDI_READPARMS: ',
     &    '"GM_background_K3dFile" has been replaced by "GM_K3dGMFile"'
        CALL PRINT_ERROR( msgBuf, myThid )
        WRITE(msgBuf,'(2A)') 'S/R GMREDI_READPARMS: ',
     &    'and is no longer allowed in file "data.gmredi"'
        CALL PRINT_ERROR( msgBuf, myThid )
      ENDIF
      IF ( GM_isopycK3dFile .NE. ' ' ) THEN
        nRetired = nRetired+1
        WRITE(msgBuf,'(2A)') 'S/R GMREDI_READPARMS: ',
     &    '"GM_isopycK3dFile" has been replaced by "GM_K3dRediFile"'
        CALL PRINT_ERROR( msgBuf, myThid )
        WRITE(msgBuf,'(2A)') 'S/R GMREDI_READPARMS: ',
     &    'and is no longer allowed in file "data.gmredi"'
        CALL PRINT_ERROR( msgBuf, myThid )
      ENDIF

      IF ( nRetired .GT. 0 ) THEN
        WRITE(msgBuf,'(2A)') 'S/R GMREDI_READPARMS: ',
     &              'Error reading parameter file "data.gmredi":'
        CALL PRINT_ERROR( msgBuf, myThid )
        WRITE(msgBuf,'(I4,A)') nRetired,
     &      ' out of date parameters were found in the namelist'
        CALL PRINT_ERROR( msgBuf, myThid )
        CALL ALL_PROC_DIE( 0 )
        STOP 'ABNORMAL END: S/R GMREDI_READPARMS'
      ENDIF

      _END_MASTER(myThid)

C--   Everyone else must wait for the parameters to be loaded
      _BARRIER

#endif /* ALLOW_GMREDI */

      RETURN
      END

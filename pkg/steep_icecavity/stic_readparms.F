#include "STIC_OPTIONS.h"

CBOP
C !ROUTINE: STIC_READPARMS

C !INTERFACE: ==========================================================
      SUBROUTINE STIC_READPARMS( myThid )

C !DESCRIPTION:
C     Initialize STIC parameters, read in data.stic

C !USES: ===============================================================
      IMPLICIT NONE
#include "SIZE.h"
#include "EEPARAMS.h"
#include "PARAMS.h"
#include "STIC.h"
#ifdef ALLOW_COST
# include "STIC_COST.h"
#endif /* ALLOW_COST */
#ifdef ALLOW_MNC
# include "MNC_PARAMS.h"
#endif

C !INPUT PARAMETERS: ===================================================
C  myThid               :: thread number
      INTEGER myThid

C !OUTPUT PARAMETERS: ==================================================
C  none

#ifdef ALLOW_STEEP_ICECAVITY

C !LOCAL VARIABLES: ====================================================
C  iUnit                :: unit number for I/O
C  msgBuf               :: message buffer
      INTEGER iUnit
      CHARACTER*(MAX_LEN_MBUF) msgBuf
CEOP

      NAMELIST /STIC_PARM01/
     &     STICsaltToHeatRatio,
     &     STICheatTransCoeff,
     &     STICsaltTransCoeff,
     &     STICMassStepping,
     &     rhoSTIC, STICkappa,
     &     STIClatentHeat, STICHeatCapacity_Cp,
     &     no_slip_stic, STICDragLinear,
     &     STICDragQuadratic, STICselectDragQuadr,
     &     STICthetaSurface,
     &     STICsalinity,
     &     useISOMIPTD,
     &     STICconserve, STICboundaryLayer,
     &     SHI_withBL_realFWflux, SHI_withBL_uStarTopDz,
     &     STICwriteState,
     &     STIC_dumpFreq,
     &     STIC_taveFreq,
     &     STIC_tave_mnc,
     &     STIC_dump_mnc,
     &     STICtopoFile,
     &     STICmassFile, STICloadAnomalyFile,
     &     STICMassDynTendFile, STICTransCoeffTFile,
     &     ICEFRONTlengthFile, ICEFRONTdepthFile,
     &     STICDynMassOnly,
     &     STICadvDiffHeatFlux,
     &     STICuseGammaFrict, STIC_oldCalcUStar,
     &     shiCdrag, shiZetaN, shiRc,
     &     shiPrandtl, shiSchmidt, shiKinVisc,
#ifdef ALLOW_COST
     &     mult_stic,
     &     mult_shifwflx, wshifwflx0, shifwflx_errfile,
#endif
     &     STICremeshFrequency,
     &     STICsplitThreshold, STICmergeThreshold

C---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|

      IF ( .NOT.useSTIC ) THEN
C-    pkg STIC is not used
        _BEGIN_MASTER(myThid)
C-    Track pkg activation status:
         STICisOn = .FALSE.
C     print a (weak) warning if data.stic is found
         CALL PACKAGES_UNUSED_MSG( 'useSTIC', ' ', ' ' )
        _END_MASTER(myThid)
        RETURN
      ENDIF

      _BEGIN_MASTER(myThid)

C This routine has been called by the main model so we set our
C internal flag to indicate we are in business
      STICisOn = .TRUE.

C Set defaults values for parameters in STIC.h
      useISOMIPTD              = .FALSE.
      STICconserve         = .FALSE.
      STICboundaryLayer    = .FALSE.
      SHI_withBL_realFWflux    = .FALSE.
      SHI_withBL_uStarTopDz    = .FALSE.
      STICMassStepping     = .FALSE.
      STICDynMassOnly      = .FALSE.
      STICtopoFile         = ' '
      STICmassFile         = ' '
      STICloadAnomalyFile  = ' '
      STICMassDynTendFile  = ' '
      STICTransCoeffTFile  = ' '
      ICEFRONTlengthFile       = ' '
      ICEFRONTdepthFile        = ' '
      STIClatentHeat       =  334.0 _d 3
      STICHeatCapacity_Cp  = 2000.0 _d 0
      rhoSTIC              =  917.0 _d 0
      STICsaltToHeatRatio  =   5.05 _d -03
      STICheatTransCoeff   =    1.0 _d -04
      STICsaltTransCoeff   = UNSET_RL
C--   Molecular thermal conductivity ice shelf (m^2/s)
      STICkappa            =   1.54 _d -06
      STICthetaSurface     = - 20.0 _d 0
      STICsalinity         = 0.0 _d 0
      no_slip_stic         = no_slip_bottom
      STICDragLinear       = bottomDragLinear
      STICDragQuadratic    = UNSET_RL
      STICselectDragQuadr  = -1
      STICwriteState       = .FALSE.
      STIC_dumpFreq        = dumpFreq
      STIC_taveFreq        = taveFreq
      STICadvDiffHeatFlux  = .FALSE.
      STICuseGammaFrict    = .FALSE.
      STIC_oldCalcUStar    = .FALSE.
      STICremeshFrequency  = 0.
      STICsplitThreshold   = hFacMin*1.1 _d 0 + 1. _d 0
      STICmergeThreshold   = hFacMin*0.9 _d 0
C these params. are default of Holland and Jenkins (1999)
      shiCdrag                 = 0.0015 _d 0
      shiZetaN                 = 0.052 _d 0
      shiRc                    = 0.2 _d 0
      shiPrandtl               = 13.8 _d 0
      shiSchmidt               = 2432.0 _d 0
      shiKinVisc               = 1.95 _d -6
#ifdef ALLOW_COST
      mult_stic                = 0. _d 0
      mult_shifwflx            = 0. _d 0
      wshifwflx0               = 0. _d 0
      shifwflx_errfile         = ' '
#endif
#ifdef ALLOW_MNC
      STIC_tave_mnc = timeave_mnc
      STIC_dump_mnc = snapshot_mnc
#else
      STIC_tave_mnc = .FALSE.
      STIC_dump_mnc = .FALSE.
#endif

C Open and read the data.stic file
      WRITE(msgBuf,'(A)') ' STIC_READPARMS: opening data.stic'
      CALL PRINT_MESSAGE(msgBuf, standardMessageUnit,
     &                   SQUEEZE_RIGHT, myThid )
      CALL OPEN_COPY_DATA_FILE(
     I                   'data.stic', 'STIC_READPARMS',
     O                   iUnit,
     I                   myThid )
      READ(UNIT=iUnit,NML=STIC_PARM01)
      WRITE(msgBuf,'(A)')
     &  ' STIC_READPARMS: finished reading data.stic'
      CALL PRINT_MESSAGE(msgBuf, standardMessageUnit,
     &                   SQUEEZE_RIGHT, myThid )

C Close the open data file
#ifdef SINGLE_DISK_IO
      CLOSE(iUnit)
#else
      CLOSE(iUnit,STATUS='DELETE')
#endif /* SINGLE_DISK_IO */

C Now set-up any remaining parameters that result from the input parameters
      IF ( STICsaltTransCoeff .EQ. UNSET_RL )
     &     STICsaltTransCoeff =
     &     STICsaltToHeatRatio * STICheatTransCoeff

C New calcUstar expression not available with STICboundaryLayer:
      IF ( STICboundaryLayer ) STIC_oldCalcUStar = .TRUE.
C  specific options within STICboundaryLayer:
      SHI_withBL_realFWflux = SHI_withBL_realFWflux .AND.
     &    STICboundaryLayer .AND. useRealFreshWaterFlux
      SHI_withBL_uStarTopDz = SHI_withBL_uStarTopDz .AND.
     &    STICboundaryLayer .AND. STICuseGammaFrict

C Set quadratic bottom drag depending on choices:
      IF ( STICDragQuadratic .EQ. UNSET_RL) THEN
       IF ( STICuseGammaFrict ) THEN
        STICDragQuadratic = shiCdrag
       ELSE
        STICDragQuadratic = bottomDragQuadratic
       ENDIF
      ENDIF
      IF ( STICDragQuadratic.EQ.0. _d 0 ) THEN
        STICselectDragQuadr = -1
      ELSEIF ( STICselectDragQuadr.EQ.-1 ) THEN
        STICselectDragQuadr = MAX( 0, selectBotDragQuadr )
      ENDIF

C-    Set Output type flags :
      STIC_tave_mdsio = .TRUE.
      STIC_dump_mdsio = .TRUE.
#ifdef ALLOW_MNC
      IF (useMNC) THEN
        IF ( .NOT.outputTypesInclusive
     &       .AND. STIC_tave_mnc ) STIC_tave_mdsio = .FALSE.
        IF ( .NOT.outputTypesInclusive
     &       .AND. STIC_dump_mnc ) STIC_dump_mdsio = .FALSE.
      ENDIF
#endif

      _END_MASTER(myThid)
C Everyone else must wait for the parameters to be loaded
      _BARRIER

#endif /* ALLOW_STEEP_ICECAVITY */

      RETURN
      END

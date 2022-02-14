#include "SHELFICE_OPTIONS.h"

CBOP
C !ROUTINE: SHELFICE_READPARMS

C !INTERFACE: ==========================================================
      SUBROUTINE SHELFICE_READPARMS( myThid )

C !DESCRIPTION:
C     Initialize SHELFICE parameters, read in data.shelfice

C !USES: ===============================================================
      IMPLICIT NONE
#include "SIZE.h"
#include "EEPARAMS.h"
#include "PARAMS.h"
#include "SHELFICE.h"
#ifdef ALLOW_COST
# include "SHELFICE_COST.h"
#endif /* ALLOW_COST */
#ifdef ALLOW_MNC
# include "MNC_PARAMS.h"
#endif

C !INPUT PARAMETERS: ===================================================
C  myThid               :: thread number
      INTEGER myThid

C !OUTPUT PARAMETERS: ==================================================
C  none

#ifdef ALLOW_SHELFICE

C !LOCAL VARIABLES: ====================================================
C  iUnit                :: unit number for I/O
C  msgBuf               :: message buffer
      INTEGER iUnit
      CHARACTER*(MAX_LEN_MBUF) msgBuf
CEOP

      NAMELIST /SHELFICE_PARM01/
     &     SHELFICEsaltToHeatRatio,
     &     SHELFICEheatTransCoeff,
     &     SHELFICEsaltTransCoeff,
     &     SHELFICEMassStepping,
     &     rhoShelfice, SHELFICEkappa,
     &     SHELFICElatentHeat, SHELFICEHeatCapacity_Cp,
     &     no_slip_shelfice, SHELFICEDragLinear,
     &     SHELFICEDragQuadratic, SHELFICEselectDragQuadr,
     &     SHELFICEthetaSurface,
     &     useISOMIPTD,
     &     SHELFICEconserve, SHELFICEboundaryLayer,
     &     SHI_withBL_realFWflux, SHI_withBL_uStarTopDz,
     &     SHELFICEwriteState,
     &     SHELFICE_dumpFreq,
     &     SHELFICE_taveFreq,
     &     SHELFICE_tave_mnc,
     &     SHELFICE_dump_mnc,
     &     SHELFICEtopoFile,
     &     SHELFICEmassFile, SHELFICEloadAnomalyFile,
     &     SHELFICEMassDynTendFile, SHELFICETransCoeffTFile,
     &     SHELFICEDynMassOnly,
     &     SHELFICEadvDiffHeatFlux,
     &     SHELFICEuseGammaFrict, SHELFICE_oldCalcUStar,
     &     shiCdrag, shiZetaN, shiRc,
     &     shiPrandtl, shiSchmidt, shiKinVisc,
#ifdef ALLOW_COST
     &     mult_shelfice,
     &     mult_shifwflx, wshifwflx0, shifwflx_errfile,
#endif
     &     SHELFICEremeshFrequency,
     &     SHELFICEsplitThreshold, SHELFICEmergeThreshold

C---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|

      IF ( .NOT.useShelfIce ) THEN
C-    pkg SHELFICE is not used
        _BEGIN_MASTER(myThid)
C-    Track pkg activation status:
         SHELFICEisOn = .FALSE.
C     print a (weak) warning if data.shelfice is found
         CALL PACKAGES_UNUSED_MSG( 'useShelfIce', ' ', ' ' )
        _END_MASTER(myThid)
        RETURN
      ENDIF

      _BEGIN_MASTER(myThid)

C This routine has been called by the main model so we set our
C internal flag to indicate we are in business
      SHELFICEisOn = .TRUE.

C Set defaults values for parameters in SHELFICE.h
      useISOMIPTD              = .FALSE.
      SHELFICEconserve         = .FALSE.
      SHELFICEboundaryLayer    = .FALSE.
      SHI_withBL_realFWflux    = .FALSE.
      SHI_withBL_uStarTopDz    = .FALSE.
      SHELFICEMassStepping     = .FALSE.
      SHELFICEDynMassOnly      = .FALSE.
      SHELFICEtopoFile         = ' '
      SHELFICEmassFile         = ' '
      SHELFICEloadAnomalyFile  = ' '
      SHELFICEMassDynTendFile  = ' '
      SHELFICETransCoeffTFile  = ' '
      SHELFICElatentHeat       =  334.0 _d 3
      SHELFICEHeatCapacity_Cp  = 2000.0 _d 0
      rhoShelfIce              =  917.0 _d 0
      SHELFICEsaltToHeatRatio  =   5.05 _d -03
      SHELFICEheatTransCoeff   =    1.0 _d -04
      SHELFICEsaltTransCoeff   = UNSET_RL
      SHELFICEkappa            =   1.54 _d -06
      SHELFICEthetaSurface     = - 20.0 _d 0
      no_slip_shelfice         = no_slip_bottom
      SHELFICEDragLinear       = bottomDragLinear
      SHELFICEDragQuadratic    = UNSET_RL
      SHELFICEselectDragQuadr  = -1
      SHELFICEwriteState       = .FALSE.
      SHELFICE_dumpFreq        = dumpFreq
      SHELFICE_taveFreq        = taveFreq
      SHELFICEadvDiffHeatFlux  = .FALSE.
      SHELFICEuseGammaFrict    = .FALSE.
      SHELFICE_oldCalcUStar    = .FALSE.
      SHELFICEremeshFrequency  = 0.
      SHELFICEsplitThreshold   = hFacMin*1.1 _d 0 + 1. _d 0
      SHELFICEmergeThreshold   = hFacMin*0.9 _d 0
C these params. are default of Holland and Jenkins (1999)
      shiCdrag                 = 0.0015 _d 0
      shiZetaN                 = 0.052 _d 0
      shiRc                    = 0.2 _d 0
      shiPrandtl               = 13.8 _d 0
      shiSchmidt               = 2432.0 _d 0
      shiKinVisc               = 1.95 _d -6
#ifdef ALLOW_COST
      mult_shelfice            = 0. _d 0
      mult_shifwflx            = 0. _d 0
      wshifwflx0               = 0. _d 0
      shifwflx_errfile         = ' '
#endif
#ifdef ALLOW_MNC
      SHELFICE_tave_mnc = timeave_mnc
      SHELFICE_dump_mnc = snapshot_mnc
#else
      SHELFICE_tave_mnc = .FALSE.
      SHELFICE_dump_mnc = .FALSE.
#endif

C Open and read the data.shelfice file
      WRITE(msgBuf,'(A)') ' SHELFICE_READPARMS: opening data.shelfice'
      CALL PRINT_MESSAGE(msgBuf, standardMessageUnit,
     &                   SQUEEZE_RIGHT, myThid )
      CALL OPEN_COPY_DATA_FILE(
     I                   'data.shelfice', 'SHELFICE_READPARMS',
     O                   iUnit,
     I                   myThid )
      READ(UNIT=iUnit,NML=SHELFICE_PARM01)
      WRITE(msgBuf,'(A)')
     &  ' SHELFICE_READPARMS: finished reading data.shelfice'
      CALL PRINT_MESSAGE(msgBuf, standardMessageUnit,
     &                   SQUEEZE_RIGHT, myThid )

C Close the open data file
#ifdef SINGLE_DISK_IO
      CLOSE(iUnit)
#else
      CLOSE(iUnit,STATUS='DELETE')
#endif /* SINGLE_DISK_IO */

C Now set-up any remaining parameters that result from the input parameters
      IF ( SHELFICEsaltTransCoeff .EQ. UNSET_RL )
     &     SHELFICEsaltTransCoeff =
     &     SHELFICEsaltToHeatRatio * SHELFICEheatTransCoeff

C New calcUstar expression not available with SHELFICEboundaryLayer:
      IF ( SHELFICEboundaryLayer ) SHELFICE_oldCalcUStar = .TRUE.
C  specific options within SHELFICEboundaryLayer:
      SHI_withBL_realFWflux = SHI_withBL_realFWflux .AND.
     &    SHELFICEboundaryLayer .AND. useRealFreshWaterFlux
      SHI_withBL_uStarTopDz = SHI_withBL_uStarTopDz .AND.
     &    SHELFICEboundaryLayer .AND. SHELFICEuseGammaFrict

C Set quadratic bottom drag depending on choices:
      IF ( SHELFICEDragQuadratic .EQ. UNSET_RL) THEN
       IF ( SHELFICEuseGammaFrict ) THEN
        SHELFICEDragQuadratic = shiCdrag
       ELSE
        SHELFICEDragQuadratic = bottomDragQuadratic
       ENDIF
      ENDIF
      IF ( SHELFICEDragQuadratic.EQ.0. _d 0 ) THEN
        SHELFICEselectDragQuadr = -1
      ELSEIF ( SHELFICEselectDragQuadr.EQ.-1 ) THEN
        SHELFICEselectDragQuadr = MAX( 0, selectBotDragQuadr )
      ENDIF

C-    Set Output type flags :
      SHELFICE_tave_mdsio = .TRUE.
      SHELFICE_dump_mdsio = .TRUE.
#ifdef ALLOW_MNC
      IF (useMNC) THEN
        IF ( .NOT.outputTypesInclusive
     &       .AND. SHELFICE_tave_mnc ) SHELFICE_tave_mdsio = .FALSE.
        IF ( .NOT.outputTypesInclusive
     &       .AND. SHELFICE_dump_mnc ) SHELFICE_dump_mdsio = .FALSE.
      ENDIF
#endif

      _END_MASTER(myThid)
C Everyone else must wait for the parameters to be loaded
      _BARRIER

#endif /* ALLOW_SHELFICE */

      RETURN
      END

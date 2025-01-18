#include "PACKAGES_CONFIG.h"
#include "CPP_OPTIONS.h"
#ifdef ALLOW_MOM_COMMON
# include "MOM_COMMON_OPTIONS.h"
#endif
#ifdef ALLOW_AUTODIFF
# include "AUTODIFF_OPTIONS.h"
#endif
#ifdef ALLOW_CTRL
# include "CTRL_OPTIONS.h"
#endif
#ifdef ALLOW_SALT_PLUME
# include "SALT_PLUME_OPTIONS.h"
#endif
#ifdef ALLOW_ECCO
# include "ECCO_OPTIONS.h"
#endif

#ifdef ALLOW_AUTODIFF
# ifdef ALLOW_GGL90
#  include "GGL90_OPTIONS.h"
# endif
# ifdef ALLOW_GMREDI
#  include "GMREDI_OPTIONS.h"
# endif
# ifdef ALLOW_KPP
#  include "KPP_OPTIONS.h"
# endif
# ifdef ALLOW_SEAICE
#  include "SEAICE_OPTIONS.h"
# endif
# ifdef ALLOW_EXF
#  include "EXF_OPTIONS.h"
# endif
#ifdef ALLOW_OBCS
# include "OBCS_OPTIONS.h"
#endif
#endif /* ALLOW_AUTODIFF */

CBOP
C     !ROUTINE: DO_OCEANIC_PHYS
C     !INTERFACE:
      SUBROUTINE DO_OCEANIC_PHYS(myTime, myIter, myThid)
C     !DESCRIPTION: \bv
C     *==========================================================*
C     | SUBROUTINE DO_OCEANIC_PHYS
C     | o Controlling routine for oceanic physics and
C     |   parameterization
C     *==========================================================*
C     | o originally, part of S/R thermodynamics
C     *==========================================================*
C     \ev

C     !CALLING SEQUENCE:
C     DO_OCEANIC_PHYS
C       |
C       |-- OBCS_CALC
C       |
C       |-- OCN_APPLY_IMPORT
C       |
C       |-- FRAZIL_CALC_RHS
C       |
C       |-- THSICE_MAIN
C       |
C       |-- SEAICE_FAKE
C       |-- SEAICE_MODEL
C       |-- SEAICE_COST_SENSI
C       |
C       |-- OCN_EXPORT_DATA
C       |
C       |-- STIC_THERMODYNAMICS
C       |-- SHELFICE_THERMODYNAMICS
C       |
C       |-- ICEFRONT_THERMODYNAMICS
C       |
C       |-- SALT_PLUME_DO_EXCH
C       |
C       |-- FREEZE_SURFACE
C       |
C       |-- EXTERNAL_FORCING_SURF
C       |
C       |-- OBCS_ADJUST
C       |
C       |- k loop (Nr:1):
C       | - DWNSLP_CALC_RHO
C       | - BBL_CALC_RHO
C       | - FIND_RHO_2D @ p(k)
C       | - FIND_RHO_2D @ p(k-1)
C       | - GRAD_SIGMA
C       | - CALC_IVDC
C       | - DIAGS_RHO_L
C       |- end k loop.
C       |
C       |-- CALC_OCE_MXLAYER
C       |
C       |-- SALT_PLUME_CALC_DEPTH
C       |-- SALT_PLUME_VOLFRAC
C       |-- SALT_PLUME_APPLY
C       |-- SALT_PLUME_APPLY
C       |-- SALT_PLUME_FORCING_SURF
C       |
C       |-- KPP_CALC
C       |-- KPP_CALC_DUMMY
C       |
C       |-- PP81_CALC
C       |
C       |-- KL10_CALC
C       |
C       |-- MY82_CALC
C       |
C       |-- GGL90_CALC
C       |
C       |-- TIMEAVE_SURF_FLUX
C       |
C       |-- GMREDI_CALC_TENSOR
C       |-- GMREDI_CALC_TENSOR_DUMMY
C       |
C       |-- DWNSLP_CALC_FLOW
C       |-- DWNSLP_CALC_FLOW
C       |
C       |-- OFFLINE_GET_DIFFUS
C       |
C       |-- BBL_CALC_RHS
C       |
C       |-- MYPACKAGE_CALC_RHS
C       |
C       |-- GMREDI_DO_EXCH
C       |
C       |-- KPP_DO_EXCH
C       |
C       |-- GGL90_EXCHANGES
C       |
C       |-- DIAGS_RHO_G
C       |-- DIAGS_OCEANIC_SURF_FLUX
C       |-- SALT_PLUME_DIAGNOSTICS_FILL
C       |
C       |-- ECCO_PHYS

C     !USES:
      IMPLICIT NONE
C     == Global variables ===
#include "SIZE.h"
#include "EEPARAMS.h"
#include "PARAMS.h"
#include "GRID.h"
#include "DYNVARS.h"
#ifdef ALLOW_TIMEAVE
# include "TIMEAVE_STATV.h"
#endif
#ifdef ALLOW_OFFLINE
# include "OFFLINE_SWITCH.h"
#endif

#ifdef ALLOW_AUTODIFF
# ifdef ALLOW_AUTODIFF_TAMC
#  include "tamc.h"
# endif
# include "FFIELDS.h"
# include "SURFACE.h"
# include "EOS.h"
# ifdef ALLOW_GMREDI
#  include "GMREDI.h"
# endif
# ifdef ALLOW_KPP
#  include "KPP.h"
# endif
# ifdef ALLOW_GGL90
#  include "GGL90.h"
# endif
# ifdef ALLOW_EBM
#  include "EBM.h"
# endif
# ifdef ALLOW_EXF
#  ifdef ALLOW_CTRL
#   include "CTRL_SIZE.h"
#   include "CTRL.h"
#  endif
#  include "EXF_FIELDS.h"
#  ifdef ALLOW_BULKFORMULAE
#   include "EXF_CONSTANTS.h"
#  endif
# endif
# ifdef ALLOW_SEAICE
#  include "SEAICE_SIZE.h"
#  include "SEAICE.h"
#  include "SEAICE_PARAMS.h"
# endif
# ifdef ALLOW_THSICE
#  include "THSICE_VARS.h"
# endif
# ifdef ALLOW_SALT_PLUME
#  include "SALT_PLUME.h"
# endif
# ifdef ALLOW_OBCS
#  include "OBCS_PARAMS.h"
#  include "OBCS_FIELDS.h"
# endif
#endif /* ALLOW_AUTODIFF */

#ifdef ALLOW_TAPENADE
c# ifdef ALLOW_KPP
c#  include "KPP_PARAMS.h"
c# endif
# ifdef ALLOW_SHELFICE
#  include "SHELFICE.h"
# endif
#endif /* ALLOW_TAPENADE */

C     !INPUT/OUTPUT PARAMETERS:
C     == Routine arguments ==
C     myTime :: Current time in simulation
C     myIter :: Current iteration number in simulation
C     myThid :: Thread number for this instance of the routine.
      _RL myTime
      INTEGER myIter
      INTEGER myThid

C     !LOCAL VARIABLES:
C     == Local variables
C     rhoKp1,rhoKm1 :: Density at current level, and @ level minus one
C     iMin, iMax    :: Ranges and sub-block indices on which calculations
C     jMin, jMax       are applied.
C     bi, bj        :: tile indices
C     msgBuf        :: Temp. for building output string
C     i,j,k         :: loop indices
C     kSrf          :: surface index
      _RL rhoKp1  (1-OLx:sNx+OLx,1-OLy:sNy+OLy)
      _RL rhoKm1  (1-OLx:sNx+OLx,1-OLy:sNy+OLy)
      _RL sigmaX  (1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nr)
      _RL sigmaY  (1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nr)
      _RL sigmaR  (1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nr)
      INTEGER iMin, iMax
      INTEGER jMin, jMax
      INTEGER bi, bj
      INTEGER i, j, k, kSrf
      CHARACTER*(MAX_LEN_MBUF) msgBuf
      INTEGER doDiagsRho
      LOGICAL calcGMRedi, calcKPP, calcConvect
#ifdef ALLOW_DIAGNOSTICS
      LOGICAL  DIAGNOSTICS_IS_ON
      EXTERNAL DIAGNOSTICS_IS_ON
#endif /* ALLOW_DIAGNOSTICS */
#ifdef ALLOW_AUTODIFF
      _RL thetaRef
#endif /* ALLOW_AUTODIFF */
#ifdef ALLOW_AUTODIFF_TAMC
C     tkey :: tape key (tile dependent)
      INTEGER tkey
#endif
CEOP

      kSrf = 1
      IF ( usingPCoords ) kSrf = Nr

#ifdef ALLOW_DEBUG
      IF (debugMode) CALL DEBUG_ENTER('DO_OCEANIC_PHYS',myThid)
#endif

      doDiagsRho = 0
#ifdef ALLOW_DIAGNOSTICS
      IF ( useDiagnostics .AND. fluidIsWater ) THEN
        IF ( DIAGNOSTICS_IS_ON('MXLDEPTH',myThid) )
     &       doDiagsRho = doDiagsRho + 1
        IF ( DIAGNOSTICS_IS_ON('DRHODR  ',myThid) )
     &       doDiagsRho = doDiagsRho + 2
        IF ( DIAGNOSTICS_IS_ON('WdRHO_P ',myThid) )
     &       doDiagsRho = doDiagsRho + 4
        IF ( DIAGNOSTICS_IS_ON('WdRHOdP ',myThid) )
     &       doDiagsRho = doDiagsRho + 8
      ENDIF
#endif /* ALLOW_DIAGNOSTICS */

      calcGMRedi  = useGMRedi
      calcKPP     = useKPP
      calcConvect = ivdc_kappa.NE.0.
#ifdef ALLOW_OFFLINE
      IF ( useOffLine ) THEN
        calcGMRedi = useGMRedi .AND. .NOT.offlineLoadGMRedi
        calcKPP    = useKPP    .AND. .NOT.offlineLoadKPP
        calcConvect=calcConvect.AND. .NOT.offlineLoadConvec
      ENDIF
#endif /* ALLOW_OFFLINE */

#ifdef  ALLOW_OBCS
      IF (useOBCS) THEN
C--   Calculate future values on open boundaries
C--   moved before SEAICE_MODEL call since SEAICE_MODEL needs seaice-obcs fields
# ifdef ALLOW_AUTODIFF_TAMC
CADJ STORE theta = comlev1, key=ikey_dynamics, kind=isbyte
CADJ STORE salt  = comlev1, key=ikey_dynamics, kind=isbyte
CADJ STORE etaN  = comlev1, key=ikey_dynamics, kind=isbyte
#  ifdef ALLOW_OBCS_STEVENS
CADJ STORE uVel  = comlev1, key=ikey_dynamics, kind=isbyte
CADJ STORE vVel  = comlev1, key=ikey_dynamics, kind=isbyte
#   ifdef ALLOW_OBCS_EAST
CADJ STORE OBEtStevens = comlev1, key=ikey_dynamics, kind=isbyte
CADJ STORE OBEsStevens = comlev1, key=ikey_dynamics, kind=isbyte
#   endif
#   ifdef ALLOW_OBCS_WEST
CADJ STORE OBWtStevens = comlev1, key=ikey_dynamics, kind=isbyte
CADJ STORE OBWsStevens = comlev1, key=ikey_dynamics, kind=isbyte
#   endif
#   ifdef ALLOW_OBCS_NORTH
CADJ STORE OBNtStevens = comlev1, key=ikey_dynamics, kind=isbyte
CADJ STORE OBNsStevens = comlev1, key=ikey_dynamics, kind=isbyte
#   endif
#   ifdef ALLOW_OBCS_SOUTH
CADJ STORE OBStStevens = comlev1, key=ikey_dynamics, kind=isbyte
CADJ STORE OBSsStevens = comlev1, key=ikey_dynamics, kind=isbyte
#   endif
#  endif /* ALLOW_OBCS_STEVENS */
# endif
# ifdef ALLOW_DEBUG
       IF (debugMode) CALL DEBUG_CALL('OBCS_CALC',myThid)
# endif
       CALL OBCS_CALC( myTime+deltaTClock, myIter+1,
     I                 uVel, vVel, wVel, theta, salt, myThid )
      ENDIF
# if ( defined ALLOW_AUTODIFF_TAMC && defined ALLOW_OBCS_BALANCE )
C     This needs to be done ***after*** the if-block to avoid calling
C     S/R OBCS_CALC in the AD code.
#  ifdef ALLOW_OBCS_NORTH
CADJ STORE OBNv        = comlev1, key=ikey_dynamics, kind=isbyte
#  endif
#  ifdef ALLOW_OBCS_SOUTH
CADJ STORE OBSv        = comlev1, key=ikey_dynamics, kind=isbyte
#  endif
#  ifdef ALLOW_OBCS_EAST
CADJ STORE OBEu        = comlev1, key=ikey_dynamics, kind=isbyte
#  endif
#  ifdef ALLOW_OBCS_WEST
CADJ STORE OBWu        = comlev1, key=ikey_dynamics, kind=isbyte
#  endif
# endif /* ALLOW_AUTODIFF_TAMC and ALLOW_OBCS_BALANCE */
#endif  /* ALLOW_OBCS */

#ifdef ALLOW_OCN_COMPON_INTERF
C--    Apply imported data (from coupled interface) to forcing fields
C jmc: moved here before any freezing/seaice pkg adjustment of surf-fluxes
      IF ( useCoupler ) THEN
         CALL OCN_APPLY_IMPORT( .TRUE., myTime, myIter, myThid )
      ENDIF
#endif /* ALLOW_OCN_COMPON_INTERF */

#ifdef ALLOW_AUTODIFF
      DO bj=myByLo(myThid),myByHi(myThid)
       DO bi=myBxLo(myThid),myBxHi(myThid)
        DO j=1-OLy,sNy+OLy
         DO i=1-OLx,sNx+OLx
          adjustColdSST_diag(i,j,bi,bj) = 0. _d 0
# ifdef ALLOW_SALT_PLUME
          saltPlumeDepth(i,j,bi,bj) = 0. _d 0
          saltPlumeFlux(i,j,bi,bj)  = 0. _d 0
# endif
         ENDDO
        ENDDO
       ENDDO
      ENDDO
#endif /* ALLOW_AUTODIFF */

#ifdef ALLOW_FRAZIL
      IF ( useFRAZIL ) THEN
C--   Freeze water in the ocean interior and let it rise to the surface
CADJ STORE theta = comlev1, key=ikey_dynamics, kind=isbyte
CADJ STORE salt  = comlev1, key=ikey_dynamics, kind=isbyte
       CALL FRAZIL_CALC_RHS( myTime, myIter, myThid )
      ENDIF
#endif /* ALLOW_FRAZIL */

#if (defined ALLOW_THSICE) && !(defined ALLOW_ATM2D)
      IF ( useThSIce .AND. fluidIsWater ) THEN
# ifdef ALLOW_AUTODIFF_TAMC
#  ifdef ALLOW_SEAICE
CADJ STORE uice,vice         = comlev1, key=ikey_dynamics, kind=isbyte
#  endif
CADJ STORE iceMask,iceHeight = comlev1, key=ikey_dynamics, kind=isbyte
CADJ STORE snowHeight, Tsrf  = comlev1, key=ikey_dynamics, kind=isbyte
CADJ STORE Qice1, Qice2      = comlev1, key=ikey_dynamics, kind=isbyte
CADJ STORE sHeating,snowAge  = comlev1, key=ikey_dynamics, kind=isbyte
CADJ STORE hocemxl, icflxsw  = comlev1, key=ikey_dynamics, kind=isbyte
CADJ STORE salt,theta        = comlev1, key=ikey_dynamics, kind=isbyte
CADJ STORE uvel,vvel         = comlev1, key=ikey_dynamics, kind=isbyte
CADJ STORE qnet,qsw, empmr   = comlev1, key=ikey_dynamics, kind=isbyte
CADJ STORE atemp,aqh,precip  = comlev1, key=ikey_dynamics, kind=isbyte
CADJ STORE swdown,lwdown     = comlev1, key=ikey_dynamics, kind=isbyte
#  ifdef NONLIN_FRSURF
CADJ STORE hFac_surfC        = comlev1, key=ikey_dynamics, kind=isbyte
#  endif
# endif /* ALLOW_AUTODIFF_TAMC */
# ifdef ALLOW_DEBUG
        IF (debugMode) CALL DEBUG_CALL('THSICE_MAIN',myThid)
# endif
C--     Step forward Therm.Sea-Ice variables
C       and modify forcing terms including effects from ice
        CALL TIMER_START('THSICE_MAIN     [DO_OCEANIC_PHYS]', myThid)
        CALL THSICE_MAIN( myTime, myIter, myThid )
        CALL TIMER_STOP( 'THSICE_MAIN     [DO_OCEANIC_PHYS]', myThid)
      ENDIF
#endif /* ALLOW_THSICE */

#ifdef ALLOW_SEAICE
# ifdef ALLOW_AUTODIFF_TAMC
CADJ STORE qnet  = comlev1, key=ikey_dynamics, kind=isbyte
CADJ STORE qsw   = comlev1, key=ikey_dynamics, kind=isbyte
CADJ STORE fu,fv = comlev1, key=ikey_dynamics, kind=isbyte
#  if (defined ALLOW_EXF) && (defined ALLOW_ATM_TEMP)
CADJ STORE evap  = comlev1, key=ikey_dynamics, kind=isbyte
#  endif
CADJ STORE etan      = comlev1, key=ikey_dynamics, kind=isbyte
CADJ STORE theta     = comlev1, key=ikey_dynamics, kind=isbyte
CADJ STORE salt      = comlev1, key=ikey_dynamics, kind=isbyte
CADJ STORE uvel,vvel = comlev1, key=ikey_dynamics, kind=isbyte
CADJ STORE phiHydLow = comlev1, key=ikey_dynamics, byte=isbyte
# endif
      IF ( useSEAICE ) THEN
# ifdef ALLOW_AUTODIFF_TAMC
CADJ STORE uvel,vvel         = comlev1, key=ikey_dynamics, kind=isbyte
CADJ STORE uice,vice         = comlev1, key=ikey_dynamics, kind=isbyte
#  ifdef SEAICE_USE_GROWTH_ADX
CADJ STORE tices             = comlev1, key=ikey_dynamics, kind=isbyte
#  endif
#  ifdef ALLOW_EXF
CADJ STORE atemp,aqh,precip  = comlev1, key=ikey_dynamics, kind=isbyte
CADJ STORE swdown,lwdown     = comlev1, key=ikey_dynamics, kind=isbyte
CADJ STORE uwind,vwind       = comlev1, key=ikey_dynamics, kind=isbyte
#  endif
#  ifdef SEAICE_VARIABLE_SALINITY
CADJ STORE hsalt             = comlev1, key=ikey_dynamics, kind=isbyte
#  endif
#  ifdef ATMOSPHERIC_LOADING
CADJ STORE pload, siceload   = comlev1, key=ikey_dynamics, kind=isbyte
#  endif
#  ifdef NONLIN_FRSURF
CADJ STORE recip_hfacc       = comlev1, key=ikey_dynamics, kind=isbyte
#  endif
#  ifdef ALLOW_THSICE
C-- store thSIce vars before advection (called from SEAICE_MODEL) updates them:
CADJ STORE iceMask,iceHeight = comlev1, key=ikey_dynamics, kind=isbyte
CADJ STORE snowHeight,hOceMxL= comlev1, key=ikey_dynamics, kind=isbyte
CADJ STORE Qice1, Qice2      = comlev1, key=ikey_dynamics, kind=isbyte
#  endif /* ALLOW_THSICE */
# endif /* ALLOW_AUTODIFF_TAMC */
# ifdef ALLOW_DEBUG
        IF (debugMode) CALL DEBUG_CALL('SEAICE_MODEL',myThid)
# endif
        CALL TIMER_START('SEAICE_MODEL    [DO_OCEANIC_PHYS]', myThid)
        CALL SEAICE_MODEL( myTime, myIter, myThid )
        CALL TIMER_STOP ('SEAICE_MODEL    [DO_OCEANIC_PHYS]', myThid)
# ifdef ALLOW_AUTODIFF_TAMC
CADJ STORE tices = comlev1, key=ikey_dynamics, kind=isbyte
CADJ STORE heff  = comlev1, key=ikey_dynamics, kind=isbyte
CADJ STORE hsnow = comlev1, key=ikey_dynamics, kind=isbyte
CADJ STORE area  = comlev1, key=ikey_dynamics, kind=isbyte
CADJ STORE uIce  = comlev1, key=ikey_dynamics, kind=isbyte
CADJ STORE vIce  = comlev1, key=ikey_dynamics, kind=isbyte
# endif
# ifdef ALLOW_COST
        CALL SEAICE_COST_SENSI ( myTime, myIter, myThid )
# endif
# ifdef ALLOW_AUTODIFF
      ELSEIF ( SEAICEadjMODE .EQ. -1 ) THEN
#  ifdef ALLOW_AUTODIFF_TAMC
CADJ STORE area = comlev1, key=ikey_dynamics, kind=isbyte
#  endif
        CALL SEAICE_FAKE( myTime, myIter, myThid )
# endif /* ALLOW_AUTODIFF */
      ENDIF
#endif /* ALLOW_SEAICE */

#if (defined ALLOW_OCN_COMPON_INTERF) && (defined ALLOW_THSICE)
C--   After seaice-dyn and advection of pkg/thsice fields,
C     Export ocean coupling fields to coupled interface (only with pkg/thsice)
      IF ( useCoupler ) THEN
# ifdef ALLOW_DEBUG
        IF (debugMode) CALL DEBUG_CALL('OCN_EXPORT_DATA',myThid)
# endif
         CALL TIMER_START('OCN_EXPORT_DATA [DO_OCEANIC_PHYS]', myThid)
         CALL OCN_EXPORT_DATA( myTime, myIter, myThid )
         CALL TIMER_STOP ('OCN_EXPORT_DATA [DO_OCEANIC_PHYS]', myThid)
      ENDIF
#endif /* ALLOW_OCN_COMPON_INTERF & ALLOW_THSICE */

#ifdef ALLOW_AUTODIFF_TAMC
CADJ STORE sst, sss          = comlev1, key=ikey_dynamics, kind=isbyte
CADJ STORE qsw               = comlev1, key=ikey_dynamics, kind=isbyte
# ifdef ALLOW_SEAICE
CADJ STORE area              = comlev1, key=ikey_dynamics, kind=isbyte
# endif
#endif

#ifdef ALLOW_SHELFICE
      IF ( useShelfIce .AND. fluidIsWater ) THEN
#ifdef ALLOW_AUTODIFF_TAMC
CADJ STORE salt, theta       = comlev1, key=ikey_dynamics, kind=isbyte
CADJ STORE uvel, vvel        = comlev1, key=ikey_dynamics, kind=isbyte
#endif
#ifdef ALLOW_STEEP_ICECAVITY
       IF ( useSTIC ) THEN
# ifdef ALLOW_DEBUG
        IF (debugMode) CALL DEBUG_CALL('STIC_THERMODYNAMICS',myThid)
# endif
C     use stic_thermodynamics that includes icefront melt processes
        CALL TIMER_START('STIC_THERMODYNAMICS [DO_OCEANIC_PHYS]',myThid)
        CALL STIC_THERMODYNAMICS( myTime, myIter, myThid )
        CALL TIMER_STOP ('STIC_THERMODYNAMICS [DO_OCEANIC_PHYS]',myThid)
       ELSE
#else
       IF ( .TRUE. ) THEN
#endif
#ifdef ALLOW_DEBUG
        IF (debugMode) CALL DEBUG_CALL('SHELFICE_THERMODYNAMICS',myThid)
#endif
C     compute temperature and (virtual) salt flux at the
C     shelf-ice ocean interface
        CALL TIMER_START('SHELFICE_THERMODYNAMICS [DO_OCEANIC_PHYS]',
     &       myThid)
        CALL SHELFICE_THERMODYNAMICS( myTime, myIter, myThid )
        CALL TIMER_STOP ('SHELFICE_THERMODYNAMICS [DO_OCEANIC_PHYS]',
     &       myThid)
       ENDIF
      ENDIF
#endif /* ALLOW_SHELFICE */

#ifdef ALLOW_ICEFRONT
      IF ( useICEFRONT .AND. fluidIsWater ) THEN
#ifdef ALLOW_DEBUG
       IF (debugMode) CALL DEBUG_CALL('ICEFRONT_THERMODYNAMICS',myThid)
#endif
C     compute temperature and (virtual) salt flux at the
C     ice-front ocean interface
       CALL TIMER_START('ICEFRONT_THERMODYNAMICS [DO_OCEANIC_PHYS]',
     &       myThid)
       CALL ICEFRONT_THERMODYNAMICS( myTime, myIter, myThid )
       CALL TIMER_STOP( 'ICEFRONT_THERMODYNAMICS [DO_OCEANIC_PHYS]',
     &      myThid)
      ENDIF
#endif /* ALLOW_ICEFRONT */

#ifdef ALLOW_SALT_PLUME
      IF ( useSALT_PLUME ) THEN
Catn: exchanging saltPlumeFlux:
        CALL SALT_PLUME_DO_EXCH( myTime, myIter, myThid )
      ENDIF
#endif /* ALLOW_SALT_PLUME */

C--   Freeze water at the surface
      IF ( allowFreezing ) THEN
#ifdef ALLOW_AUTODIFF_TAMC
CADJ STORE theta             = comlev1, key=ikey_dynamics, kind=isbyte
#endif
        CALL FREEZE_SURFACE( myTime, myIter, myThid )
      ENDIF

      iMin = 1-OLx
      iMax = sNx+OLx
      jMin = 1-OLy
      jMax = sNy+OLy

C---  Determines forcing terms based on external fields
C     relaxation terms, etc.
#ifdef ALLOW_AUTODIFF
# ifdef ALLOW_AUTODIFF_TAMC
CADJ STORE salt, theta       = comlev1, key=ikey_dynamics, kind=isbyte
# endif
#else  /* ALLOW_AUTODIFF */
C--   if fluid is not water, by-pass surfaceForcing, find_rho, gmredi
C     and all vertical mixing schemes, but keep OBCS_CALC
      IF ( fluidIsWater ) THEN
#endif /* ALLOW_AUTODIFF */
#ifdef ALLOW_DEBUG
      IF (debugMode) CALL DEBUG_CALL('EXTERNAL_FORCING_SURF',myThid)
#endif
        CALL EXTERNAL_FORCING_SURF(
     I             iMin, iMax, jMin, jMax,
     I             myTime, myIter, myThid )
#ifdef ALLOW_AUTODIFF_TAMC
C     Avoid calling S/R EXTERNAL_FORCING_SURF in AD routine.
CADJ STORE EmPmR             = comlev1, key=ikey_dynamics, kind=isbyte
CADJ STORE uvel, vvel        = comlev1, key=ikey_dynamics, kind=isbyte
#endif

#ifdef  ALLOW_OBCS
      IF (useOBCS) THEN
C--   After all surface fluxes are known apply balancing fluxes and
C--   apply tidal forcing to open boundaries
# ifdef ALLOW_DEBUG
       IF (debugMode) CALL DEBUG_CALL('OBCS_ADJUST',myThid)
# endif
       CALL OBCS_ADJUST(
     I      myTime+deltaTClock, myIter+1, myThid )
      ENDIF
#endif  /* ALLOW_OBCS */

#ifdef ALLOW_AUTODIFF_TAMC
C--   HPF directive to help TAF
CHPF$ INDEPENDENT
#endif /* ALLOW_AUTODIFF_TAMC */
      DO bj=myByLo(myThid),myByHi(myThid)
#ifdef ALLOW_AUTODIFF_TAMC
C--   HPF directive to help TAF
CHPF$ INDEPENDENT
#endif /* ALLOW_AUTODIFF_TAMC */
       DO bi=myBxLo(myThid),myBxHi(myThid)

#ifdef ALLOW_AUTODIFF_TAMC
        tkey = bi + (bj-1)*nSx + (ikey_dynamics-1)*nSx*nSy
#endif /* ALLOW_AUTODIFF_TAMC */

C--   Set up work arrays with valid (i.e. not NaN) values
C     These inital values do not alter the numerical results. They
C     just ensure that all memory references are to valid floating
C     point numbers. This prevents spurious hardware signals due to
C     uninitialised but inert locations.
        DO k=1,Nr
         DO j=1-OLy,sNy+OLy
          DO i=1-OLx,sNx+OLx
C This is currently used by GMRedi, IVDC, MXL-depth  and Diagnostics
           sigmaX(i,j,k) = 0. _d 0
           sigmaY(i,j,k) = 0. _d 0
           sigmaR(i,j,k) = 0. _d 0
          ENDDO
         ENDDO
        ENDDO

        DO j=1-OLy,sNy+OLy
         DO i=1-OLx,sNx+OLx
          rhoKm1 (i,j)   = 0. _d 0
          rhoKp1 (i,j)   = 0. _d 0
         ENDDO
        ENDDO
#ifdef ALLOW_AUTODIFF
cph all the following init. are necessary for TAF
cph although some of these are re-initialised later.
        DO k=1,Nr
         DO j=1-OLy,sNy+OLy
          DO i=1-OLx,sNx+OLx
           rhoInSitu(i,j,k,bi,bj) = 0.
# ifdef ALLOW_GGL90
           GGL90viscArU(i,j,k,bi,bj)  = 0. _d 0
           GGL90viscArV(i,j,k,bi,bj)  = 0. _d 0
           GGL90diffKr(i,j,k,bi,bj)  = 0. _d 0
# endif /* ALLOW_GGL90 */
# ifdef ALLOW_SALT_PLUME
#  ifdef SALT_PLUME_VOLUME
           SPforcingS(i,j,k,bi,bj) = 0. _d 0
           SPforcingT(i,j,k,bi,bj) = 0. _d 0
#  endif
# endif /* ALLOW_SALT_PLUME */
          ENDDO
         ENDDO
        ENDDO
#ifdef ALLOW_OFFLINE
       IF ( calcConvect ) THEN
#endif
        DO k=1,Nr
         DO j=1-OLy,sNy+OLy
          DO i=1-OLx,sNx+OLx
           IVDConvCount(i,j,k,bi,bj) = 0.
          ENDDO
         ENDDO
        ENDDO
#ifdef ALLOW_OFFLINE
       ENDIF
       IF ( calcGMRedi ) THEN
#endif
# ifdef ALLOW_GMREDI
        DO k=1,Nr
         DO j=1-OLy,sNy+OLy
          DO i=1-OLx,sNx+OLx
           Kwx(i,j,k,bi,bj)  = 0. _d 0
           Kwy(i,j,k,bi,bj)  = 0. _d 0
           Kwz(i,j,k,bi,bj)  = 0. _d 0
           Kux(i,j,k,bi,bj)  = 0. _d 0
           Kvy(i,j,k,bi,bj)  = 0. _d 0
#  ifdef GM_EXTRA_DIAGONAL
           Kuz(i,j,k,bi,bj)  = 0. _d 0
           Kvz(i,j,k,bi,bj)  = 0. _d 0
#  endif
#  ifdef GM_BOLUS_ADVEC
           GM_PsiX(i,j,k,bi,bj)  = 0. _d 0
           GM_PsiY(i,j,k,bi,bj)  = 0. _d 0
#  endif
#  ifdef GM_VISBECK_VARIABLE_K
           VisbeckK(i,j,bi,bj)   = 0. _d 0
#  endif
          ENDDO
         ENDDO
        ENDDO
# endif /* ALLOW_GMREDI */
#ifdef ALLOW_OFFLINE
       ENDIF
       IF ( calcKPP ) THEN
#endif
# ifdef ALLOW_KPP
        DO k=1,Nr
         DO j=1-OLy,sNy+OLy
          DO i=1-OLx,sNx+OLx
           KPPdiffKzS(i,j,k,bi,bj)  = 0. _d 0
           KPPdiffKzT(i,j,k,bi,bj)  = 0. _d 0
          ENDDO
         ENDDO
        ENDDO
# endif /* ALLOW_KPP */
#ifdef ALLOW_OFFLINE
       ENDIF
#endif
#endif /* ALLOW_AUTODIFF */

#ifdef ALLOW_AUTODIFF_TAMC
CADJ STORE theta(:,:,:,bi,bj)     = comlev1_bibj, key=tkey, kind=isbyte
CADJ STORE salt (:,:,:,bi,bj)     = comlev1_bibj, key=tkey, kind=isbyte
CADJ STORE totphihyd(:,:,:,bi,bj) = comlev1_bibj, key=tkey, kind=isbyte
# ifdef ALLOW_KPP
CADJ STORE uvel (:,:,:,bi,bj)     = comlev1_bibj, key=tkey, kind=isbyte
CADJ STORE vvel (:,:,:,bi,bj)     = comlev1_bibj, key=tkey, kind=isbyte
# endif
#endif /* ALLOW_AUTODIFF_TAMC */

C--   Always compute density (stored in common block) here; even when it is not
C     needed here, will be used anyway in calc_phi_hyd (data flow easier this way)
#ifdef ALLOW_DEBUG
        IF (debugMode) CALL DEBUG_CALL('FIND_RHO_2D (xNr)',myThid)
#endif
#ifdef ALLOW_AUTODIFF
        IF ( fluidIsWater ) THEN
#endif /* ALLOW_AUTODIFF */
#ifdef ALLOW_DOWN_SLOPE
         IF ( useDOWN_SLOPE ) THEN
           DO k=1,Nr
            CALL DWNSLP_CALC_RHO(
     I                  theta, salt,
     O                  rhoInSitu(1-OLx,1-OLy,k,bi,bj),
     I                  k, bi, bj, myTime, myIter, myThid )
           ENDDO
         ENDIF
#endif /* ALLOW_DOWN_SLOPE */
#ifdef ALLOW_BBL
         IF ( useBBL ) THEN
C     pkg/bbl requires in-situ bbl density for depths equal to and deeper
C     than the bbl. To reduce computation and storage requirement,
C     these densities are stored in the dry grid boxes of rhoInSitu.
C     See BBL_CALC_RHO for details.
           DO k=Nr,1,-1
            CALL BBL_CALC_RHO(
     I                  theta, salt,
     O                  rhoInSitu,
     I                  k, bi, bj, myTime, myIter, myThid )

           ENDDO
         ENDIF
#endif /* ALLOW_BBL */
         IF ( .NOT. ( useDOWN_SLOPE .OR. useBBL ) ) THEN
           DO k=1,Nr
            CALL FIND_RHO_2D(
     I                iMin, iMax, jMin, jMax, k,
     I                theta(1-OLx,1-OLy,k,bi,bj),
     I                salt (1-OLx,1-OLy,k,bi,bj),
     O                rhoInSitu(1-OLx,1-OLy,k,bi,bj),
     I                k, bi, bj, myThid )
           ENDDO
         ENDIF
#ifdef ALLOW_AUTODIFF
        ELSE
C-        fluid is not water:
          DO k=1,Nr
           IF ( select_rStar.GE.1 .OR. selectSigmaCoord.GE.1 ) THEN
C-    isothermal (theta=const) reference state
             thetaRef = thetaConst
           ELSE
C-    horizontally uniform (tRef) reference state
             thetaRef = tRef(k)
           ENDIF
           DO j=1-OLy,sNy+OLy
            DO i=1-OLx,sNx+OLx
             rhoInSitu(i,j,k,bi,bj) =
     &         ( theta(i,j,k,bi,bj)
     &              *( salt(i,j,k,bi,bj)*atm_Rq + oneRL )
     &         - thetaRef )*maskC(i,j,k,bi,bj)
            ENDDO
           ENDDO
          ENDDO
        ENDIF
# ifdef ALLOW_AUTODIFF_TAMC
CADJ STORE rhoInSitu(:,:,:,bi,bj) = comlev1_bibj, key=tkey, kind=isbyte
# endif
#endif /* ALLOW_AUTODIFF */

#ifdef ALLOW_DEBUG
        IF (debugMode) THEN
          WRITE(msgBuf,'(A,2(I4,A))')
     &         'ENTERING UPWARD K LOOP (bi=', bi, ', bj=', bj,')'
          CALL DEBUG_MSG(msgBuf(1:43),myThid)
        ENDIF
#endif

C--     Start of diagnostic loop
        DO k=Nr,1,-1

C--       Calculate gradients of potential density for isoneutral
C         slope terms (e.g. GM/Redi tensor or IVDC diffusivity)
          IF ( calcGMRedi .OR. (k.GT.1 .AND. calcConvect)
     &         .OR. usePP81 .OR. useKL10
     &         .OR. useMY82 .OR. useGGL90
     &         .OR. useSALT_PLUME .OR. doDiagsRho.GE.1 ) THEN
            IF (k.GT.1) THEN
             IF ( usingZCoords ) THEN
              DO j=jMin,jMax
               DO i=iMin,iMax
                rhoKp1(i,j) = rhoInSitu(i,j,k,bi,bj)
               ENDDO
              ENDDO
              CALL FIND_RHO_2D(
     I                 iMin, iMax, jMin, jMax, k,
     I                 theta(1-OLx,1-OLy,k-1,bi,bj),
     I                 salt (1-OLx,1-OLy,k-1,bi,bj),
     O                 rhoKm1,
     I                 k-1, bi, bj, myThid )
             ELSE
              CALL FIND_RHO_2D(
     I                 iMin, iMax, jMin, jMax, k-1,
     I                 theta(1-OLx,1-OLy,k,bi,bj),
     I                 salt (1-OLx,1-OLy,k,bi,bj),
     O                 rhoKp1,
     I                 k, bi, bj, myThid )
              DO j=jMin,jMax
               DO i=iMin,iMax
                rhoKm1(i,j) = rhoInSitu(i,j,k-1,bi,bj)
               ENDDO
              ENDDO
             ENDIF
            ENDIF
#ifdef ALLOW_DEBUG
            IF (debugMode) CALL DEBUG_CALL('GRAD_SIGMA',myThid)
#endif
            CALL GRAD_SIGMA(
     I             bi, bj, iMin, iMax, jMin, jMax, k,
     I             rhoInSitu(1-OLx,1-OLy,k,bi,bj), rhoKm1, rhoKp1,
     O             sigmaX, sigmaY, sigmaR,
     I             myThid )

#ifdef ALLOW_LEITH_QG
            DO j=jMin,jMax
             DO i=iMin,iMax
              sigmaRfield(i,j,k,bi,bj)=sigmaR(i,j,k)
             ENDDO
            ENDDO
#endif /* ALLOW_LEITH_QG */

#ifdef ALLOW_AUTODIFF
#ifdef GMREDI_WITH_STABLE_ADJOINT
cgf zero out adjoint fields to stabilize pkg/gmredi adjoint
cgf -> cuts adjoint dependency from slope to state
            CALL ZERO_ADJ_LOC( Nr, sigmaX, myThid)
            CALL ZERO_ADJ_LOC( Nr, sigmaY, myThid)
            CALL ZERO_ADJ_LOC( Nr, sigmaR, myThid)
#endif
#endif /* ALLOW_AUTODIFF */
          ENDIF

C--       Implicit Vertical Diffusion for Convection
          IF (k.GT.1 .AND. calcConvect) THEN
#ifdef ALLOW_DEBUG
            IF (debugMode) CALL DEBUG_CALL('CALC_IVDC',myThid)
#endif
            CALL CALC_IVDC(
     I        bi, bj, iMin, iMax, jMin, jMax, k,
     I        sigmaR,
     I        myTime, myIter, myThid)
          ENDIF

#ifdef ALLOW_DIAGNOSTICS
          IF ( doDiagsRho.GE.4 ) THEN
            CALL DIAGS_RHO_L( doDiagsRho, k, bi, bj,
     I                        rhoInSitu(1-OLx,1-OLy,1,bi,bj),
     I                        rhoKm1, wVel,
     I                        myTime, myIter, myThid )
          ENDIF
#endif

C--     end of diagnostic k loop (Nr:1)
        ENDDO

#ifdef ALLOW_AUTODIFF_TAMC
C     Avoid recomputing sigmaR and IVDConvCount in AD routine.
CADJ STORE sigmaR                   =comlev1_bibj,key=tkey,kind=isbyte
CADJ STORE IVDConvCount(:,:,:,bi,bj)=comlev1_bibj,key=tkey,kind=isbyte
#endif

C--     Diagnose Mixed Layer Depth:
        IF ( calcGMRedi .OR. MOD(doDiagsRho,2).EQ.1 ) THEN
          CALL CALC_OCE_MXLAYER(
     I              rhoInSitu(1-OLx,1-OLy,kSrf,bi,bj), sigmaR,
     I              bi, bj, myTime, myIter, myThid )
        ENDIF

#ifdef ALLOW_SALT_PLUME
        IF ( useSALT_PLUME ) THEN
          CALL SALT_PLUME_CALC_DEPTH(
     I              rhoInSitu(1-OLx,1-OLy,kSrf,bi,bj), sigmaR,
     I              bi, bj, myTime, myIter, myThid )
#ifdef SALT_PLUME_VOLUME
          CALL SALT_PLUME_VOLFRAC(
     I              bi, bj, myTime, myIter, myThid )
C-- get forcings for kpp
          CALL SALT_PLUME_APPLY(
     I              1, bi, bj, recip_hFacC(1-OLx,1-OLy,kSrf,bi,bj),
     I              theta, 0,
     I              myTime, myIter, myThid )
          CALL SALT_PLUME_APPLY(
     I              2, bi, bj, recip_hFacC(1-OLx,1-OLy,kSrf,bi,bj),
     I              salt, 0,
     I              myTime, myIter, myThid )
C-- need to call this S/R from here to apply just before kpp
          CALL SALT_PLUME_FORCING_SURF(
     I              bi, bj, iMin, iMax, jMin, jMax,
     I              myTime, myIter, myThid )
#endif /* SALT_PLUME_VOLUME */
        ENDIF
# ifdef ALLOW_AUTODIFF_TAMC
CADJ STORE saltplumedepth(:,:,bi,bj)= comlev1_bibj,key=tkey,kind=isbyte
CADJ STORE saltplumeflux(:,:,bi,bj) = comlev1_bibj,key=tkey,kind=isbyte
# endif /* ALLOW_AUTODIFF_TAMC */
#endif /* ALLOW_SALT_PLUME */

#ifdef ALLOW_DIAGNOSTICS
        IF ( MOD(doDiagsRho,4).GE.2 ) THEN
          CALL DIAGNOSTICS_FILL (sigmaR, 'DRHODR  ', 0, Nr,
     &         2, bi, bj, myThid)
        ENDIF
#endif /* ALLOW_DIAGNOSTICS */

C--    This is where EXTERNAL_FORCING_SURF(bi,bj) used to be called;
C      now called earlier, before bi,bj loop.

#ifdef ALLOW_AUTODIFF_TAMC
cph needed for KPP
CADJ STORE surfaceForcingU(:,:,bi,bj)=comlev1_bibj,key=tkey,kind=isbyte
CADJ STORE surfaceForcingV(:,:,bi,bj)=comlev1_bibj,key=tkey,kind=isbyte
CADJ STORE surfaceForcingS(:,:,bi,bj)=comlev1_bibj,key=tkey,kind=isbyte
CADJ STORE surfaceForcingT(:,:,bi,bj)=comlev1_bibj,key=tkey,kind=isbyte
#endif /* ALLOW_AUTODIFF_TAMC */

#ifdef  ALLOW_KPP
C--     Compute KPP mixing coefficients
        IF ( calcKPP ) THEN
#ifdef ALLOW_DEBUG
          IF (debugMode) CALL DEBUG_CALL('KPP_CALC',myThid)
#endif
          CALL TIMER_START('KPP_CALC [DO_OCEANIC_PHYS]', myThid)
          CALL KPP_CALC(
     I                  bi, bj, myTime, myIter, myThid )
          CALL TIMER_STOP ('KPP_CALC [DO_OCEANIC_PHYS]', myThid)
#if (defined ALLOW_AUTODIFF) && !(defined ALLOW_OFFLINE)
        ELSE
          CALL KPP_CALC_DUMMY(
     I                  bi, bj, myTime, myIter, myThid )
#endif /* ALLOW_AUTODIFF and not ALLOW_OFFLINE */
        ENDIF
#endif  /* ALLOW_KPP */

#ifdef  ALLOW_PP81
C--     Compute PP81 mixing coefficients
        IF (usePP81) THEN
#ifdef ALLOW_DEBUG
          IF (debugMode) CALL DEBUG_CALL('PP81_CALC',myThid)
#endif
          CALL PP81_CALC(
     I                     bi, bj, sigmaR, myTime, myIter, myThid )
        ENDIF
#endif /* ALLOW_PP81 */

#ifdef  ALLOW_KL10
C--     Compute KL10 mixing coefficients
        IF (useKL10) THEN
#ifdef ALLOW_DEBUG
          IF (debugMode) CALL DEBUG_CALL('KL10_CALC',myThid)
#endif
          CALL KL10_CALC(
     I                     bi, bj, sigmaR, myTime, myIter, myThid )
        ENDIF
#endif /* ALLOW_KL10 */

#ifdef  ALLOW_MY82
C--     Compute MY82 mixing coefficients
        IF (useMY82) THEN
#ifdef ALLOW_DEBUG
          IF (debugMode) CALL DEBUG_CALL('MY82_CALC',myThid)
#endif
          CALL MY82_CALC(
     I                     bi, bj, sigmaR, myTime, myIter, myThid )
        ENDIF
#endif /* ALLOW_MY82 */

#ifdef  ALLOW_GGL90
#ifdef ALLOW_AUTODIFF_TAMC
CADJ STORE GGL90TKE(:,:,:,bi,bj) = comlev1_bibj, key=tkey, kind=isbyte
#endif /* ALLOW_AUTODIFF_TAMC */
C--     Compute GGL90 mixing coefficients
        IF ( useGGL90 .AND. Nr.GT.1 ) THEN
#ifdef ALLOW_DEBUG
          IF (debugMode) CALL DEBUG_CALL('GGL90_CALC',myThid)
#endif
          CALL TIMER_START('GGL90_CALC [DO_OCEANIC_PHYS]', myThid)
          CALL GGL90_CALC(
     I                     bi, bj, sigmaR, myTime, myIter, myThid )
          CALL TIMER_STOP ('GGL90_CALC [DO_OCEANIC_PHYS]', myThid)
        ENDIF
#endif /* ALLOW_GGL90 */

#ifdef ALLOW_TIMEAVE
        IF ( taveFreq.GT. 0. _d 0 ) THEN
          CALL TIMEAVE_SURF_FLUX( bi, bj, myTime, myIter, myThid)
        ENDIF
        IF ( taveFreq.GT.0. .AND. calcConvect ) THEN
          CALL TIMEAVE_CUMULATE(ConvectCountTave, IVDConvCount,
     I                           Nr, deltaTClock, bi, bj, myThid)
        ENDIF
#endif /* ALLOW_TIMEAVE */

#ifdef ALLOW_GMREDI
#ifdef ALLOW_AUTODIFF_TAMC
# ifndef GM_EXCLUDE_CLIPPING
cph storing here is needed only for one GMREDI_OPTIONS:
cph define GM_BOLUS_ADVEC
cph keep it although TAF says you dont need to.
cph but I have avoided the #ifdef for now, in case more things change
CADJ STORE sigmaX(:,:,:) = comlev1_bibj, key = tkey, kind = isbyte
CADJ STORE sigmaY(:,:,:) = comlev1_bibj, key = tkey, kind = isbyte
CADJ STORE sigmaR(:,:,:) = comlev1_bibj, key = tkey, kind = isbyte
# endif
#endif /* ALLOW_AUTODIFF_TAMC */

C--     Calculate iso-neutral slopes for the GM/Redi parameterisation
        IF ( calcGMRedi ) THEN
#ifdef ALLOW_DEBUG
          IF (debugMode) CALL DEBUG_CALL('GMREDI_CALC_TENSOR',myThid)
#endif
          CALL GMREDI_CALC_TENSOR(
     I             iMin, iMax, jMin, jMax,
     I             sigmaX, sigmaY, sigmaR,
     I             bi, bj, myTime, myIter, myThid )
#if (defined ALLOW_AUTODIFF) && !(defined ALLOW_OFFLINE)
        ELSE
          CALL GMREDI_CALC_TENSOR_DUMMY(
     I             iMin, iMax, jMin, jMax,
     I             sigmaX, sigmaY, sigmaR,
     I             bi, bj, myTime, myIter, myThid )
#endif /* ALLOW_AUTODIFF and not ALLOW_OFFLINE */
        ENDIF
#endif /* ALLOW_GMREDI */

#ifdef ALLOW_DOWN_SLOPE
        IF ( useDOWN_SLOPE ) THEN
C--     Calculate Downsloping Flow for Down_Slope parameterization
         IF ( usingPCoords ) THEN
          CALL DWNSLP_CALC_FLOW(
     I                bi, bj, kSurfC, rhoInSitu,
     I                myTime, myIter, myThid )
         ELSE
          CALL DWNSLP_CALC_FLOW(
     I                bi, bj, kLowC, rhoInSitu,
     I                myTime, myIter, myThid )
         ENDIF
        ENDIF
#endif /* ALLOW_DOWN_SLOPE */

C--   end bi,bj loops.
       ENDDO
      ENDDO

#ifndef ALLOW_AUTODIFF
C---  if fluid Is Water: end
      ENDIF
#endif

#ifdef ALLOW_OFFLINE
      IF ( useOffLine ) THEN
#ifdef ALLOW_DEBUG
        IF (debugMode) CALL DEBUG_CALL('OFFLINE_GET_DIFFUS',myThid)
#endif /* ALLOW_DEBUG */
        CALL OFFLINE_GET_DIFFUS( myTime, myIter, myThid )
      ENDIF
#endif /* ALLOW_OFFLINE */

#ifdef ALLOW_BBL
      IF ( useBBL ) THEN
       CALL BBL_CALC_RHS(
     I                          myTime, myIter, myThid )
      ENDIF
#endif /* ALLOW_BBL */

#ifdef ALLOW_MYPACKAGE
      IF ( useMYPACKAGE ) THEN
       CALL MYPACKAGE_CALC_RHS(
     I                          myTime, myIter, myThid )
      ENDIF
#endif /* ALLOW_MYPACKAGE */

#ifdef ALLOW_GMREDI
      IF ( calcGMRedi ) THEN
        CALL GMREDI_DO_EXCH( myTime, myIter, myThid )
      ENDIF
#endif /* ALLOW_GMREDI */

#ifdef ALLOW_KPP
      IF ( calcKPP ) THEN
        CALL KPP_DO_EXCH( myThid )
      ENDIF
#endif /* ALLOW_KPP */

#ifdef ALLOW_GGL90
      IF ( useGGL90 )
     &  CALL GGL90_EXCHANGES( myThid )
#endif /* ALLOW_GGL90 */

#ifdef ALLOW_DIAGNOSTICS
      IF ( fluidIsWater .AND. useDiagnostics ) THEN
        CALL DIAGS_RHO_G(
     I                    rhoInSitu, uVel, vVel, wVel,
     I                    myTime, myIter, myThid )
      ENDIF
      IF ( useDiagnostics ) THEN
        CALL DIAGS_OCEANIC_SURF_FLUX( myTime, myIter, myThid )
      ENDIF
      IF ( calcConvect .AND. useDiagnostics ) THEN
        CALL DIAGNOSTICS_FILL( IVDConvCount, 'CONVADJ ',
     &                               0, Nr, 0, 1, 1, myThid )
      ENDIF
#ifdef ALLOW_SALT_PLUME
      IF ( useDiagnostics )
     &      CALL SALT_PLUME_DIAGNOSTICS_FILL(bi,bj,myThid)
#endif
#endif

#ifdef ALLOW_DEBUG
      IF (debugMode) CALL DEBUG_LEAVE('DO_OCEANIC_PHYS',myThid)
#endif

      RETURN
      END

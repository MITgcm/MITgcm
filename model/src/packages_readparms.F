#include "PACKAGES_CONFIG.h"
#include "CPP_OPTIONS.h"

CStartOfInterface
CBOP
C     !ROUTINE: PACKAGES_READPARMS
C     !INTERFACE:
      SUBROUTINE PACKAGES_READPARMS(myThid)

C     !DESCRIPTION: \bv
C     *==========================================================*
C     | SUBROUTINE PACKAGES_READPARMS
C     | o Read runtime package configuration parameters
C     *==========================================================*
C     |   Packages can have internal runtime configuration
C     |   parameters. A package provides a routine
C     |   ${PKGNAME}_READ_PARMS to read these parameters. In
C     |   general this routine should read parameters from a file
C     |   called data.${pkgname}.
C     |   This routine (S/R PACKAGES_READPARMS) calls per-package
C     |   parameter reading routines.
C     *==========================================================*
C     \ev

C     !CALLING SEQUENCE:
C     PACKAGES_READPARMS
C       |
C       |-- MNC_READPARMS
C       |
C       |-- CAL_READPARMS
C       |
C       |-- EXF_READPARMS
C       |
C       |-- OBCS_READPARMS
C       |
C       |-- SHAP_FILT_READPARMS
C       |
C       |-- ZONAL_FILT_READPARMS
C       |
C       |-- OPPS_READPARMS
C       |
C       |-- PP81_READPARMS
C       |
C       |-- KL10_READPARMS
C       |
C       |-- MY82_READPARMS
C       |
C       |-- GGL90_READPARMS
C       |
C       |-- KPP_READPARMS
C       |
C       |-- GMREDI_READPARMS
C       |
C       |-- BBL_READPARMS
C       |
C       |-- DWNSLP_READPARMS
C       |
C       |-- BULKF_READPARMS
C       |
C       |-- EBM_READPARMS
C       |
C       |-- CHEAPAML_READPARMS
C       |
C       |-- FLT_READPARMS
C       |
C       |-- LONGSTEP_READPARMS
C       |-- PTRACERS_READPARMS
C       |
C       |-- GCHEM_READPARMS
C       |
C       |-- RBCS_READPARMS
C       |
C       |-- OFFLINE_READPARMS
C       |
C       |-- MATRIX_READPARMS
C       |
C       |-- STREAMICE_READPARMS
C       |
C       |-- SHELFICE_READPARMS
C       |
C       |-- STIC_READPARMS
C       |
C       |-- ICEFRONT_READPARMS
C       |
C       |-- SEAICE_READPARMS
C       |
C       |-- SALT_PLUME_READPARMS
C       |
C       |-- THSICE_READPARMS
C       |
C       |-- LAND_READPARMS
C       |
C       |-- AUTODIFF_READPARMS
C       |
C       |-- OPTIM_READPARMS
C       |
C       |-- CTRL_READPARMS
C       |
C       |-- COST_READPARMS
C       |
C       |-- GRDCHK_READPARMS
C       |
C       |-- SMOOTH_READPARMS
C       |
C       |-- ECCO_READPARMS
C       |
C       |-- PROFILES_READPARMS
C       |
C       |-- OBSFIT_READPARMS
C       |
C       |-- SBO_READPARMS
C       |
C       |-- ATM_PHYS_READPARMS
C       |
C       |-- FIZHI_READPARMS
C       |
C       |-- DIAGNOSTICS_READPARMS
C       |
C       |-- REGRID_READPARMS
C       |
C       |-- LAYERS_READPARMS
C       |
C       |-- NEST_CHILD_READPARMS
C       |-- NEST_PARENT_READPARMS
C       |
C       |-- NEST2W_C_READPARMS
C       |-- NEST2W_P_READPARMS
C       |
C       |-- CPL_READPARMS
C       |
C       |-- OASIS_READPARMS
C       |
C       |-- RUNCLOCK_READPARMS
C       |
C       |-- MYPACKAGE_READPARMS

C     !USES:
      IMPLICIT NONE
C     == Global variables ==
#include "SIZE.h"
#include "EEPARAMS.h"
#include "PARAMS.h"

C     == Routine arguments ==
      INTEGER myThid
CEOP

#ifdef ALLOW_MNC
C--   if useMNC=T, read run-time flags for MNC; otherwise just return
      CALL MNC_READPARMS( myThid )
#endif

#ifdef ALLOW_CAL
C--   if useCAL=T, set the calendar package parameters.
C      otherwise, just set cal_setStatus=-1 and return
      CALL CAL_READPARMS( myThid )
#endif

#ifdef ALLOW_EXF
C--   if useEXF=T, set EXF parameters; otherwise just return
      CALL EXF_READPARMS  ( myThid )
#endif

#ifdef ALLOW_OBCS
C--   if useOBCS=T, set OBCS parameters; otherwise just return
      CALL OBCS_READPARMS( myThid )
#endif

#ifdef ALLOW_SHAP_FILT
C--   if useSHAP_FILT=T, set Shapiro-Filter parameters; otherwise just return
      CALL SHAP_FILT_READPARMS( myThid )
#endif

#ifdef ALLOW_ZONAL_FILT
C--   if useZONAL_FILT=T, set Zonal-Filter parameters; otherwise just return
      CALL ZONAL_FILT_READPARMS( myThid )
#endif

#ifdef ALLOW_OPPS
C--   if useOPPS=T, set OPPS parameters; otherwise just return
      CALL OPPS_READPARMS( myThid )
#endif

#ifdef ALLOW_PP81
C--   if usePP81=T, set PP81 parameters; otherwise just return
      CALL PP81_READPARMS( myThid )
#endif

#ifdef ALLOW_KL10
C--   if useKL10=T, set KL10 parameters; otherwise just return
      CALL KL10_READPARMS( myThid )
#endif

#ifdef ALLOW_MY82
C--   if useMY82=T, set MY82 parameters; otherwise just return
      CALL MY82_READPARMS( myThid )
#endif

#ifdef ALLOW_GGL90
C--   if useGGL90=T, set GGL90 parameters; otherwise just return
      CALL GGL90_READPARMS( myThid )
#endif

#ifdef ALLOW_KPP
C--   if useKPP=T, set KPP parameters; otherwise just return
      CALL KPP_READPARMS( myThid )
#endif

#ifdef ALLOW_GMREDI
C--   if useGMRedi=T, set GM/Redi parameters; otherwise just return
      CALL GMREDI_READPARMS( myThid )
#endif

#ifdef ALLOW_BBL
C--   if useBBL=T, set BBL parameters; otherwise just return
      CALL BBL_READPARMS( myThid )
#endif

#ifdef ALLOW_DOWN_SLOPE
C--   if useDOWN_SLOPE=T, set Down-Slope flow parameters; otherwise just return
      CALL DWNSLP_READPARMS( myThid )
#endif

#ifdef ALLOW_BULK_FORCE
C--   if useBulkForce=T, set BULKF parameters; otherwise just return
      CALL BULKF_READPARMS( myThid )
#endif

#ifdef ALLOW_EBM
C--   if useEBM=T, set EBM parameters; otherwise just return
      CALL EBM_READPARMS( myThid )
#endif

#ifdef ALLOW_CHEAPAML
C--   if useCheapAML=T, set CheapAML parameters; otherwise just return
      CALL CHEAPAML_READPARMS( myThid )
#endif

#ifdef ALLOW_FLT
C--   if useFLT=T, set Floats (FLT) parameters; otherwise just return
      CALL FLT_READPARMS( myThid )
#endif /* ALLOW_FLT */

#ifdef ALLOW_LONGSTEP
C--   if usePTRACERS=T, set Long-Step parameters; otherwise just return
C- note : need to call LONGSTEP_READPARMS before PTRACERS_READPARMS
      CALL LONGSTEP_READPARMS( myThid )
#endif
#ifdef ALLOW_PTRACERS
C--   if usePTRACERS=T, set Passive-Traces parameters; otherwise just return
      CALL PTRACERS_READPARMS( myThid )
#endif

#ifdef ALLOW_GCHEM
C--   if useGCHEM=T, set GCHEM parameters; otherwise just return
      CALL GCHEM_READPARMS( myThid )
#endif

#ifdef ALLOW_RBCS
C--   if useRBCS=T, set RBCS parameters; otherwise just return
      CALL RBCS_READPARMS( myThid )
#endif

#ifdef ALLOW_OFFLINE
C--   if useOffLine=T, set OFFLINE parameters; otherwise just return
      CALL OFFLINE_READPARMS( myThid )
#endif

#ifdef ALLOW_MATRIX
C--   if useMATRIX=T, set MATRIX parameters; otherwise just return
      CALL MATRIX_READPARMS ( myThid )
#endif

#ifdef ALLOW_STREAMICE
C--   if useStreamIce=T, set STREAMICE parameters; otherwise just return
      CALL STREAMICE_READPARMS( myThid )
#endif

#ifdef ALLOW_SHELFICE
C--   if useShelfIce=T, set SHELFICE parameters; otherwise just return
      CALL SHELFICE_READPARMS( myThid )
#endif

#ifdef ALLOW_STEEP_ICECAVITY
C--   if useSTIC=T, set STEEP_ICECAVITY parameters; otherwise just return
      CALL STIC_READPARMS( myThid )
#endif

#ifdef ALLOW_ICEFRONT
C--   if useICEFRONT=T, set ICEFRONT parameters; otherwise just return
      CALL ICEFRONT_READPARMS( myThid )
#endif

#ifdef ALLOW_SEAICE
C--   if useSEAICE=T, set SEAICE parameters; otherwise just return
      CALL SEAICE_READPARMS( myThid )
#endif

#ifdef ALLOW_SALT_PLUME
C--   if useSALT_PLUME=T, set SALT_PLUME parameters; otherwise just return
      CALL SALT_PLUME_READPARMS( myThid )
#endif

#ifdef ALLOW_THSICE
C--   if useThSIce=T, set Therm.Sea-Ice parameters; otherwise just return
      CALL THSICE_READPARMS( myThid )
#endif

#ifdef ALLOW_LAND
C--   if useLand=T, set LAND pkg parameters; otherwise just return
      CALL LAND_READPARMS( myThid )
#endif

#ifdef ALLOW_AUTODIFF
C--   Initialise autodiff parameters
      CALL AUTODIFF_READPARMS( myThid )
#endif

#ifdef ALLOW_CTRL
C--   Initialise the optim. parameters.
      CALL OPTIM_READPARMS( myThid )
C--   Initialise the control parameters
      CALL CTRL_READPARMS( myThid )
#endif

#ifdef ALLOW_COST
C--   Initialise the cost parameters
      CALL COST_READPARMS( myThid )
#endif

#ifdef ALLOW_GRDCHK
C--   if useGrdChk=T, set gradient-check parameters; otherwise just return
      CALL GRDCHK_READPARMS( myThid )
#endif

#ifdef ALLOW_SMOOTH
C--   if useSMOOTH=T, set SMOOTH parameters; otherwise just return
      CALL SMOOTH_READPARMS ( myThid )
#endif

#ifdef ALLOW_ECCO
c--   Initialize ECCO package parameters
      CALL ECCO_READPARMS ( myThid )
#endif

#ifdef ALLOW_PROFILES
C--   if usePROFILES=T, set PROFILES parameters; otherwise just return
      CALL PROFILES_READPARMS ( myThid )
#endif

#ifdef ALLOW_OBSFIT
C--   if useOBSFIT=T, set OBSFIT parameters; otherwise just return
      CALL OBSFIT_READPARMS ( myThid )
#endif

#ifdef ALLOW_SBO
C--   if useSBO=T, set SBO parameters; otherwise just return
      CALL SBO_READPARMS( myThid )
#endif

#ifdef ALLOW_ATM_PHYS
C--   if useAtm_Phys=T, set Atm_Phys parameters; otherwise just return
      CALL ATM_PHYS_READPARMS( myThid )
#endif

#ifdef ALLOW_FIZHI
C--   if useFizhi=T, set fizhi parameters; otherwise just return
      CALL FIZHI_READPARMS( myThid )
#endif /* ALLOW_FIZHI */

#ifdef ALLOW_DIAGNOSTICS
C--   if useDiagnostics=T, set diagnostics choices
C      otherwise, just set pkgStatus=-1 and return
      CALL DIAGNOSTICS_READPARMS( myThid )
#endif /* ALLOW_DIAGNOSTICS */

#ifdef ALLOW_REGRID
C--   if useREGRID=T, set REGRID parameters; otherwise just return
      CALL REGRID_READPARMS( myThid )
#endif /* ALLOW_REGRID */

#ifdef ALLOW_LAYERS
C--   if useLayers=T, set layers parameters; otherwise just return
      CALL LAYERS_READPARMS( myThid )
#endif /* ALLOW_LAYERS */

#ifdef ALLOW_NEST_CHILD
C--    Initialize nest(ing) package parameters x CHILD
       IF (useNEST_CHILD) CALL NEST_CHILD_READPARMS ( myThid )
#endif /* ALLOW_NEST_CHILD */
C
#ifdef ALLOW_NEST_PARENT
C--    Initialize nest(ing) package parameters x PARENT
       IF (useNEST_PARENT) CALL NEST_PARENT_READPARMS ( myThid )
#endif /* ALLOW_NEST_PARENT */

#ifdef ALLOW_NEST2W_CHILD
C--    Initialize Child 2-way nest(ing) package parameters
       IF (useNest2W_child) CALL NEST2W_C_READPARMS( myThid )
#endif /* ALLOW_NEST2W_CHILD */
C
#ifdef ALLOW_NEST2W_PARENT
C--    Initialize Parent 2-way nest(ing) package parameters
       IF (useNest2W_parent) CALL NEST2W_P_READPARMS( myThid )
#endif /* ALLOW_NEST2W_PARENT */

#ifdef COMPONENT_MODULE
C--   set Coupling parameters
      IF ( useCoupler ) CALL CPL_READPARMS( myThid )
#endif /* COMPONENT_MODULE */

#ifdef ALLOW_OASIS
       IF (useOASIS) CALL OASIS_READPARMS ( myThid )
#endif

#ifdef ALLOW_RUNCLOCK
C--   if useRunClock=T, set RUNCLOCK parameters; otherwise just return
      CALL RUNCLOCK_READPARMS( myThid )
#endif

#ifdef ALLOW_MYPACKAGE
C--   if useMYPACKAGE=T, set mypackage parameters; otherwise just return
      CALL MYPACKAGE_READPARMS( myThid )
#endif

      RETURN
      END

C $Header: /u/gcmpack/MITgcm/model/src/packages_readparms.F,v 1.68 2011/08/07 07:08:15 dimitri Exp $
C $Name:  $

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
C       |-- RUNCLOCK_READPARMS
C       |
C       |-- CAL_READPARMS
C       |
C       |-- EXF_READPARMS
C       |
C       |-- GMREDI_READPARMS
C       |
C       |-- DWNSLP_READPARMS
C       |
C       |-- BBL_READPARMS
C       |
C       |-- KPP_READPARMS
C       |
C       |-- PP81_READPARMS
C       |
C       |-- MY82_READPARMS
C       |
C       |-- GGL90_READPARMS
C       |
C       |-- OPPS_READPARMS
C       |
C       |-- SBO_READPARMS
C       |
C       |-- SEAICE_READPARMS
C       |
C       |-- SHELFICE_READPARMS
C       |
C       |-- ICEFRONT_READPARMS
C       |
C       |-- SHAP_FILT_READPARMS
C       |
C       |-- ZONAL_FILT_READPARMS
C       |
C       |-- OBCS_READPARMS
C       |
C       |-- OFFLINE_READPARMS
C       |
C       |-- BULKF_READPARMS
C       |
C       |-- THSICE_READPARMS
C       |
C       |-- LONGSTEP_READPARMS
C       |-- PTRACERS_READPARMS
C       |
C       |-- GCHEM_READPARMS
C       |
C       |-- RBCS_READPARMS
C       |
C       |-- SALT_PLUME_READPARMS
C       |
C       |-- CHEAPAML_READPARMS
C       |
C       |-- OPTIM_READPARMS
C       |
C       |-- CTRL_READPARMS
C       |
C       |-- COST_READPARMS
C       |
C       |-- GRDCHK_READPARMS
C       |
C       |-- EBM_READPARMS
C       |
C       |-- SMOOTH_READPARMS
C       |
C       |-- ECCO_READPARMS
C       |
C       |-- PROFILES_READPARMS
C       |
C       |-- FLT_READPARMS
C       |
C       |-- NEST_CHILD_READPARMS
C       |-- NEST_PARENT_READPARMS
C       |
C       |-- CPL_READPARMS
C       |
C       |-- FIZHI_READPARMS
C       |
C       |-- DIAGNOSTICS_READPARMS
C       |
C       |-- MATRIX_READPARMS
C       |
C       |-- REGRID_READPARMS
C       |
C       |-- LAYERS_READPARMS
C       |
C       |-- OASIS_READPARMS
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
C     Read run-time flags for MNC
      IF (useMNC) THEN
        CALL MNC_READPARMS( myThid )
      ENDIF
#endif

#ifdef ALLOW_RUNCLOCK
C--   Initialize RUNCLOCK parameters
      IF (useRunClock) CALL RUNCLOCK_READPARMS( myThid )
#endif

#ifdef ALLOW_CAL
C--   Initialise the calendar package.
      IF ( useCAL ) CALL CAL_READPARMS( myThid )
#endif

#ifdef ALLOW_EXF
C--   Custom routine to set forcing fields.
      IF (useEXF) CALL EXF_READPARMS  ( myThid )
#endif

#ifdef ALLOW_GMREDI
C--   Initialize GM/Redi parameters
      IF (useGMRedi) CALL GMREDI_READPARMS( myThid )
#endif

#ifdef ALLOW_DOWN_SLOPE
C--   Initialize Down-Slope flow parameters
      IF (useDOWN_SLOPE) CALL DWNSLP_READPARMS( myThid )
#endif

#ifdef ALLOW_BBL
C--   Initialize BBL parameters
      IF (useBBL) CALL BBL_READPARMS( myThid )
#endif

#ifdef ALLOW_KPP
C--   Initialize KPP parameters
      IF (useKPP) CALL KPP_READPARMS( myThid )
#endif

#ifdef ALLOW_PP81
C--   Initialize PP81 parameters
      IF (usePP81) CALL PP81_READPARMS( myThid )
#endif

#ifdef ALLOW_MY82
C--   Initialize MY82 parameters
      IF (useMY82) CALL MY82_READPARMS( myThid )
#endif

#ifdef ALLOW_GGL90
C--   Initialize GGL90 parameters
      IF (useGGL90) CALL GGL90_READPARMS( myThid )
#endif

#ifdef ALLOW_OPPS
C--   Initialize OPPS parameters
      IF (useOPPS) CALL OPPS_READPARMS( myThid )
#endif

#ifdef ALLOW_SBO
C--   Initialize SBO parameters
      IF (useSBO) CALL SBO_READPARMS( myThid )
#endif

#ifdef ALLOW_SEAICE
C--   Initialize SEAICE parameters
      IF (useSEAICE) CALL SEAICE_READPARMS( myThid )
#endif

#ifdef ALLOW_SHELFICE
C--   Initialize SHELFICE parameters
      IF (useShelfIce) CALL SHELFICE_READPARMS( myThid )
#endif

#ifdef ALLOW_ICEFRONT
C--   Initialize ICEFRONT parameters
      IF (useICEFRONT) CALL ICEFRONT_READPARMS( myThid )
#endif

#ifdef ALLOW_SHAP_FILT
      IF (useSHAP_FILT) CALL SHAP_FILT_READPARMS( myThid )
#endif

#ifdef ALLOW_ZONAL_FILT
      IF (useZONAL_FILT) CALL ZONAL_FILT_READPARMS( myThid )
#endif

#ifdef ALLOW_OBCS
      IF (useOBCS) CALL OBCS_READPARMS( myThid )
#endif

#ifdef ALLOW_OFFLINE
      IF (useOffLine) CALL OFFLINE_READPARMS( myThid )
#endif

#ifdef ALLOW_BULK_FORCE
C -- initialize Bulkf parameters
      IF (useBulkForce) CALL BULKF_READPARMS( myThid )
#endif

#ifdef ALLOW_THSICE
C -- initialize Therm.Sea-Ice parameters
      IF (useThSIce) CALL THSICE_READPARMS( myThid )
#endif

#ifdef ALLOW_LONGSTEP
C-- note: need to call LONGSTEP_READPARMS before PTRACERS_READPARMS
      IF (usePTRACERS) CALL LONGSTEP_READPARMS( myThid )
#endif
#ifdef ALLOW_PTRACERS
      IF (usePTRACERS) CALL PTRACERS_READPARMS( myThid )
#endif

#ifdef ALLOW_GCHEM
      IF (useGCHEM) CALL GCHEM_READPARMS( myThid )
#endif

#ifdef ALLOW_RBCS
      IF (useRBCS) CALL RBCS_READPARMS( myThid )
#endif

#ifdef ALLOW_SALT_PLUME
      IF (useSALT_PLUME) CALL SALT_PLUME_READPARMS( myThid )
#endif

#ifdef ALLOW_CHEAPAML
C -- initialize cheapaml parameters
      IF (useCheapAML) CALL CHEAPAML_READPARMS( myThid )
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
C--   Initialise gradient check parameters
      if (useGrdchk) CALL GRDCHK_READPARMS( myThid )
#endif

#ifdef ALLOW_EBM
      IF (useEBM) CALL EBM_READPARMS( myThid )
#endif

#ifdef ALLOW_SMOOTH
c--   Initialize SMOOTH package parameters
      IF ( useSMOOTH ) CALL SMOOTH_READPARMS ( myThid )
#endif

#ifdef ALLOW_ECCO
c--   Initialize ECCO package parameters
      CALL ECCO_READPARMS ( myThid )
#endif

#ifdef ALLOW_PROFILES
c--   Initialize PROFILES package parameters
      CALL PROFILES_READPARMS ( myThid )
#endif

#ifdef ALLOW_FLT
      IF ( useFLT ) CALL FLT_READPARMS( myThid )
#endif /* ALLOW_FLT */

#ifdef ALLOW_NEST_CHILD
C--    Initialize nest(ing) package parameters x CHILD
       IF (useNEST_CHILD) CALL NEST_CHILD_READPARMS ( myThid )
#endif /* ALLOW_NEST_CHILD */
C
#ifdef ALLOW_NEST_PARENT
C--    Initialize nest(ing) package parameters x PARENT
       IF (useNEST_PARENT) CALL NEST_PARENT_READPARMS ( myThid )
#endif /* ALLOW_NEST_PARENT */

#ifdef COMPONENT_MODULE
C--   set Coupling parameters
      IF ( useCoupler ) CALL CPL_READPARMS( myThid )
#endif /* COMPONENT_MODULE */

#ifdef ALLOW_FIZHI
C--   set model date and time and other fizhi parameters
      IF ( useFizhi ) CALL FIZHI_READPARMS( myThid )
#endif /* ALLOW_FIZHI */

#ifdef ALLOW_DIAGNOSTICS
C--   set diagnostics choices
      IF ( useDiagnostics ) CALL DIAGNOSTICS_READPARMS( myThid )
#endif /* ALLOW_DIAGNOSTICS */

#ifdef ALLOW_MATRIX
C--    Initialize matrix package parameters
       IF (useMATRIX) CALL MATRIX_READPARMS ( myThid )
#endif

#ifdef ALLOW_REGRID
      IF ( useREGRID ) CALL REGRID_READPARMS( myThid )
#endif /* ALLOW_REGRID */

#ifdef ALLOW_LAYERS
      IF ( useLayers ) CALL LAYERS_READPARMS( myThid )
#endif /* ALLOW_LAYERS */

#ifdef ALLOW_OASIS
       IF (useOASIS) CALL OASIS_READPARMS ( myThid )
#endif

#ifdef ALLOW_MYPACKAGE
      IF (useMYPACKAGE) CALL MYPACKAGE_READPARMS( myThid )
#endif

      RETURN
      END

C $Header: /u/gcmpack/MITgcm/pkg/seaice/seaice_model.F,v 1.44 2007/05/01 13:42:15 jmc Exp $
C $Name:  $

#include "SEAICE_OPTIONS.h"
 
CBOP
C !ROUTINE: SEAICE_MODEL

C !INTERFACE: ==========================================================
      SUBROUTINE SEAICE_MODEL( myTime, myIter, myThid )

C !DESCRIPTION: \bv
C     /===========================================================\
C     | SUBROUTINE SEAICE_MODEL                                   |
C     | o Time stepping of a dynamic/thermodynamic sea ice model. |
C     |  Dynamics solver: Zhang/Hibler, JGR, 102, 8691-8702, 1997 |
C     |  Thermodynamics:        Hibler, MWR, 108, 1943-1973, 1980 |
C     |  Rheology:              Hibler, JPO,   9,  815- 846, 1979 |
C     |  Snow:          Zhang et al.  , JPO,  28,  191- 217, 1998 |
C     |  Parallel forward ice model written by Jinlun Zhang PSC/UW|
C     |  & coupled into MITgcm by Dimitris Menemenlis (JPL) 2/2001|
C     |  zhang@apl.washington.edu / menemenlis@jpl.nasa.gov       |
C     |===========================================================|
C     \===========================================================/
      IMPLICIT NONE
c \ev
 
C !USES: ===============================================================
C     === Global variables ===
#include "SIZE.h"
#include "EEPARAMS.h"
#include "DYNVARS.h"
#include "PARAMS.h"
#include "GRID.h"
#include "FFIELDS.h"
#include "SEAICE.h"
#include "SEAICE_PARAMS.h"

#ifdef SEAICE_EXTERNAL_FORCING
# include "SEAICE_FFIELDS.h"
#endif

#ifdef ALLOW_AUTODIFF_TAMC
# include "tamc.h"
#endif

C !INPUT PARAMETERS: ===================================================
C     === Routine arguments ===
C     myTime - Simulation time
C     myIter - Simulation timestep number
C     myThid - Thread no. that called this routine.
      _RL     myTime
      INTEGER myIter
      INTEGER myThid
CEndOfInterface

C !LOCAL VARIABLES: ====================================================
C     === Local variables ===
C     i,j,bi,bj - Loop counters
      INTEGER i, j, bi, bj
      LOGICAL  DIFFERENT_MULTIPLE
      EXTERNAL DIFFERENT_MULTIPLE
CEOP

#ifdef ALLOW_DEBUG
      IF ( debugLevel .GE. debLevB )
     &   CALL DEBUG_ENTER( 'SEAICE_MODEL', myThid )
#endif

#ifdef SEAICE_EXTERNAL_FORCING
C--   Atmospheric state and runoff are from
C     pkg/exf, which does not update edges.
      CALL EXCH_UV_XY_RL(uwind,vwind,.TRUE.,myThid)
#ifdef ALLOW_ATM_TEMP
      _EXCH_XY_R8( atemp,  myThid )
      _EXCH_XY_R8( aqh,    myThid )
      _EXCH_XY_R8( precip, myThid )
      _EXCH_XY_R8( evap,   myThid )
#else /* ALLOW_ATM_TEMP */
      STOP 'ABNORMAL END: S/R SEAICE_MODEL: ATM_TEMP undef'
#endif /* ALLOW_ATM_TEMP */
#ifdef ALLOW_DOWNWARD_RADIATION
      _EXCH_XY_R8( lwdown, myThid )
      _EXCH_XY_R8( swdown, mythid )
#else / ALLOW_DOWNWARD_RADIATION */
      STOP 'ABNORMAL END: S/R SEAICE_MODEL: DOWNWARD_RADIATION undef'
#endif / ALLOW_DOWNWARD_RADIATION */
      _EXCH_XY_R8( runoff, myThid )
#else /* SEAICE_EXTERNAL_FORCING */
C--   Load atmospheric state and runoff.
      CALL SEAICE_GET_FORCING ( myTime, myIter, myThid )
#endif /* SEAICE_EXTERNAL_FORCING */

#ifdef ALLOW_THSICE
      IF ( useThSice ) THEN
C--   Map thSice-variables to HEFF and AREA
       CALL SEAICE_MAP_THSICE( myTime, myIter, myThid )
      ENDIF
#endif /* ALLOW_THSICE */

#ifdef ALLOW_AUTODIFF_TAMC
CADJ STORE uwind = comlev1, key = ikey_dynamics
CADJ STORE vwind = comlev1, key = ikey_dynamics
CADJ STORE heff  = comlev1, key = ikey_dynamics
CADJ STORE heffm = comlev1, key = ikey_dynamics
CADJ STORE area  = comlev1, key = ikey_dynamics
# ifdef SEAICE_ALLOW_DYNAMICS
#  ifdef SEAICE_CGRID
cphCADJ STORE uice  = comlev1, key = ikey_dynamics
cphCADJ STORE vice  = comlev1, key = ikey_dynamics
#  endif
# endif
#endif /* ALLOW_AUTODIFF_TAMC */

C solve ice momentum equations and calculate ocean surface stress
      IF ( 
     &  DIFFERENT_MULTIPLE(SEAICE_deltaTdyn,myTime,SEAICE_deltaTtherm)
     &   ) THEN
#ifdef ALLOW_DEBUG
         IF ( debugLevel .GE. debLevB )
     &    CALL DEBUG_CALL( 'SEAICE_DYNSOLVER', myThid )
#endif
#ifdef SEAICE_CGRID
         CALL TIMER_START('SEAICE_DYNSOLVER   [SEAICE_MODEL]',myThid)
         CALL SEAICE_DYNSOLVER ( myTime, myIter, myThid )
         CALL TIMER_STOP ('SEAICE_DYNSOLVER   [SEAICE_MODEL]',myThid)
#else
         CALL TIMER_START('DYNSOLVER          [SEAICE_MODEL]',myThid)
         CALL DYNSOLVER ( myTime, myIter, myThid )
         CALL TIMER_STOP ('DYNSOLVER          [SEAICE_MODEL]',myThid)
#endif /* SEAICE_CGRID */
      ENDIF

#ifdef ALLOW_THSICE
      IF ( .NOT.useThSice ) THEN
#endif
C--   Only call advection of heff, area, and snow and 
C--   growth for the generic 0-layer thermodynamics of seaice
C--   if useThSice=.false., otherwise the 3-layer Winton thermodynamics
C--   (called from DO_OCEANIC_PHYSICS) take care of this

C NOW DO ADVECTION and DIFFUSION
      IF ( SEAICEadvHeff .OR. SEAICEadvArea .OR. SEAICEadvSnow ) THEN
#ifdef ALLOW_DEBUG
       IF ( debugLevel .GE. debLevB )
     &      CALL DEBUG_CALL( 'SEAICE_ADVDIFF', myThid )
#endif
       CALL SEAICE_ADVDIFF( myTime, myIter, myThid )
      ENDIF
#ifdef ALLOW_AUTODIFF_TAMC
CADJ STORE heffm  = comlev1, key = ikey_dynamics
cph-test(
cphCADJ STORE heff   = comlev1, key = ikey_dynamics
cphCADJ STORE area   = comlev1, key = ikey_dynamics
cphCADJ STORE hsnow  = comlev1, key = ikey_dynamics
cphCADJ STORE qnet   = comlev1, key = ikey_dynamics
cphCADJ STORE qsw    = comlev1, key = ikey_dynamics
cphCADJ STORE tice   = comlev1, key = ikey_dynamics
cph-test)
# ifdef SEAICE_ALLOW_DYNAMICS
cphCADJ STORE uice   = comlev1, key = ikey_dynamics
cphCADJ STORE vice   = comlev1, key = ikey_dynamics
# endif
#endif /* ALLOW_AUTODIFF_TAMC */

C     thermodynamics growth
C     must call growth after calling advection 
C     because of ugly time level business
       IF ( usePW79thermodynamics ) THEN
#ifdef ALLOW_DEBUG
        IF ( debugLevel .GE. debLevB )
     &       CALL DEBUG_CALL( 'SEAICE_GROWTH', myThid )
#endif
        CALL SEAICE_GROWTH( myTime, myIter, myThid )
       ENDIF

C--   Update overlap regions for a bunch of stuff
       _BARRIER
       CALL EXCH_3D_RL( HEFF, 3, myThid )
       CALL EXCH_3D_RL( AREA, 3, myThid )
       _EXCH_XY_R8( HSNOW,  myThid )
       _EXCH_XY_R4(EmPmR, myThid )
       _EXCH_XY_R4(Qnet , myThid )
#ifdef SHORTWAVE_HEATING
       _EXCH_XY_R4(Qsw  , myThid )
#endif
#ifdef ATMOSPHERIC_LOADING
       IF ( useRealFreshWaterFlux ) 
     &      _EXCH_XY_RS( sIceLoad, myThid )
#endif

#ifdef ALLOW_THSICE
C     endif .not.useThSice
      ENDIF
#endif /* ALLOW_THSICE */
CML   This has already been done in seaice_ocean_stress/ostres, so why repeat?
CML   CALL EXCH_UV_XY_RS(fu,fv,.TRUE.,myThid)

#ifdef ALLOW_DEBUG
      IF ( debugLevel .GE. debLevB )
     &     CALL DEBUG_LEAVE( 'SEAICE_MODEL', myThid )
#endif

      RETURN
      END

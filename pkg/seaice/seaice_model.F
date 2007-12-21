C $Header: /u/gcmpack/MITgcm/pkg/seaice/seaice_model.F,v 1.61 2007/12/21 01:32:35 dimitri Exp $
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
C \ev

C !USES: ===============================================================
#include "SIZE.h"
#include "EEPARAMS.h"
#include "DYNVARS.h"
#include "PARAMS.h"
#include "GRID.h"
#include "FFIELDS.h"
#include "SEAICE.h"
#include "SEAICE_PARAMS.h"
#ifdef ALLOW_EXF
# include "EXF_OPTIONS.h"
# include "EXF_FIELDS.h"
#endif
#ifdef ALLOW_SALT_PLUME
# include "SALT_PLUME.h"
#endif
#ifdef ALLOW_AUTODIFF_TAMC
# include "tamc.h"
#endif

C !INPUT PARAMETERS: ===================================================
C     myTime - Simulation time
C     myIter - Simulation timestep number
C     myThid - Thread no. that called this routine.
      _RL     myTime
      INTEGER myIter
      INTEGER myThid
CEndOfInterface

C !LOCAL VARIABLES: ====================================================
C     i,j,bi,bj :: Loop counters
C     iceFld    :: Copy of seaice field
      INTEGER i, j, bi, bj
      _RL iceFld (1-OLx:sNx+OLx,1-OLy:sNy+OLy)
      LOGICAL  DIFFERENT_MULTIPLE
      EXTERNAL DIFFERENT_MULTIPLE
CEOP

#ifdef ALLOW_DEBUG
      IF ( debugLevel .GE. debLevB )
     &   CALL DEBUG_ENTER( 'SEAICE_MODEL', myThid )
#endif

C--   Winds are from pkg/exf, which does not update edges.
      CALL EXCH_UV_AGRID_3D_RL( uwind, vwind, .TRUE., 1, myThid )

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
CADJ STORE seaicemasku    = comlev1, key = ikey_dynamics
CADJ STORE seaicemaskv    = comlev1, key = ikey_dynamics
CADJ STORE fu    = comlev1, key = ikey_dynamics
CADJ STORE fv    = comlev1, key = ikey_dynamics
CADJ STORE uice  = comlev1, key = ikey_dynamics
CADJ STORE vice  = comlev1, key = ikey_dynamics
CADJ STORE eta   = comlev1, key = ikey_dynamics
CADJ STORE zeta  = comlev1, key = ikey_dynamics

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

#ifdef ALLOW_OBCS
#ifndef DISABLE_SEAICE_OBCS
       IF ( useOBCS ) THEN
C-     Apply ice velocity open boundary conditions
          CALL OBCS_APPLY_UVICE( uice, vice, myThid )
       ENDIF
#endif /* DISABLE_SEAICE_OBCS */
#endif /* ALLOW_OBCS */

#ifdef ALLOW_THSICE
      IF ( .NOT.useThSice ) THEN
#endif
C--   Only call advection of heff, area, snow, and salt and 
C--   growth for the generic 0-layer thermodynamics of seaice
C--   if useThSice=.false., otherwise the 3-layer Winton thermodynamics
C--   (called from DO_OCEANIC_PHYSICS) take care of this

C NOW DO ADVECTION and DIFFUSION
      IF ( SEAICEadvHeff .OR. SEAICEadvArea .OR. SEAICEadvSnow
     &        .OR. SEAICEadvSalt ) THEN
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

#ifdef ALLOW_OBCS
#ifndef DISABLE_SEAICE_OBCS
       IF ( useOBCS ) THEN
C-     Apply open boundary conditions
        DO bj=myByLo(myThid),myByHi(myThid)
         DO bi=myBxLo(myThid),myBxHi(myThid)
          IF ( SEAICEadvHeff ) THEN
           DO j=1-Oly,sNy+Oly
            DO i=1-Olx,sNx+Olx
             iceFld(i,j) = HEFF(i,j,1,bi,bj)
            ENDDO
           ENDDO
           CALL OBCS_APPLY_HEFF( bi, bj, iceFld, myThid )
           DO j=1-Oly,sNy+Oly
            DO i=1-Olx,sNx+Olx
             HEFF(i,j,1,bi,bj) = iceFld(i,j)
            ENDDO
           ENDDO
          ENDIF
          IF ( SEAICEadvArea ) THEN
           DO j=1-Oly,sNy+Oly
            DO i=1-Olx,sNx+Olx
             iceFld(i,j) = AREA(i,j,1,bi,bj)
            ENDDO
           ENDDO
           CALL OBCS_APPLY_AREA( bi, bj, iceFld, myThid )
           DO j=1-Oly,sNy+Oly
            DO i=1-Olx,sNx+Olx
             AREA(i,j,1,bi,bj) = iceFld(i,j)
            ENDDO
           ENDDO
          ENDIF
#ifdef SEAICE_SALINITY
          IF ( SEAICEadvSALT ) THEN
           DO j=1-Oly,sNy+Oly
            DO i=1-Olx,sNx+Olx
             iceFld(i,j) = HSALT(i,j,bi,bj)
            ENDDO
           ENDDO
           CALL OBCS_APPLY_HSALT( bi, bj, iceFld, myThid )
           DO j=1-Oly,sNy+Oly
            DO i=1-Olx,sNx+Olx
             HSALT(i,j,bi,bj) = iceFld(i,j)
            ENDDO
           ENDDO
          ENDIF
#endif /* SEAICE_SALINITY */
          IF ( SEAICEadvSNOW ) THEN
           DO j=1-Oly,sNy+Oly
            DO i=1-Olx,sNx+Olx
             iceFld(i,j) = HSNOW(i,j,bi,bj)
            ENDDO
           ENDDO
           CALL OBCS_APPLY_HSNOW( bi, bj, iceFld, myThid )
           DO j=1-Oly,sNy+Oly
            DO i=1-Olx,sNx+Olx
             HSNOW(i,j,bi,bj) = iceFld(i,j)
            ENDDO
           ENDDO
          ENDIF
         ENDDO
        ENDDO
       ENDIF
#endif /* DISABLE_SEAICE_OBCS */
#endif /* ALLOW_OBCS */

C--   Update overlap regions for a bunch of stuff
       _BARRIER
       CALL EXCH_3D_RL( HEFF, 3, myThid )
       CALL EXCH_3D_RL( AREA, 3, myThid )
       _EXCH_XY_R8( HSNOW,  myThid )
#ifdef SEAICE_SALINITY
       _EXCH_XY_R8( HSALT,  myThid )
#endif
       _EXCH_XY_R4(EmPmR, myThid )
       _EXCH_XY_R4(saltFlux, myThid )
       _EXCH_XY_R4(Qnet , myThid )
#ifdef SHORTWAVE_HEATING
       _EXCH_XY_R4(Qsw  , myThid )
#endif /* SHORTWAVE_HEATING */
#ifdef ALLOW_SALT_PLUME
       IF ( useSALT_PLUME )
     &       _EXCH_XY_R4(saltPlumeFlux, myThid )
#endif /* ALLOW_SALT_PLUME */
#ifdef ATMOSPHERIC_LOADING
       IF ( useRealFreshWaterFlux )
     &      _EXCH_XY_RS( sIceLoad, myThid )
#endif

#ifdef ALLOW_DIAGNOSTICS
       IF ( useDiagnostics ) THEN
C     diagnostics for "non-state variables" that are modified by
C     the seaice model 
# ifdef ALLOW_EXF
        CALL DIAGNOSTICS_FILL(UWIND  ,'SIuwind ',0,1 ,0,1,1,myThid)
        CALL DIAGNOSTICS_FILL(VWIND  ,'SIvwind ',0,1 ,0,1,1,myThid)
# endif
        CALL DIAGNOSTICS_FILL(FU     ,'SIfu    ',0,1 ,0,1,1,myThid)
        CALL DIAGNOSTICS_FILL(FV     ,'SIfv    ',0,1 ,0,1,1,myThid)
        CALL DIAGNOSTICS_FILL(EmPmR  ,'SIempmr ',0,1 ,0,1,1,myThid)
        CALL DIAGNOSTICS_FILL(Qnet   ,'SIqnet  ',0,1 ,0,1,1,myThid)
        CALL DIAGNOSTICS_FILL(Qsw    ,'SIqsw   ',0,1 ,0,1,1,myThid)
       ENDIF
#endif /* ALLOW_DIAGNOSTICS */


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

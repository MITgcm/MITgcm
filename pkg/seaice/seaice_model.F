C $Header: /u/gcmpack/MITgcm/pkg/seaice/seaice_model.F,v 1.32 2006/05/06 19:13:51 heimbach Exp $
C $Name:  $

#include "SEAICE_OPTIONS.h"
 
CBOP
C !ROUTINE: SEAICE_MODEL

C !INTERFACE: ==========================================================
      SUBROUTINE seaice_model( myTime, myIter, myThid )

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

#ifdef SEAICE_EXTERNAL_FORCING
C--   Atmospheric state and runoff are from
C     pkg/exf, which does not update edges.
      CALL EXCH_UV_XY_RL(uwind,vwind,.TRUE.,myThid)
      _EXCH_XY_R8( atemp,  myThid )
      _EXCH_XY_R8( aqh,    myThid )
      _EXCH_XY_R8( lwdown, myThid )
      _EXCH_XY_R8( swdown, mythid )
      _EXCH_XY_R8( precip, myThid )
      _EXCH_XY_R8( evap,   myThid )
      _EXCH_XY_R8( runoff, myThid )
#else /* SEAICE_EXTERNAL_FORCING */
C--   Load atmospheric state and runoff.
      CALL SEAICE_GET_FORCING ( myTime, myIter, myThid )
#endif /* SEAICE_EXTERNAL_FORCING */

#ifdef ALLOW_AUTODIFF_TAMC
CADJ STORE uwind = comlev1, key = ikey_dynamics
CADJ STORE vwind = comlev1, key = ikey_dynamics
CADJ STORE heff  = comlev1, key = ikey_dynamics
CADJ STORE heffm = comlev1, key = ikey_dynamics
# ifdef SEAICE_ALLOW_DYNAMICS
CADJ STORE area  = comlev1, key = ikey_dynamics
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

#ifdef ALLOW_AUTODIFF_TAMC
# ifdef SEAICE_ALLOW_DYNAMICS
cphCADJ STORE heffm  = comlev1, key = ikey_dynamics
cphCADJ STORE heff   = comlev1, key = ikey_dynamics
cphCADJ STORE area   = comlev1, key = ikey_dynamics
cphCADJ STORE uice   = comlev1, key = ikey_dynamics
cphCADJ STORE vice   = comlev1, key = ikey_dynamics
# endif
#endif /* ALLOW_AUTODIFF_TAMC */
C NOW DO ADVECTION and DIFFUSION
      CALL SEAICE_ADVDIFF( myTime, myIter, myThid ) 
#ifdef ALLOW_AUTODIFF_TAMC
# ifdef SEAICE_ALLOW_DYNAMICS
CADJ STORE heffm  = comlev1, key = ikey_dynamics
CADJ STORE heff   = comlev1, key = ikey_dynamics
CADJ STORE area   = comlev1, key = ikey_dynamics
cphCADJ STORE uice   = comlev1, key = ikey_dynamics
cphCADJ STORE vice   = comlev1, key = ikey_dynamics
# endif
#endif /* ALLOW_AUTODIFF_TAMC */

C NOW DO GROWTH
C     MUST CALL GROWTH ONLY AFTER CALLING ADVECTION
      CALL GROWTH( myTime, myIter, myThid )

C--   Update overlap regions for a bunch of stuff
      _BARRIER
      CALL SEAICE_EXCH( HEFF, myThid )
      CALL SEAICE_EXCH( AREA, myThid )
      CALL EXCH_UV_XY_RS(fu,fv,.TRUE.,myThid)
      _EXCH_XY_R4(EmPmR, myThid )
      _EXCH_XY_R4(Qnet , myThid )
#ifdef SHORTWAVE_HEATING
      _EXCH_XY_R4(Qsw  , myThid )
#endif
      _EXCH_XYZ_R8(theta , myThid )

C--   Sea ice diagnostics.
C     <= moved to S/R do_the_model_io with other pkgs

C--   Write sea ice restart files
C     <= moved to S/R packages_write_pickup with other pkgs

C---------------------------------------------------
C OOH NOOOO we need to move the whole stuff
C---------------------------------------------------
#ifdef ALLOW_AUTODIFF_TAMC
CRG CADJ store UICE,VICE,AREA,HEFF,fu,fv,EmPmR,Qnet,Qsw = comlev1_bibj
#endif

C--   Call sea-ice cost function routine
CRG      CALL SEAICE_COST( myTime, myIter, myThid )

      RETURN
      END

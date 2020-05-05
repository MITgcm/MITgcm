#include "SEAICE_OPTIONS.h"
#ifdef ALLOW_AUTODIFF
# include "AUTODIFF_OPTIONS.h"
#endif

CBOP
C !ROUTINE: SEAICE_MODEL

C !INTERFACE: ==========================================================
      SUBROUTINE SEAICE_MODEL( myTime, myIter, myThid )

C !DESCRIPTION: \bv
C     *===========================================================*
C     | SUBROUTINE SEAICE_MODEL                                   |
C     | o Time stepping of a dynamic/thermodynamic sea ice model. |
C     |  Dynamics solver: Zhang/Hibler, JGR, 102, 8691-8702, 1997 |
C     |  Thermodynamics:        Hibler, MWR, 108, 1943-1973, 1980 |
C     |  Rheology:              Hibler, JPO,   9,  815- 846, 1979 |
C     |  Snow:          Zhang et al.  , JPO,  28,  191- 217, 1998 |
C     |  Parallel forward ice model written by Jinlun Zhang PSC/UW|
C     |  & coupled into MITgcm by Dimitris Menemenlis (JPL) 2/2001|
C     |  zhang@apl.washington.edu / menemenlis@jpl.nasa.gov       |
C     *===========================================================*
C     *===========================================================*
      IMPLICIT NONE
C \ev

C !USES: ===============================================================
#include "SIZE.h"
#include "EEPARAMS.h"
#include "DYNVARS.h"
#include "PARAMS.h"
#include "GRID.h"
#include "FFIELDS.h"
#include "SEAICE_SIZE.h"
#include "SEAICE_PARAMS.h"
#include "SEAICE.h"
#include "SEAICE_TRACER.h"
#ifdef ALLOW_EXF
# include "EXF_OPTIONS.h"
# include "EXF_FIELDS.h"
#endif
#ifdef ALLOW_AUTODIFF_TAMC
# include "tamc.h"
#endif

C !INPUT PARAMETERS: ===================================================
C     myTime :: Current time in simulation
C     myIter :: Current iteration number in simulation
C     myThid :: my Thread Id number
      _RL     myTime
      INTEGER myIter
      INTEGER myThid

C !LOCAL VARIABLES: ====================================================
C     i,j,bi,bj :: Loop counters
      INTEGER i, j
      INTEGER bi, bj
#ifdef ALLOW_EXF
      INTEGER grpDiag
#endif
#ifdef ALLOW_SITRACER
      INTEGER iTr
#endif
#ifndef SEAICE_CGRID
      _RL uLoc(1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      _RL vLoc(1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
#endif
CEOP
#ifdef ALLOW_DIAGNOSTICS
      _RL hTmp(1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      _RL recip_deltaTtherm
      LOGICAL  DIAGNOSTICS_IS_ON
      EXTERNAL DIAGNOSTICS_IS_ON
#endif
#ifdef ALLOW_DEBUG
      IF (debugMode) CALL DEBUG_ENTER( 'SEAICE_MODEL', myThid )
#endif

#ifdef ALLOW_THSICE
      IF ( useThSice ) THEN
C--   Map thSice-variables to HEFF and AREA
       CALL SEAICE_MAP_THSICE( myTime, myIter, myThid )
      ENDIF
#endif /* ALLOW_THSICE */

#ifdef ALLOW_EXF
      IF ( useEXF ) THEN
C--   Winds are from pkg/exf, which does not update edges.
       CALL EXCH_UV_AGRID_3D_RL( uwind, vwind, .TRUE., 1, myThid )
       IF ( useDiagnostics ) THEN
C-    Fill-in EXF wind-stess diags, weighted by open-ocean fraction
        grpDiag = -1
        IF ( SEAICEuseDYNAMICS ) grpDiag = 1
        CALL EXF_WEIGHT_SFX_DIAGS(
     I                  AREA, grpDiag, myTime, myIter, myThid )
       ENDIF
      ENDIF
#endif /* ALLOW_EXF */

#ifdef ALLOW_AUTODIFF_TAMC
      DO bj=myByLo(myThid),myByHi(myThid)
       DO bi=myBxLo(myThid),myBxHi(myThid)
        DO j=1-OLy,sNy+OLy
         DO i=1-OLx,sNx+OLx
          uIceNm1(i,j,bi,bj) = 0. _d 0
          vIceNm1(i,j,bi,bj) = 0. _d 0
# ifdef ALLOW_SITRACER
          DO iTr = 1, SItrMaxNum
           SItrBucket(i,j,bi,bj,iTr) = 0. _d 0
          ENDDO
# endif
         ENDDO
        ENDDO
       ENDDO
      ENDDO
CADJ STORE uwind = comlev1, key=ikey_dynamics, kind=isbyte
CADJ STORE vwind = comlev1, key=ikey_dynamics, kind=isbyte
CADJ STORE heff  = comlev1, key=ikey_dynamics, kind=isbyte
CADJ STORE area  = comlev1, key=ikey_dynamics, kind=isbyte
# ifdef SEAICE_ALLOW_DYNAMICS
#  ifdef SEAICE_CGRID
CADJ STORE hsnow = comlev1, key=ikey_dynamics, kind=isbyte
CADJ STORE seaicemasku = comlev1, key=ikey_dynamics, kind=isbyte
CADJ STORE seaicemaskv = comlev1, key=ikey_dynamics, kind=isbyte
CADJ STORE fu    = comlev1, key=ikey_dynamics, kind=isbyte
CADJ STORE fv    = comlev1, key=ikey_dynamics, kind=isbyte
CADJ STORE uice  = comlev1, key=ikey_dynamics, kind=isbyte
CADJ STORE vice  = comlev1, key=ikey_dynamics, kind=isbyte
cphCADJ STORE eta   = comlev1, key=ikey_dynamics, kind=isbyte
cphCADJ STORE zeta  = comlev1, key=ikey_dynamics, kind=isbyte
cph(
CADJ STORE dwatn    = comlev1, key=ikey_dynamics, kind=isbyte
#ifdef SEAICE_ALLOW_BOTTOMDRAG
CADJ STORE cbotc    = comlev1, key=ikey_dynamics, kind=isbyte
#endif /* SEAICE_ALLOW_BOTTOMDRAG */
cccCADJ STORE press0   = comlev1, key=ikey_dynamics, kind=isbyte
cccCADJ STORE zmax   = comlev1, key=ikey_dynamics, kind=isbyte
cccCADJ STORE zmin  = comlev1, key=ikey_dynamics, kind=isbyte
cph)
#   ifdef SEAICE_ALLOW_EVP
CADJ STORE seaice_sigma1  = comlev1, key=ikey_dynamics, kind=isbyte
CADJ STORE seaice_sigma2  = comlev1, key=ikey_dynamics, kind=isbyte
CADJ STORE seaice_sigma12 = comlev1, key=ikey_dynamics, kind=isbyte
#   endif
#  endif
# endif
# ifdef ALLOW_SITRACER
CADJ STORE siceload  = comlev1, key=ikey_dynamics, kind=isbyte
CADJ STORE sitracer  = comlev1, key=ikey_dynamics, kind=isbyte
# endif
#endif /* ALLOW_AUTODIFF_TAMC */

C solve ice momentum equations and calculate ocean surface stress
#ifdef ALLOW_DEBUG
      IF (debugMode) CALL DEBUG_CALL( 'SEAICE_DYNSOLVER', myThid )
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

C--   Apply ice velocity open boundary conditions
#ifdef ALLOW_OBCS
# ifndef DISABLE_SEAICE_OBCS
       IF ( useOBCS ) CALL OBCS_ADJUST_UVICE( uice, vice, myThid )
# endif /* DISABLE_SEAICE_OBCS */
#endif /* ALLOW_OBCS */

#ifdef ALLOW_THSICE
      IF ( useThSice ) THEN
#ifndef OLD_THSICE_CALL_SEQUENCE
#ifdef ALLOW_DEBUG
        IF (debugMode) CALL DEBUG_CALL( 'THSICE_DO_ADVECT', myThid )
#endif
        CALL THSICE_DO_ADVECT( 0, 0, myTime, myIter, myThid )
#endif /* OLD_THSICE_CALL_SEQUENCE */
      ELSE
#endif /* ALLOW_THSICE */
C--   Only call advection of heff, area, snow, and salt and
C--   growth for the generic 0-layer thermodynamics of seaice
C--   if useThSice=.false., otherwise the 3-layer Winton thermodynamics
C--   (called from DO_OCEANIC_PHYSICS) take care of this
#ifdef ALLOW_DIAGNOSTICS
       IF ( useDiagnostics
     &      .AND. DIAGNOSTICS_IS_ON('SIenph  ',myThid) ) THEN
        DO bj=myByLo(myThid),myByHi(myThid)
         DO bi=myBxLo(myThid),myBxHi(myThid)
          DO j=1-OLy,sNy+OLy
           DO i=1-OLx,sNx+OLx
            hTmp(I,J,bi,bj)=HEFF(I,J,bi,bj)
           ENDDO
          ENDDO
         ENDDO
        ENDDO
       ENDIF
#endif
C NOW DO ADVECTION and DIFFUSION
      IF ( SEAICEadvHeff .OR. SEAICEadvArea .OR. SEAICEadvSnow
     &        .OR. SEAICEadvSalt ) THEN
#ifdef ALLOW_DEBUG
       IF (debugMode) CALL DEBUG_CALL( 'SEAICE_ADVDIFF', myThid )
#endif
#ifdef SEAICE_CGRID
       CALL SEAICE_ADVDIFF( uIce, vIce, myTime, myIter, myThid )
#else /* SEAICE_CGRID */
       CALL SEAICE_ADVDIFF( uLoc, vLoc, myTime, myIter, myThid )
#endif /* SEAICE_CGRID */
      ENDIF

C     After advection, the sea ice variables may have unphysical values
C     e.g., < 0, that are regularized here. Concentration as a special case
C     may be > 1 in convergent motion and a ridging algorithm redistributes
C     the ice to limit the concentration to 1.
      CALL SEAICE_REG_RIDGE( myTime, myIter, myThid )

#ifdef ALLOW_EXF
      IF ( useEXF .AND. useDiagnostics ) THEN
C-    Fill-in EXF surface flux diags, weighted by open-ocean fraction
        grpDiag = -2
        IF ( usePW79thermodynamics ) grpDiag = 2
        CALL EXF_WEIGHT_SFX_DIAGS(
     I                  AREA, grpDiag, myTime, myIter, myThid )
      ENDIF
#endif /* ALLOW_EXF */

#ifdef DISABLE_SEAICE_GROWTH
       IF ( .TRUE. ) THEN
#else /* DISABLE_SEAICE_GROWTH */
C     thermodynamics growth
C     must call growth after calling advection
C     because of ugly time level business
       IF ( usePW79thermodynamics ) THEN
#ifdef ALLOW_SEAICE_GROWTH_ADX
#ifdef ALLOW_DEBUG
        IF (debugMode) CALL DEBUG_CALL( 'SEAICE_GROWTH_ADX', myThid )
#endif
        CALL SEAICE_GROWTH_ADX( myTime, myIter, myThid )
#else
#ifdef ALLOW_DEBUG
        IF (debugMode) CALL DEBUG_CALL( 'SEAICE_GROWTH', myThid )
#endif
        CALL SEAICE_GROWTH( myTime, myIter, myThid )
#endif
       ELSE
#endif /* DISABLE_SEAICE_GROWTH */
        DO bj=myByLo(myThid),myByHi(myThid)
         DO bi=myBxLo(myThid),myBxHi(myThid)
          DO j=1,sNy
           DO i=1,sNx
            sIceLoad(i,j,bi,bj) = HEFF(i,j,bi,bj)*SEAICE_rhoIce
     &                         + HSNOW(i,j,bi,bj)*SEAICE_rhoSnow
           ENDDO
          ENDDO
c#ifdef SEAICE_CAP_ICELOAD
c         sIceTooHeavy = rhoConst*drF(1) / 5. _d 0
c         DO j=1,sNy
c          DO i=1,sNx
c           sIceLoad(i,j,bi,bj) = MIN( sIceLoad(i,j,bi,bj),
c    &                                 sIceTooHeavy )
c          ENDDO
c         ENDDO
c#endif
         ENDDO
        ENDDO
       ENDIF

#ifdef ALLOW_SITRACER
# ifdef ALLOW_AUTODIFF_TAMC
CADJ STORE sitracer  = comlev1, key=ikey_dynamics, kind=isbyte
# endif
       CALL SEAICE_TRACER_PHYS ( myTime, myIter, myThid )
#endif

C--   Apply ice tracer open boundary conditions
#ifdef ALLOW_OBCS
# ifndef DISABLE_SEAICE_OBCS
       IF ( useOBCS ) CALL OBCS_APPLY_SEAICE( myThid )
# endif /* DISABLE_SEAICE_OBCS */
#endif /* ALLOW_OBCS */

C--   Update overlap regions for a bunch of stuff
       _EXCH_XY_RL( HEFF,  myThid )
       _EXCH_XY_RL( AREA,  myThid )
       _EXCH_XY_RL( HSNOW, myThid )
#ifdef SEAICE_ITD
       CALL EXCH_3D_RL( HEFFITD,  nITD, myThid )
       CALL EXCH_3D_RL( AREAITD,  nITD, myThid )
       CALL EXCH_3D_RL( HSNOWITD, nITD, myThid )
#endif
#ifdef SEAICE_VARIABLE_SALINITY
       _EXCH_XY_RL( HSALT, myThid )
#endif
#ifdef ALLOW_SITRACER
       DO iTr = 1, SItrNumInUse
        _EXCH_XY_RL( SItracer(1-OLx,1-OLy,1,1,iTr),myThid )
       ENDDO
#endif
       _EXCH_XY_RS(EmPmR, myThid )
       _EXCH_XY_RS(saltFlux, myThid )
       _EXCH_XY_RS(Qnet , myThid )
#ifdef SHORTWAVE_HEATING
       _EXCH_XY_RS(Qsw  , myThid )
#endif /* SHORTWAVE_HEATING */
#ifdef ATMOSPHERIC_LOADING
       IF ( useRealFreshWaterFlux )
     &      _EXCH_XY_RS( sIceLoad, myThid )
#endif

#ifdef ALLOW_OBCS
C--   In case we use scheme with a large stencil that extends into overlap:
C     no longer needed with the right masking in advection & diffusion S/R.
c      IF ( useOBCS ) THEN
c       DO bj=myByLo(myThid),myByHi(myThid)
c        DO bi=myBxLo(myThid),myBxHi(myThid)
c          CALL OBCS_COPY_TRACER( HEFF(1-OLx,1-OLy,bi,bj),
c    I                            1, bi, bj, myThid )
c          CALL OBCS_COPY_TRACER( AREA(1-OLx,1-OLy,bi,bj),
c    I                            1, bi, bj, myThid )
c          CALL OBCS_COPY_TRACER( HSNOW(1-OLx,1-OLy,bi,bj),
c    I                            1, bi, bj, myThid )
#ifdef SEAICE_VARIABLE_SALINITY
c          CALL OBCS_COPY_TRACER( HSALT(1-OLx,1-OLy,bi,bj),
c    I                            1, bi, bj, myThid )
#endif
c        ENDDO
c       ENDDO
c      ENDIF
#endif /* ALLOW_OBCS */

#ifdef ALLOW_DIAGNOSTICS
      IF ( useDiagnostics ) THEN
C     diagnostics for "non-state variables" that are modified by
C     the seaice model
# ifdef ALLOW_EXF
c       CALL DIAGNOSTICS_FILL(UWIND   ,'SIuwind ',0,1 ,0,1,1,myThid)
c       CALL DIAGNOSTICS_FILL(VWIND   ,'SIvwind ',0,1 ,0,1,1,myThid)
# endif
c       CALL DIAGNOSTICS_FILL_RS(FU   ,'SIfu    ',0,1 ,0,1,1,myThid)
c       CALL DIAGNOSTICS_FILL_RS(FV   ,'SIfv    ',0,1 ,0,1,1,myThid)
        CALL DIAGNOSTICS_FILL_RS(EmPmR,'SIempmr ',0,1 ,0,1,1,myThid)
        CALL DIAGNOSTICS_FILL_RS(Qnet ,'SIqnet  ',0,1 ,0,1,1,myThid)
        CALL DIAGNOSTICS_FILL_RS(Qsw  ,'SIqsw   ',0,1 ,0,1,1,myThid)
        IF ( DIAGNOSTICS_IS_ON('SIenph  ',myThid) ) THEN
         recip_deltaTtherm = 1. / SEAICE_deltaTtherm
         DO bj=myByLo(myThid),myByHi(myThid)
          DO bi=myBxLo(myThid),myBxHi(myThid)
           DO j=1,sNy
            DO i=1,sNx
             hTmp(I,J,bi,bj) = 0.5 _d 0 * SEAICE_rhoIce
     &            * 0.5 _d 0 * (
     &              UICE (I,J,bi,bj)**2 + UICE (I+1,J,  bi,bj)**2
     &            + VICE (I,J,bi,bj)**2 + VICE (I,  J+1,bi,bj)**2 )
     &            * (HEFF(I,J,bi,bj) - hTmp(I,J,bi,bj))
     &             *recip_deltaTtherm
            ENDDO
           ENDDO
          ENDDO
         ENDDO
         CALL DIAGNOSTICS_FILL(hTmp,'SIenph  ',0,1,0,1,1,myThid)
        ENDIF
       ENDIF
#endif /* ALLOW_DIAGNOSTICS */

#ifdef ALLOW_THSICE
C     endif .not.useThSice
      ENDIF
#endif /* ALLOW_THSICE */
CML   This has already been done in seaice_ocean_stress/ostres, so why repeat?
CML   CALL EXCH_UV_XY_RS(fu,fv,.TRUE.,myThid)

#ifdef ALLOW_EXF
# ifdef ALLOW_AUTODIFF_TAMC
#  if (defined (ALLOW_AUTODIFF_MONITOR))
        CALL EXF_ADJOINT_SNAPSHOTS( 3, myTime, myIter, myThid )
#  endif
# endif
#endif

#ifdef ALLOW_DEBUG
      IF (debugMode) CALL DEBUG_LEAVE( 'SEAICE_MODEL', myThid )
#endif

      RETURN
      END

C $Header: /u/gcmpack/MITgcm/pkg/seaice/seaice_model.F,v 1.104 2012/12/27 23:05:47 gforget Exp $
C $Name:  $

#include "SEAICE_OPTIONS.h"

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
C     myTime - Simulation time
C     myIter - Simulation timestep number
C     myThid - Thread no. that called this routine.
      _RL     myTime
      INTEGER myIter
      INTEGER myThid
CEndOfInterface

C !LOCAL VARIABLES: ====================================================
C     i,j,bi,bj :: Loop counters
#ifdef SEAICE_DEBUG
CToM<<<
C     msgBuf      :: Informational/error message buffer
      CHARACTER*(MAX_LEN_MBUF) msgBuf
      CHARACTER*10 HlimitMsgFormat
#endif
#ifdef SEAICE_ITD
      INTEGER IT
#endif
#if defined(ALLOW_AUTODIFF_TAMC) || defined(SEAICE_ITD)
C>>>ToM
      INTEGER i, j, bi, bj
#endif
#ifdef ALLOW_SITRACER
      INTEGER iTr
#endif
CEOP

#ifdef ALLOW_DEBUG
      IF (debugMode) CALL DEBUG_ENTER( 'SEAICE_MODEL', myThid )
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
CADJ STORE heffm = comlev1, key=ikey_dynamics, kind=isbyte
CADJ STORE area  = comlev1, key=ikey_dynamics, kind=isbyte
# ifdef SEAICE_ALLOW_DYNAMICS
#  ifdef SEAICE_CGRID
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
cccCADJ STORE press0   = comlev1, key=ikey_dynamics, kind=isbyte
cccCADJ STORE taux   = comlev1, key=ikey_dynamics, kind=isbyte
cccCADJ STORE tauy  = comlev1, key=ikey_dynamics, kind=isbyte
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
      IF ( .NOT.useThSice ) THEN
#endif
C--   Only call advection of heff, area, snow, and salt and
C--   growth for the generic 0-layer thermodynamics of seaice
C--   if useThSice=.false., otherwise the 3-layer Winton thermodynamics
C--   (called from DO_OCEANIC_PHYSICS) take care of this

C NOW DO ADVECTION and DIFFUSION
      IF ( SEAICEadvHeff .OR. SEAICEadvArea .OR. SEAICEadvSnow
     &        .OR. SEAICEadvSalt ) THEN
CToM<<<
#ifdef SEAICE_ITD
#ifdef SEAICE_DEBUG
C     ToM: generate some test output
       WRITE(HlimitMsgFormat,'(A,I2,A)') '(A,',nITD,'F8.4)'
       DO bj=myByLo(myThid),myByHi(myThid)
        DO bi=myBxLo(myThid),myBxHi(myThid)
           WRITE(msgBuf,'(A,F8.4,x,F8.4)')
     &       ' SEAICE_MODEL: AREA and HEFF before advection: ',
     &       AREA(1,1,bi,bj), HEFF(1,1,bi,bj)
c     &       ' SEAICE_MODEL: AREA and HEFF/AREA before advection: ',
c     &       AREA(1,1,bi,bj), HEFF(1,1,bi,bj)/AREA(1,1,bi,bj)
           CALL PRINT_MESSAGE( msgBuf, standardMessageUnit,
     &       SQUEEZE_RIGHT , myThid)
           WRITE(msgBuf,HlimitMsgFormat)
     &       ' SEAICE_MODEL: HEFFITD       before advection: ',
     &       HEFFITD(1,1,:,bi,bj)
c     &       ' SEAICE_MODEL: HEFFITD/AREAITD before advection: ',
c     &       HEFFITD(1,1,:,bi,bj) / AREAITD(1,1,:,bi,bj)
           CALL PRINT_MESSAGE( msgBuf, standardMessageUnit,
     &       SQUEEZE_RIGHT , myThid)
           WRITE(msgBuf,HlimitMsgFormat)
     &       ' SEAICE_MODEL: AREAITD       before advection: ',
     &       AREAITD(1,1,:,bi,bj)
           CALL PRINT_MESSAGE( msgBuf, standardMessageUnit,
     &       SQUEEZE_RIGHT , myThid)
        ENDDO
       ENDDO
#endif
#endif
C>>>ToM
#ifdef ALLOW_DEBUG
       IF (debugMode) CALL DEBUG_CALL( 'SEAICE_ADVDIFF', myThid )
#endif
       CALL SEAICE_ADVDIFF( myTime, myIter, myThid )
CToM<<<
#ifdef SEAICE_ITD
#ifdef SEAICE_DEBUG
C     ToM: generate some test output
       WRITE(HlimitMsgFormat,'(A,I2,A)') '(A,',nITD,'F8.4)'
       DO bj=myByLo(myThid),myByHi(myThid)
        DO bi=myBxLo(myThid),myBxHi(myThid)
           WRITE(msgBuf,HlimitMsgFormat)
     &       ' SEAICE_MODEL: HEFFITD        after advection: ',
     &       HEFFITD(1,1,:,bi,bj)
           CALL PRINT_MESSAGE( msgBuf, standardMessageUnit,
     &       SQUEEZE_RIGHT , myThid)
           WRITE(msgBuf,HlimitMsgFormat)
     &       ' SEAICE_MODEL: AREAITD        after advection: ',
     &       AREAITD(1,1,:,bi,bj)
           CALL PRINT_MESSAGE( msgBuf, standardMessageUnit,
     &       SQUEEZE_RIGHT , myThid)
            WRITE(msgBuf,'(A)')
     &        ' --------------------------------------------- '
            CALL PRINT_MESSAGE( msgBuf, standardMessageUnit,
     &        SQUEEZE_RIGHT , myThid)
        ENDDO
       ENDDO
#endif
C
C     check that all ice thickness categories meet their limits
C     (includes Hibler-type ridging)
#ifdef ALLOW_DEBUG
        IF (debugMode) CALL DEBUG_CALL( 'SEAICE_ITD_REDIST', myThid )
#endif
       DO bj=myByLo(myThid),myByHi(myThid)
        DO bi=myBxLo(myThid),myBxHi(myThid)
         CALL SEAICE_ITD_REDIST(bi, bj, myTime, myIter, myThid)
        ENDDO
       ENDDO
C     update mean ice thickness HEFF and total ice concentration AREA
C     to match single category values
#ifdef ALLOW_DEBUG
        IF (debugMode) CALL DEBUG_CALL( 'SEAICE_ITD_SUM', myThid )
#endif
       DO bj=myByLo(myThid),myByHi(myThid)
        DO bi=myBxLo(myThid),myBxHi(myThid)
         CALL SEAICE_ITD_SUM(bi, bj, myTime, myIter, myThid)
        ENDDO
       ENDDO
#endif
C>>>ToM
      ENDIF
#ifdef ALLOW_AUTODIFF_TAMC
CADJ STORE heffm  = comlev1, key=ikey_dynamics, kind=isbyte
#endif /* ALLOW_AUTODIFF_TAMC */

#ifdef SEAICE_ITD
#ifdef SEAICE_DEBUG
C     ToM: generate some test output
       WRITE(HlimitMsgFormat,'(A,I2,A)') '(A,',nITD,'F8.4)'
       DO bj=myByLo(myThid),myByHi(myThid)
        DO bi=myBxLo(myThid),myBxHi(myThid)
           WRITE(msgBuf,HlimitMsgFormat)
     &       ' SEAICE_MODEL: HEFFITD          before growth: ',
     &       HEFFITD(1,1,:,bi,bj)
           CALL PRINT_MESSAGE( msgBuf, standardMessageUnit,
     &       SQUEEZE_RIGHT , myThid)
           WRITE(msgBuf,HlimitMsgFormat)
     &       ' SEAICE_MODEL: AREAITD          before growth: ',
     &       AREAITD(1,1,:,bi,bj)
           CALL PRINT_MESSAGE( msgBuf, standardMessageUnit,
     &       SQUEEZE_RIGHT , myThid)
           WRITE(msgBuf,HlimitMsgFormat)
     &       ' SEAICE_MODEL: HSNOWITD         before growth: ',
     &       HSNOWITD(1,1,:,bi,bj)
           CALL PRINT_MESSAGE( msgBuf, standardMessageUnit,
     &       SQUEEZE_RIGHT , myThid)
        ENDDO
       ENDDO
#endif
#endif

#ifndef DISABLE_SEAICE_GROWTH
C     thermodynamics growth
C     must call growth after calling advection
C     because of ugly time level business
       IF ( usePW79thermodynamics ) THEN
#ifdef ALLOW_DEBUG
        IF (debugMode) CALL DEBUG_CALL( 'SEAICE_GROWTH', myThid )
#endif
        CALL SEAICE_GROWTH( myTime, myIter, myThid )
CToM<<<
#ifdef SEAICE_ITD
#ifdef SEAICE_DEBUG
C     ToM: generate some test output
        WRITE(HlimitMsgFormat,'(A,I2,A)') '(A,',nITD,'F8.4)'
        DO bj=myByLo(myThid),myByHi(myThid)
         DO bi=myBxLo(myThid),myBxHi(myThid)
            WRITE(msgBuf,HlimitMsgFormat)
     &        ' SEAICE_MODEL: HEFFITD           after growth: ',
     &        HEFFITD(1,1,:,bi,bj)
            CALL PRINT_MESSAGE( msgBuf, standardMessageUnit,
     &        SQUEEZE_RIGHT , myThid)
            WRITE(msgBuf,HlimitMsgFormat)
     &        ' SEAICE_MODEL: AREAITD           after growth: ',
     &        AREAITD(1,1,:,bi,bj)
            CALL PRINT_MESSAGE( msgBuf, standardMessageUnit,
     &        SQUEEZE_RIGHT , myThid)
            WRITE(msgBuf,HlimitMsgFormat)
     &        ' SEAICE_MODEL: HSNOWITD          after growth: ',
     &        HSNOWITD(1,1,:,bi,bj)
            CALL PRINT_MESSAGE( msgBuf, standardMessageUnit,
     &        SQUEEZE_RIGHT , myThid)
            WRITE(msgBuf,'(A)')
     &        ' --------------------------------------------- '
            CALL PRINT_MESSAGE( msgBuf, standardMessageUnit,
     &        SQUEEZE_RIGHT , myThid)
         ENDDO
        ENDDO
#endif
C
C     redistribute sea ice into proper sea ice category after growth/melt
C     in case model runs with ice thickness distribution
C---+-|--1----+----2----+----3----+----4----+----5----+----6----+----7-|
#ifdef ALLOW_DEBUG
        IF (debugMode) CALL DEBUG_CALL( 'SEAICE_ITD_REDIST', myThid )
#endif
       DO bj=myByLo(myThid),myByHi(myThid)
        DO bi=myBxLo(myThid),myBxHi(myThid)
         CALL SEAICE_ITD_REDIST(bi, bj, myTime, myIter, myThid)
        ENDDO
       ENDDO
C     store the mean ice thickness in HEFF (for dynamic solver and diagnostics)
       DO bj=myByLo(myThid),myByHi(myThid)
        DO bi=myBxLo(myThid),myBxHi(myThid)
         CALL SEAICE_ITD_SUM(bi, bj, myTime, myIter, myThid)
        ENDDO
       ENDDO

#ifdef SEAICE_DEBUG
C     ToM: generate some test output
        WRITE(HlimitMsgFormat,'(A,I2,A)') '(A,',nITD,'F8.4)'
        DO bj=myByLo(myThid),myByHi(myThid)
         DO bi=myBxLo(myThid),myBxHi(myThid)
            WRITE(msgBuf,HlimitMsgFormat)
     &        ' SEAICE_MODEL: HEFFITD    after final sorting: ',
     &        HEFFITD(1,1,:,bi,bj)
            CALL PRINT_MESSAGE( msgBuf, standardMessageUnit,
     &        SQUEEZE_RIGHT , myThid)
            WRITE(msgBuf,HlimitMsgFormat)
     &        ' SEAICE_MODEL: AREAITD    after final sorting: ',
     &        AREAITD(1,1,:,bi,bj)
            CALL PRINT_MESSAGE( msgBuf, standardMessageUnit,
     &        SQUEEZE_RIGHT , myThid)
            WRITE(msgBuf,HlimitMsgFormat)
     &        ' SEAICE_MODEL: HSNOWITD   after final sorting: ',
     &        HSNOWITD(1,1,:,bi,bj)
            CALL PRINT_MESSAGE( msgBuf, standardMessageUnit,
     &        SQUEEZE_RIGHT , myThid)
            WRITE(msgBuf,'(A)')
     &        ' ============================================= '
            CALL PRINT_MESSAGE( msgBuf, standardMessageUnit,
     &        SQUEEZE_RIGHT , myThid)
         ENDDO
        ENDDO
#endif
#endif
C
C>>>ToM
       ENDIF
#endif /* DISABLE_SEAICE_GROWTH */

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
        CALL DIAGNOSTICS_FILL(UWIND   ,'SIuwind ',0,1 ,0,1,1,myThid)
        CALL DIAGNOSTICS_FILL(VWIND   ,'SIvwind ',0,1 ,0,1,1,myThid)
# endif
        CALL DIAGNOSTICS_FILL_RS(FU   ,'SIfu    ',0,1 ,0,1,1,myThid)
        CALL DIAGNOSTICS_FILL_RS(FV   ,'SIfv    ',0,1 ,0,1,1,myThid)
        CALL DIAGNOSTICS_FILL_RS(EmPmR,'SIempmr ',0,1 ,0,1,1,myThid)
        CALL DIAGNOSTICS_FILL_RS(Qnet ,'SIqnet  ',0,1 ,0,1,1,myThid)
        CALL DIAGNOSTICS_FILL_RS(Qsw  ,'SIqsw   ',0,1 ,0,1,1,myThid)
#ifdef SEAICE_ITD
        CALL DIAGNOSTICS_FILL(HEFFITD ,'SIheffN ',0,nITD,0,1,1,myThid)
        CALL DIAGNOSTICS_FILL(AREAITD ,'SIareaN ',0,nITD,0,1,1,myThid)
#endif
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

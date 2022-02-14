#include "BLING_OPTIONS.h"
#include "PTRACERS_OPTIONS.h"

CBOP
      subroutine BLING_CARBONATE_INIT( myThid )

C     ==========================================================
C     | subroutine bling_carbonate_init
C     | o Calculate first guess of pH
C     |   Adapted from pkg/dic/dic_surfforcing_init.F
C     ==========================================================

      IMPLICIT NONE

C     === Global variables ===
#include "SIZE.h"
#include "DYNVARS.h"
#include "EEPARAMS.h"
#include "PARAMS.h"
#include "GRID.h"
#include "FFIELDS.h"
#include "BLING_VARS.h"
#include "PTRACERS_SIZE.h"
#include "PTRACERS_PARAMS.h"
#include "PTRACERS_FIELDS.h"
#include "BLING_LOAD.h"

C     === Routine arguments ===
C     myThid               :: thread Id. number
      INTEGER  myThid

#ifdef ALLOW_BLING
C     === Local variables ===
       INTEGER i,j, k, it
       INTEGER intimeP, intime0, intime1
       _RL aWght, bWght, co3dummy
C Number of iterations for pCO2 solvers...
C Solubility relation coefficients
C local variables for carbon chem
      INTEGER iMin,iMax,jMin,jMax, bi, bj
      _RL alktmp(1-OLx:sNx+OLx,1-OLy:sNy+OLy)
      _RL phostmp(1-OLx:sNx+OLx,1-OLy:sNy+OLy)
      _RL sitmp(1-OLx:sNx+OLx,1-OLy:sNy+OLy)
      _RL thetatmp(1-OLx:sNx+OLx,1-OLy:sNy+OLy)
      _RL salttmp(1-OLx:sNx+OLx,1-OLy:sNy+OLy)
      _RL dictmp(1-OLx:sNx+OLx,1-OLy:sNy+OLy)
      LOGICAL pH_isLoaded
CEOP

#ifndef USE_SIBLING
      IF ( periodicExternalForcing ) THEN

c read in silica field
         CALL LEF_ZERO( silica0,myThid )
         CALL LEF_ZERO( silica1,myThid )

C--   Now calculate whether it is time to update the forcing arrays
       CALL GET_PERIODIC_INTERVAL(
     O                   intimeP, intime0, intime1, bWght, aWght,
     I                   externForcingCycle, externForcingPeriod,
     I                   deltaTClock, startTime, myThid )

       _BARRIER
       _BEGIN_MASTER(myThid)
        WRITE(standardMessageUnit,'(A,I10,A,2(2I5,A))')
     &   ' BLING_SURFFORCING_INIT, it=', nIter0,
     &   ' : Reading new data, i0,i1=', intime0, intime1
       _END_MASTER(myThid)

       IF ( BLING_silicaFile .NE. ' '  ) THEN
         CALL READ_REC_XY_RS( BLING_silicaFile,silica0,intime0,
     &        nIter0,myThid )
         CALL READ_REC_XY_RS( BLING_silicaFile,silica1,intime1,
     &        nIter0,myThid )
       ENDIF

#ifdef ALLOW_OFFLINE
       IF ( useOffLine ) THEN
         CALL OFFLINE_FIELDS_LOAD( startTime, nIter0, myThid )
       ENDIF
#endif

       _EXCH_XY_RS(silica0, myThid )
       _EXCH_XY_RS(silica1, myThid )

       IF ( BLING_silicaFile .NE. ' '  ) THEN
        DO bj = myByLo(myThid), myByHi(myThid)
         DO bi = myBxLo(myThid), myBxHi(myThid)
          DO j=1-OLy,sNy+OLy
           DO i=1-OLx,sNx+OLx
             silica(i,j,bi,bj)= bWght*silica0(i,j,bi,bj)
     &                        + aWght*silica1(i,j,bi,bj)
           ENDDO
          ENDDO
         ENDDO
        ENDDO
       ENDIF

c end periodicExternalForcing
      ENDIF
c end SiBLING
#endif

C =================================================================

      jMin=1
      jMax=sNy
      iMin=1
      iMax=sNx

      DO k=1,Nr
      DO bj=myByLo(myThid),myByHi(myThid)
       DO bi=myBxLo(myThid),myBxHi(myThid)
         DO j=1-OLy,sNy+OLy
          DO i=1-OLx,sNx+OLx
            pH(i,j,k,bi,bj) = 8. _d 0
          ENDDO
         ENDDO
       ENDDO
      ENDDO
      ENDDO

      DO bj=myByLo(myThid),myByHi(myThid)
       DO bi=myBxLo(myThid),myBxHi(myThid)
        DO j=1-OLy,sNy+OLy
         DO i=1-OLx,sNx+OLx
          ak0(i,j,bi,bj)=0. _d 0
          ak1(i,j,bi,bj)=0. _d 0
          ak2(i,j,bi,bj)=0. _d 0
          akw(i,j,bi,bj)=0. _d 0
          akb(i,j,bi,bj)=0. _d 0
          akf(i,j,bi,bj)=0. _d 0
          ak1p(i,j,bi,bj)=0. _d 0
          ak2p(i,j,bi,bj)=0. _d 0
          ak3p(i,j,bi,bj)=0. _d 0
          aksi(i,j,bi,bj)=0. _d 0
          fugf(i,j,bi,bj)=0. _d 0
          ff(i,j,bi,bj)=0. _d 0
          ft(i,j,bi,bj)=0. _d 0
          st(i,j,bi,bj)=0. _d 0
          bt(i,j,bi,bj)=0. _d 0
#ifdef CARBONCHEM_SOLVESAPHE
          cat(i,j,bi,bj)=0. _d 0
          akn(i,j,bi,bj)=0. _d 0
          akhs(i,j,bi,bj)=0. _d 0
          aphscale(i,j,bi,bj)=0. _d 0
#endif
         ENDDO
        ENDDO
       ENDDO
      ENDDO

      pH_isLoaded = .FALSE.
      IF ( nIter0.GT.PTRACERS_Iter0 .OR.
     &    (nIter0.EQ.PTRACERS_Iter0 .AND. pickupSuff.NE.' ')
     &   ) THEN
C       Read pH from a pickup file if needed
        CALL BLING_READ_PICKUP(
     O                        pH_isLoaded,
     I                        nIter0, myThid )
      ENDIF

      DO bj=myByLo(myThid),myByHi(myThid)
       DO bi=myBxLo(myThid),myBxHi(myThid)

C determine inorganic carbon chem coefficients

        IF ( .NOT.pH_isLoaded ) THEN
C set guess of pH for first step here

        DO k=1,Nr
         DO j=jMin,jMax
          DO i=iMin,iMax
             thetatmp(i,j) = theta(i,j,k,bi,bj)
             salttmp(i,j) = salt(i,j,k,bi,bj)
             dictmp(i,j) = PTRACER(i,j,k,bi,bj,1)
     &                          * maskC(i,j,k,bi,bj)
             alktmp(i,j) = PTRACER(i,j,k,bi,bj,2)
     &                          * maskC(i,j,k,bi,bj)
#ifndef USE_BLING_V1
             phostmp(i,j) = PTRACER(i,j,k,bi,bj,5)
     &                          * maskC(i,j,k,bi,bj)
#else
             phostmp(i,j) = PTRACER(i,j,k,bi,bj,4)
     &                          * maskC(i,j,k,bi,bj)
#endif
#ifdef USE_SIBLING
             sitmp(i,j) = PTRACER(i,j,k,bi,bj,9)
     &                          * maskC(i,j,k,bi,bj)
#else
C FOR NON-INTERACTIVE Si
             IF ( k.eq.1 ) THEN
              sitmp(i,j) = silica(i,j,bi,bj) * maskC(i,j,k,bi,bj)
             ELSE
              sitmp(i,j) = 0.03 * maskC(i,j,k,bi,bj)
             ENDIF
#endif
          ENDDO
         ENDDO

#ifdef CARBONCHEM_SOLVESAPHE
#ifdef ALLOW_DEBUG
      IF (debugMode) CALL DEBUG_CALL('DIC_COEFFS_DEEP',myThid)
#endif
        CALL DIC_COEFFS_SURF(
cav changing surftheta to thetatmp
     I                       thetatmp,salttmp,
     I                       bi,bj,iMin,iMax,jMin,jMax,myThid)
C Now correct the coefficients for pressure dependence
        CALL DIC_COEFFS_DEEP(
     I                       thetatmp,salttmp,
     I                       bi,bj,iMin,iMax,jMin,jMax,
     I                       k,myThid)
#else
#ifdef ALLOW_DEBUG
      IF (debugMode) CALL DEBUG_CALL('CARBON_COEFFS',myThid)
#endif
         CALL CARBON_COEFFS_PRESSURE_DEP(
     I                       thetatmp,salttmp,
     I                       bi,bj,iMin,iMax,jMin,jMax,k,myThid)
#endif

C====================================================================

c first approximation

          DO j=jMin,jMax
           DO i=iMin,iMax
            IF ( maskC(i,j,k,bi,bj) .NE. 0. _d 0) THEN

#ifdef CARBONCHEM_SOLVESAPHE
             IF ( selectPHsolver.GT.0 ) THEN
C Use Munhoven (2013) Solvesaphe routine to initialize pH
#ifdef ALLOW_DEBUG
              IF (debugMode) CALL DEBUG_CALL(
     &            'AHINI_FOR_AT',myThid)
#endif
C call AHINI_FOR_AT to get better initial guess of pH
              CALL AHINI_FOR_AT(
     I           alktmp(i,j)*permil,
     I           dictmp(i,j)*permil,
     I           bt(i,j,bi,bj),
     U           pH(i,j,k,bi,bj),
     I           i,j,k,bi,bj,nIter0,myThid )
CAV C$TAF STORE pH(i,j,k,bi,bj)                            = dic_surf
CAV C$TAF STORE alktmp(i,j), phostmp(i,j), sitmp(i,j)      = dic_surf
#ifdef ALLOW_DEBUG
              IF (debugMode) CALL DEBUG_CALL(
     &           'CALC_PCO2_SOLVESAPHE',myThid)
#endif
              CALL CALC_PCO2_SOLVESAPHE(
     I        thetatmp(i,j),salttmp(i,j),
     I        dictmp(i,j), phostmp(i,j),
     I        sitmp(i,j),alktmp(i,j),
     U        pH(i,j,k,bi,bj),pCO2(i,j,bi,bj),co3dummy,
     I        i,j,k,bi,bj, it ,  myThid )
             ELSE
C Use the original Follows et al. (2006) solver
#endif /* CARBONCHEM_SOLVESAPHE */
#ifdef ALLOW_DEBUG
              IF (debugMode) CALL DEBUG_CALL(
     &           'CALC_PCO2_APPROX',myThid)
#endif

             DO it=1,10
              CALL CALC_PCO2_APPROX(
     I        thetatmp(i,j),salttmp(i,j),
     I        dictmp(i,j), phostmp(i,j),
     I        sitmp(i,j),alktmp(i,j),
     I        ak1(i,j,bi,bj),ak2(i,j,bi,bj),
     I        ak1p(i,j,bi,bj),ak2p(i,j,bi,bj),ak3p(i,j,bi,bj),
     I        aks(i,j,bi,bj),akb(i,j,bi,bj),akw(i,j,bi,bj),
     I        aksi(i,j,bi,bj),akf(i,j,bi,bj),
     I        ak0(i,j,bi,bj), fugf(i,j,bi,bj),
     I        ff(i,j,bi,bj),
     I        bt(i,j,bi,bj),st(i,j,bi,bj),ft(i,j,bi,bj),
     U        pH(i,j,k,bi,bj),pCO2(i,j,bi,bj),co3dummy,
     I        i,j,k,bi,bj, it ,  myThid )
             ENDDO
            ENDIF
#ifdef CARBONCHEM_SOLVESAPHE
             ENDIF
#endif /* CARBONCHEM_SOLVESAPHE */
           ENDDO
          ENDDO
         ENDDO
        ENDIF

C     end bi,bj loops
       ENDDO
      ENDDO

#endif /* ALLOW_BLING */

      RETURN
      END

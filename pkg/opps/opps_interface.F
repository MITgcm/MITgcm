C $Header: /u/gcmpack/MITgcm/pkg/opps/opps_interface.F,v 1.5 2010/01/03 19:11:43 jmc Exp $
C $Name:  $

#include "OPPS_OPTIONS.h"

CBOP
C     !ROUTINE: OPPS_INTERFACE
C     !INTERFACE:
      SUBROUTINE OPPS_INTERFACE(
     I       bi, bj, iMin, iMax, jMin, jMax,
     I       myTime, myIter, myThid )
C     !DESCRIPTION: \bv
C     *================================================================*
C     | SUBROUTINE OPPS_INTERFACE                                      |
C     | o Driver for OPPS mixing scheme that can be called             |
C     |   instead of convective_adjustment.                            |
C     |   Reference: Paluszkiewicz+Romea, Dynamics of Atmospheres and  |
C     |   Oceans (1997) 26, pp. 95-130                                 |
C     | o Support for passive tracers by joint treatment of            |
C     |   active (theta, salt) and passive tracers. The array          |
C     |   tracerLoc(Nr,2+PTRACERS_num) contains                        |
C     |   theta    = tracerLoc(:,1),                                   |
C     |   salt     = tracerLoc(:,2), and                               |
C     |   ptracers = tracerLoc(:,3:PTRACERS_num+2). For this to        |
C     |   work, the routine opps_calc had to be modified               |
C     |   considerably. opps_calc is based on nlopps.F but there is    |
C     |   is little left of the original (see opps_calc.F)             |
C     *================================================================*
C     \ev

C     !USES:
      IMPLICIT NONE
C     == Global data ==
#include "SIZE.h"
#include "EEPARAMS.h"
#include "PARAMS.h"
#include "DYNVARS.h"
#include "GRID.h"
#include "OPPS.h"
#ifdef ALLOW_PTRACERS
#include "PTRACERS_SIZE.h"
#include "PTRACERS_PARAMS.h"
#include "PTRACERS_FIELDS.h"
#endif

C     !INPUT/OUTPUT PARAMETERS:
C     == Routine arguments ==
C     bi,bj,iMin,iMax,jMin,jMax,K - Loop counters
C     myTime - Current time in simulation
C     myIter - Current iteration in simulation
C     myThid - Thread number of this instance of S/R CONVECT
      INTEGER bi,bj,iMin,iMax,jMin,jMax
      _RL myTime
      INTEGER myIter
      INTEGER myThid

#ifdef ALLOW_OPPS

C     !LOCAL VARIABLES:
C     == Local variables ==
C     msgBuf      - Informational/error meesage buffer
      INTEGER nTracer
#ifdef ALLOW_PTRACERS
      PARAMETER( nTracer = 2+PTRACERS_num )
      INTEGER itr
#else /* not ALLOW_PTRACERS */
      PARAMETER( nTracer = 2 )
#endif /* ALLOW_PTRACERS */
      INTEGER I, J, K, kMax
      INTEGER nTracerInUse
      _RL tMin, tMax, sMin, sMax
      _RL tMinNew, tMaxNew, sMinNew, sMaxNew
      _RL wVelLoc(Nr)
      _RL tracerLoc(Nr,nTracer)
      CHARACTER*(MAX_LEN_MBUF) msgBuf
CEOP

C     initialization
#ifdef ALLOW_PTRACERS
      nTracerInUse = 2+PTRACERS_numInUse
#else
      nTracerInUse = 2
#endif /* ALLOW_PTRACERS */
      tMax    = -1. _d 23
      tMin    =  1. _d 23
      sMax    = -1. _d 23
      sMin    =  1. _d 23
      tMaxNew = -1. _d 23
      tMinNew =  1. _d 23
      sMaxNew = -1. _d 23
      sMinNew =  1. _d 23
      tMinNew =  1. _d 23
C     re-initialize convection counter
      DO K=1,Nr
       DO J=1-Oly,sNy+Oly
        DO I=1-Olx,sNx+Olx
         OPPSconvectCount(I,J,K,bi,bj) = 0. _d 0
        ENDDO
       ENDDO
      ENDDO

C
      DO J=jMin,jMax
       DO I=iMin,iMax
        IF ( kSurfC(I,J,bi,bj) .LE. Nr ) THEN
         IF ( useGCMwVel ) THEN
          DO K=1,Nr
           tracerLoc(K,1) = theta(I,J,K,bi,bj)
           tracerLoc(K,2)  = salt(I,J,K,bi,bj)
           wVelLoc(K)  = wVel(I,J,K,bi,bj)
          ENDDO
         ELSE
          DO K=1,Nr
           tracerLoc(K,1) = theta(I,J,K,bi,bj)
           tracerLoc(K,2)  = salt(I,J,K,bi,bj)
           wVelLoc(K)  = - VERTICAL_VELOCITY
          ENDDO
         ENDIF
#ifdef ALLOW_PTRACERS
         DO itr = 3, nTracerInUse
          DO K=1,Nr
           tracerLoc(K,itr) = ptracer(I,J,K,bi,bj,itr-2)
          ENDDO
         ENDDO
#endif /* ALLOW_PTRACERS */
#ifdef ALLOW_OPPS_DEBUG
         IF ( OPPSdebugLevel .GE. debLevA ) THEN
C     determine range of temperature and salinity
          tMax = -1. d 23
          tMin =  1. d 23
          sMax = -1. d 23
          sMin =  1. d 23
          DO K=1,Nr
           tMax = MAX(tracerLoc(K,1),tMax)
           tMin = MAX(tracerLoc(K,1),tMin)
           sMax = MAX(tracerLoc(K,2),sMax)
           sMin = MAX(tracerLoc(K,2),sMin)
          ENDDO
         ENDIF
#endif /* ALLOW_OPPS_DEBUG */
         kMax = kLowC(I,J,bi,bj)
         CALL OPPS_CALC(
     U        tracerLoc,
     I        wVelLoc,kMax,nTracer,nTracerInUse,
     I        I,J,bi,bj,myTime,myIter,myThid)
#ifdef ALLOW_OPPS_DEBUG
         IF ( OPPSdebugLevel .GE. debLevA ) THEN
C     determine range of temperature and salinity
          tMaxNew = -1. d 23
          tMinNew =  1. d 23
          sMaxNew = -1. d 23
          sMinNew =  1. d 23
          DO K=1,Nr
           tMaxNew = MAX(tracerLoc(K,1),tMaxNew)
           tMinNew = MAX(tracerLoc(K,1),tMinNew)
           sMaxNew = MAX(tracerLoc(K,2),sMaxNew)
           sMinNew = MAX(tracerLoc(K,2),sMinNew)
          ENDDO
          IF ( tMaxNew.GT.tMax .OR. tMinNew.LT.tMin .OR.
     &         sMaxNew.GT.sMax .OR. sMinNew.LT.sMIN ) THEN
           WRITE(msgBuf,'(A,A)') 'OPPS_INTERFACE: theta or S-range is',
     &          ' larger than before mixing'
           CALL PRINT_MESSAGE( msgBuf, standardMessageUnit,
     &          SQUEEZE_RIGHT , myThid )
           WRITE(msgBuf,'(A,2I5)') '                for (i,j) = ', I,J
           CALL PRINT_MESSAGE( msgBuf, standardMessageUnit,
     &          SQUEEZE_RIGHT , myThid )
          ENDIF
         ENDIF
#endif /* ALLOW_OPPS_DEBUG */
         DO K=1,Nr
          theta(I,J,K,bi,bj) = tracerLoc(K,1)
          salt(I,J,K,bi,bj)  = tracerLoc(K,2)
         ENDDO
#ifdef ALLOW_PTRACERS
         DO itr = 3, nTracerInUse
          DO K=1,Nr
           ptracer(I,J,K,bi,bj,itr-2) = tracerLoc(K,itr)
          ENDDO
         ENDDO
#endif /* ALLOW_PTRACERS */
        ENDIF
       ENDDO
      ENDDO
#endif /* ALLOW_OPPS */

      RETURN
      END

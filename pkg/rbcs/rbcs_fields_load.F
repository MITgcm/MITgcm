C $Header: /u/gcmpack/MITgcm/pkg/rbcs/rbcs_fields_load.F,v 1.8 2007/11/08 22:26:22 jmc Exp $
C $Name:  $

#include "CPP_OPTIONS.h"
#include "PACKAGES_CONFIG.h"

C     !ROUTINE: RBCS_FIELDS_LOAD
C     !INTERFACE:
      SUBROUTINE RBCS_FIELDS_LOAD( myTime, myIter, myThid )
C     *==========================================================*
C     | SUBROUTINE RBCS_FIELDS_LOAD
C     | o Control reading of fields from external source.
C     *==========================================================*
C     | Offline External source field loading routine.
C     | This routine is called every time we want to
C     | load a a set of external fields. The routine decides
C     | which fields to load and then reads them in.
C     | This routine needs to be customised for particular
C     | experiments.
C     *==========================================================*

C     !USES:
      IMPLICIT NONE
C     === Global variables ===
#include "SIZE.h"
#include "EEPARAMS.h"
#include "PARAMS.h"
#include "FFIELDS.h"
#include "GRID.h"
#include "DYNVARS.h"
#ifdef ALLOW_PTRACERS
#include "PTRACERS_SIZE.h"
#include "PTRACERS_PARAMS.h"
#endif
#include "RBCS.h"

C     !INPUT/OUTPUT PARAMETERS:
C     === Routine arguments ===
C     myTime :: Simulation time
C     myIter :: Simulation timestep number
C     myThid :: Thread no. that called this routine.
      _RL     myTime
      INTEGER myIter
      INTEGER myThid

C     !LOCAL VARIABLES:
C     === Local arrays ===
C     [01]      :: End points for interpolation
C     Above use static heap storage to allow exchange.
C     aWght, bWght :: Interpolation weights

      INTEGER bi,bj,i,j,k,intime0,intime1
      _RL aWght,bWght,rdt
      INTEGER nForcingPeriods,Imytm,Ifprd,Ifcyc,Iftm
      INTEGER iTracer
      INTEGER  IFNBLNK, ILNBLNK
      EXTERNAL IFNBLNK, ILNBLNK

#ifdef ALLOW_RBCS
      CALL TIMER_START('RBCS_FIELDS_LOAD      [I/O]', myThid)

C First call requires that we initialize everything to zero for safety
      IF ( myIter .EQ. nIter0 ) THEN
       DO bj = myByLo(myThid), myByHi(myThid)
       DO bi = myBxLo(myThid), myBxHi(myThid)
        DO k=1,Nr
         DO j=1-Oly,sNy+Oly
          DO i=1-Olx,sNx+Olx
           rbct0(i,j,k,bi,bj)=0. _d 0
           rbcs0(i,j,k,bi,bj)=0. _d 0
           rbct1(i,j,k,bi,bj)=0. _d 0
           rbcs1(i,j,k,bi,bj)=0. _d 0
          ENDDO
         ENDDO
        ENDDO
       ENDDO
       ENDDO
#ifdef ALLOW_PTRACERS
       DO iTracer = 1, PTRACERS_numInUse
        DO bj = myByLo(myThid), myByHi(myThid)
        DO bi = myBxLo(myThid), myBxHi(myThid)
         DO k=1,Nr
          DO j=1-Oly,sNy+Oly
           DO i=1-Olx,sNx+Olx
            rbcptr0(i,j,k,bi,bj,iTracer)=0. _d 0
            rbcptr1(i,j,k,bi,bj,iTracer)=0. _d 0
           ENDDO
          ENDDO
         ENDDO
        ENDDO
        ENDDO
       ENDDO
#endif
      ENDIF

C Now calculate whether it is time to update the forcing arrays
      IF (rbcsForcingCycle.GT.0. _d 0) THEN
       rdt = 1. _d 0 / deltaTclock
       nForcingPeriods = NINT(rbcsForcingCycle/rbcsForcingPeriod)
       Imytm = NINT( (myTime-rbcsIniter*deltaTclock)*rdt )
       Ifprd = NINT(rbcsForcingPeriod*rdt)
       Ifcyc = NINT(rbcsForcingCycle*rdt)
       Iftm  = MOD( Imytm+Ifcyc-Ifprd/2, Ifcyc)

       intime0 = 1 + INT( Iftm/Ifprd )
       intime1 = 1 + MOD( intime0,nForcingPeriods )
c      aWght = DFLOAT( Iftm-Ifprd*(intime0 - 1) ) / DFLOAT( Ifprd )
       aWght = FLOAT( Iftm-Ifprd*(intime0 - 1) )
       bWght = FLOAT( Ifprd )
       aWght =  aWght / bWght
       bWght = 1. _d 0 - aWght

      ELSE
       intime1 = 1
       intime0 = 1
       Iftm  = 1
       Ifprd = 0
       aWght = .5 _d 0
       bWght = .5 _d 0
      ENDIF

      IF (
     &  Iftm-Ifprd*(intime0-1) .EQ. 0
     &  .OR. myIter .EQ. nIter0
     & ) THEN

       _BARRIER

C      If the above condition is met then we need to read in
C      data for the period ahead and the period behind myTime.
       _BEGIN_MASTER(myThid)
       WRITE(*,*)
     &  'S/R RBCS_FIELDS_LOAD: Reading new data',myTime,myIter
     &            , nIter0, intime0,intime1
       _END_MASTER(myThid)

       IF ( relaxTFile      .NE. ' '  ) THEN
        CALL READ_REC_XYZ_RS(relaxTFile, rbct0, intime0, myIter,myThid)
        CALL READ_REC_XYZ_RS(relaxTFile, rbct1, intime1, myIter,myThid)
       ENDIF
       IF ( relaxSFile      .NE. ' '  ) THEN
        CALL READ_REC_XYZ_RS(relaxSFile, rbcs0, intime0, myIter,myThid)
        CALL READ_REC_XYZ_RS(relaxSFile, rbcs1, intime1, myIter,myThid)
       ENDIF

#ifdef ALLOW_PTRACERS
       IF (useRBCptracers) THEN
        DO iTracer = 1, PTRACERS_numInUse
         IF (useRBCptrnum(iTracer)) THEN
           CALL READ_REC_XYZ_RS( relaxPtracerFile(iTracer),
     &                           rbcptr0(1-Olx,1-Oly,1,1,1,iTracer),
     &                           intime0, myIter, myThid )
           CALL READ_REC_XYZ_RS( relaxPtracerFile(iTracer),
     &                           rbcptr1(1-Olx,1-Oly,1,1,1,iTracer),
     &                           intime1, myIter, myThid )
         ENDIF
        ENDDO
       ENDIF
#endif

       _EXCH_XYZ_R4(rbct0 , myThid )
       _EXCH_XYZ_R4(rbct1 , myThid )
       _EXCH_XYZ_R4(rbcs0 , myThid )
       _EXCH_XYZ_R4(rbcs1 , myThid )
#ifdef ALLOW_PTRACERS
       IF (useRBCptracers) THEN
        DO iTracer = 1, PTRACERS_numInUse
         _EXCH_XYZ_R4(rbcptr0(1-Olx,1-Oly,1,1,1,iTracer),myThid)
         _EXCH_XYZ_R4(rbcptr1(1-Olx,1-Oly,1,1,1,iTracer),myThid)
        ENDDO
       ENDIF
#endif

      ENDIF

C--   Interpolate
      DO bj = myByLo(myThid), myByHi(myThid)
       DO bi = myBxLo(myThid), myBxHi(myThid)
        DO k=1,Nr
         DO j=1-Oly,sNy+Oly
          DO i=1-Olx,sNx+Olx
           RBCtemp(i,j,k,bi,bj) = bWght*rbct0(i,j,k,bi,bj)
     &                           +aWght*rbct1(i,j,k,bi,bj)
           RBCsalt(i,j,k,bi,bj) = bWght*rbcs0(i,j,k,bi,bj)
     &                           +aWght*rbcs1(i,j,k,bi,bj)
          ENDDO
         ENDDO
        ENDDO
       ENDDO
      ENDDO

#ifdef ALLOW_PTRACERS
      IF (useRBCptracers) THEN
       DO iTracer = 1, PTRACERS_numInUse
        IF (useRBCptrnum(iTracer)) THEN
         DO bj = myByLo(myThid), myByHi(myThid)
          DO bi = myBxLo(myThid), myBxHi(myThid)
           DO k=1,Nr
            DO j=1-Oly,sNy+Oly
             DO i=1-Olx,sNx+Olx
               RBC_ptracers(i,j,k,bi,bj,iTracer) =
     &                      bWght*rbcptr0(i,j,k,bi,bj,iTracer)
     &                     +aWght*rbcptr1(i,j,k,bi,bj,iTracer)
             ENDDO
            ENDDO
           ENDDO
          ENDDO
         ENDDO
        ENDIF
       ENDDO
      ENDIF
#endif

      CALL TIMER_STOP ('RBCS_FIELDS_LOAD      [I/O]', myThid)

#endif /* ALLOW_RBCS */

      RETURN
      END

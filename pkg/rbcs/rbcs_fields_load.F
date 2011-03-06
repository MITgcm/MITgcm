C $Header: /u/gcmpack/MITgcm/pkg/rbcs/rbcs_fields_load.F,v 1.12 2011/03/06 23:33:55 jmc Exp $
C $Name:  $

#include "RBCS_OPTIONS.h"

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

C     !FUNCTIONS:
      INTEGER  IFNBLNK, ILNBLNK
      EXTERNAL IFNBLNK, ILNBLNK

C     !LOCAL VARIABLES:
C     === Local arrays ===
C     [01]      :: End points for interpolation
C     Above use static heap storage to allow exchange.
C     aWght, bWght :: Interpolation weights

      INTEGER bi,bj,i,j,k,intime0,intime1
      _RL aWght,bWght,rhalfdt
      INTEGER nForcingPeriods,Imytm,Ifprd,Ifcyc,Iftm,Ifmod
#ifdef ALLOW_PTRACERS
      INTEGER iTracer
#endif
      INTEGER IL, initer0, initer1
      CHARACTER*(MAX_LEN_FNAM) fullName

      INTEGER F90MODULO,arg1,arg2
C statement function to emulate Fortran 90 MODULO
C this modulo has the same sign as arg2 (and absolute value < |arg2|)
      F90MODULO(arg1,arg2) = MOD(MOD(arg1,arg2)+arg2,arg2)

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
      IF (rbcsForcingPeriod.GT.0. _d 0) THEN
C      this converts times to even integers
       rhalfdt = 2. _d 0 / deltaTclock
       nForcingPeriods = NINT(rbcsForcingCycle/rbcsForcingPeriod)
       Imytm = NINT( (myTime-rbcsForcingOffset)*rhalfdt )
       Ifprd = NINT(rbcsForcingPeriod*rhalfdt)
       Ifcyc = NINT(rbcsForcingCycle*rhalfdt)
       Iftm  = Imytm-Ifprd/2
       Ifmod = F90MODULO(Iftm,Ifprd)

       intime0 = (Iftm-Ifmod)/Ifprd
       intime1 = intime0 + 1
       IF (rbcsForcingCycle.GT.0. _d 0) THEN
         intime0 = F90MODULO(intime0,nForcingPeriods)
         intime1 = F90MODULO(intime1,nForcingPeriods)
       ENDIF
       intime0 = 1 + intime0
       intime1 = 1 + intime1

c      aWght = DFLOAT( Iftm-Ifprd*(intime0 - 1) ) / DFLOAT( Ifprd )
       aWght = FLOAT( Ifmod )
       bWght = FLOAT( Ifprd )
       aWght =  aWght / bWght
       bWght = 1. _d 0 - aWght

      ELSE
       intime1 = 1
       intime0 = 1
       Ifmod = 1
       aWght = .5 _d 0
       bWght = .5 _d 0
      ENDIF

C     for rbcsSingleTimeFiles=.TRUE.
      Ifprd = NINT(rbcsForcingPeriod/deltaTrbcs)
      initer0 = rbcsIter0 + intime0*Ifprd
      initer1 = rbcsIter0 + intime1*Ifprd

      IF (
     &  Ifmod .EQ. 0
     &  .OR. myIter .EQ. nIter0
     & ) THEN

C      If the above condition is met then we need to read in
C      data for the period ahead and the period behind myTime.
       _BEGIN_MASTER(myThid)
        WRITE(standardMessageUnit,'(A,2I5,I10,1P1E20.12)')
     &        'S/R RBCS_FIELDS_LOAD: Reading new data:',
     &         intime0, intime1, myIter, myTime
       _END_MASTER(myThid)

       IF ( useRBCtemp .AND. relaxTFile .NE. ' '  ) THEN
        IF ( rbcsSingleTimeFiles ) THEN
         IL=ILNBLNK( relaxTFile )
         WRITE(fullName,'(2a,i10.10)') relaxTFile(1:IL),'.',initer0
         CALL READ_REC_XYZ_RS(fullName, rbct0, 1, myIter, myThid)
         WRITE(fullName,'(2a,i10.10)') relaxTFile(1:IL),'.',initer1
         CALL READ_REC_XYZ_RS(fullName, rbct1, 1, myIter, myThid)
        ELSE
         CALL READ_REC_XYZ_RS(relaxTFile,rbct0,intime0,myIter,myThid)
         CALL READ_REC_XYZ_RS(relaxTFile,rbct1,intime1,myIter,myThid)
        ENDIF
       ENDIF
       IF ( useRBCsalt .AND. relaxSFile .NE. ' '  ) THEN
        IF ( rbcsSingleTimeFiles ) THEN
         IL=ILNBLNK( relaxSFile )
         WRITE(fullName,'(2a,i10.10)') relaxSFile(1:IL),'.',initer0
         CALL READ_REC_XYZ_RS(fullName, rbcs0, 1, myIter, myThid)
         WRITE(fullName,'(2a,i10.10)') relaxSFile(1:IL),'.',initer1
         CALL READ_REC_XYZ_RS(fullName, rbcs1, 1, myIter, myThid)
        ELSE
         CALL READ_REC_XYZ_RS(relaxSFile,rbcs0,intime0,myIter,myThid)
         CALL READ_REC_XYZ_RS(relaxSFile,rbcs1,intime1,myIter,myThid)
        ENDIF
       ENDIF

#ifdef ALLOW_PTRACERS
       IF ( usePTRACERS ) THEN
        DO iTracer = 1, PTRACERS_numInUse
         IF ( useRBCptrnum(iTracer) .AND.
     &        relaxPtracerFile(iTracer).NE. ' ' ) THEN
          IF ( rbcsSingleTimeFiles ) THEN
           IL=ILNBLNK( relaxPtracerFile(iTracer) )
           WRITE(fullName,'(2a,i10.10)') relaxPtracerFile(iTracer)(1:IL)
     &                                   ,'.',initer0
           CALL READ_REC_XYZ_RS( fullName,
     &                           rbcptr0(1-Olx,1-Oly,1,1,1,iTracer),
     &                           1, myIter, myThid )
           WRITE(fullName,'(2a,i10.10)') relaxPtracerFile(iTracer)(1:IL)
     &                                   ,'.',initer1
           CALL READ_REC_XYZ_RS( fullName,
     &                           rbcptr1(1-Olx,1-Oly,1,1,1,iTracer),
     &                           1, myIter, myThid )
          ELSE
           CALL READ_REC_XYZ_RS( relaxPtracerFile(iTracer),
     &                           rbcptr0(1-Olx,1-Oly,1,1,1,iTracer),
     &                           intime0, myIter, myThid )
           CALL READ_REC_XYZ_RS( relaxPtracerFile(iTracer),
     &                           rbcptr1(1-Olx,1-Oly,1,1,1,iTracer),
     &                           intime1, myIter, myThid )
          ENDIF
         ENDIF
        ENDDO
       ENDIF
#endif

       IF ( useRBCtemp .AND. relaxTFile .NE. ' '  ) THEN
         CALL EXCH_XYZ_RS( rbct0 , myThid )
         CALL EXCH_XYZ_RS( rbct1 , myThid )
       ENDIF
       IF ( useRBCsalt .AND. relaxSFile .NE. ' '  ) THEN
         CALL EXCH_XYZ_RS( rbcs0 , myThid )
         CALL EXCH_XYZ_RS( rbcs1 , myThid )
       ENDIF
#ifdef ALLOW_PTRACERS
       IF (usePTRACERS) THEN
        DO iTracer = 1, PTRACERS_numInUse
         IF ( useRBCptrnum(iTracer) ) THEN
          CALL EXCH_XYZ_RS( rbcptr0(1-Olx,1-Oly,1,1,1,iTracer),myThid )
          CALL EXCH_XYZ_RS( rbcptr1(1-Olx,1-Oly,1,1,1,iTracer),myThid )
         ENDIF
        ENDDO
       ENDIF
#endif /* ALLOW_PTRACERS */

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
      IF ( usePTRACERS ) THEN
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
#endif /* ALLOW_PTRACERS */

      CALL TIMER_STOP ('RBCS_FIELDS_LOAD      [I/O]', myThid)

#endif /* ALLOW_RBCS */

      RETURN
      END

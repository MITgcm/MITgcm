C $Header: /u/gcmpack/MITgcm/pkg/rbcs/rbcs_fields_load.F,v 1.15 2011/06/07 22:25:10 jmc Exp $
C $Name:  $

#include "RBCS_OPTIONS.h"

C     !ROUTINE: RBCS_FIELDS_LOAD
C     !INTERFACE:
      SUBROUTINE RBCS_FIELDS_LOAD( myTime, myIter, myThid )

C     !DESCRIPTION: \bv
C     *==========================================================*
C     | SUBROUTINE RBCS_FIELDS_LOAD
C     | o Control reading of fields from external source.
C     *==========================================================*
C     | RBCS External source field loading routine.
C     | This routine is called every time we want to
C     | load a a set of external fields. The routine decides
C     | which fields to load and then reads them in.
C     *==========================================================*
C     \ev

C     !USES:
      IMPLICIT NONE
C     === Global variables ===
#include "SIZE.h"
#include "EEPARAMS.h"
#include "PARAMS.h"
#ifdef ALLOW_PTRACERS
#include "PTRACERS_SIZE.h"
#include "PTRACERS_PARAMS.h"
#endif
#include "RBCS_SIZE.h"
#include "RBCS_PARAMS.h"
#include "RBCS_FIELDS.h"

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

      INTEGER bi, bj, i, j, k
      INTEGER intimeP, intime0, intime1
      _RL aWght, bWght, locTime
      INTEGER Ifprd
#ifdef ALLOW_PTRACERS
      INTEGER iTracer
#endif
      INTEGER IL, initer0, initer1
      CHARACTER*(MAX_LEN_FNAM) fullName
CEOP

#ifdef ALLOW_RBCS
      CALL TIMER_START('RBCS_FIELDS_LOAD      [I/O]', myThid)

C--   First call requires that we initialize everything to zero for safety
C      <= already done in RBCS_INIT_VARIA

C--   Now calculate whether it is time to update the forcing arrays
      bi = myBxLo(myThid)
      bj = myByLo(myThid)
      IF (rbcsForcingPeriod.GT.0. _d 0) THEN
       locTime = myTime - rbcsForcingOffset
       CALL GET_PERIODIC_INTERVAL(
     O                   intimeP, intime0, intime1, bWght, aWght,
     I                   rbcsForcingCycle, rbcsForcingPeriod,
     I                   deltaTclock, locTime, myThid )
#ifdef ALLOW_DEBUG
       IF ( debugLevel.GE.debLevB ) THEN
        _BEGIN_MASTER(myThid)
        WRITE(standardMessageUnit,'(A,I10,A,4I5,A,2F14.10)')
     &   ' RBCS_FIELDS_LOAD,', myIter,
     &   ' : iP,iLd,i0,i1=', intimeP,rbcsLdRec(bi,bj), intime0,intime1,
     &   ' ; Wght=', bWght, aWght
        _END_MASTER(myThid)
       ENDIF
#endif /* ALLOW_DEBUG */
      ELSE
       intimeP = 1
       intime1 = 1
       intime0 = 1
       aWght = .5 _d 0
       bWght = .5 _d 0
      ENDIF

#ifdef ALLOW_AUTODIFF_TAMC
C-    assuming that we call S/R RBCS_FIELDS_LOAD at each time-step and
C     with increasing time, this will catch when we need to load new records;
C     But with Adjoint run, this is not always the case => might end-up using
C     the wrong time-records
      IF ( intime0.NE.intimeP .OR. myIter.EQ.nIter0 ) THEN
#else /* ALLOW_AUTODIFF_TAMC */
C-    Make no assumption on sequence of calls to RBCS_FIELDS_LOAD ;
C     This is the correct formulation (works in Adjoint run).
C     Unfortunatly, produces many recomputations <== not used until it is fixed
      IF ( intime1.NE.rbcsLdRec(bi,bj) ) THEN
#endif /* ALLOW_AUTODIFF_TAMC */

C--   If the above condition is met then we need to read in
C     data for the period ahead and the period behind myTime.
       IF ( debugLevel.GE.debLevZero ) THEN
        _BEGIN_MASTER(myThid)
        WRITE(standardMessageUnit,'(A,I10,A,2(2I5,A))')
     &    ' RBCS_FIELDS_LOAD, it=', myIter,
     &    ' : Reading new data, i0,i1=', intime0, intime1,
     &    ' (prev=', intimeP, rbcsLdRec(bi,bj), ' )'
        _END_MASTER(myThid)
       ENDIF

C     for rbcsSingleTimeFiles=.TRUE.
       Ifprd = NINT(rbcsForcingPeriod/deltaTrbcs)
       initer0 = rbcsIter0 + intime0*Ifprd
       initer1 = rbcsIter0 + intime1*Ifprd

#ifndef DISABLE_RBCS_MOM
       IF ( useRBCuVel .AND. relaxUFile.NE.' '  ) THEN
        IF ( rbcsSingleTimeFiles ) THEN
         IL=ILNBLNK( relaxUFile )
         WRITE(fullName,'(2A,I10.10)') relaxUFile(1:IL),'.',initer0
         CALL READ_REC_XYZ_RS(fullName, rbcu0, 1, myIter, myThid)
         WRITE(fullName,'(2A,I10.10)') relaxUFile(1:IL),'.',initer1
         CALL READ_REC_XYZ_RS(fullName, rbcu1, 1, myIter, myThid)
        ELSE
         CALL READ_REC_XYZ_RS(relaxUFile,rbcu0,intime0,myIter,myThid)
         CALL READ_REC_XYZ_RS(relaxUFile,rbcu1,intime1,myIter,myThid)
        ENDIF
       ENDIF
       IF ( useRBCvVel .AND. relaxVFile.NE.' '  ) THEN
        IF ( rbcsSingleTimeFiles ) THEN
         IL=ILNBLNK( relaxVFile )
         WRITE(fullName,'(2A,I10.10)') relaxVFile(1:IL),'.',initer0
         CALL READ_REC_XYZ_RS(fullName, rbcv0, 1, myIter, myThid)
         WRITE(fullName,'(2A,I10.10)') relaxVFile(1:IL),'.',initer1
         CALL READ_REC_XYZ_RS(fullName, rbcv1, 1, myIter, myThid)
        ELSE
         CALL READ_REC_XYZ_RS(relaxVFile,rbcv0,intime0,myIter,myThid)
         CALL READ_REC_XYZ_RS(relaxVFile,rbcv1,intime1,myIter,myThid)
        ENDIF
       ENDIF
       IF ( (useRBCuVel .AND. relaxUFile.NE.' ') .OR.
     &      (useRBCvVel .AND. relaxVFile.NE.' ') ) THEN
        CALL EXCH_UV_XYZ_RS( rbcu0, rbcv0, .TRUE., myThid )
        CALL EXCH_UV_XYZ_RS( rbcu1, rbcv1, .TRUE., myThid )
       ENDIF
#endif /* DISABLE_RBCS_MOM */
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
        CALL EXCH_XYZ_RS( rbct0 , myThid )
        CALL EXCH_XYZ_RS( rbct1 , myThid )
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
        CALL EXCH_XYZ_RS( rbcs0 , myThid )
        CALL EXCH_XYZ_RS( rbcs1 , myThid )
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
          CALL EXCH_XYZ_RS( rbcptr0(1-Olx,1-Oly,1,1,1,iTracer),myThid )
          CALL EXCH_XYZ_RS( rbcptr1(1-Olx,1-Oly,1,1,1,iTracer),myThid )
         ENDIF
        ENDDO
       ENDIF
#endif /* ALLOW_PTRACERS */

C-    save newly loaded time-record
       DO bj = myByLo(myThid), myByHi(myThid)
         DO bi = myBxLo(myThid), myBxHi(myThid)
           rbcsLdRec(bi,bj) = intime1
         ENDDO
       ENDDO

C--   end if-block for loading new time-records
      ENDIF

C--   Interpolate
      DO bj = myByLo(myThid), myByHi(myThid)
       DO bi = myBxLo(myThid), myBxHi(myThid)
#ifndef DISABLE_RBCS_MOM
        IF ( useRBCuVel .OR. useRBCvVel ) THEN
         DO k=1,Nr
          DO j=1-Oly,sNy+Oly
           DO i=1-Olx,sNx+Olx
            RBCuVel(i,j,k,bi,bj) = bWght*rbcu0(i,j,k,bi,bj)
     &                            +aWght*rbcu1(i,j,k,bi,bj)
            RBCvVel(i,j,k,bi,bj) = bWght*rbcv0(i,j,k,bi,bj)
     &                            +aWght*rbcv1(i,j,k,bi,bj)
           ENDDO
          ENDDO
         ENDDO
        ENDIF
#endif /* DISABLE_RBCS_MOM */
         DO k=1,Nr
          DO j=1-Oly,sNy+Oly
           DO i=1-Olx,sNx+Olx
            RBCtemp(i,j,k,bi,bj) = bWght*rbct0(i,j,k,bi,bj)
     &                            +aWght*rbct1(i,j,k,bi,bj)
            RBCsalt(i,j,k,bi,bj) = bWght*rbcs0(i,j,k,bi,bj)
     &                            +aWght*rbcs1(i,j,k,bi,bj)
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

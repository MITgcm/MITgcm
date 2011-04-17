C $Header: /u/gcmpack/MITgcm/pkg/cfc/cfc_fields_load.F,v 1.17 2011/04/17 21:01:36 jmc Exp $
C $Name:  $

#include "GCHEM_OPTIONS.h"

CStartOfInterFace
      SUBROUTINE CFC_FIELDS_LOAD (
     I           myIter,myTime,myThid)

C     *==========================================================*
C     | SUBROUTINE CFC_FIELDS_LOAD
C     *==========================================================*

      IMPLICIT NONE

C     == GLobal variables ==
#include "SIZE.h"
#include "EEPARAMS.h"
#include "PARAMS.h"
#include "GRID.h"
#include "CFC.h"
#ifdef ALLOW_EXF
# include "EXF_FIELDS.h"
#endif
#ifdef ALLOW_SEAICE
# include "SEAICE.h"
#endif

C     == Routine arguments ==
      INTEGER myIter
      _RL myTime
      INTEGER myThid

C     == Local variables ==
      INTEGER intimeP, intime0, intime1
       INTEGER bi, bj, i, j
      _RL aWght, bWght
      _RL wind

      IF ( CFC_forcingCycle .GT. 0. _d 0 ) THEN

C First call requires that we initialize everything to zero for safety
cQQQ need to check timing
       IF ( myIter .EQ. nIter0 ) THEN
         DO bj = myByLo(myThid), myByHi(myThid)
          DO bi = myBxLo(myThid), myBxHi(myThid)
            CFC_ldRec(bi,bj) = 0
          ENDDO
         ENDDO
         CALL LEF_ZERO( wind0,myThid )
         CALL LEF_ZERO( wind1,myThid )
         CALL LEF_ZERO( atmosp0,myThid )
         CALL LEF_ZERO( atmosp1,myThid )
         CALL LEF_ZERO( ice0,myThid )
         CALL LEF_ZERO( ice1,myThid )
       ENDIF

C--   Now calculate whether it is time to update the forcing arrays
       CALL GET_PERIODIC_INTERVAL(
     O                   intimeP, intime0, intime1, bWght, aWght,
     I                   CFC_forcingCycle, CFC_forcingPeriod,
     I                   deltaTclock, myTime, myThid )

       bi = myBxLo(myThid)
       bj = myByLo(myThid)
#ifdef ALLOW_DEBUG
       IF ( debugLevel.GE.debLevA ) THEN
        _BEGIN_MASTER(myThid)
        WRITE(standardMessageUnit,'(A,I10,A,4I5,A,2F14.10)')
     &   ' CFC_FIELDS_LOAD,', myIter,
     &   ' : iP,iLd,i0,i1=', intimeP,CFC_ldRec(bi,bj), intime0,intime1,
     &   ' ; Wght=', bWght, aWght
        _END_MASTER(myThid)
       ENDIF
#endif /* ALLOW_DEBUG */

#ifdef ALLOW_AUTODIFF_TAMC
C-    assuming that we call S/R CFC_FIELDS_LOAD at each time-step and
C     with increasing time, this will catch when we need to load new records;
C     But with Adjoint run, this is not always the case => might end-up using
C     the wrong time-records
       IF ( intime0.NE.intimeP .OR. myIter.EQ.nIter0 ) THEN
#else /* ALLOW_AUTODIFF_TAMC */
C-    Make no assumption on sequence of calls to CFC_FIELDS_LOAD ;
C     This is the correct formulation (works in Adjoint run).
C     Unfortunatly, might produce many recomputations <== not used until it is fixed
       IF ( intime1.NE.CFC_ldRec(bi,bj) ) THEN
#endif /* ALLOW_AUTODIFF_TAMC */

C--   If the above condition is met then we need to read in
C     data for the period ahead and the period behind myTime.
        _BEGIN_MASTER(myThid)
        WRITE(standardMessageUnit,'(A,I10,A,2(2I5,A))')
     &   ' CFC_FIELDS_LOAD, it=', myIter,
     &   ' : Reading new data, i0,i1=', intime0, intime1,
     &    ' (prev=', intimeP, CFC_ldRec(bi,bj), ' )'
        _END_MASTER(myThid)

        _BARRIER

        IF ( CFC_windFile .NE. ' '  .AND. .NOT.useEXF ) THEN
         CALL READ_REC_XY_RS( CFC_windFile,wind0,intime0,
     &        myIter,myThid )
         CALL READ_REC_XY_RS( CFC_windFile,wind1,intime1,
     &        myIter,myThid )
        ENDIF
        IF ( CFC_atmospFile .NE. ' '  ) THEN
         CALL READ_REC_XY_RS( CFC_atmospFile,atmosp0,intime0,
     &        myIter,myThid )
         CALL READ_REC_XY_RS( CFC_atmospFile,atmosp1,intime1,
     &        myIter,myThid )
        ENDIF
        IF ( CFC_iceFile .NE. ' '  .AND. .NOT.useSEAICE ) THEN
         CALL READ_REC_XY_RS( CFC_iceFile,ice0,intime0,
     &       myIter,myThid )
         CALL READ_REC_XY_RS( CFC_iceFile,ice1,intime1,
     &       myIter,myThid )
        ENDIF

        IF (.NOT.useEXF) THEN
         _EXCH_XY_RS(wind0, myThid )
         _EXCH_XY_RS(wind1, myThid )
        ENDIF
         _EXCH_XY_RS(atmosp0, myThid )
         _EXCH_XY_RS(atmosp1, myThid )
        IF (.NOT.useSEAICE) THEN
         _EXCH_XY_RS(ice0, myThid )
         _EXCH_XY_RS(ice1, myThid )
        ENDIF

C-    save newly loaded time-record
        DO bj = myByLo(myThid), myByHi(myThid)
         DO bi = myBxLo(myThid), myBxHi(myThid)
           CFC_ldRec(bi,bj) = intime1
         ENDDO
        ENDDO

C--   end if-block for loading new time-records
       ENDIF

#ifdef ALLOW_EXF
       IF ( useEXF ) THEN
       DO bj = myByLo(myThid), myByHi(myThid)
        DO bi = myBxLo(myThid), myBxHi(myThid)
         DO j=1-Oly,sNy+Oly
          DO i=1-Olx,sNx+Olx
C     sh = max(wspeed,umin), with default umin=0.5m/s
C           pisvel(i,j,bi,bj)=(0.31 _d 0*wspeed(i,j,bi,bj)**2)/3.6 _d 5
           pisvel(i,j,bi,bj)=(0.31 _d 0*sh(i,j,bi,bj)**2)/3.6 _d 5
          ENDDO
         ENDDO
        ENDDO
       ENDDO
       ELSE
#else
       IF (.TRUE.) THEN
#endif /* ALLOW_EXF */
       DO bj = myByLo(myThid), myByHi(myThid)
        DO bi = myBxLo(myThid), myBxHi(myThid)
         DO j=1-Oly,sNy+Oly
          DO i=1-Olx,sNx+Olx
           IF ( CFC_windFile .NE. ' '  ) THEN
             wind = bWght*wind0(i,j,bi,bj)
     &                           +aWght*wind1(i,j,bi,bj)
           ELSE
             wind = 5. _d 0*maskC(i,j,1,bi,bj)
           ENDIF
c calculate piston velocity
c QQ: note - we should have wind speed variance in here
c following Wannikof (1992)
           pisvel(i,j,bi,bj)=(0.31 _d 0*wind**2)/3.6 _d 5
          ENDDO
         ENDDO
        ENDDO
       ENDDO
       ENDIF
C
       DO bj = myByLo(myThid), myByHi(myThid)
        DO bi = myBxLo(myThid), myBxHi(myThid)
         DO j=1-Oly,sNy+Oly
          DO i=1-Olx,sNx+Olx
           IF ( CFC_atmospFile .NE. ' '  ) THEN
             ATMOSP(i,j,bi,bj) = bWght*atmosp0(i,j,bi,bj)
     &                          +aWght*atmosp1(i,j,bi,bj)
           ELSE
             ATMOSP(i,j,bi,bj) = maskC(i,j,1,bi,bj)
           ENDIF
          ENDDO
         ENDDO
        ENDDO
       ENDDO
#ifdef ALLOW_SEAICE
       IF (useSEAICE) THEN
       DO bj = myByLo(myThid), myByHi(myThid)
        DO bi = myBxLo(myThid), myBxHi(myThid)
         DO j=1-Oly,sNy+Oly
          DO i=1-Olx,sNx+Olx
           FIce(I,J,bi,bj) = AREA(I,J,bi,bj)
          ENDDO
         ENDDO
        ENDDO
       ENDDO
       ELSE
#else
       IF (.TRUE.) THEN
#endif /* ALLOW_SEAICE */
       DO bj = myByLo(myThid), myByHi(myThid)
        DO bi = myBxLo(myThid), myBxHi(myThid)
         DO j=1-Oly,sNy+Oly
          DO i=1-Olx,sNx+Olx
           IF ( CFC_iceFile .NE. ' '  ) THEN
            FIce(i,j,bi,bj)    = bWght*ice0(i,j,bi,bj)
     &                          +aWght*ice1(i,j,bi,bj)
           ELSE
            FIce(i,j,bi,bj) =0. _d 0
           ENDIF
          ENDDO
         ENDDO
        ENDDO
       ENDDO
       ENDIF

C endif for periodicForcing
      ENDIF

      RETURN
      END

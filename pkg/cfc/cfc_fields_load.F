#include "GCHEM_OPTIONS.h"
#ifdef ALLOW_SEAICE
# include "SEAICE_OPTIONS.h"
#endif /* ALLOW_SEAICE */
#ifdef ALLOW_THSICE
# include "THSICE_OPTIONS.h"
#endif /* ALLOW_THSICE */
#ifdef ALLOW_AUTODIFF
# include "AUTODIFF_OPTIONS.h"
#endif

CBOP
C     !ROUTINE: CFC12_FORCING
C     !INTERFACE:
      SUBROUTINE CFC_FIELDS_LOAD (
     I           myTime, myIter, myThid )

C     !DESCRIPTION:
C     *==========================================================*
C     | SUBROUTINE CFC_FIELDS_LOAD
C     *==========================================================*

C     !USES:
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
# include "SEAICE_SIZE.h"
# include "SEAICE.h"
#endif
#ifdef ALLOW_THSICE
# include "THSICE_VARS.h"
#endif /* ALLOW_THSICE */

C     !INPUT/OUTPUT PARAMETERS:
C     myTime     :: current time in simulation
C     myIter     :: current iteration number
C     myThid     :: my Thread Id number
      _RL myTime
      INTEGER myIter
      INTEGER myThid

C     !LOCAL VARIABLES:
      INTEGER intimeP, intime0, intime1
       INTEGER bi, bj, i, j
      _RL aWght, bWght
      _RL locWind(1-OLx:sNx+OLx,1-OLy:sNy+OLy)
CEOP

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
     I                   deltaTClock, myTime, myThid )

       bi = myBxLo(myThid)
       bj = myByLo(myThid)
#ifdef ALLOW_DEBUG
       IF ( debugLevel.GE.debLevB ) THEN
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
        IF ( debugLevel.GE.debLevZero ) THEN
         _BEGIN_MASTER(myThid)
         WRITE(standardMessageUnit,'(A,I10,A,2(2I5,A))')
     &    ' CFC_FIELDS_LOAD, it=', myIter,
     &    ' : Reading new data, i0,i1=', intime0, intime1,
     &    ' (prev=', intimeP, CFC_ldRec(bi,bj), ' )'
         _END_MASTER(myThid)
        ENDIF

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
        IF ( CFC_iceFile .NE. ' ' .AND.
     &       .NOT.useSEAICE .AND. .NOT.useThSIce ) THEN
         CALL READ_REC_XY_RS( CFC_iceFile,ice0,intime0,
     &       myIter,myThid )
         CALL READ_REC_XY_RS( CFC_iceFile,ice1,intime1,
     &       myIter,myThid )
        ENDIF

        IF ( .NOT.useEXF ) THEN
         _EXCH_XY_RS(wind0, myThid )
         _EXCH_XY_RS(wind1, myThid )
        ENDIF
         _EXCH_XY_RS(atmosp0, myThid )
         _EXCH_XY_RS(atmosp1, myThid )
        IF ( .NOT.useSEAICE .AND. .NOT.useThSIce ) THEN
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

C endif for periodicForcing
      ENDIF

      DO bj = myByLo(myThid), myByHi(myThid)
       DO bi = myBxLo(myThid), myBxHi(myThid)

#ifdef ALLOW_EXF
        IF ( useEXF ) THEN
          DO j=1-OLy,sNy+OLy
           DO i=1-OLx,sNx+OLx
C     sh = max(wspeed,umin), with default umin=0.5m/s
c            locWind(i,j) = wspeed(i,j,bi,bj)
             locWind(i,j) = sh(i,j,bi,bj)
           ENDDO
          ENDDO
        ELSEIF ( CFC_forcingCycle.GT.zeroRL
     &           .AND. CFC_windFile.NE.' ' ) THEN
#else
        IF     ( CFC_forcingCycle.GT.zeroRL
     &           .AND. CFC_windFile.NE.' ' ) THEN
#endif /* ALLOW_EXF */
          DO j=1-OLy,sNy+OLy
           DO i=1-OLx,sNx+OLx
             locWind(i,j) = bWght*wind0(i,j,bi,bj)
     &                    + aWght*wind1(i,j,bi,bj)
           ENDDO
          ENDDO
        ELSE
          DO j=1-OLy,sNy+OLy
           DO i=1-OLx,sNx+OLx
             locWind(i,j) = 5. _d 0*maskC(i,j,1,bi,bj)
           ENDDO
          ENDDO
        ENDIF
c calculate piston velocity
c QQ: note - we should have wind speed variance in here
c following Wannikof (1992)
        DO j=1-OLy,sNy+OLy
          DO i=1-OLx,sNx+OLx
            pisVel(i,j,bi,bj)=(0.31 _d 0*locWind(i,j)**2)/3.6 _d 5
          ENDDO
        ENDDO

        IF ( CFC_forcingCycle.GT.zeroRL
     &       .AND. CFC_atmospFile.NE.' ' ) THEN
          DO j=1-OLy,sNy+OLy
           DO i=1-OLx,sNx+OLx
             ATMOSP(i,j,bi,bj) = bWght*atmosp0(i,j,bi,bj)
     &                         + aWght*atmosp1(i,j,bi,bj)
           ENDDO
          ENDDO
        ELSE
          DO j=1-OLy,sNy+OLy
           DO i=1-OLx,sNx+OLx
             ATMOSP(i,j,bi,bj) = maskC(i,j,1,bi,bj)
           ENDDO
          ENDDO
        ENDIF

        IF ( useThSIce ) THEN
#ifdef ALLOW_THSICE
          DO j=1-OLy,sNy+OLy
           DO i=1-OLx,sNx+OLx
             FIce(i,j,bi,bj) = iceMask(i,j,bi,bj)
           ENDDO
          ENDDO
#endif /* ALLOW_THSICE */
        ELSEIF ( useSEAICE ) THEN
#ifdef ALLOW_SEAICE
          DO j=1-OLy,sNy+OLy
           DO i=1-OLx,sNx+OLx
             FIce(i,j,bi,bj) = AREA(i,j,bi,bj)
           ENDDO
          ENDDO
#endif /* ALLOW_SEAICE */
        ELSEIF ( CFC_forcingCycle.GT.zeroRL
     &           .AND. CFC_iceFile.NE.' ' ) THEN
         DO j=1-OLy,sNy+OLy
          DO i=1-OLx,sNx+OLx
             FIce(i,j,bi,bj) = bWght*ice0(i,j,bi,bj)
     &                       + aWght*ice1(i,j,bi,bj)
          ENDDO
         ENDDO
        ELSE
         DO j=1-OLy,sNy+OLy
          DO i=1-OLx,sNx+OLx
             FIce(i,j,bi,bj) = 0. _d 0
          ENDDO
         ENDDO
        ENDIF

C--   end bi.bj loops
       ENDDO
      ENDDO

      RETURN
      END

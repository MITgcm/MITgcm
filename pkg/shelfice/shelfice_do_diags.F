C $Header: /u/gcmpack/MITgcm/pkg/shelfice/Attic/shelfice_do_diags.F,v 1.2 2006/02/10 10:00:42 mlosch Exp $
C $Name:  $

#include "SHELFICE_OPTIONS.h"

      SUBROUTINE SHELFICE_DO_DIAGS( myTime, myIter, myThid )
C     /==========================================================\
C     | SUBROUTINE SHELFICE_DO_DIAGS                               |
C     | o Do SHELFICE diagnostic output.                           |
C     \==========================================================/
      IMPLICIT NONE

C     === Global variables ===
#include "SIZE.h"
#include "EEPARAMS.h"
#include "PARAMS.h"
#include "SHELFICE_DIAGS.h"
#include "SHELFICE.h"

C     == Routine arguments ==
C     myTime        - Current time of simulation ( s )
C     myIter        - Iteration number
C     myThid        - Number of this instance of SHELFICE_DO_DIAGS
      _RL     myTime
      INTEGER myIter
      INTEGER myThid

C     == Local variables ==
      CHARACTER*(MAX_LEN_MBUF) suff
      LOGICAL  DIFFERENT_MULTIPLE
      EXTERNAL DIFFERENT_MULTIPLE
      INTEGER i, j, k, bi, bj
      INTEGER thisdate(4), prevdate(4)
      LOGICAL dumpFiles
      CHARACTER*(1) pf

      IF ( writeBinaryPrec .EQ. precFloat64 ) THEN
        pf(1:1) = 'D'
      ELSE
        pf(1:1) = 'R'
      ENDIF
      
      IF (SHELFICEwriteState) THEN

       IF ( DIFFERENT_MULTIPLE(SHELFICE_dumpFreq,myTime,deltaTClock)
     &      ) THEN
            
#ifdef ALLOW_MNC
        IF ( useMNC .AND. SHELFICE_dump_mnc ) THEN
         CALL MNC_CW_SET_UDIM('shelfice', -1, myThid)
         CALL MNC_CW_RL_W_S('D','shelfice',0,0,'T', myTime, myThid)
         CALL MNC_CW_SET_UDIM('shelfice', 0, myThid)
         CALL MNC_CW_I_W_S('I','shelfice',0,0,'iter', myIter, myThid)
         CALL MNC_CW_RL_W_S('D','shelfice',0,0,'model_time',
     &        myTime,myThid)
         CALL MNC_CW_RS_W(pf,'shelfice',0,0,'fwFlux',
     &        shelfIceFreshWaterFlux,myThid)
         CALL MNC_CW_RS_W(pf,'shelfice',0,0,'heatFlux',
     &        shelfIceHeatFlux,myThid)
        ENDIF
#endif
        IF (SHELFICE_dump_mdsio) THEN
         WRITE(suff,'(I10.10)') myIter
         _BARRIER
         _BEGIN_MASTER( myThid )
         CALL WRITE_FLD_XY_RS( 'SHICE_fwFlux.',suff,
     &        shelficeFreshWaterFlux,myIter,myThid)
         CALL WRITE_FLD_XY_RS( 'SHICE_heatFlux.',suff,
     &        shelficeHeatFlux,myIter,myThid)
         _END_MASTER( myThid )
         _BARRIER
        ENDIF
        
       ENDIF
      ENDIF

C----------------------------------------------------------------
C     Do SHELFICE time averaging.
C----------------------------------------------------------------

#ifdef ALLOW_TIMEAVE

      IF ( SHELFICE_taveFreq .GT. 0 ) THEN
C--   Time-cumulations
      DO bj = myByLo(myThid), myByHi(myThid)
       DO bi = myBxLo(myThid), myBxHi(myThid)
        DO j=1,sNy
         DO i=1,sNx
          shelfice_FrshFluxtave(i,j,1,bi,bj)=
     &         shelfice_frshFluxtave(i,j,1,bi,bj)
     &         + shelficeFreshWaterFlux(i,j,bi,bj) *deltaTclock
          shelfice_heatFluxtave(i,j,1,bi,bj) =
     &         shelfice_heatFluxtave(i,j,1,bi,bj) 
     &         + shelficeHeatFlux(i,j,bi,bj)  *deltaTclock
         ENDDO
        ENDDO
        SHELFICE_TimeAve(bi,bj)=SHELFICE_TimeAve(bi,bj)+deltaTclock
       ENDDO
      ENDDO

C     Dump files and restart average computation if needed
      dumpFiles = .FALSE.
      IF ( myIter .NE. nIter0 ) THEN
       IF ( DIFFERENT_MULTIPLE(SHELFICE_taveFreq,myTime,deltaTClock) )
     &      dumpFiles = .TRUE.
#ifdef ALLOW_CAL
       IF ( calendarDumps .AND. (
     &      (SHELFICE_taveFreq.GE. 2592000.AND. 
     &       SHELFICE_taveFreq.LE. 2678400).OR.
     &      (SHELFICE_taveFreq.GE.31104000.AND.
     &       SHELFICE_taveFreq.LE.31968000)))
     &      THEN
C--   Convert approximate months (30-31 days) and years (360-372 days)
C     to exact calendar months and years.
C-    First determine calendar dates for this and previous time step.
        call cal_GetDate( myiter  ,mytime            ,thisdate,mythid )
        call cal_GetDate( myiter-1,mytime-deltaTClock,prevdate,mythid )
        dumpFiles = .FALSE.
C-    Monthly SHELFICE_taveFreq:
        IF(  SHELFICE_taveFreq.GE. 2592000 .AND. 
     &       SHELFICE_taveFreq.LE. 2678400 .AND. 
     &       (thisdate(1)-prevdate(1)).GT.50   ) dumpFiles = .TRUE.
C-    Yearly  SHELFICE_taveFreq:
        IF(  SHELFICE_taveFreq.GE.31104000 .AND. 
     &       SHELFICE_taveFreq.LE.31968000 .AND.
     &       (thisdate(1)-prevdate(1)).GT.5000 ) dumpFiles = .TRUE.
       ENDIF
#endif
      ENDIF
      
      IF (dumpFiles) THEN
C      Normalize by integrated time
       DO bj = myByLo(myThid), myByHi(myThid)
        DO bi = myBxLo(myThid), myBxHi(myThid)
         CALL TIMEAVE_NORMALIZ(shelfice_frshFluxtave,SHELFICE_timeave,
     &        1,bi,bj,myThid)
         CALL TIMEAVE_NORMALIZ(shelfice_heatFluxtave,SHELFICE_timeave,
     &        1,bi,bj,myThid)
        ENDDO
       ENDDO

#ifdef ALLOW_MNC
       IF (useMNC .AND. SHELFICE_tave_mnc) THEN
        CALL MNC_CW_SET_UDIM('shelfice_tave', -1, myThid)
        CALL MNC_CW_RL_W_S('D','shelfice_tave',0,0,'T', myTime, myThid)
        CALL MNC_CW_SET_UDIM('shelfice_tave', 0, myThid)
        CALL MNC_CW_I_W_S('I','shelfice_tave',0,0,'iter', myIter,
     &       myThid)
C        CALL MNC_CW_RL_W_S('D','shelfice_tave',0,0,'model_time',
C     &        myTime,myThid)
        CALL MNC_CW_RL_W(pf,'shelfice_tave',0,0,
     &       'fwFluxtave',Shelfice_FrshFluxtave,myThid)
        CALL MNC_CW_RL_W(pf,'shelfice_tave',0,0,
     &       'heatFluxtave',Shelfice_HeatFluxtave,myThid)
       ENDIF
#endif
       IF (SHELFICE_tave_mdsio) THEN
        WRITE(suff,'(I10.10)') myIter
        _BARRIER
        _BEGIN_MASTER( myThid )
        CALL WRITE_FLD_XY_RL('SHICE_fwFluxtave.',suff,
     &       shelfice_frshFluxtave,myIter,myThid)
        CALL WRITE_FLD_XY_RL('SHICE_heatFluxtave.',suff,
     &       shelfice_heatFluxtave ,myIter,myThid)
        _END_MASTER( myThid )
        _BARRIER
       ENDIF
       
C      Reset averages to zero
       DO bj = myByLo(myThid), myByHi(myThid)
        DO bi = myBxLo(myThid), myBxHi(myThid)
         CALL TIMEAVE_RESET(shelfice_frshFluxtave,1,bi,bj,myThid)
         CALL TIMEAVE_RESET(shelfice_heatFluxtave ,1,bi,bj,myThid)
         SHELFICE_TimeAve(bi,bj) = 0. _d 0
        ENDDO
       ENDDO
       
      ENDIF
C     ( SHELFICE_taveFreq .GT. 0 )
      ENDIF
#endif /* ALLOW_TIMEAVE */

      RETURN
      END

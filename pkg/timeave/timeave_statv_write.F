C $Header: /u/gcmpack/MITgcm/pkg/timeave/timeave_statv_write.F,v 1.34 2005/11/08 23:05:45 cnh Exp $
C $Name:  $
#include "TIMEAVE_OPTIONS.h"

C---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|
CBOP
C     !ROUTINE: TIMEAVE_STATV_WRITE

C     !INTERFACE:
      SUBROUTINE TIMEAVE_STATV_WRITE(myTime, myIter, myThid)

C     !DESCRIPTION:
C     At the end of average period, write the time-average
C     state-variables on file ; then reset for next period

C     !USES:
      IMPLICIT NONE
#include "SIZE.h"
#include "EEPARAMS.h"
#include "PARAMS.h"
#ifdef ALLOW_MNC
#include "MNC_PARAMS.h"
#endif
#include "DYNVARS.h"
#include "GRID.h"
#include "TIMEAVE_STATV.h"
      LOGICAL  DIFFERENT_MULTIPLE
      EXTERNAL DIFFERENT_MULTIPLE
      INTEGER  IO_ERRCOUNT
      EXTERNAL IO_ERRCOUNT

C     !INPUT PARAMETERS:
C     myThid - Thread number for this instance of the routine.
C     myIter - Iteration number
C     myTime - Current time of simulation ( s )
      INTEGER myThid
      INTEGER myIter
      _RL     myTime
CEOP

#ifdef ALLOW_TIMEAVE 
C     !LOCAL VARIABLES:
C     suff         :: Hold suffix part of a filename
C     TimeAve      :: total time over average
C     useVariableK :: T when vertical diffusion is not constant
      LOGICAL useVariableK
      CHARACTER*(MAX_LEN_FNAM) suff
      INTEGER bi,bj
      INTEGER beginIOErrCount
      INTEGER endIOErrCount
      CHARACTER*(MAX_LEN_MBUF) msgBuf
      LOGICAL dumpFiles
#ifdef ALLOW_MNC
      CHARACTER*(1) pf
#endif
#ifdef ALLOW_CAL
      INTEGER thisdate(4), prevdate(4)
#endif

C     Final Time Averages and Dump Files if needed
      dumpFiles = .FALSE.
      IF ( DIFFERENT_MULTIPLE(taveFreq,myTime,deltaTClock) )
     &     dumpFiles = .TRUE.
#ifdef ALLOW_CAL
      IF ( calendarDumps .AND. (
     &       ( taveFreq.GE. 2592000 .AND. taveFreq.LE. 2678400 ) .OR.
     &       ( taveFreq.GE.31104000 .AND. taveFreq.LE.31968000 ))) THEN
C--   Convert approximate months (30-31 days) and years (360-372 days)
C     to exact calendar months and years.
C-    First determine calendar dates for this and previous time step.
         call cal_GetDate( myiter  ,mytime            ,thisdate,mythid )
         call cal_GetDate( myiter-1,mytime-deltaTClock,prevdate,mythid )
         dumpFiles = .FALSE.
C-    Monthly taveFreq:
         IF( taveFreq.GE. 2592000 .AND. taveFreq.LE. 2678400 .AND.
     &        (thisdate(1)-prevdate(1)).GT.50   ) dumpFiles = .TRUE.
C-    Yearly  taveFreq:
         IF( taveFreq.GE.31104000 .AND. taveFreq.LE.31968000 .AND.
     &        (thisdate(1)-prevdate(1)).GT.5000 ) dumpFiles = .TRUE.
      ENDIF
#endif

      IF (dumpFiles) THEN
        useVariableK = useKPP .OR. usePP81 .OR. useMY82 .OR. useGGL90
     &      .OR. useGMredi .OR. ivdc_kappa.NE.0.
        WRITE(suff,'(I10.10)') myIter

        DO bj = myByLo(myThid), myByHi(myThid)
        DO bi = myBxLo(myThid), myBxHi(myThid)

C         Normalize by integrated time
          CALL TIMEAVE_NORMALIZ(uFluxtave,TimeAve_full,1 ,bi,bj,myThid)
          CALL TIMEAVE_NORMALIZ(vFluxtave,TimeAve_full,1 ,bi,bj,myThid)
          CALL TIMEAVE_NORMALIZ(tFluxtave,TimeAve_full,1 ,bi,bj,myThid)
          CALL TIMEAVE_NORMALIZ(sFluxtave,TimeAve_full,1 ,bi,bj,myThid)
          CALL TIMEAVE_NORMALIZ(etatave,  TimeAve_half,1 ,bi,bj,myThid)
          CALL TIMEAVE_NORMALIZ(thetatave,TimeAve_half,Nr,bi,bj,myThid)
          CALL TIMEAVE_NORMALIZ(salttave, TimeAve_half,Nr,bi,bj,myThid)
          CALL TIMEAVE_NORMALIZ(uVeltave, TimeAve_half,Nr,bi,bj,myThid)
          CALL TIMEAVE_NORMALIZ(vVeltave, TimeAve_half,Nr,bi,bj,myThid)
          CALL TIMEAVE_NORMALIZ(wVeltave, TimeAve_half,Nr,bi,bj,myThid)
          CALL TIMEAVE_NORMALIZ(phiHydLowtave,TimeAve_full,1,
     &         bi,bj,myThid)
          CALL TIMEAVE_NORMALIZ(UTtave,   TimeAve_half,Nr,bi,bj,myThid)
          CALL TIMEAVE_NORMALIZ(VTtave,   TimeAve_half,Nr,bi,bj,myThid)
          CALL TIMEAVE_NORMALIZ(WTtave,   TimeAve_half,Nr,bi,bj,myThid)
          CALL TIMEAVE_NORMALIZ(UStave,   TimeAve_half,Nr,bi,bj,myThid)
          CALL TIMEAVE_NORMALIZ(VStave,   TimeAve_half,Nr,bi,bj,myThid)
          CALL TIMEAVE_NORMALIZ(WStave,   TimeAve_half,Nr,bi,bj,myThid)
          CALL TIMEAVE_NORMALIZ(Eta2tave, TimeAve_half,1 ,bi,bj,myThid)
          CALL TIMEAVE_NORMALIZ(TTtave,   TimeAve_half,Nr,bi,bj,myThid)
          CALL TIMEAVE_NORMALIZ(UUtave,   TimeAve_half,Nr,bi,bj,myThid)
          CALL TIMEAVE_NORMALIZ(VVtave,   TimeAve_half,Nr,bi,bj,myThid)
          CALL TIMEAVE_NORMALIZ(UVtave,   TimeAve_half,Nr,bi,bj,myThid)
C         CALL TIMEAVE_NORMALIZ(KEtave,   TimeAve_half,Nr,bi,bj,myThid)
#ifdef NONLIN_FRSURF
C         Normalize by integrated time
          CALL TIMEAVE_NORMALIZ(hUtave,   TimeAve_half,Nr,bi,bj,myThid)
          CALL TIMEAVE_NORMALIZ(hVtave,   TimeAve_half,Nr,bi,bj,myThid)
C         CALL TIMEAVE_NORMALIZ(hFacCtave,TimeAve_half,Nr,bi,bj,myThid)
C         CALL TIMEAVE_NORMALIZ(hFacWtave,TimeAve_half,Nr,bi,bj,myThid)
C         CALL TIMEAVE_NORMALIZ(hFacStave,TimeAve_half,Nr,bi,bj,myThid)
#endif /* NONLIN_FRSURF */ 

          CALL TIMEAVE_NORMALIZ(TdiffRtave,TimeAve_full,Nr,
     &         bi,bj,myThid)
#ifdef ALLOW_MOM_VECINV
          CALL TIMEAVE_NORMALIZ(uZetatave,TimeAve_full,Nr,bi,bj,myThid)
          CALL TIMEAVE_NORMALIZ(vZetatave,TimeAve_full,Nr,bi,bj,myThid)
#endif
          CALL TIMEAVE_NORMALIZ(phiHydtave,TimeAve_full,Nr,
     &         bi,bj,myThid)
          CALL TIMEAVE_NORMALIZ(phiHydLow2Tave,TimeAve_full,1,
     &         bi,bj,myThid)
          CALL TIMEAVE_NORMALIZ(ConvectCountTave,TimeAve_full,Nr,
     &         bi,bj,myThid)
        ENDDO
        ENDDO

C       Write to files
        _BARRIER
C       _BEGIN_MASTER( myThid )

        IF (timeave_mdsio) THEN
          
C         Set IO "context" for writing state
#ifdef USE_DFILE
          CALL DFILE_SET_RW
          CALL DFILE_SET_CONT_ON_ERROR
#endif
C         Read IO error counter
          beginIOErrCount = IO_ERRCOUNT(myThid)
          CALL WRITE_FLD_XY_RL('ETAtave.' ,suff,etatave  ,myIter,myThid)
          CALL WRITE_FLD_XYZ_RL('Ttave.',suff,thetatave,myIter,myThid)
          CALL WRITE_FLD_XYZ_RL('Stave.',suff,salttave,myIter,myThid)
          CALL WRITE_FLD_XYZ_RL('uVeltave.',suff,uVeltave,myIter,myThid)
          CALL WRITE_FLD_XYZ_RL('vVeltave.',suff,vVeltave,myIter,myThid)
          CALL WRITE_FLD_XYZ_RL('wVeltave.',suff,wVeltave,myIter,myThid)
          CALL WRITE_FLD_XY_RL('Eta2tave.',suff,Eta2tave ,myIter,myThid)
          CALL WRITE_FLD_XYZ_RL('UTtave.',suff,UTtave,myIter,myThid)
          CALL WRITE_FLD_XYZ_RL('VTtave.',suff,VTtave,myIter,myThid)
          CALL WRITE_FLD_XYZ_RL('WTtave.',suff,WTtave,myIter,myThid)
          CALL WRITE_FLD_XYZ_RL('UStave.',suff,UStave,myIter,myThid)
          CALL WRITE_FLD_XYZ_RL('VStave.',suff,VStave,myIter,myThid)
          CALL WRITE_FLD_XYZ_RL('WStave.',suff,WStave,myIter,myThid)
          CALL WRITE_FLD_XYZ_RL('TTtave.',suff,TTtave,myIter,myThid)
          CALL WRITE_FLD_XYZ_RL('UUtave.',suff,UUtave,myIter,myThid)
          CALL WRITE_FLD_XYZ_RL('VVtave.',suff,VVtave,myIter,myThid)
          CALL WRITE_FLD_XYZ_RL('UVtave.',suff,UVtave,myIter,myThid)
C         CALL WRITE_FLD_XYZ_RL('KEtave.',suff,KEtave,myIter,myThid)
          
          IF (useVariableK)
     &         CALL WRITE_FLD_XYZ_RL('Tdiftave.',suff,TdiffRtave,
     &         myIter,myThid)
#ifdef ALLOW_MOM_VECINV
          IF (vectorInvariantMomentum) THEN
            CALL WRITE_FLD_XYZ_RL('uZtave.',suff,uZetatave,
     &           myIter,myThid)
            CALL WRITE_FLD_XYZ_RL('vZtave.',suff,vZetatave,
     &           myIter,myThid)
          ENDIF
#endif /* ALLOW_MOM_VECINV */
          CALL WRITE_FLD_XYZ_RL('PhHytave.',suff,phiHydtave,
     &         myIter,myThid)
          IF ( fluidIsWater ) THEN
            CALL WRITE_FLD_XY_RL('PHLtave.',suff,phiHydLowtave,
     &           myIter,myThid)
            CALL WRITE_FLD_XY_RL('uFluxtave.',suff,uFluxtave,
     &           myIter,myThid)
            CALL WRITE_FLD_XY_RL('vFluxtave.',suff,vFluxtave,
     &           myIter,myThid)
            CALL WRITE_FLD_XY_RL('tFluxtave.',suff,tFluxtave,
     &           myIter,myThid)
            CALL WRITE_FLD_XY_RL('sFluxtave.',suff,sFluxtave,
     &           myIter,myThid)
            CALL WRITE_FLD_XY_RL('PHL2tave.',suff,phiHydLow2tave,
     &           myIter,myThid)
            CALL WRITE_FLD_XYZ_RL('Convtave.',suff,ConvectCountTave,
     &           myIter,myThid)
          ENDIF
          
#ifdef NONLIN_FRSURF
          CALL WRITE_FLD_XYZ_RL('hUtave.',suff,hUtave,myIter,myThid)
          CALL WRITE_FLD_XYZ_RL('hVtave.',suff,hVtave,myIter,myThid)
C         CALL WRITE_FLD_XYZ_RL('hFacCtave.',suff,hFacCtave,myIter,myThid)
C         CALL WRITE_FLD_XYZ_RL('hFacWtave.',suff,hFacWtave,myIter,myThid)
C         CALL WRITE_FLD_XYZ_RL('hFacStave.',suff,hFacStave,myIter,myThid)
#endif /* NONLIN_FRSURF */ 
          
C         Reread IO error counter
          endIOErrCount = IO_ERRCOUNT(myThid)

C         Check for IO errors
          IF ( endIOErrCount .NE. beginIOErrCount ) THEN
            WRITE(msgBuf,'(A)')  'S/R WRITE_TIME_AVERAGES'
            CALL PRINT_ERROR( msgBuf, 1 )
            WRITE(msgBuf,'(A)')  'Error writing out data'
            CALL PRINT_ERROR( msgBuf, 1 )
            WRITE(msgBuf,'(A,I10)') 'Timestep ',myIter
            CALL PRINT_ERROR( msgBuf, 1 )
          ELSE
            WRITE(msgBuf,'(A,I10)')  
     &           '// Time-average data written, t-step', myIter
            CALL PRINT_MESSAGE( msgBuf, standardMessageUnit, 
     &           SQUEEZE_RIGHT, 1 )
            WRITE(msgBuf,'(A)')  ' '
            CALL PRINT_MESSAGE( msgBuf, standardMessageUnit, 
     &           SQUEEZE_RIGHT, 1 )
          ENDIF
          
        ENDIF

#ifdef ALLOW_MNC        
        IF (useMNC .AND. timeave_mnc) THEN
          
          IF ( writeBinaryPrec .EQ. precFloat64 ) THEN
            pf(1:1) = 'D'
          ELSE
            pf(1:1) = 'R'
          ENDIF

          CALL MNC_CW_SET_UDIM('tave', -1, myThid)
          CALL MNC_CW_RL_W_S('D','tave',0,0,'T', myTime, myThid)
          CALL MNC_CW_SET_UDIM('tave', 0, myThid)
          CALL MNC_CW_I_W_S('I','tave',0,0,'iter', myIter, myThid)
C         CALL MNC_CW_RL_W_S('D','tave',0,0,'model_time',myTime,myThid)
          CALL MNC_CW_RL_W(pf,'tave',0,0,'ETAtave',etatave,myThid)
C         EH3: something is wrong here...
C         CALL MNC_CW_RL_W(pf,'tave',0,0,'Eta2tave',Eta2tave,myThid)
          CALL MNC_CW_RL_W(pf,'tave',0,0,'Ttave',thetatave,myThid)
          CALL MNC_CW_RL_W(pf,'tave',0,0,'Stave',salttave,myThid)
          CALL MNC_CW_RL_W(pf,'tave',0,0,'uVeltave',uVeltave,myThid)
          CALL MNC_CW_RL_W(pf,'tave',0,0,'vVeltave',vVeltave,myThid)
          CALL MNC_CW_RL_W(pf,'tave',0,0,'wVeltave',wVeltave,myThid)
          CALL MNC_CW_RL_W(pf,'tave',0,0,'UTtave',UTtave,myThid)
          CALL MNC_CW_RL_W(pf,'tave',0,0,'VTtave',VTtave,myThid)
          CALL MNC_CW_RL_W(pf,'tave',0,0,'WTtave',WTtave,myThid)
          CALL MNC_CW_RL_W(pf,'tave',0,0,'UStave',UStave,myThid)
          CALL MNC_CW_RL_W(pf,'tave',0,0,'VStave',VStave,myThid)
          CALL MNC_CW_RL_W(pf,'tave',0,0,'WStave',WStave,myThid)
          CALL MNC_CW_RL_W(pf,'tave',0,0,'TTtave',TTtave,myThid)
          CALL MNC_CW_RL_W(pf,'tave',0,0,'UUtave',UUtave,myThid)
          CALL MNC_CW_RL_W(pf,'tave',0,0,'VVtave',VVtave,myThid)
          CALL MNC_CW_RL_W(pf,'tave',0,0,'UVtave',UVtave,myThid)
C         CALL MNC_CW_RL_W(pf,'tave',0,0,'KEtave',KEtave,myThid)
          IF (useVariableK) THEN
            CALL MNC_CW_RL_W(pf,'tave',0,0,'Tdiftave',
     &           TdiffRtave,myThid)
          ENDIF
#ifdef ALLOW_MOM_VECINV
          IF (vectorInvariantMomentum) THEN
            CALL MNC_CW_RL_W(pf,'tave',0,0,'uZtave',uZetatave,myThid)
            CALL MNC_CW_RL_W(pf,'tave',0,0,'vZtave',vZetatave,myThid)
          ENDIF
#endif /* ALLOW_MOM_VECINV */
          CALL MNC_CW_RL_W(pf,'tave',0,0,'PhHytave',
     &         phiHydtave,myThid)
          IF ( fluidIsWater ) THEN
            CALL MNC_CW_RL_W(pf,'tave',0,0,'PHLtave',
     &           phiHydLowtave,myThid)
            CALL MNC_CW_RL_W(pf,'tave',0,0,'PHL2tave',
     &           phiHydLow2tave,myThid)
            CALL MNC_CW_RL_W(pf,'tave',0,0,'Convtave',
     &           ConvectCountTave,myThid)
            CALL MNC_CW_RL_W(pf,'tave',0,0,'uFluxtave',
     &           uFluxtave,myThid)
            CALL MNC_CW_RL_W(pf,'tave',0,0,'vFluxtave',
     &           vFluxtave,myThid)
            CALL MNC_CW_RL_W(pf,'tave',0,0,'tFluxtave',
     &           tFluxtave,myThid)
            CALL MNC_CW_RL_W(pf,'tave',0,0,'sFluxtave',
     &           sFluxtave,myThid)
          ENDIF

#ifdef NONLIN_FRSURF
          CALL MNC_CW_RL_W(pf,'tave',0,0,'hUtave',hUtave,myThid)
          CALL MNC_CW_RL_W(pf,'tave',0,0,'hVtave',hVtave,myThid)
C         CALL MNC_CW_RL_W(pf,'tave',0,0,'hFacCtave',hFacCtave,myThid)
C         CALL MNC_CW_RL_W(pf,'tave',0,0,'hFacWtave',hFacWtave,myThid)
C         CALL MNC_CW_RL_W(pf,'tave',0,0,'hFacStave',hFacStave,myThid)
#endif /* NONLIN_FRSURF */ 

        ENDIF
#endif /* ALLOW_MNC */

C       _END_MASTER( myThid )
        _BARRIER
        
        DO bj = myByLo(myThid), myByHi(myThid)
          DO bi = myBxLo(myThid), myBxHi(myThid)
            
C           Like before the 1rst iteration,
C           ==> call TIMEAVE_STATVARS with myIter=nIter0 :
C           1) Reset the averages to zero ;
C           2) Start to cumulate state-variables with Half time step.
            
            CALL TIMEAVE_STATVARS(myTime, nIter0, bi, bj, myThid)
            
          ENDDO
        ENDDO
        
      ENDIF
      
#endif /* ALLOW_TIMEAVE */

      RETURN
      END

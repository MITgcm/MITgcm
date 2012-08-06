C $Header: /u/gcmpack/MITgcm/pkg/timeave/timeave_statv_write.F,v 1.43 2012/08/06 16:56:59 jmc Exp $
C $Name:  $

#include "TIMEAVE_OPTIONS.h"

CBOP
C     !ROUTINE: TIMEAVE_STATV_WRITE

C     !INTERFACE:
      SUBROUTINE TIMEAVE_STATV_WRITE( myTime, myIter, myThid )

C     !DESCRIPTION:
C     At the end of average period, write the time-average
C     state-variables on file ; then reset for next period

C     !USES:
      IMPLICIT NONE
#include "SIZE.h"
#include "EEPARAMS.h"
#include "PARAMS.h"
#include "DYNVARS.h"
#include "GRID.h"
#include "TIMEAVE_STATV.h"
#ifdef ALLOW_MNC
#include "MNC_PARAMS.h"
#endif

C     !INPUT PARAMETERS:
C     myTime :: Current time of simulation ( s )
C     myIter :: Iteration number
C     myThid :: Thread number for this instance of the routine.
      _RL     myTime
      INTEGER myIter
      INTEGER myThid
CEOP

#ifdef ALLOW_TIMEAVE
C     !FUNCTIONS:
      LOGICAL  DIFFERENT_MULTIPLE
      EXTERNAL DIFFERENT_MULTIPLE
      INTEGER  IO_ERRCOUNT
      EXTERNAL IO_ERRCOUNT

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

C---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|

C     Final Time Averages and Dump Files if needed
      dumpFiles = DIFFERENT_MULTIPLE(taveFreq,myTime,deltaTClock)
#ifdef ALLOW_CAL
      IF ( useCAL ) THEN
         CALL CAL_TIME2DUMP( zeroRL, taveFreq, deltaTClock,
     U                       dumpFiles,
     I                       myTime, myIter, myThid )
      ENDIF
#endif

      IF (dumpFiles) THEN
        useVariableK = useKPP .OR. usePP81 .OR. useMY82 .OR. useGGL90
     &      .OR. useGMredi .OR. ivdc_kappa.NE.0.
        WRITE(suff,'(I10.10)') myIter

        DO bj = myByLo(myThid), myByHi(myThid)
         DO bi = myBxLo(myThid), myBxHi(myThid)

C         Normalize by integrated time
          CALL TIMEAVE_NORMALIZE(uFluxtave,timeAve_full,1 ,bi,bj,myThid)
          CALL TIMEAVE_NORMALIZE(vFluxtave,timeAve_full,1 ,bi,bj,myThid)
          CALL TIMEAVE_NORMALIZE(tFluxtave,timeAve_full,1 ,bi,bj,myThid)
          CALL TIMEAVE_NORMALIZE(sFluxtave,timeAve_full,1 ,bi,bj,myThid)
          CALL TIMEAVE_NORMALIZE(etatave,  timeAve_half,1 ,bi,bj,myThid)
          CALL TIMEAVE_NORMALIZE(thetatave,timeAve_half,Nr,bi,bj,myThid)
          CALL TIMEAVE_NORMALIZE(salttave, timeAve_half,Nr,bi,bj,myThid)
          CALL TIMEAVE_NORMALIZE(uVeltave, timeAve_half,Nr,bi,bj,myThid)
          CALL TIMEAVE_NORMALIZE(vVeltave, timeAve_half,Nr,bi,bj,myThid)
          CALL TIMEAVE_NORMALIZE(wVeltave, timeAve_half,Nr,bi,bj,myThid)
          CALL TIMEAVE_NORMALIZE(phiHydLowtave,timeAve_full,1,
     &         bi,bj,myThid)
          CALL TIMEAVE_NORMALIZE(UTtave,   timeAve_half,Nr,bi,bj,myThid)
          CALL TIMEAVE_NORMALIZE(VTtave,   timeAve_half,Nr,bi,bj,myThid)
          CALL TIMEAVE_NORMALIZE(WTtave,   timeAve_half,Nr,bi,bj,myThid)
          CALL TIMEAVE_NORMALIZE(UStave,   timeAve_half,Nr,bi,bj,myThid)
          CALL TIMEAVE_NORMALIZE(VStave,   timeAve_half,Nr,bi,bj,myThid)
          CALL TIMEAVE_NORMALIZE(WStave,   timeAve_half,Nr,bi,bj,myThid)
          CALL TIMEAVE_NORMALIZE(Eta2tave, timeAve_half,1 ,bi,bj,myThid)
          CALL TIMEAVE_NORMALIZE(TTtave,   timeAve_half,Nr,bi,bj,myThid)
          CALL TIMEAVE_NORMALIZE(UUtave,   timeAve_half,Nr,bi,bj,myThid)
          CALL TIMEAVE_NORMALIZE(VVtave,   timeAve_half,Nr,bi,bj,myThid)
          CALL TIMEAVE_NORMALIZE(UVtave,   timeAve_half,Nr,bi,bj,myThid)
C         CALL TIMEAVE_NORMALIZE(KEtave,   timeAve_half,Nr,bi,bj,myThid)
#ifdef NONLIN_FRSURF
C         Normalize by integrated time
          CALL TIMEAVE_NORMALIZE(hUtave,   timeAve_half,Nr,bi,bj,myThid)
          CALL TIMEAVE_NORMALIZE(hVtave,   timeAve_half,Nr,bi,bj,myThid)
C         CALL TIMEAVE_NORMALIZE(hFacCtave,timeAve_half,Nr,bi,bj,myThid)
C         CALL TIMEAVE_NORMALIZE(hFacWtave,timeAve_half,Nr,bi,bj,myThid)
C         CALL TIMEAVE_NORMALIZE(hFacStave,timeAve_half,Nr,bi,bj,myThid)
#endif /* NONLIN_FRSURF */

          CALL TIMEAVE_NORMALIZE(TdiffRtave,timeAve_full,Nr,
     &         bi,bj,myThid)
#ifdef ALLOW_MOM_VECINV
          CALL TIMEAVE_NORMALIZE(uZetatave,timeAve_full,Nr,bi,bj,myThid)
          CALL TIMEAVE_NORMALIZE(vZetatave,timeAve_full,Nr,bi,bj,myThid)
#endif
          CALL TIMEAVE_NORMALIZE(phiHydtave,timeAve_full,Nr,
     &         bi,bj,myThid)
          CALL TIMEAVE_NORMALIZE(phiHydLow2Tave,timeAve_full,1,
     &         bi,bj,myThid)
          CALL TIMEAVE_NORMALIZE(ConvectCountTave,timeAve_full,Nr,
     &         bi,bj,myThid)
         ENDDO
        ENDDO

C       Write to files
        _BARRIER

        IF (timeave_mdsio) THEN

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
C-          any thread that detects an error should report
            WRITE(msgBuf,'(A)')  'S/R WRITE_TIME_AVERAGES'
            CALL PRINT_ERROR( msgBuf, myThid )
            WRITE(msgBuf,'(A)')  'Error writing out data'
            CALL PRINT_ERROR( msgBuf, myThid )
            WRITE(msgBuf,'(A,I10)') 'Timestep ',myIter
            CALL PRINT_ERROR( msgBuf, myThid )
          ELSE
C-          normal case: 1 message is enough
            _BEGIN_MASTER( myThid )
            WRITE(msgBuf,'(A,I10)')
     &           '// Time-average data written, t-step', myIter
            CALL PRINT_MESSAGE( msgBuf, standardMessageUnit,
     &           SQUEEZE_RIGHT, myThid )
            WRITE(msgBuf,'(A)')  ' '
            CALL PRINT_MESSAGE( msgBuf, standardMessageUnit,
     &           SQUEEZE_RIGHT, myThid )
            _END_MASTER( myThid )
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
          CALL MNC_CW_RL_W(pf,'tave',0,0,'Eta2tave',Eta2tave,myThid)
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

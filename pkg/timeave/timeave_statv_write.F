C $Header: /u/gcmpack/MITgcm/pkg/timeave/timeave_statv_write.F,v 1.10 2003/10/23 07:14:49 dimitri Exp $
C $Name:  $
#include "TIMEAVE_OPTIONS.h"

      SUBROUTINE TIMEAVE_STATV_WRITE(myTime, myIter, myThid)
C     /==========================================================\
C     | SUBROUTINE TIMEAVE_STATV_WRITE                           |
C     | o At the end of average period, write the time-average   |
C     |   state-variables on file ; then reset for next period   |
C     \==========================================================/
      IMPLICIT NONE

C     == Global variables ===
#include "SIZE.h"
#include "EEPARAMS.h"
#include "PARAMS.h"
#include "DYNVARS.h"
#include "GRID.h"
#include "TIMEAVE_STATV.h"

      LOGICAL  DIFFERENT_MULTIPLE
      EXTERNAL DIFFERENT_MULTIPLE
      INTEGER  IO_ERRCOUNT
      EXTERNAL IO_ERRCOUNT

C     == Routine arguments ==
C     myThid - Thread number for this instance of the routine.
C     myIter - Iteration number
C     myTime - Current time of simulation ( s )
      INTEGER myThid
      INTEGER myIter
      _RL     myTime

#ifdef ALLOW_TIMEAVE 

C     == Local variables ==
C     suff - Hold suffix part of a filename
C     TimeAve - total time over average
C     useVariableK   = T when vertical diffusion is not constant
      LOGICAL useVariableK
      INTEGER prevPrec
      CHARACTER*(MAX_LEN_FNAM) suff
      INTEGER bi,bj,k
      INTEGER beginIOErrCount
      INTEGER endIOErrCount
      CHARACTER*(MAX_LEN_MBUF) msgBuf
 
C-    Final Time Averages and Dump Files if needed
      IF(DIFFERENT_MULTIPLE(taveFreq,myTime,myTime-deltaTClock) )THEN

       useVariableK = useKPP .OR. useGMredi .OR. ivdc_kappa.NE.0.
       WRITE(suff,'(I10.10)') myIter

       DO bj = myByLo(myThid), myByHi(myThid)
        DO bi = myBxLo(myThid), myBxHi(myThid)
C-     Normalize by integrated time
          CALL TIMEAVE_NORMALIZ(uFluxtave,TimeAve_half,1 ,bi,bj,myThid)
          CALL TIMEAVE_NORMALIZ(vFluxtave,TimeAve_half,1 ,bi,bj,myThid)
          CALL TIMEAVE_NORMALIZ(tFluxtave,TimeAve_half,1 ,bi,bj,myThid)
          CALL TIMEAVE_NORMALIZ(sFluxtave,TimeAve_half,1 ,bi,bj,myThid)
          CALL TIMEAVE_NORMALIZ(etatave,  TimeAve_half,1 ,bi,bj,myThid)
          CALL TIMEAVE_NORMALIZ(thetatave,TimeAve_half,Nr,bi,bj,myThid)
          CALL TIMEAVE_NORMALIZ(salttave, TimeAve_half,Nr,bi,bj,myThid)
          CALL TIMEAVE_NORMALIZ(uVeltave, TimeAve_half,Nr,bi,bj,myThid)
          CALL TIMEAVE_NORMALIZ(vVeltave, TimeAve_half,Nr,bi,bj,myThid)
          CALL TIMEAVE_NORMALIZ(wVeltave, TimeAve_half,Nr,bi,bj,myThid)
          CALL TIMEAVE_NORMALIZ(Eta2tave, TimeAve_half,1 ,bi,bj,myThid)
          CALL TIMEAVE_NORMALIZ(TTtave,   TimeAve_half,Nr,bi,bj,myThid)
          CALL TIMEAVE_NORMALIZ(UUtave,   TimeAve_half,Nr,bi,bj,myThid)
          CALL TIMEAVE_NORMALIZ(VVtave,   TimeAve_half,Nr,bi,bj,myThid)
          CALL TIMEAVE_NORMALIZ(UVtave,   TimeAve_half,Nr,bi,bj,myThid)
c         CALL TIMEAVE_NORMALIZ(KEtave,   TimeAve_half,Nr,bi,bj,myThid)
#ifdef NONLIN_FRSURF
C-     Normalize by integrated time
          CALL TIMEAVE_NORMALIZ(hUtave,   TimeAve_half,Nr,bi,bj,myThid)
          CALL TIMEAVE_NORMALIZ(hVtave,   TimeAve_half,Nr,bi,bj,myThid)
c         CALL TIMEAVE_NORMALIZ(hFacCtave,TimeAve_half,Nr,bi,bj,myThid)
c         CALL TIMEAVE_NORMALIZ(hFacWtave,TimeAve_half,Nr,bi,bj,myThid)
c         CALL TIMEAVE_NORMALIZ(hFacStave,TimeAve_half,Nr,bi,bj,myThid)
#endif /* NONLIN_FRSURF */ 
          CALL TIMEAVE_NORMALIZ(UTtave,   TimeAve_half,Nr,bi,bj,myThid)
          CALL TIMEAVE_NORMALIZ(VTtave,   TimeAve_half,Nr,bi,bj,myThid)
          CALL TIMEAVE_NORMALIZ(WTtave,   TimeAve_half,Nr,bi,bj,myThid)

          CALL TIMEAVE_NORMALIZ(TdiffRtave,TimeAve_full,Nr,
     &                            bi,bj,myThid)
#ifndef DISABLE_MOM_VECINV
          CALL TIMEAVE_NORMALIZ(uZetatave,TimeAve_full,Nr,bi,bj,myThid)
          CALL TIMEAVE_NORMALIZ(vZetatave,TimeAve_full,Nr,bi,bj,myThid)
#endif
          CALL TIMEAVE_NORMALIZ(phiHydtave,TimeAve_full,Nr,
     &                            bi,bj,myThid)
          CALL TIMEAVE_NORMALIZ(phiHydLowtave,TimeAve_full,1,
     &                            bi,bj,myThid)
          CALL TIMEAVE_NORMALIZ(phiHydLow2Tave,TimeAve_full,1,
     &                            bi,bj,myThid)
          CALL TIMEAVE_NORMALIZ(ConvectCountTave,TimeAve_full,Nr,
     &                            bi,bj,myThid)
        ENDDO
       ENDDO

C      Write to files
       _BARRIER
       _BEGIN_MASTER( myThid )

C--     Set IO "context" for writing state
#ifdef USE_DFILE
       CALL DFILE_SET_RW
       CALL DFILE_SET_CONT_ON_ERROR
#endif
C--    Read IO error counter
       beginIOErrCount = IO_ERRCOUNT(myThid)
c      prevPrec = writeBinaryPrec
c      writeBinaryPrec = precFloat32
       CALL WRITE_FLD_XY_RL('uFluxtave.',suff,uFluxtave,myIter,myThid)
       CALL WRITE_FLD_XY_RL('vFluxtave.',suff,vFluxtave,myIter,myThid)
       CALL WRITE_FLD_XY_RL('tFluxtave.',suff,tFluxtave,myIter,myThid)
       CALL WRITE_FLD_XY_RL('sFluxtave.',suff,sFluxtave,myIter,myThid)
       CALL WRITE_FLD_XY_RL('ETAtave.'  ,suff,etatave  ,myIter,myThid)
       CALL WRITE_FLD_XY_RL('Eta2tave.' ,suff,Eta2tave ,myIter,myThid)

       CALL WRITE_FLD_XYZ_RL('Ttave.',suff,thetatave,myIter,myThid)
       CALL WRITE_FLD_XYZ_RL('Stave.',suff,salttave,myIter,myThid)
       CALL WRITE_FLD_XYZ_RL('uVeltave.',suff,uVeltave,myIter,myThid)
       CALL WRITE_FLD_XYZ_RL('vVeltave.',suff,vVeltave,myIter,myThid)
       CALL WRITE_FLD_XYZ_RL('wVeltave.',suff,wVeltave,myIter,myThid)
       CALL WRITE_FLD_XYZ_RL('TTtave.',suff,TTtave,myIter,myThid)
       CALL WRITE_FLD_XYZ_RL('UUtave.',suff,UUtave,myIter,myThid)
       CALL WRITE_FLD_XYZ_RL('VVtave.',suff,VVtave,myIter,myThid)
       CALL WRITE_FLD_XYZ_RL('UVtave.',suff,UVtave,myIter,myThid)
c      CALL WRITE_FLD_XYZ_RL('KEtave.',suff,KEtave,myIter,myThid)
       CALL WRITE_FLD_XYZ_RL('UTtave.',suff,UTtave,myIter,myThid)
       CALL WRITE_FLD_XYZ_RL('VTtave.',suff,VTtave,myIter,myThid)
       CALL WRITE_FLD_XYZ_RL('WTtave.',suff,WTtave,myIter,myThid)

       IF (useVariableK)
     & CALL WRITE_FLD_XYZ_RL('Tdiftave.',suff,TdiffRtave,myIter,myThid)
#ifndef DISABLE_MOM_VECINV
       IF (vectorInvariantMomentum) THEN
         CALL WRITE_FLD_XYZ_RL('uZtave.',suff,uZetatave,myIter,myThid)
         CALL WRITE_FLD_XYZ_RL('vZtave.',suff,vZetatave,myIter,myThid)
       ENDIF
#endif
       CALL WRITE_FLD_XYZ_RL('PhHytave.',suff,phiHydtave,myIter,myThid)
       IF ( buoyancyRelation .NE. 'ATMOSPHERIC' ) THEN
       CALL WRITE_FLD_XY_RL('PHLtave.',suff,phiHydLowtave,
     &      myIter,myThid)
       CALL WRITE_FLD_XY_RL('PHL2tave.',suff,phiHydLow2tave,
     &      myIter,myThid)
       CALL WRITE_FLD_XYZ_RL('Convtave.',suff,ConvectCountTave,
     &                       myIter,myThid)
       ENDIF

#ifdef NONLIN_FRSURF
       CALL WRITE_FLD_XYZ_RL('hUtave.',suff,hUtave,myIter,myThid)
       CALL WRITE_FLD_XYZ_RL('hVtave.',suff,hVtave,myIter,myThid)
c      CALL WRITE_FLD_XYZ_RL('hFacCtave.',suff,hFacCtave,myIter,myThid)
c      CALL WRITE_FLD_XYZ_RL('hFacWtave.',suff,hFacWtave,myIter,myThid)
c      CALL WRITE_FLD_XYZ_RL('hFacStave.',suff,hFacStave,myIter,myThid)
#endif /* NONLIN_FRSURF */ 

c      writeBinaryPrec = prevPrec
C--    Reread IO error counter
       endIOErrCount = IO_ERRCOUNT(myThid)

C--    Check for IO errors
       IF ( endIOErrCount .NE. beginIOErrCount ) THEN
         WRITE(msgBuf,'(A)')  'S/R WRITE_TIME_AVERAGES'
         CALL PRINT_ERROR( msgBuf, 1 )
         WRITE(msgBuf,'(A)')  'Error writing out data'
         CALL PRINT_ERROR( msgBuf, 1 )
         WRITE(msgBuf,'(A,I10)') 'Timestep ',myIter
         CALL PRINT_ERROR( msgBuf, 1 )
       ELSE
         WRITE(msgBuf,'(A,I10)')  
     &    '// Time-average data written, t-step', myIter
         CALL PRINT_MESSAGE( msgBuf, standardMessageUnit, 
     &    SQUEEZE_RIGHT, 1 )
         WRITE(msgBuf,'(A)')  ' '
         CALL PRINT_MESSAGE( msgBuf, standardMessageUnit, 
     &    SQUEEZE_RIGHT, 1 )
       ENDIF

       _END_MASTER( myThid )
       _BARRIER

       DO bj = myByLo(myThid), myByHi(myThid)
        DO bi = myBxLo(myThid), myBxHi(myThid)

C-- Like before the 1rst iteration,
C  ==> call TIMEAVE_STATVARS with myIter=nIter0 :
C    1) Reset the averages to zero ;
C    2) Start to cumulate state-variables with Half time step.

         CALL TIMEAVE_STATVARS(myTime, nIter0, bi, bj, myThid)

        ENDDO
       ENDDO

      ENDIF
 
#endif /* ALLOW_TIMEAVE */

      RETURN
      END

C $Header: /u/gcmpack/MITgcm/pkg/aim/Attic/aim_write_diags.F,v 1.5 2001/05/29 14:01:37 adcroft Exp $
C $Name:  $

#include "AIM_OPTIONS.h"

CStartOfInterFace
      SUBROUTINE AIM_WRITE_DIAGS( myTime, myIter, myThid )
C     /==========================================================\
C     | SUBROUTINE AIM_WRITE_DIAGS                               |
C     | o Write out AIM diagnostic output                        |
C     \==========================================================/
      IMPLICIT NONE

C     === Global variables ===
#include "SIZE.h"
#include "EEPARAMS.h"
#include "PARAMS.h"
#include "AIM_DIAGS.h"
      LOGICAL  DIFFERENT_MULTIPLE
      EXTERNAL DIFFERENT_MULTIPLE

C Common (shared with RD_WR_FLD routines in read_write_fld.F)
      COMMON /RD_WR_FLD/ globalFile
      LOGICAL globalFile 

C     == Routine arguments ==
C     myTime - Current time of simulation ( s )
C     myIter - Iteration number
C     myThid - Number of this instance of the routine
      _RL     myTime
      INTEGER myIter
      INTEGER myThid
CEndOfInterface

#ifdef ALLOW_AIM

C     == Local variables ==
      INTEGER bi, bj, K
      CHARACTER*(MAX_LEN_MBUF) fn
      LOGICAL gf
c_jmc gf = .TRUE.
      gf = globalFile

#ifdef ALLOW_TIMEAVE

C     Dump files and restart average computation if needed
      IF ( myIter.NE.nIter0 .AND.
     &     DIFFERENT_MULTIPLE(taveFreq,myTime,
     &     myTime-deltaTClock)
     &     ) THEN

C      Normalize by integrated time
       DO bj = myByLo(myThid), myByHi(myThid)
        DO bi = myBxLo(myThid), myBxHi(myThid)
         CALL TIMEAVE_NORMALIZ(USTRtave,AIM_timeave,1,bi,bj,myThid)
         CALL TIMEAVE_NORMALIZ(VSTRtave,AIM_timeave,1,bi,bj,myThid)
         CALL TIMEAVE_NORMALIZ(TSRtave,AIM_timeave,1,bi,bj,myThid)
         CALL TIMEAVE_NORMALIZ(OLRtave,AIM_timeave,1,bi,bj,myThid)
         CALL TIMEAVE_NORMALIZ(SSRtave,AIM_timeave,1,bi,bj,myThid)
         CALL TIMEAVE_NORMALIZ(SLRtave,AIM_timeave,1,bi,bj,myThid)
         CALL TIMEAVE_NORMALIZ(SHFtave,AIM_timeave,1,bi,bj,myThid)
         CALL TIMEAVE_NORMALIZ(EVAPtave,AIM_timeave,1,bi,bj,myThid)
         CALL TIMEAVE_NORMALIZ(PRECNVtave,AIM_timeave,1,bi,bj,myThid)
         CALL TIMEAVE_NORMALIZ(PRECLStave,AIM_timeave,1,bi,bj,myThid)
         CALL TIMEAVE_NORMALIZ(CLOUDCtave,AIM_timeave,1,bi,bj,myThid)
        ENDDO
       ENDDO


       _BARRIER
       _BEGIN_MASTER( myThid )
        WRITE(fn,'(A,I10.10)') 'aimphyvars.',myIter
        CALL MDSWRITEFIELD(fn,precFloat32,gf,
     &        'RL',1,USTRtave,   1,myIter,myThid)
        CALL MDSWRITEFIELD(fn,precFloat32,gf,
     &        'RL',1,VSTRtave,   2,myIter,myThid)
        CALL MDSWRITEFIELD(fn,precFloat32,gf,
     &        'RL',1,TSRtave ,   3,myIter,myThid)
        CALL MDSWRITEFIELD(fn,precFloat32,gf,
     &        'RL',1,OLRtave ,   4,myIter,myThid)
        CALL MDSWRITEFIELD(fn,precFloat32,gf,
     &        'RL',1,SSRtave ,   5,myIter,myThid)
        CALL MDSWRITEFIELD(fn,precFloat32,gf,
     &        'RL',1,SLRtave ,   6,myIter,myThid)
        CALL MDSWRITEFIELD(fn,precFloat32,gf,
     &        'RL',1,SHFtave ,   7,myIter,myThid)
        CALL MDSWRITEFIELD(fn,precFloat32,gf,
     &        'RL',1,EVAPtave,   8,myIter,myThid)
        CALL MDSWRITEFIELD(fn,precFloat32,gf,
     &        'RL',1,PRECNVtave, 9,myIter,myThid)
        CALL MDSWRITEFIELD(fn,precFloat32,gf,
     &        'RL',1,PRECLStave,10,myIter,myThid)
        CALL MDSWRITEFIELD(fn,precFloat32,gf,
     &        'RL',1,CLOUDCtave,11,myIter,myThid)
       _END_MASTER( myThid )
       _BARRIER

C      Reset averages to zero
       DO bj = myByLo(myThid), myByHi(myThid)
        DO bi = myBxLo(myThid), myBxHi(myThid)
         CALL TIMEAVE_RESET(USTRtave,  1, bi, bj, myThid)
         CALL TIMEAVE_RESET(VSTRtave,  1, bi, bj, myThid)
         CALL TIMEAVE_RESET(TSRtave,   1, bi, bj, myThid)
         CALL TIMEAVE_RESET(OLRtave,   1, bi, bj, myThid)
         CALL TIMEAVE_RESET(SSRtave,   1, bi, bj, myThid)
         CALL TIMEAVE_RESET(SLRtave,   1, bi, bj, myThid)
         CALL TIMEAVE_RESET(SHFtave,   1, bi, bj, myThid)
         CALL TIMEAVE_RESET(EVAPtave,  1, bi, bj, myThid)
         CALL TIMEAVE_RESET(PRECNVtave,1, bi, bj, myThid)
         CALL TIMEAVE_RESET(PRECLStave,1, bi, bj, myThid)
         CALL TIMEAVE_RESET(CLOUDCtave,1, bi, bj, myThid)
         DO k=1,Nr
           AIM_TimeAve(k,bi,bj)=0.
         ENDDO                     
        ENDDO
       ENDDO

      ENDIF

#endif /* ALLOW_TIMEAVE */

#endif /* ALLOW_AIM */

      RETURN
      END

C $Header: /u/gcmpack/MITgcm/pkg/ocn_compon_interf/Attic/cpl_write_diags.F,v 1.1 2003/12/15 02:49:09 jmc Exp $
C $Name:  $

#include "PACKAGES_CONFIG.h"
#include "CPP_OPTIONS.h"

CStartOfInterFace
      SUBROUTINE CPL_WRITE_DIAGS( myTime, myIter, myThid )
C     /==========================================================\
C     | SUBROUTINE CPL_WRITE_DIAGS                               |
C     | o Calculate CPL diagnostics                              |
C     | - Oceanic version -                                      |
C     \==========================================================/
      IMPLICIT NONE

C     === Global variables ===
#include "SIZE.h"
#include "EEPARAMS.h"
#include "PARAMS.h"
#include "CPL_PARAMS.h"
#include "OCNCPL.h"
#include "CPL_DIAGS.h"

      LOGICAL  DIFFERENT_MULTIPLE
      EXTERNAL DIFFERENT_MULTIPLE

C     == Routine arguments ==
C     myTime - Current time of simulation ( s )
C     myIter - Iteration number
C     myThid - Number of this instance of the routine
      _RL     myTime
      INTEGER myIter
      INTEGER myThid
CEndOfInterface

#ifdef COMPONENT_MODULE

C     == Local variables ==
C     bi,bj  - Tile index
      INTEGER bi,bj
      CHARACTER*(MAX_LEN_MBUF) fn
      LOGICAL gf

#ifdef ALLOW_TIMEAVE

C     Dump files and restart average computation if needed
      IF ( myIter.NE.nIter0 .AND.
     &     DIFFERENT_MULTIPLE(cpl_taveFreq,myTime,myTime-deltaTClock)
     &   ) THEN

C      Normalize by integrated time
       DO bj = myByLo(myThid), myByHi(myThid)
        DO bi = myBxLo(myThid), myBxHi(myThid)
         CALL TIMEAVE_NORMALIZ(HFtave, CPL_TimeAve, 1, bi, bj, myThid)
         CALL TIMEAVE_NORMALIZ(TXtave, CPL_TimeAve, 1, bi, bj, myThid)
         CALL TIMEAVE_NORMALIZ(TYtave, CPL_TimeAve, 1, bi, bj, myThid)
         CALL TIMEAVE_NORMALIZ(FWtave, CPL_TimeAve, 1, bi, bj, myThid)
         CALL TIMEAVE_NORMALIZ(UGtave, CPL_TimeAve, 1, bi, bj, myThid)
         CALL TIMEAVE_NORMALIZ(VGtave, CPL_TimeAve, 1, bi, bj, myThid)
         CALL TIMEAVE_NORMALIZ(QLTtave, CPL_TimeAve, 1, bi, bj, myThid)
         CALL TIMEAVE_NORMALIZ(QSNtave, CPL_TimeAve, 1, bi, bj, myThid)
         CALL TIMEAVE_NORMALIZ(QLWtave, CPL_TimeAve, 1, bi, bj, myThid)
         CALL TIMEAVE_NORMALIZ(QSWtave, CPL_TimeAve, 1, bi, bj, myThid)
         CALL TIMEAVE_NORMALIZ(SSTtave, CPL_TimeAve, 1, bi, bj, myThid)
        ENDDO
       ENDDO

       _BARRIER
       _BEGIN_MASTER( myThid )
         gf = globalFiles
         WRITE(fn,'(A,I10.10)') 'cplFldtave.',myIter
         CALL MDSWRITEFIELD(fn,precFloat32,gf,
     &        'RL',1,HFtave,   1,myIter,myThid)
         CALL MDSWRITEFIELD(fn,precFloat32,gf,
     &        'RL',1,TXtave,   2,myIter,myThid)
         CALL MDSWRITEFIELD(fn,precFloat32,gf,
     &        'RL',1,TYtave,   3,myIter,myThid)
         CALL MDSWRITEFIELD(fn,precFloat32,gf,
     &        'RL',1,FWtave,   4,myIter,myThid)
         CALL MDSWRITEFIELD(fn,precFloat32,gf,
     &        'RL',1,UGtave,   5,myIter,myThid)
         CALL MDSWRITEFIELD(fn,precFloat32,gf,
     &        'RL',1,VGtave,   6,myIter,myThid)
         CALL MDSWRITEFIELD(fn,precFloat32,gf,
     &        'RL',1,QLTtave,  7 ,myIter,myThid)
         CALL MDSWRITEFIELD(fn,precFloat32,gf,
     &        'RL',1,QSNtave,  8 ,myIter,myThid)
         CALL MDSWRITEFIELD(fn,precFloat32,gf,
     &        'RL',1,QLWtave,  9 ,myIter,myThid)
         CALL MDSWRITEFIELD(fn,precFloat32,gf,
     &        'RL',1,QSWtave,  10,myIter,myThid)
         CALL MDSWRITEFIELD(fn,precFloat32,gf,
     &        'RL',1,SSTtave,  11,myIter,myThid)

       _END_MASTER( myThid )
       _BARRIER

C      Reset diagnostic counters
       DO bj = myByLo(myThid), myByHi(myThid)
        DO bi = myBxLo(myThid), myBxHi(myThid)
         CALL TIMEAVE_RESET(HFtave, 1, bi, bj, myThid)
         CALL TIMEAVE_RESET(TXtave, 1, bi, bj, myThid)
         CALL TIMEAVE_RESET(TYtave, 1, bi, bj, myThid)
         CALL TIMEAVE_RESET(FWtave, 1, bi, bj, myThid)
         CALL TIMEAVE_RESET(UGtave, 1, bi, bj, myThid)
         CALL TIMEAVE_RESET(VGtave, 1, bi, bj, myThid)
         CALL TIMEAVE_RESET(QLTtave, 1, bi, bj, myThid)
         CALL TIMEAVE_RESET(QSNtave, 1, bi, bj, myThid)
         CALL TIMEAVE_RESET(QLWtave, 1, bi, bj, myThid)
         CALL TIMEAVE_RESET(QSWtave, 1, bi, bj, myThid)
         CALL TIMEAVE_RESET(SSTtave, 1, bi, bj, myThid)
         CPL_TimeAve(1,bi,bj) = 0.
        ENDDO
       ENDDO

      ENDIF

#endif /* ALLOW_TIMEAVE */

#endif /* COMPONENT_MODULE */

      RETURN
      END




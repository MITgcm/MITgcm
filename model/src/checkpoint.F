C $Header: /u/gcmpack/MITgcm/model/src/Attic/checkpoint.F,v 1.7 2001/03/12 20:44:27 jmc Exp $

#include "CPP_OPTIONS.h"

C--  File read_write.F: Routines to handle mid-level I/O interface.
C--   Contents
C--   o SET_WRITE_GLOBAL_PICKUP 
C--   o READ_CHECKPOINT - Write out checkpoint files for restarting.
C--   o WRITE_CHECKPOINT - Write out checkpoint files for restarting.

      SUBROUTINE SET_WRITE_GLOBAL_PICKUP ( flag )
      IMPLICIT NONE
C SET_WRITE_GLOBAL_FLD( flag ) sets an internal logical state to
C indicate whether files written by subsequent call to the
C READ_WRITE_FLD package should create "global" or "tiled" files.
C   flag = .TRUE.  indicates "global" files
C   flag = .FALSE. indicates "tiled" files
C
C Arguments
      LOGICAL flag
C Common
      COMMON /PCKP_GBLFLS/ globalFile
      LOGICAL globalFile
C
      globalFile=flag
C
      RETURN
      END

CStartofinterface
      SUBROUTINE READ_CHECKPOINT ( myIter, myThid )
C     /==========================================================\
C     | SUBROUTINE READ_PICKUP                                   |
C     | o Controlling routine for IO to write restart file.      |
C     |==========================================================|
C     | Read model checkpoint files for use in restart.          |
C     \==========================================================/
      IMPLICIT NONE

C     == Global variables ===
#include "SIZE.h"
#include "EEPARAMS.h"
#include "PARAMS.h"
#include "DYNVARS.h"
#ifdef ALLOW_NONHYDROSTATIC
#include "GW.h"
#include "CG3D.h"
#endif

      INTEGER  IO_ERRCOUNT
      EXTERNAL IO_ERRCOUNT

C     == Routine arguments ==
C     myThid - Thread number for this instance of the routine.
C     myIter - Iteration number
      INTEGER myThid
      INTEGER myIter
CEndofinterface

C     == Local variables ==
      INTEGER oldPrec
      CHARACTER*(MAX_LEN_FNAM) fn
      INTEGER prec

C--    Going to really do some IO. Make everyone except master thread wait.
       _BARRIER
       _BEGIN_MASTER( myThid )

C       Force 64-bit IO
        oldPrec        = readBinaryPrec
        readBinaryPrec = precFloat64

#ifdef OLD_STYLE_WITH_MANY_FILES
C--     Read model fields
C       Raw fields
        CALL READ_REC_XYZ_RL(  'uVel',   uVel, 1,myIter, myThid)
        CALL READ_REC_XYZ_RL(    'gU',     gU, 1,myIter, myThid)
        CALL READ_REC_XYZ_RL( 'guNm1',  gUNm1, 1,myIter, myThid)
        CALL READ_REC_XYZ_RL(  'vVel',   vVel, 1,myIter, myThid)
        CALL READ_REC_XYZ_RL(    'gV',     gV, 1,myIter, myThid)
        CALL READ_REC_XYZ_RL( 'gvNm1',  gVNm1, 1,myIter, myThid)
        CALL READ_REC_XYZ_RL( 'theta',  theta, 1,myIter, myThid)
        CALL READ_REC_XYZ_RL(    'gT',     gT, 1,myIter, myThid)
        CALL READ_REC_XYZ_RL( 'gtNm1',  gTNm1, 1,myIter, myThid)
        CALL READ_REC_XYZ_RL(  'salt',   salt, 1,myIter, myThid)
        CALL READ_REC_XYZ_RL(    'gS',     gS, 1,myIter, myThid)
        CALL READ_REC_XYZ_RL( 'gsNm1',  gSNm1, 1,myIter, myThid)
        CALL READ_REC_XY_RL ('etaN', etaN, 1,myIter, myThid)
#ifdef INCLUDE_CD_CODE
        CALL READ_REC_XY_RL
     &               ( 'etaNm1', etaNm1, 1,myIter, myThid)
        CALL READ_REC_XYZ_RL(  'uVelD', uVelD, 1,myIter, myThid)
        CALL READ_REC_XYZ_RL(  'vVelD', vVelD, 1,myIter, myThid)
        CALL READ_REC_XYZ_RL(  'uNm1',  uNM1, 1,myIter, myThid)
        CALL READ_REC_XYZ_RL(  'vNm1',  vNM1, 1,myIter, myThid)
        CALL READ_REC_XYZ_RL(  'guCD',  guCD, 1,myIter, myThid)
        CALL READ_REC_XYZ_RL(  'gvCD',  gvCD, 1,myIter, myThid)
#endif
#ifdef ALLOW_NONHYDROSTATIC
        IF ( nonHydrostatic ) THEN
         CALL READ_REC_XYZ_RL('cg3d_x',cg3d_x,1,myIter,myThid)
         CALL READ_REC_XYZ_RL( 'gW',gW,       1,myIter,myThid)
c        CALL READ_REC_XYZ_RL( 'gWnm1',gWnm1, 1,myIter,myThid)
        ENDIF
#endif
#else

        prec = precFloat64

C--     Read model fields
        WRITE(fn,'(A,I10.10)') 'pickup.',myIter
        CALL MDSREADFIELD(fn,prec,'RL',Nr,uVel,   1,myThid)
        CALL MDSREADFIELD(fn,prec,'RL',Nr,gU,     2,myThid)
        CALL MDSREADFIELD(fn,prec,'RL',Nr,gUnm1,  3,myThid)
        CALL MDSREADFIELD(fn,prec,'RL',Nr,vVel,   4,myThid)
        CALL MDSREADFIELD(fn,prec,'RL',Nr,gV,     5,myThid)
        CALL MDSREADFIELD(fn,prec,'RL',Nr,gVnm1,  6,myThid)
        CALL MDSREADFIELD(fn,prec,'RL',Nr,theta,  7,myThid)
        CALL MDSREADFIELD(fn,prec,'RL',Nr,gT,     8,myThid)
        CALL MDSREADFIELD(fn,prec,'RL',Nr,gTnm1,  9,myThid)
        CALL MDSREADFIELD(fn,prec,'RL',Nr,salt,  10,myThid)
        CALL MDSREADFIELD(fn,prec,'RL',Nr,gS,    11,myThid)
        CALL MDSREADFIELD(fn,prec,'RL',Nr,gSnm1, 12,myThid)
        CALL MDSREADFIELD(fn,prec,'RL', 1,etaN,12*Nr+1,myThid)
#ifdef INCLUDE_CD_CODE
        WRITE(fn,'(A,I10.10)') 'pickup_cd.',myIter
        CALL MDSREADFIELD(fn,prec,'RL',Nr,uVelD,    1,myThid)
        CALL MDSREADFIELD(fn,prec,'RL',Nr,vVelD,    2,myThid)
        CALL MDSREADFIELD(fn,prec,'RL',Nr,uNM1,     3,myThid)
        CALL MDSREADFIELD(fn,prec,'RL',Nr,vNM1,     4,myThid)
        CALL MDSREADFIELD(fn,prec,'RL',Nr,guCD,     5,myThid)
        CALL MDSREADFIELD(fn,prec,'RL',Nr,gvCD,     6,myThid)
        CALL MDSREADFIELD(fn,prec,'RL', 1,etaNm1,6*Nr+1,myThid)
#endif
#ifdef ALLOW_NONHYDROSTATIC
        IF ( nonHydrostatic ) THEN
         WRITE(fn,'(A,I10.10)') 'pickup_nh.',myIter
         CALL MDSREADFIELD(fn,prec,'RL',Nr,cg3d_x,1,myThid)
         CALL MDSREADFIELD(fn,prec,'RL',Nr,gW,   2,myThid)
c        CALL MDSREADFIELD(fn,prec,'RL',Nr,gWnm1,3,myThid)
        ENDIF
#endif

#endif

C       Reset default IO precision
        readBinaryPrec = oldPrec

       _END_MASTER( myThid )
       _BARRIER

C--    Fill in edge regions
      _EXCH_XYZ_R8(uVel  , myThid )
      _EXCH_XYZ_R8(gu    , myThid )
      _EXCH_XYZ_R8(guNM1 , myThid )
      _EXCH_XYZ_R8(vVel  , myThid )
      _EXCH_XYZ_R8(gv    , myThid )
      _EXCH_XYZ_R8(gvNM1 , myThid )
      _EXCH_XYZ_R8(theta , myThid )
      _EXCH_XYZ_R8(gt    , myThid )
      _EXCH_XYZ_R8(gtNM1 , myThid )
      _EXCH_XYZ_R8(salt  , myThid )
      _EXCH_XYZ_R8(gs    , myThid )
      _EXCH_XYZ_R8(gsNM1 , myThid )
      _EXCH_XY_R8 (etaN, myThid )
#ifdef INCLUDE_CD_CODE
      _EXCH_XY_R8( etaNm1, myThid )
      _EXCH_XYZ_R8( uVelD,    myThid )
      _EXCH_XYZ_R8( vVelD,    myThid )
      _EXCH_XYZ_R8( uNM1,     myThid )
      _EXCH_XYZ_R8( vNM1,     myThid )
      _EXCH_XYZ_R8( guCD,     myThid )
      _EXCH_XYZ_R8( gvCD,     myThid )
#endif
#ifdef ALLOW_NONHYDROSTATIC
        IF ( nonHydrostatic ) THEN
         _EXCH_XYZ_R8(cg3d_x, myThid )
         _EXCH_XYZ_R8(gW    , myThid )
c        _EXCH_XYZ_R8(gWNM1 , myThid )
        ENDIF
#endif

      RETURN
      END

CStartofinterface
      SUBROUTINE WRITE_CHECKPOINT ( modelEnd, myCurrentTime, 
     &                              myIter, myThid )
C     /==========================================================\
C     | SUBROUTINE WRITE_PICKUP                                  |
C     | o Controlling routine for IO to write restart file.      |
C     |==========================================================|
C     | Write model checkpoint files for use in restart.         |
C     | This routine writes both "rolling-checkpoint" files      |
C     | and permanent checkpoint files. A rolling checkpoint     |
C     | works through a circular list of suffices. Generally the |
C     | circular list has two entries so that a rolling          |
C     | checkpoint will overwrite the last rolling checkpoint    |
C     | but one. This is useful for running long jobs without    |
C     | filling too much disk space.                             |
C     |  In a permanent checkpoint data is written suffixed by   |
C     | the current timestep number. This sort of checkpoint can |
C     | be used to provided a snap-shot from which the model     |
C     | can be rerun.                                            |
C     \==========================================================/
      IMPLICIT NONE

C     == Global variables ===
#include "SIZE.h"
#include "EEPARAMS.h"
#include "PARAMS.h"
#include "DYNVARS.h"
#ifdef ALLOW_NONHYDROSTATIC
#include "GW.h"
#include "CG3D.h"
#endif

      LOGICAL  DIFFERENT_MULTIPLE
      EXTERNAL DIFFERENT_MULTIPLE
      INTEGER  IO_ERRCOUNT
      EXTERNAL IO_ERRCOUNT

C     == Routine arguments ==
C     modelEnd    - Checkpoint call at end of model run.
C     myThid - Thread number for this instance of the routine.
C     myIter - Iteration number
C     myCurrentTime - Current time of simulation ( s )
      LOGICAL modelEnd    
      INTEGER myThid
      INTEGER myIter
      _RL     myCurrentTime
CEndofinterface

C     == Common blocks ==
      COMMON /PCKP_GBLFLS/ globalFile
      LOGICAL globalFile

C     == Local variables ==
C     permCheckPoint - Flag indicating whether a permanent checkpoint will
C                      be written.
      LOGICAL permCheckPoint  
      INTEGER oldPrec
      CHARACTER*(MAX_LEN_FNAM) fn
      INTEGER prec
      LOGICAL lgf

      permCheckPoint = .FALSE.
      permCheckPoint=
     & DIFFERENT_MULTIPLE(pChkptFreq,myCurrentTime,
     &                    myCurrentTime-deltaTClock)

      IF (
     &    (.NOT. modelEnd .AND. (
     &     permCheckPoint
     &     .OR.
     &     DIFFERENT_MULTIPLE(chkptFreq,
     &    myCurrentTime,myCurrentTime-deltaTClock)
     &     ) 
     &    ) 
     &     .OR.
     &    (
     &     modelEnd 
     &     .AND. .NOT.
     &     permCheckPoint 
     &     .AND. .NOT.
     &     DIFFERENT_MULTIPLE(chkptFreq,
     &    myCurrentTime,myCurrentTime-deltaTClock)
     &    )
     & ) THEN

C--    Going to really do some IO. Make everyone except master thread wait.
       _BARRIER
       _BEGIN_MASTER( myThid )

C       Force 64-bit IO
        oldPrec = writeBinaryPrec
        writeBinaryPrec = precFloat64

#ifdef OLD_STYLE_WITH_MANY_FILES
C--     Write model fields
C       Raw fields
        CALL WRITE_REC_XYZ_RL(  'uVel',   uVel,  1,myIter, myThid)
        CALL WRITE_REC_XYZ_RL(    'gU',     gU,  1,myIter, myThid)
        CALL WRITE_REC_XYZ_RL( 'gUNm1',  gUNm1,  1,myIter, myThid)
        CALL WRITE_REC_XYZ_RL(  'vVel',   vVel,  1,myIter, myThid)
        CALL WRITE_REC_XYZ_RL(    'gV',     gV,  1,myIter, myThid)
        CALL WRITE_REC_XYZ_RL( 'gVNm1',  gVNm1,  1,myIter, myThid)
        CALL WRITE_REC_XYZ_RL( 'theta',  theta,  1,myIter, myThid)
        CALL WRITE_REC_XYZ_RL(    'gT',     gT,  1,myIter, myThid)
        CALL WRITE_REC_XYZ_RL( 'gTNm1',  gTNm1,  1,myIter, myThid)
        CALL WRITE_REC_XYZ_RL(  'salt',   salt,  1,myIter, myThid)
        CALL WRITE_REC_XYZ_RL(    'gS',     gS,  1,myIter, myThid)
        CALL WRITE_REC_XYZ_RL( 'gSNm1',  gSNm1,  1,myIter, myThid)
        CALL WRITE_REC_XY_RL ('etaN', etaN,  1,myIter, myThid)
#ifdef INCLUDE_CD_CODE
        CALL WRITE_REC_XY_RL
     &   ( 'etaNm1', etaNm1,  1,myIter, myThid)
        CALL WRITE_REC_XYZ_RL(  'uVelD', uVelD,  1,myIter, myThid)
        CALL WRITE_REC_XYZ_RL(  'vVelD', vVelD,  1,myIter, myThid)
        CALL WRITE_REC_XYZ_RL(  'uNM1',  uNM1,  1,myIter, myThid)
        CALL WRITE_REC_XYZ_RL(  'vNM1',  vNM1,  1,myIter, myThid)
        CALL WRITE_REC_XYZ_RL(  'guCD',  guCD,  1,myIter, myThid)
        CALL WRITE_REC_XYZ_RL(  'gvCD',  gvCD,  1,myIter, myThid)
#endif
#ifdef ALLOW_NONHYDROSTATIC
        IF ( nonHydrostatic ) THEN
         CALL WRITE_REC_XYZ_RL('cg3d_x',cg3d_x,1,myIter,myThid)
         CALL WRITE_REC_XYZ_RL( 'gW',gW, 1,myIter,myThid)
c        CALL WRITE_REC_XYZ_RL( 'gWnm1',gWnm1, 1,myIter,myThid)
        ENDIF
#endif

#else

        prec = precFloat64
        lgf = globalFile

C--     Write model fields
        IF ( permCheckPoint ) THEN
         WRITE(fn,'(A,I10.10)') 'pickup.',myIter
        ELSE
         WRITE(fn,'(A,A)') 'pickup.',checkPtSuff(nCheckLev)
        ENDIF
        CALL MDSWRITEFIELD(fn,prec,lgf,'RL',Nr,uVel,  1,myIter,myThid)
        CALL MDSWRITEFIELD(fn,prec,lgf,'RL',Nr,gU,    2,myIter,myThid)
        CALL MDSWRITEFIELD(fn,prec,lgf,'RL',Nr,gUnm1, 3,myIter,myThid)
        CALL MDSWRITEFIELD(fn,prec,lgf,'RL',Nr,vVel,  4,myIter,myThid)
        CALL MDSWRITEFIELD(fn,prec,lgf,'RL',Nr,gV,    5,myIter,myThid)
        CALL MDSWRITEFIELD(fn,prec,lgf,'RL',Nr,gVnm1, 6,myIter,myThid)
        CALL MDSWRITEFIELD(fn,prec,lgf,'RL',Nr,theta, 7,myIter,myThid)
        CALL MDSWRITEFIELD(fn,prec,lgf,'RL',Nr,gT,    8,myIter,myThid)
        CALL MDSWRITEFIELD(fn,prec,lgf,'RL',Nr,gTnm1, 9,myIter,myThid)
        CALL MDSWRITEFIELD(fn,prec,lgf,'RL',Nr,salt, 10,myIter,myThid)
        CALL MDSWRITEFIELD(fn,prec,lgf,'RL',Nr,gS,   11,myIter,myThid)
        CALL MDSWRITEFIELD(fn,prec,lgf,'RL',Nr,gSnm1,12,myIter,myThid)
        CALL MDSWRITEFIELD(fn,prec,lgf,'RL', 1,etaN,12*Nr+1,
     &                     myIter,myThid)
#ifdef INCLUDE_CD_CODE
        IF ( permCheckPoint ) THEN
         WRITE(fn,'(A,I10.10)') 'pickup_cd.',myIter
        ELSE
         WRITE(fn,'(A,A)') 'pickup_cd.',checkPtSuff(nCheckLev)
        ENDIF
        CALL MDSWRITEFIELD(fn,prec,lgf,'RL',Nr,uVelD,1,myIter,myThid)
        CALL MDSWRITEFIELD(fn,prec,lgf,'RL',Nr,vVelD,2,myIter,myThid)
        CALL MDSWRITEFIELD(fn,prec,lgf,'RL',Nr,uNM1, 3,myIter,myThid)
        CALL MDSWRITEFIELD(fn,prec,lgf,'RL',Nr,vNM1, 4,myIter,myThid)
        CALL MDSWRITEFIELD(fn,prec,lgf,'RL',Nr,guCD, 5,myIter,myThid)
        CALL MDSWRITEFIELD(fn,prec,lgf,'RL',Nr,gvCD, 6,myIter,myThid)
        CALL MDSWRITEFIELD(fn,prec,lgf,'RL', 1,etaNm1,6*Nr+1,
     &                     myIter,myThid)
#endif
#ifdef ALLOW_NONHYDROSTATIC
        IF ( nonHydrostatic ) THEN
         IF ( permCheckPoint ) THEN
          WRITE(fn,'(A,I10.10)') 'pickup_nh.',myIter
         ELSE
          WRITE(fn,'(A,A)') 'pickup_nh.',checkPtSuff(nCheckLev)
         ENDIF
         WRITE(fn,'(A,I10.10)') 'pickup_nh.',myIter
         CALL MDSWRITEFIELD(fn,prec,lgf,'RL',Nr,cg3d_x,1,myIter,myThid)
         CALL MDSWRITEFIELD(fn,prec,lgf,'RL',Nr,gW,   2,myIter,myThid)
c        CALL MDSWRITEFIELD(fn,prec,lgf,'RL',Nr,gWnm1,3,myIter,myThid)
        ENDIF
#endif
        IF ( .NOT. permCheckPoint ) THEN
         nCheckLev = MOD(nCheckLev, maxNoChkptLev)+1
        ENDIF

#endif

C--     Reset binary precision
        writeBinaryPrec = oldPrec

       _END_MASTER( myThid )
       _BARRIER

      ENDIF

      RETURN
      END

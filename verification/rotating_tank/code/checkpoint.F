C $Header: /u/gcmpack/MITgcm/verification/rotating_tank/code/Attic/checkpoint.F,v 1.4 2005/11/08 05:21:00 jmc Exp $
C $Name:  $

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

CBOP
C     !ROUTINE: READ_CHECKPOINT
C     !INTERFACE:
      SUBROUTINE READ_CHECKPOINT ( myIter, myThid )
C     !DESCRIPTION: \bv
C     *==========================================================*
C     | SUBROUTINE READ_PICKUP                                    
C     | o Controlling routine for IO to write restart file.       
C     *==========================================================*
C     | Read model checkpoint files for use in restart.           
C     *==========================================================*
C     \ev
C     !USES:
      IMPLICIT NONE
C     == Global variables ===
#include "SIZE.h"
#include "EEPARAMS.h"
#include "PARAMS.h"
#include "DYNVARS.h"
#ifdef ALLOW_NONHYDROSTATIC
#include "NH_VARS.h"
#endif
      INTEGER  IO_ERRCOUNT
      EXTERNAL IO_ERRCOUNT

C     !INPUT/OUTPUT PARAMETERS:
C     == Routine arguments ==
C     myThid - Thread number for this instance of the routine.
C     myIter - Iteration number
      INTEGER myThid
      INTEGER myIter

C     !LOCAL VARIABLES:
C     == Local variables ==
C     oldPrec :: Temp. for hold I/O precision information
C     prec    
C     fn      :: Temp. for building file name.
      INTEGER oldPrec
      CHARACTER*(MAX_LEN_FNAM) fn
      INTEGER prec
CEOP

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

#ifdef ALLOW_NONHYDROSTATIC
        IF ( nonHydrostatic ) THEN
         CALL READ_REC_XYZ_RL('phi_nh',phi_nh,1,myIter,myThid)
         CALL READ_REC_XYZ_RL( 'gW',gW,       1,myIter,myThid)
c        CALL READ_REC_XYZ_RL( 'gWnm1',gWnm1, 1,myIter,myThid)
        ENDIF
#endif

#else /* OLD_STYLE_WITH_MANY_FILES */

        prec = precFloat64

C--     Read model fields
        WRITE(fn,'(A,I10.10)') 'pickup.',myIter
        CALL MDSREADFIELD(fn,prec,'RL',Nr,uVel,   1,myThid)
        CALL MDSREADFIELD(fn,prec,'RL',Nr,gU,     2,myThid)
        CALL MDSREADFIELD(fn,prec,'RL',Nr,gUnm1,  3,myThid)
        CALL MDSREADFIELD(fn,prec,'RL',Nr,vVel,   4,myThid)
        CALL MDSREADFIELD(fn,prec,'RL',Nr,gV,     5,myThid)
        CALL MDSREADFIELD(fn,prec,'RL',Nr,gVnm1,  6,myThid)
        CALL MDSREADFIELD(fn,prec,'RL',Nr,wVel,  7,myThid)
        CALL MDSREADFIELD(fn,prec,'RL',Nr,theta,  8,myThid)
        CALL MDSREADFIELD(fn,prec,'RL',Nr,gT,     9,myThid)
        CALL MDSREADFIELD(fn,prec,'RL',Nr,gTnm1,  10,myThid)
        CALL MDSREADFIELD(fn,prec,'RL',Nr,salt,  11,myThid)
        CALL MDSREADFIELD(fn,prec,'RL',Nr,gS,    12,myThid)
        CALL MDSREADFIELD(fn,prec,'RL',Nr,gSnm1, 13,myThid)
        CALL MDSREADFIELD(fn,prec,'RL', 1,etaN,13*Nr+1,myThid)
#ifdef NONLIN_FRSURF
        IF ( nonlinFreeSurf.GE.0)
     &  CALL MDSREADFIELD(fn,prec,'RL',1,etaH,13*Nr+2,myThid)
#endif

#ifdef ALLOW_NONHYDROSTATIC
        IF ( nonHydrostatic ) THEN
         WRITE(fn,'(A,I10.10)') 'pickup_nh.',myIter
         CALL MDSREADFIELD(fn,prec,'RL',Nr,phi_nh,1,myThid)
         CALL MDSREADFIELD(fn,prec,'RL',Nr,gW,   2,myThid)
c        CALL MDSREADFIELD(fn,prec,'RL',Nr,gWnm1,3,myThid)
        ENDIF
#endif

#endif /* OLD_STYLE_WITH_MANY_FILES */

C       Reset default IO precision
        readBinaryPrec = oldPrec

       _END_MASTER( myThid )
       _BARRIER

C--    Fill in edge regions
      CALL EXCH_UV_XYZ_RL(uVel,vVel,.TRUE.,myThid)
      CALL EXCH_UV_XYZ_RL(gU,gV,.TRUE.,myThid)
      CALL EXCH_UV_XYZ_RL(gUnm1,gVnm1,.TRUE.,myThid)
c     _EXCH_XYZ_R8(uVel  , myThid )
c     _EXCH_XYZ_R8(gu    , myThid )
c     _EXCH_XYZ_R8(guNM1 , myThid )
c     _EXCH_XYZ_R8(vVel  , myThid )
c     _EXCH_XYZ_R8(gv    , myThid )
c     _EXCH_XYZ_R8(gvNM1 , myThid )
      _EXCH_XYZ_R8(theta , myThid )
      _EXCH_XYZ_R8(gt    , myThid )
      _EXCH_XYZ_R8(gtNM1 , myThid )
      _EXCH_XYZ_R8(salt  , myThid )
      _EXCH_XYZ_R8(gs    , myThid )
      _EXCH_XYZ_R8(gsNM1 , myThid )
      _EXCH_XY_R8 (etaN, myThid )
      _EXCH_XY_R8( etaH, myThid )

#ifdef ALLOW_NONHYDROSTATIC
        IF ( nonHydrostatic ) THEN
         _EXCH_XYZ_R8(phi_nh, myThid )
         _EXCH_XYZ_R8(gW    , myThid )
c        _EXCH_XYZ_R8(gWNM1 , myThid )
        ENDIF
#endif

      RETURN
      END

CBOP
C     !ROUTINE: WRITE_CHECKPOINT
C     !INTERFACE:
      SUBROUTINE WRITE_CHECKPOINT ( modelEnd, myTime, 
     &                              myIter, myThid )
C     !DESCRIPTION: \bv
C     *==========================================================*
C     | SUBROUTINE WRITE_CHECKPOINT                               
C     | o Controlling routine for IO to write restart file.       
C     *==========================================================*
C     | Write model checkpoint files for use in restart.          
C     | This routine writes both "rolling-checkpoint" files       
C     | and permanent checkpoint files. A rolling checkpoint      
C     | works through a circular list of suffices. Generally the  
C     | circular list has two entries so that a rolling           
C     | checkpoint will overwrite the last rolling checkpoint     
C     | but one. This is useful for running long jobs without     
C     | filling too much disk space.                              
C     |  In a permanent checkpoint data is written suffixed by    
C     | the current timestep number. This sort of checkpoint can  
C     | be used to provided a snap-shot from which the model      
C     | can be rerun.                                             
C     *==========================================================*
C     \ev

C     !USES:
      IMPLICIT NONE
C     == Global variables ===
#include "SIZE.h"
#include "EEPARAMS.h"
#include "PARAMS.h"
#include "DYNVARS.h"
#ifdef ALLOW_NONHYDROSTATIC
#include "NH_VARS.h"
#endif
      LOGICAL  DIFFERENT_MULTIPLE
      EXTERNAL DIFFERENT_MULTIPLE
      INTEGER  IO_ERRCOUNT
      EXTERNAL IO_ERRCOUNT

C     !INPUT/OUTPUT PARAMETERS:
C     == Routine arguments ==
C     modelEnd    :: Checkpoint call at end of model run.
C     myThid :: Thread number for this instance of the routine.
C     myIter :: Iteration number
C     myTime :: Current time of simulation ( s )
      LOGICAL modelEnd    
      INTEGER myThid
      INTEGER myIter
      _RL     myTime

C     == Common blocks ==
      COMMON /PCKP_GBLFLS/ globalFile
      LOGICAL globalFile

C     !LOCAL VARIABLES:
C     == Local variables ==
C     permCheckPoint :: Flag indicating whether a permanent checkpoint will
C                       be written.
C     oldPrc :: Temp. for holding I/O precision
C     fn     :: Temp. for building file name string.
C     lgf    :: Flag to indicate whether to use global file mode.
      LOGICAL permCheckPoint  
      INTEGER oldPrec
      CHARACTER*(MAX_LEN_FNAM) fn
      INTEGER prec
      LOGICAL lgf
CEOP

      permCheckPoint = .FALSE.
      permCheckPoint=
     &     DIFFERENT_MULTIPLE(pChkptFreq,myTime,deltaTClock)

      IF (
     &    (.NOT. modelEnd .AND. (
     &     permCheckPoint
     &     .OR.
     &     DIFFERENT_MULTIPLE(chkptFreq,myTime,deltaTClock)
     &     ) .AND. myIter.NE.nIter0
     &    ) 
     &     .OR.
     &    (
     &     modelEnd 
     &     .AND. .NOT.
     &     permCheckPoint 
     &     .AND. .NOT.
     &     DIFFERENT_MULTIPLE(chkptFreq,myTime,deltaTClock)
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

#ifdef ALLOW_NONHYDROSTATIC
        IF ( nonHydrostatic ) THEN
         CALL WRITE_REC_XYZ_RL('phi_nh',phi_nh,1,myIter,myThid)
         CALL WRITE_REC_XYZ_RL( 'gW',gW, 1,myIter,myThid)
c        CALL WRITE_REC_XYZ_RL( 'gWnm1',gWnm1, 1,myIter,myThid)
        ENDIF
#endif

#else /* OLD_STYLE_WITH_MANY_FILES */

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
        CALL MDSWRITEFIELD(fn,prec,lgf,'RL',Nr,wVel, 7,myIter,myThid)
        CALL MDSWRITEFIELD(fn,prec,lgf,'RL',Nr,theta, 8,myIter,myThid)
        CALL MDSWRITEFIELD(fn,prec,lgf,'RL',Nr,gT,    9,myIter,myThid)
        CALL MDSWRITEFIELD(fn,prec,lgf,'RL',Nr,gTnm1, 10,myIter,myThid)
        CALL MDSWRITEFIELD(fn,prec,lgf,'RL',Nr,salt, 11,myIter,myThid)
        CALL MDSWRITEFIELD(fn,prec,lgf,'RL',Nr,gS,   12,myIter,myThid)
        CALL MDSWRITEFIELD(fn,prec,lgf,'RL',Nr,gSnm1,13,myIter,myThid)
        CALL MDSWRITEFIELD(fn,prec,lgf,'RL', 1,etaN,13*Nr+1,
     &                     myIter,myThid)
#ifdef NONLIN_FRSURF
        CALL MDSWRITEFIELD(fn,prec,lgf,'RL', 1,etaH,13*Nr+2,
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
         CALL MDSWRITEFIELD(fn,prec,lgf,'RL',Nr,phi_nh,1,myIter,myThid)
         CALL MDSWRITEFIELD(fn,prec,lgf,'RL',Nr,gW,   2,myIter,myThid)
c        CALL MDSWRITEFIELD(fn,prec,lgf,'RL',Nr,gWnm1,3,myIter,myThid)
        ENDIF
#endif

        IF ( .NOT. permCheckPoint ) THEN
         nCheckLev = MOD(nCheckLev, maxNoChkptLev)+1
        ENDIF

#endif /* OLD_STYLE_WITH_MANY_FILES */

C--     Reset binary precision
        writeBinaryPrec = oldPrec

       _END_MASTER( myThid )
       _BARRIER

      ENDIF

      RETURN
      END

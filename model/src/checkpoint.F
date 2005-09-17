C $Header: /u/gcmpack/MITgcm/model/src/Attic/checkpoint.F,v 1.57 2005/09/17 03:17:05 edhill Exp $
C $Name:  $

#include "PACKAGES_CONFIG.h"
#include "CPP_OPTIONS.h"

C---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|
CBOP
C     !ROUTINE: SET_WRITE_GLOBAL_PICKUP
C     !INTERFACE:
      SUBROUTINE SET_WRITE_GLOBAL_PICKUP( flag )

C     !DESCRIPTION:
C     Sets an internal logical state to indicate whether files written
C     by subsequent calls to the READ_WRITE_FLD package should create
C     "global" or "tiled" files:
C     \begin{center}
C       \begin{tabular}[h]{|l|l|}\hline
C         \texttt{flag}  &  Meaning  \\\hline
C         \texttt{.TRUE.}  &  use ``global'' files  \\
C         \texttt{.TRUE.}  &  use ``tiled'' files  \\\hline
C       \end{tabular}
C     \end{center}

C     !USES:
      IMPLICIT NONE

C     !INPUT PARAMETERS:
      LOGICAL flag
CEOP
      COMMON /PCKP_GBLFLS/ globalFile
      LOGICAL globalFile
      
      globalFile = flag

      RETURN
      END

C---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|
CBOP
C     !ROUTINE: READ_CHECKPOINT
C     !INTERFACE:
      SUBROUTINE READ_CHECKPOINT( 
     I     myIter, myThid )

C     !DESCRIPTION:
C     This is the controlling routine for IO to write restart (or
C     ``pickup'' or ``checkpoint'') files.  It calls routines from other
C     packages (\textit{eg.} mdsio and mnc) to do the per-variable
C     reads.

C     !USES:
      IMPLICIT NONE
#include "SIZE.h"
#include "EEPARAMS.h"
#include "PARAMS.h"
#ifdef ALLOW_MNC
#include "MNC_PARAMS.h"
#endif
#include "DYNVARS.h"
#include "SURFACE.h"
#ifdef ALLOW_NONHYDROSTATIC
#include "GW.h"
#include "SOLVE_FOR_PRESSURE3D.h"
#endif
      INTEGER  IO_ERRCOUNT
      EXTERNAL IO_ERRCOUNT

C     !INPUT/OUTPUT PARAMETERS:
C     myThid - Thread number for this instance of the routine.
C     myIter - Iteration number
      INTEGER myThid
      INTEGER myIter
CEOP

C     !LOCAL VARIABLES:
C     oldPrec :: Temp. for hold I/O precision information
C     prec    
C     fn      :: Temp. for building file name.
      INTEGER oldPrec, prec
      INTEGER i, j, nj
      CHARACTER*(MAX_LEN_FNAM) fn
      CHARACTER*(10) suff

C     Suffix for pickup files
      DO i = 1,MAX_LEN_FNAM
        fn(i:i) = ' '
      ENDDO
      IF (pickupSuff .EQ. ' ') THEN
        WRITE(suff,'(I10.10)') myIter
      ELSE
        WRITE(suff,'(A10)') pickupSuff
      ENDIF
      WRITE(fn,'(A,A10)') 'pickup.',suff
      
C     Going to really do some IO. Make everyone except master thread wait.
      _BARRIER
      _BEGIN_MASTER( myThid )

      IF (pickup_read_mdsio) THEN
      
#ifdef OLD_STYLE_WITH_MANY_FILES

C       Force 64-bit IO
        oldPrec        = readBinaryPrec
        readBinaryPrec = precFloat64

C       Read model fields
C       Raw fields
        CALL READ_REC_XYZ_RL(  'uVel',   uVel, 1,myIter,myThid)
        CALL READ_REC_XYZ_RL(    'gU',     gU, 1,myIter,myThid)
        CALL READ_REC_XYZ_RL( 'guNm1',  guNm1, 1,myIter,myThid)
        CALL READ_REC_XYZ_RL(  'vVel',   vVel, 1,myIter,myThid)
        CALL READ_REC_XYZ_RL(    'gV',     gV, 1,myIter,myThid)
        CALL READ_REC_XYZ_RL( 'gvNm1',  gvNm1, 1,myIter,myThid)
        CALL READ_REC_XYZ_RL( 'theta',  theta, 1,myIter,myThid)
        CALL READ_REC_XYZ_RL(    'gT',     gT, 1,myIter,myThid)
        CALL READ_REC_XYZ_RL( 'gtNm1',  gtNm1, 1,myIter,myThid)
        CALL READ_REC_XYZ_RL(  'salt',   salt, 1,myIter,myThid)
        CALL READ_REC_XYZ_RL(    'gS',     gS, 1,myIter,myThid)
        CALL READ_REC_XYZ_RL( 'gsNm1',  gsNm1, 1,myIter,myThid)
        CALL READ_REC_XY_RL (  'etaN',   etaN, 1,myIter,myThid)
        
#ifdef ALLOW_NONHYDROSTATIC
        IF ( nonHydrostatic ) THEN
          CALL READ_REC_XYZ_RL('phi_nh',phi_nh,1,myIter,myThid)
          CALL READ_REC_XYZ_RL(    'gW',    gW,1,myIter,myThid)
c         CALL READ_REC_XYZ_RL( 'gWnm1', gWnm1,1,myIter,myThid)
        ENDIF
#endif

C       Reset default IO precision
        readBinaryPrec = oldPrec
      
#else /* OLD_STYLE_WITH_MANY_FILES */
      
        prec = precFloat64

#ifdef ALLOW_MDSIO

C       Read model fields
        IF ( usePickupBeforeC54 ) THEN
#ifndef ALLOW_ADAMSBASHFORTH_3
          CALL MDSREADFIELD(fn,prec,'RL',Nr,uVel,   1,myThid)
          CALL MDSREADFIELD(fn,prec,'RL',Nr,gU,     2,myThid)
          CALL MDSREADFIELD(fn,prec,'RL',Nr,guNm1,  3,myThid)
          CALL MDSREADFIELD(fn,prec,'RL',Nr,vVel,   4,myThid)
          CALL MDSREADFIELD(fn,prec,'RL',Nr,gV,     5,myThid)
          CALL MDSREADFIELD(fn,prec,'RL',Nr,gvNm1,  6,myThid)
          CALL MDSREADFIELD(fn,prec,'RL',Nr,theta,  7,myThid)
          CALL MDSREADFIELD(fn,prec,'RL',Nr,gT,     8,myThid)
          CALL MDSREADFIELD(fn,prec,'RL',Nr,gtNm1,  9,myThid)
          CALL MDSREADFIELD(fn,prec,'RL',Nr,salt,  10,myThid)
          CALL MDSREADFIELD(fn,prec,'RL',Nr,gS,    11,myThid)
          CALL MDSREADFIELD(fn,prec,'RL',Nr,gsNm1, 12,myThid)
#endif /*  ALLOW_ADAMSBASHFORTH_3 */
          CALL MDSREADFIELD(fn,prec,'RL', 1,etaN,12*Nr+1,myThid)
#ifdef NONLIN_FRSURF
          IF (nonlinFreeSurf .GE. 0) THEN
            CALL MDSREADFIELD(fn,prec,'RL',1,etaH,12*Nr+2,myThid)
          ENDIF
#endif
        ELSE 
#ifdef ALLOW_ADAMSBASHFORTH_3
          j = 3
          IF ( startFromPickupAB2 ) j = 2
          nj = 0
          CALL MDSREADFIELD(fn,prec,'RL',Nr,uVel, nj+1,myThid)
          CALL MDSREADFIELD(fn,prec,'RL',Nr,
     &                guNm(1-Olx,1-Oly,1,1,1,1),  nj+2,myThid)
          CALL MDSREADFIELD(fn,prec,'RL',Nr,
     &                guNm(1-Olx,1-Oly,1,1,1,2),  nj+j,myThid)
          nj = j
          CALL MDSREADFIELD(fn,prec,'RL',Nr,vVel, nj+1,myThid)
          CALL MDSREADFIELD(fn,prec,'RL',Nr,
     &                gvNm(1-Olx,1-Oly,1,1,1,1),  nj+2,myThid)
          CALL MDSREADFIELD(fn,prec,'RL',Nr,
     &                gvNm(1-Olx,1-Oly,1,1,1,2),  nj+j,myThid)
          nj = 2*j
          CALL MDSREADFIELD(fn,prec,'RL',Nr,theta,nj+1,myThid)
          CALL MDSREADFIELD(fn,prec,'RL',Nr,
     &                gtNm(1-Olx,1-Oly,1,1,1,1),  nj+2,myThid)
          CALL MDSREADFIELD(fn,prec,'RL',Nr,
     &                gtNm(1-Olx,1-Oly,1,1,1,2),  nj+j,myThid)
          nj = 3*j
          CALL MDSREADFIELD(fn,prec,'RL',Nr,salt, nj+1,myThid)
          CALL MDSREADFIELD(fn,prec,'RL',Nr,
     &                gsNm(1-Olx,1-Oly,1,1,1,1),  nj+2,myThid)
          CALL MDSREADFIELD(fn,prec,'RL',Nr,
     &                gsNm(1-Olx,1-Oly,1,1,1,2),  nj+j,myThid)
          nj = 4*j
#else /*  ALLOW_ADAMSBASHFORTH_3 */
          CALL MDSREADFIELD(fn,prec,'RL',Nr,uVel,   1,myThid)
          CALL MDSREADFIELD(fn,prec,'RL',Nr,guNm1,  2,myThid)
          CALL MDSREADFIELD(fn,prec,'RL',Nr,vVel,   3,myThid)
          CALL MDSREADFIELD(fn,prec,'RL',Nr,gvNm1,  4,myThid)
          CALL MDSREADFIELD(fn,prec,'RL',Nr,theta,  5,myThid)
          CALL MDSREADFIELD(fn,prec,'RL',Nr,gtNm1,  6,myThid)
          CALL MDSREADFIELD(fn,prec,'RL',Nr,salt,   7,myThid)
          CALL MDSREADFIELD(fn,prec,'RL',Nr,gsNm1,  8,myThid)
          nj = 8
#endif /*  ALLOW_ADAMSBASHFORTH_3 */
          CALL MDSREADFIELD(fn,prec,'RL', 1,etaN,    nj*Nr+1,myThid)
#ifdef EXACT_CONSERV
          IF (exactConserv) THEN
            CALL MDSREADFIELD(fn,prec,'RL',1,dEtaHdt,nj*Nr+2,myThid)
          ENDIF
          IF (nonlinFreeSurf .GT. 0) THEN 
            CALL MDSREADFIELD(fn,prec,'RL',1,etaH,   nj*Nr+3,myThid)
          ENDIF
#endif
        ENDIF 

        IF ( useDynP_inEos_Zc ) THEN
          WRITE(fn,'(A,A10)') 'pickup_ph.',suff
          CALL MDSREADFIELD(fn,prec,'RL',Nr,totPhiHyd,1,myThid)
        ENDIF
#ifdef ALLOW_NONHYDROSTATIC
        IF ( nonHydrostatic ) THEN
          WRITE(fn,'(A,A10)') 'pickup_nh.',suff
          CALL MDSREADFIELD(fn,prec,'RL',Nr,phi_nh,1,myThid)
          CALL MDSREADFIELD(fn,prec,'RL',Nr,    gW,2,myThid)
c         CALL MDSREADFIELD(fn,prec,'RL',Nr, gWnm1,3,myThid)
        ENDIF
#endif

#endif /* ALLOW_MDSIO */

#endif /* OLD_STYLE_WITH_MANY_FILES */

      ENDIF
      
#ifdef ALLOW_MNC
      IF (useMNC .AND. pickup_read_mnc) THEN
        WRITE(fn,'(A)') 'pickup'
        CALL MNC_FILE_CLOSE_ALL_MATCHING(fn, myThid)
        CALL MNC_CW_SET_UDIM(fn, 1, myThid)
        CALL MNC_CW_SET_CITER(fn, 3, 3, myIter, -1, myThid)
        CALL MNC_CW_RL_R('D',fn,0,0,'U',uVel, myThid)
        CALL MNC_CW_RL_R('D',fn,0,0,'V',vVel, myThid)
        CALL MNC_CW_RL_R('D',fn,0,0,'Temp',theta, myThid)
        CALL MNC_CW_RL_R('D',fn,0,0,'S',salt, myThid)
        CALL MNC_CW_RL_R('D',fn,0,0,'Eta',etaN, myThid)
#ifndef ALLOW_ADAMSBASHFORTH_3
        CALL MNC_CW_RL_R('D',fn,0,0,'gUnm1',guNm1, myThid)
        CALL MNC_CW_RL_R('D',fn,0,0,'gVnm1',gvNm1, myThid)
        CALL MNC_CW_RL_R('D',fn,0,0,'gTnm1',gtNm1, myThid)
        CALL MNC_CW_RL_R('D',fn,0,0,'gSnm1',gsNm1, myThid)
#endif /* ALLOW_ADAMSBASHFORTH_3 */
C#ifdef NONLIN_FRSURF
C        IF ( nonlinFreeSurf.GE.0 .AND. usePickupBeforeC54 )
C     &    CALL MNC_CW_RL_R('D',fn,0,0,'EtaH', etaH, myThid)
C#endif
#ifdef EXACT_CONSERV
        IF (exactConserv) THEN
          CALL MNC_CW_RL_R('D',fn,0,0,'dEtaHdt',dEtaHdt,myThid)
        ENDIF
        IF (nonlinFreeSurf .GT. 0) THEN
          CALL MNC_CW_RL_R('D',fn,0,0,'EtaH', etaH, myThid)
        ENDIF
#endif
#ifdef ALLOW_NONHYDROSTATIC
        IF (nonHydrostatic) THEN
          CALL MNC_CW_RL_R('D',fn,0,0,'phi_nh', phi_nh, myThid)
          CALL MNC_CW_RL_R('D',fn,0,0,'gW', gW, myThid)
        ENDIF
#endif
        IF ( useDynP_inEos_Zc ) THEN
          CALL MNC_CW_RL_R('D',fn,0,0,'phiHyd',totPhiHyd,myThid)
        ENDIF
      ENDIF
#endif /*  ALLOW_MNC  */

      _END_MASTER( myThid )
      _BARRIER
      
C     Fill in edge regions
      CALL EXCH_UV_XYZ_RL(uVel,vVel,.TRUE.,myThid)
      _EXCH_XYZ_R8(theta , myThid )
      _EXCH_XYZ_R8(salt  , myThid )
c     CALL EXCH_UV_XYZ_RL(gU,gV,.TRUE.,myThid)
c     _EXCH_XYZ_R8(gt    , myThid )
c     _EXCH_XYZ_R8(gs    , myThid )
#ifdef ALLOW_ADAMSBASHFORTH_3
      CALL EXCH_UV_XYZ_RL(guNm(1-Olx,1-Oly,1,1,1,1),
     &                    gvNm(1-Olx,1-Oly,1,1,1,1),.TRUE.,myThid)
      _EXCH_XYZ_R8( gtNm(1-Olx,1-Oly,1,1,1,1), myThid )
      _EXCH_XYZ_R8( gsNm(1-Olx,1-Oly,1,1,1,1), myThid )
      CALL EXCH_UV_XYZ_RL(guNm(1-Olx,1-Oly,1,1,1,2),
     &                    gvNm(1-Olx,1-Oly,1,1,1,2),.TRUE.,myThid)
      _EXCH_XYZ_R8( gtNm(1-Olx,1-Oly,1,1,1,2), myThid )
      _EXCH_XYZ_R8( gsNm(1-Olx,1-Oly,1,1,1,2), myThid )
#else /* ALLOW_ADAMSBASHFORTH_3 */
      CALL EXCH_UV_XYZ_RL(guNm1,gvNm1,.TRUE.,myThid)
      _EXCH_XYZ_R8(gtNm1 , myThid )
      _EXCH_XYZ_R8(gsNm1 , myThid )
#endif /* ALLOW_ADAMSBASHFORTH_3 */
      _EXCH_XY_R8 (etaN, myThid )
      _EXCH_XY_R8( etaH,    myThid )
#ifdef EXACT_CONSERV
      _EXCH_XY_R8( detaHdt, myThid )
#endif

      IF ( useDynP_inEos_Zc )
     &     _EXCH_XYZ_RL( totPhiHyd, myThid )

#ifdef ALLOW_NONHYDROSTATIC
      IF ( nonHydrostatic ) THEN
        _EXCH_XYZ_R8(phi_nh, myThid )
        _EXCH_XYZ_R8(gW    , myThid )
c       _EXCH_XYZ_R8(gWNM1 , myThid )
      ENDIF
#endif
      
      RETURN
      END

C---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|
CBOP
C     !ROUTINE: WRITE_CHECKPOINT
C     !INTERFACE:
      SUBROUTINE WRITE_CHECKPOINT( 
     I     modelEnd, myTime, 
     I     myIter, myThid )

C     !DESCRIPTION:
C     This is the controlling routine for IO to write restart (or
C     ``pickup'' or ``checkpoint'') files.  It calls routines from other
C     packages (\textit{eg.} mdsio and mnc) to do the per-variable
C     writes.
C
C     Both ``rolling-checkpoint'' files and permanent checkpoint files
C     are written here. A rolling checkpoint works through a circular
C     list of suffices. Generally the circular list has two entries so
C     that a rolling checkpoint will overwrite the last rolling
C     checkpoint but one. This is useful for running long jobs without
C     filling too much disk space.  In a permanent checkpoint, data is
C     written suffixed by the current timestep number. Permanent
C     checkpoints can be used to provide snap-shots from which the
C     model can be restarted.

C     !USES:
      IMPLICIT NONE
#include "SIZE.h"
#include "EEPARAMS.h"
#include "PARAMS.h"
#ifdef ALLOW_MNC
#include "MNC_PARAMS.h"
#endif
      LOGICAL  DIFFERENT_MULTIPLE
      EXTERNAL DIFFERENT_MULTIPLE
      INTEGER  IO_ERRCOUNT
      EXTERNAL IO_ERRCOUNT

C     !INPUT PARAMETERS:
C     modelEnd  :: Checkpoint call at end of model run.
C     myThid    :: Thread number for this instance of the routine.
C     myIter    :: Iteration number
C     myTime    :: Current time of simulation ( s )
      LOGICAL modelEnd    
      INTEGER myThid
      INTEGER myIter
      _RL     myTime
CEOP

C     !LOCAL VARIABLES:
C     permCheckPoint :: Flag indicating whether a permanent checkpoint will
C                       be written.
C     tempCheckPoint :: Flag indicating if it is time to write a non-permanent
C                       checkpoint (that will be permanent if permCheckPoint=T)
      LOGICAL permCheckPoint, tempCheckPoint  
      INTEGER thisdate(4), prevdate(4)

      permCheckPoint = .FALSE.
      tempCheckPoint = .FALSE.
      permCheckPoint =
     &     DIFFERENT_MULTIPLE(pChkptFreq,myTime,deltaTClock)
      tempCheckPoint =
     &     DIFFERENT_MULTIPLE( ChkptFreq,myTime,deltaTClock)

#ifdef ALLOW_CAL
      IF ( calendarDumps ) THEN
C--   Convert approximate months (30-31 days) and years (360-372 days)
C     to exact calendar months and years.
C-    First determine calendar dates for this and previous time step.
         call cal_GetDate( myiter  ,mytime            ,thisdate,mythid )
         call cal_GetDate( myiter-1,mytime-deltaTClock,prevdate,mythid )
C-    Monthly pChkptFreq:
         IF( pChkptFreq.GE. 2592000 .AND. pChkptFreq.LE. 2678400 ) THEN
            permCheckPoint = .FALSE.
            IF((thisdate(1)-prevdate(1)) .GT. 50  )permCheckPoint=.TRUE.
         ENDIF
C-    Yearly  pChkptFreq:
         IF( pChkptFreq.GE.31104000 .AND. pChkptFreq.LE.31968000 ) THEN
            permCheckPoint = .FALSE.
            IF((thisdate(1)-prevdate(1)) .GT. 5000)permCheckPoint=.TRUE.
         ENDIF
C-    Monthly  ChkptFreq:
         IF(  ChkptFreq.GE. 2592000 .AND.  ChkptFreq.LE. 2678400 ) THEN
            tempCheckPoint = .FALSE.
            IF((thisdate(1)-prevdate(1)) .GT. 50  )tempCheckPoint=.TRUE.
         ENDIF
C-    Yearly   ChkptFreq:
         IF(  ChkptFreq.GE.31104000 .AND.  ChkptFreq.LE.31968000 ) THEN
            tempCheckPoint = .FALSE.
            IF((thisdate(1)-prevdate(1)) .GT. 5000)tempCheckPoint=.TRUE.
         ENDIF
      ENDIF
#endif

      IF (
     &     ( .NOT.modelEnd .AND. (permCheckPoint.OR.tempCheckPoint) )
     &     .OR.
     &     ( modelEnd .AND. .NOT.(permCheckPoint.OR.tempCheckPoint) )
     &     ) THEN

        CALL WRITE_CHECKPOINT_NOW(
     &       permCheckPoint, myTime, myIter, myThid )


      ENDIF
      RETURN
      END

C---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|
CBOP
C     !ROUTINE: WRITE_CHECKPOINT_NOW
C     !INTERFACE:
      SUBROUTINE WRITE_CHECKPOINT_NOW( 
     I     permCheckPoint, myTime, 
     I     myIter, myThid )

C     !DESCRIPTION:
C     Write the checkpoint and do it NOW.

C     !USES:
      IMPLICIT NONE
#include "SIZE.h"
#include "EEPARAMS.h"
#include "PARAMS.h"
#ifdef ALLOW_MNC
#include "MNC_PARAMS.h"
#endif
#include "DYNVARS.h"
#include "SURFACE.h"
#ifdef ALLOW_NONHYDROSTATIC
#include "GW.h"
#include "SOLVE_FOR_PRESSURE3D.h"
#endif
      INTEGER  IO_ERRCOUNT
      EXTERNAL IO_ERRCOUNT
      COMMON /PCKP_GBLFLS/ globalFile
      LOGICAL globalFile

C     !INPUT PARAMETERS:
C     permCheckPoint  :: Is or is not a permanent checkpoint.
C     myThid          :: Thread number for this instance of the routine.
C     myIter          :: Iteration number
C     myTime          :: Current time of simulation ( s )
      LOGICAL permCheckPoint
      INTEGER myThid
      INTEGER myIter
      _RL     myTime
CEOP

C     !LOCAL VARIABLES:
C     oldPrc :: Temp. for holding I/O precision
C     fn     :: Temp. for building file name string.
C     lgf    :: Flag to indicate whether to use global file mode.
      INTEGER oldPrec, prec
      INTEGER i, nj
      CHARACTER*(MAX_LEN_FNAM) fn
      CHARACTER*(MAX_LEN_MBUF) msgBuf
      LOGICAL lgf

C     Write model fields
      DO i = 1,MAX_LEN_FNAM
        fn(i:i) = ' '
      ENDDO
      IF ( permCheckPoint ) THEN
        WRITE(fn,'(A,I10.10)') 'pickup.',myIter
      ELSE
        WRITE(fn,'(A,A)') 'pickup.',checkPtSuff(nCheckLev)
      ENDIF
          
C     Going to really do some IO. Make everyone except master thread wait.
      _BARRIER
      _BEGIN_MASTER( myThid )
      
      IF (pickup_write_mdsio) THEN

#ifdef OLD_STYLE_WITH_MANY_FILES
      
C       Force 64-bit IO
        oldPrec = writeBinaryPrec
        writeBinaryPrec = precFloat64
C       Write model fields
C       Raw fields
        CALL WRITE_REC_XYZ_RL(  'uVel',   uVel, 1,myIter,myThid)
        CALL WRITE_REC_XYZ_RL(    'gU',     gU, 1,myIter,myThid)
        CALL WRITE_REC_XYZ_RL( 'gUNm1',  gUNm1, 1,myIter,myThid)
        CALL WRITE_REC_XYZ_RL(  'vVel',   vVel, 1,myIter,myThid)
        CALL WRITE_REC_XYZ_RL(    'gV',     gV, 1,myIter,myThid)
        CALL WRITE_REC_XYZ_RL( 'gVNm1',  gVNm1, 1,myIter,myThid)
        CALL WRITE_REC_XYZ_RL( 'theta',  theta, 1,myIter,myThid)
        CALL WRITE_REC_XYZ_RL(    'gT',     gT, 1,myIter,myThid)
        CALL WRITE_REC_XYZ_RL( 'gTNm1',  gTNm1, 1,myIter,myThid)
        CALL WRITE_REC_XYZ_RL(  'salt',   salt, 1,myIter,myThid)
        CALL WRITE_REC_XYZ_RL(    'gS',     gS, 1,myIter,myThid)
        CALL WRITE_REC_XYZ_RL( 'gSNm1',  gSNm1, 1,myIter,myThid)
        CALL WRITE_REC_XY_RL (  'etaN',  etaN,  1,myIter,myThid)
#ifdef ALLOW_NONHYDROSTATIC
        IF ( nonHydrostatic ) THEN
          CALL WRITE_REC_XYZ_RL('phi_nh',phi_nh,1,myIter,myThid)
          CALL WRITE_REC_XYZ_RL(    'gW',    gW,1,myIter,myThid)
C         CALL WRITE_REC_XYZ_RL( 'gWnm1', gWnm1,1,myIter,myThid)
        ENDIF
#endif
C       Reset binary precision
        writeBinaryPrec = oldPrec
        
#else /* OLD_STYLE_WITH_MANY_FILES */
      
        prec = precFloat64
        lgf = globalFile

#ifdef ALLOW_MDSIO

#ifdef ALLOW_ADAMSBASHFORTH_3
        CALL MDSWRITEFIELD(fn,prec,lgf,'RL',Nr,uVel,  1,myIter,myThid)
        CALL MDSWRITEFIELD(fn,prec,lgf,'RL',Nr,
     &                     guNm(1-Olx,1-Oly,1,1,1,1), 2,myIter,myThid)
        CALL MDSWRITEFIELD(fn,prec,lgf,'RL',Nr,
     &                     guNm(1-Olx,1-Oly,1,1,1,2), 3,myIter,myThid)
        CALL MDSWRITEFIELD(fn,prec,lgf,'RL',Nr,vVel,  4,myIter,myThid)
        CALL MDSWRITEFIELD(fn,prec,lgf,'RL',Nr,
     &                     gvNm(1-Olx,1-Oly,1,1,1,1), 5,myIter,myThid)
        CALL MDSWRITEFIELD(fn,prec,lgf,'RL',Nr,
     &                     gvNm(1-Olx,1-Oly,1,1,1,2), 6,myIter,myThid)
        CALL MDSWRITEFIELD(fn,prec,lgf,'RL',Nr,theta, 7,myIter,myThid)
        CALL MDSWRITEFIELD(fn,prec,lgf,'RL',Nr,
     &                     gtNm(1-Olx,1-Oly,1,1,1,1), 8,myIter,myThid)
        CALL MDSWRITEFIELD(fn,prec,lgf,'RL',Nr,
     &                     gtNm(1-Olx,1-Oly,1,1,1,2), 9,myIter,myThid)
        CALL MDSWRITEFIELD(fn,prec,lgf,'RL',Nr,salt, 10,myIter,myThid)
        CALL MDSWRITEFIELD(fn,prec,lgf,'RL',Nr,
     &                     gsNm(1-Olx,1-Oly,1,1,1,1),11,myIter,myThid)
        CALL MDSWRITEFIELD(fn,prec,lgf,'RL',Nr,
     &                     gsNm(1-Olx,1-Oly,1,1,1,2),12,myIter,myThid)
        nj = 12
#else /*  ALLOW_ADAMSBASHFORTH_3 */
        CALL MDSWRITEFIELD(fn,prec,lgf,'RL',Nr,uVel, 1,myIter,myThid)
        CALL MDSWRITEFIELD(fn,prec,lgf,'RL',Nr,guNm1,2,myIter,myThid)
        CALL MDSWRITEFIELD(fn,prec,lgf,'RL',Nr,vVel, 3,myIter,myThid)
        CALL MDSWRITEFIELD(fn,prec,lgf,'RL',Nr,gvNm1,4,myIter,myThid)
        CALL MDSWRITEFIELD(fn,prec,lgf,'RL',Nr,theta,5,myIter,myThid)
        CALL MDSWRITEFIELD(fn,prec,lgf,'RL',Nr,gtNm1,6,myIter,myThid)
        CALL MDSWRITEFIELD(fn,prec,lgf,'RL',Nr,salt, 7,myIter,myThid)
        CALL MDSWRITEFIELD(fn,prec,lgf,'RL',Nr,gsNm1,8,myIter,myThid)
        nj = 8
#endif /*  ALLOW_ADAMSBASHFORTH_3 */
        CALL MDSWRITEFIELD(fn,prec,lgf,'RL', 1,etaN,   nj*Nr+1,
     &       myIter,myThid)
#ifdef EXACT_CONSERV
        CALL MDSWRITEFIELD(fn,prec,lgf,'RL', 1,dEtaHdt,nj*Nr+2,
     &       myIter,myThid)
        CALL MDSWRITEFIELD(fn,prec,lgf,'RL', 1,etaHnm1,nj*Nr+3,
     &       myIter,myThid)
#endif /* EXACT_CONSERV */
        IF ( useDynP_inEos_Zc ) THEN
          IF ( permCheckPoint ) THEN
            WRITE(fn,'(A,I10.10)') 'pickup_ph.',myIter
          ELSE
            WRITE(fn,'(A,A)') 'pickup_ph.',checkPtSuff(nCheckLev)
          ENDIF
          CALL MDSWRITEFIELD(fn,prec,lgf,'RL',Nr,totPhiHyd,
     &         1,myIter,myThid)
        ENDIF
#ifdef ALLOW_NONHYDROSTATIC
        IF ( nonHydrostatic ) THEN
          IF ( permCheckPoint ) THEN
            WRITE(fn,'(A,I10.10)') 'pickup_nh.',myIter
          ELSE
            WRITE(fn,'(A,A)') 'pickup_nh.',checkPtSuff(nCheckLev)
          ENDIF
          CALL MDSWRITEFIELD(fn,prec,lgf,'RL',Nr,phi_nh, 1,
     &         myIter,myThid)
          CALL MDSWRITEFIELD(fn,prec,lgf,'RL',Nr,gW,     2,
     &         myIter,myThid)
C         CALL MDSWRITEFIELD(fn,prec,lgf,'RL',Nr,gWnm1,  3,
C         &    myIter,myThid)
        ENDIF
#endif /* ALLOW_NONHYDROSTATIC */

#endif /* ALLOW_MDSIO */

#endif /* OLD_STYLE_WITH_MANY_FILES */

      ENDIF

#ifdef ALLOW_MNC
      IF (useMNC .AND. pickup_write_mnc) THEN
        IF ( permCheckPoint ) THEN
          WRITE(fn,'(A)') 'pickup'
        ELSE
          WRITE(fn,'(A,A)') 'pickup.',checkPtSuff(nCheckLev)
        ENDIF
C       First ***define*** the file group name
        CALL MNC_CW_SET_UDIM(fn, 0, myThid)
        IF ( permCheckPoint ) THEN
          CALL MNC_CW_SET_CITER(fn, 3, 3, myIter, 0, myThid)
        ELSE
          CALL MNC_CW_SET_CITER(fn, 2, -1, -1, -1, myThid)
        ENDIF
C       Then set the actual unlimited dimension
        CALL MNC_CW_SET_UDIM(fn, 1, myThid)
        CALL MNC_CW_RL_W_S('D',fn,0,0,'T', myTime, myThid)
        CALL MNC_CW_I_W_S('I',fn,0,0,'iter', myIter, myThid)
        CALL MNC_CW_RL_W('D',fn,0,0,'U', uVel, myThid)
        CALL MNC_CW_RL_W('D',fn,0,0,'V', vVel, myThid)
        CALL MNC_CW_RL_W('D',fn,0,0,'Temp', theta, myThid)
        CALL MNC_CW_RL_W('D',fn,0,0,'S', salt, myThid)
        CALL MNC_CW_RL_W('D',fn,0,0,'Eta', etaN, myThid)
#ifndef ALLOW_ADAMSBASHFORTH_3
        CALL MNC_CW_RL_W('D',fn,0,0,'gUnm1', guNm1, myThid)
        CALL MNC_CW_RL_W('D',fn,0,0,'gVnm1', gvNm1, myThid)
        CALL MNC_CW_RL_W('D',fn,0,0,'gTnm1', gtNm1, myThid)
        CALL MNC_CW_RL_W('D',fn,0,0,'gSnm1', gsNm1, myThid)
#endif /* ALLOW_ADAMSBASHFORTH_3 */
#ifdef EXACT_CONSERV
        CALL MNC_CW_RL_W('D',fn,0,0,'dEtaHdt', dEtaHdt, myThid)
        CALL MNC_CW_RL_W('D',fn,0,0,'EtaH', etaHnm1, myThid)
#endif
#ifdef ALLOW_NONHYDROSTATIC
        IF ( nonHydrostatic ) THEN
          CALL MNC_CW_RL_W('D',fn,0,0,'phi_nh', phi_nh, myThid)
          CALL MNC_CW_RL_W('D',fn,0,0,'gW', gW, myThid)
        ENDIF
#endif
        IF ( useDynP_inEos_Zc ) THEN
          CALL MNC_CW_RL_W('D',fn,0,0,'phiHyd', totPhiHyd, myThid)
        ENDIF
      ENDIF
#endif /*  ALLOW_MNC  */

C     Write suffix for stdout information
      IF ( permCheckPoint ) THEN
        WRITE(fn,'(I10.10)') myIter
      ELSE
        WRITE(fn,'(A)') checkPtSuff(nCheckLev)
      ENDIF
      
      IF ( .NOT. permCheckPoint ) THEN
        nCheckLev = MOD(nCheckLev, maxNoChkptLev)+1
      ENDIF
      
      _END_MASTER(myThid)
      _BARRIER
      
C     Write information to stdout so there is a record that the
C     checkpoint was completed
      _BEGIN_MASTER(myThid)
      WRITE(msgBuf,'(A11,I10,1X,A10)')
     &     "%CHECKPOINT ",myIter,fn
      CALL PRINT_MESSAGE(msgBuf,standardMessageUnit,SQUEEZE_RIGHT,1)
      _END_MASTER(myThid)
      
      RETURN
      END

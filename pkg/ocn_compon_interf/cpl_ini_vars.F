C $Header: /u/gcmpack/MITgcm/pkg/ocn_compon_interf/cpl_ini_vars.F,v 1.1 2003/12/15 02:49:09 jmc Exp $
C $Name:  $

#include "PACKAGES_CONFIG.h"
#include "CPP_OPTIONS.h"

CStartOfInterface
      SUBROUTINE CPL_INI_VARS( myThid )
C     /==========================================================\
C     | SUBROUTINE CPL_INI_VARS                                  |
C     | o Initialisation routine for arrays that are used to     |
C     |   buffer data that interfaces to the coupling layer.     |
C     | - Oceanic version -                                      |
C     |==========================================================|
C     | For now this routine sets these arrays to the right      |
C     | state for an initial start-up. Eventually it should      |
C     | include some way to load this data from non-volatile     |
C     | external storage if they are needed during a restart.    |
C     \==========================================================/
      IMPLICIT NONE

C     == Global variables ==
#include "SIZE.h"
#include "EEPARAMS.h"
#include "PARAMS.h"
#include "GRID.h"
#include "OCNCPL.h"
#include "CPL_DIAGS.h"

C     == Routine arguments ==
C     myThid - Thread number for this instance of the routine
      INTEGER myThid
CEndOfInterface

C     == Local variables ==
C     I,J,K,bi,bj - Loop counters
      INTEGER prec
      CHARACTER*(MAX_LEN_FNAM) fn
      INTEGER I,J,K,bi,bj

      DO bj=myByLo(myThid),myByHi(myThid)
       DO bi=myBxLo(myThid),myByLo(myThid)
        DO J=1-Oly,sNy+Oly
         DO I=1-Olx,sNx+Olx
C         Export field
          SSTocn2cpl(I,J,bi,bj) = 0.
C         Import field
          HeatFlux  (I,J,bi,bj)  = 0.
          tauX      (I,J,bi,bj)  = 0.
          tauY      (I,J,bi,bj)  = 0.
          FWflux    (I,J,bi,bj)  = 0.
          uVelGround(I,J,bi,bj)  = 0.
          vVelGround(I,J,bi,bj)  = 0.
          qLatent   (I,J,bi,bj)  = 0.
          qSensible (I,J,bi,bj)  = 0.
          qLongwave (I,J,bi,bj)  = 0.
          qShortwave(I,J,bi,bj)  = 0.
         ENDDO
        ENDDO
       ENDDO
      ENDDO

      IF ( .FALSE. ) THEN
       WRITE(fn,'(A,I10.10)') 'pickup_cpl.',nIter0
       CALL MDSREADFIELD(fn,prec,'RL',1,SSTocn2cpl, 1,myThid)
       CALL MDSREADFIELD(fn,prec,'RL',1,HeatFlux  , 2,myThid)
       CALL MDSREADFIELD(fn,prec,'RL',1,tauX      , 3,myThid)
       CALL MDSREADFIELD(fn,prec,'RL',1,tauY      , 4,myThid)
       CALL MDSREADFIELD(fn,prec,'RL',1,FWFlux    , 5,myThid)
       CALL MDSREADFIELD(fn,prec,'RL',1,uVelGround, 6,myThid)
       CALL MDSREADFIELD(fn,prec,'RL',1,vVelGround, 7,myThid)
       CALL MDSREADFIELD(fn,prec,'RL',1,qLatent   , 8,myThid)
       CALL MDSREADFIELD(fn,prec,'RL',1,qSensible , 9,myThid)
       CALL MDSREADFIELD(fn,prec,'RL',1,qLongwave ,10,myThid)
       CALL MDSREADFIELD(fn,prec,'RL',1,qShortwave,11,myThid)
      ENDIF

#ifdef ALLOW_TIMEAVE
C     Initialise diagnostic counters ( these are cleared on model starti i.e. not
C     loaded from history file for now ).
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
#endif /* ALLOW_TIMEAVE */


      RETURN
      END

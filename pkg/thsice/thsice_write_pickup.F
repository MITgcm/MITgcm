C $Header: /u/gcmpack/MITgcm/pkg/thsice/thsice_write_pickup.F,v 1.2 2005/06/24 04:36:54 edhill Exp $
C $Name:  $

#include "THSICE_OPTIONS.h"
CBOP
C     !ROUTINE: THSICE_WRITE_PICKUP
C     !INTERFACE:
      SUBROUTINE THSICE_WRITE_PICKUP( 
     I                       prec, lgf, permCheckPoint, 
     I                                  myIter, myThid )

C     !DESCRIPTION: \bv
C     *==========================================================*
C     | S/R THSICE_WRITE_PICKUP
C     | o Write thsice pickup file
C     *==========================================================*
C     \ev

C     !USES:
      IMPLICIT NONE

C     === Global variables ===
#include "SIZE.h"
#include "EEPARAMS.h"
#include "PARAMS.h"
#include "THSICE_PARAMS.h"
#include "THSICE_VARS.h"

C     == Common blocks ==
      COMMON /PCKP_GBLFLS/ globalFile
      LOGICAL globalFile

C     !INPUT/OUTPUT PARAMETERS:
C     == Routine arguments ==
      INTEGER prec
      LOGICAL lgf
      LOGICAL permCheckPoint
      INTEGER myIter
      INTEGER myThid
CEOP

#ifdef ALLOW_THSICE

C     == Local variables ==      
      CHARACTER*(MAX_LEN_FNAM) fn

      IF ( permCheckPoint ) THEN
        WRITE(fn,'(A,I10.10)') 'pickup_ic.',myIter
      ELSE
        WRITE(fn,'(A,A)') 'pickup_ic.',checkPtSuff(nCheckLev)
      ENDIF

      IF ( thSIce_pickup_write_mdsio ) THEN
        CALL MDSWRITEFIELD(fn,prec,lgf,'RL',1,iceMask,   1,
     &                                               myIter,myThid)
        CALL MDSWRITEFIELD(fn,prec,lgf,'RL',1,iceHeight, 2,
     &                                               myIter,myThid)
        CALL MDSWRITEFIELD(fn,prec,lgf,'RL',1,snowHeight,3,
     &                                               myIter,myThid)
        CALL MDSWRITEFIELD(fn,prec,lgf,'RL',1,Tsrf,   4, myIter,myThid)
        CALL MDSWRITEFIELD(fn,prec,lgf,'RL',1,Tice1,  5, myIter,myThid)
        CALL MDSWRITEFIELD(fn,prec,lgf,'RL',1,Tice2,  6, myIter,myThid)
        CALL MDSWRITEFIELD(fn,prec,lgf,'RL',1,Qice1,  7, myIter,myThid)
        CALL MDSWRITEFIELD(fn,prec,lgf,'RL',1,Qice2,  8, myIter,myThid)
        CALL MDSWRITEFIELD(fn,prec,lgf,'RL',1,snowAge,9, myIter,myThid)

        IF ( stepFwd_oceMxL ) THEN
        CALL MDSWRITEFIELD(fn,prec,lgf,'RL',1,tOceMxL,10,myIter,myThid)
        CALL MDSWRITEFIELD(fn,prec,lgf,'RL',1,sOceMxL,11,myIter,myThid)
        ENDIF
      ENDIF

#ifdef ALLOW_MNC
      IF ( thSIce_pickup_write_mnc ) THEN
        CALL MNC_CW_SET_UDIM(fn, -1, myThid)
        CALL MNC_CW_I_W_S('I',fn,0,0,'iter', myIter, myThid)
        CALL MNC_CW_SET_UDIM(fn, 0, myThid)
C       CALL MNC_CW_RL_W_S('D',fn,0,0,'T', myTime, myThid)
        CALL MNC_CW_RL_W('D',fn,0,0,'iceMask',   iceMask,   myThid)
        CALL MNC_CW_RL_W('D',fn,0,0,'iceHeight', iceHeight, myThid)
        CALL MNC_CW_RL_W('D',fn,0,0,'snowHeight',snowHeight,myThid)
        CALL MNC_CW_RL_W('D',fn,0,0,'Tsrf',      Tsrf,      myThid)
        CALL MNC_CW_RL_W('D',fn,0,0,'Tice1',     Tice1,     myThid)
        CALL MNC_CW_RL_W('D',fn,0,0,'Tice2',     Tice1,     myThid)
        CALL MNC_CW_RL_W('D',fn,0,0,'Qice1',     Qice1,     myThid)
        CALL MNC_CW_RL_W('D',fn,0,0,'Qice2',     Qice2,     myThid)
        CALL MNC_CW_RL_W('D',fn,0,0,'snowAge',   snowAge,   myThid)
        IF ( stepFwd_oceMxL ) THEN
          CALL MNC_CW_RL_W('D',fn,0,0,'tOceMxL',tOceMxL,myThid)
          CALL MNC_CW_RL_W('D',fn,0,0,'sOceMxL',sOceMxL,myThid)
        ENDIF
      ENDIF
#endif  /* ALLOW_MNC */

#endif  /* ALLOW_THSICE */
      RETURN
      END

C---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|

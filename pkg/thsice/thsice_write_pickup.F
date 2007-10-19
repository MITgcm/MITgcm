C $Header: /u/gcmpack/MITgcm/pkg/thsice/thsice_write_pickup.F,v 1.5 2007/10/19 03:21:39 jmc Exp $
C $Name:  $

#include "THSICE_OPTIONS.h"
CBOP
C     !ROUTINE: THSICE_WRITE_PICKUP
C     !INTERFACE:
      SUBROUTINE THSICE_WRITE_PICKUP( permPickup, suff,
     I                                myTime, myIter, myThid )

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

C     !INPUT/OUTPUT PARAMETERS:
C     == Routine arguments ==
C     permPickup :: write a permanent pickup
C     suff    :: suffix for pickup file (eg. ckptA or 0000000010)
C     myTime  :: Current time in simulation
C     myIter  :: Current iteration number in simulation
C     myThid  :: My Thread Id number
      LOGICAL permPickup
      CHARACTER*(*) suff
      _RL     myTime
      INTEGER myIter
      INTEGER myThid
CEOP

#ifdef ALLOW_THSICE

C     == Local variables ==
      CHARACTER*(MAX_LEN_FNAM) fn
      INTEGER prec

      IF ( thSIce_pickup_write_mdsio ) THEN
        prec = precFloat64
        WRITE(fn,'(A,A)') 'pickup_ic.',suff

        CALL WRITE_REC_3D_RL( fn,prec, 1, iceMask,  1, myIter,myThid )
        CALL WRITE_REC_3D_RL( fn,prec, 1, iceHeight,2, myIter,myThid )
        CALL WRITE_REC_3D_RL( fn,prec, 1,snowHeight,3, myIter,myThid )
        CALL WRITE_REC_3D_RL( fn,prec, 1, Tsrf,     4, myIter,myThid )
        CALL WRITE_REC_3D_RL( fn,prec, 1, Tice1,    5, myIter,myThid )
        CALL WRITE_REC_3D_RL( fn,prec, 1, Tice2,    6, myIter,myThid )
        CALL WRITE_REC_3D_RL( fn,prec, 1, Qice1,    7, myIter,myThid )
        CALL WRITE_REC_3D_RL( fn,prec, 1, Qice2,    8, myIter,myThid )
        CALL WRITE_REC_3D_RL( fn,prec, 1, snowAge,  9, myIter,myThid )

        IF ( stepFwd_oceMxL ) THEN
         CALL WRITE_REC_3D_RL( fn,prec,1, tOceMxL, 10, myIter,myThid )
         CALL WRITE_REC_3D_RL( fn,prec,1, sOceMxL, 11, myIter,myThid )
        ENDIF
      ENDIF

#ifdef ALLOW_MNC
      IF ( thSIce_pickup_write_mnc ) THEN
        IF ( permPickup ) THEN
          WRITE(fn,'(A)') 'pickup_ic'
        ELSE
          WRITE(fn,'(A,A)') 'pickup_ic.',suff
        ENDIF
C       First ***define*** the file group name
        CALL MNC_CW_SET_UDIM(fn, 0, myThid)
        IF ( permPickup ) THEN
          CALL MNC_CW_SET_CITER(fn, 3, 3, myIter, 0, myThid)
        ELSE
          CALL MNC_CW_SET_CITER(fn, 2, -1, -1, -1, myThid)
        ENDIF
C       Then set the actual unlimited dimension
        CALL MNC_CW_SET_UDIM(fn, 1, myThid)
        CALL MNC_CW_I_W_S('I',fn,0,0,'iter', myIter, myThid)
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

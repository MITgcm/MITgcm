C $Header: /u/gcmpack/MITgcm/pkg/thsice/thsice_write_pickup.F,v 1.1 2004/04/07 23:40:34 jmc Exp $
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
C---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|

#endif  /* ALLOW_THSICE */
      RETURN
      END

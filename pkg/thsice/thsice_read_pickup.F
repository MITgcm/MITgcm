C $Header: /u/gcmpack/MITgcm/pkg/thsice/thsice_read_pickup.F,v 1.1 2004/04/07 23:40:34 jmc Exp $
C $Name:  $

#include "THSICE_OPTIONS.h"

CBOP
C     !ROUTINE: THSICE_READ_PICKUP
C     !INTERFACE:
      SUBROUTINE THSICE_READ_PICKUP( prec, myIter, myThid )

C     !DESCRIPTION: \bv
C     *==========================================================*
C     | S/R THSICE_READ_PICKUP
C     | o Read thsice pickup file
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
      INTEGER prec
      INTEGER myIter
      INTEGER myThid
CEOP

#ifdef ALLOW_THSICE

C     == Local variables ==
      CHARACTER*(MAX_LEN_FNAM) fn

        WRITE(fn,'(A,I10.10)') 'pickup_ic.',myIter
        CALL MDSREADFIELD(fn,prec,'RL',1,iceMask,   1,myThid)
        CALL MDSREADFIELD(fn,prec,'RL',1,iceHeight, 2,myThid)
        CALL MDSREADFIELD(fn,prec,'RL',1,snowHeight,3,myThid)
        CALL MDSREADFIELD(fn,prec,'RL',1,Tsrf,      4,myThid)
        CALL MDSREADFIELD(fn,prec,'RL',1,Tice1,     5,myThid)
        CALL MDSREADFIELD(fn,prec,'RL',1,Tice2,     6,myThid)
        CALL MDSREADFIELD(fn,prec,'RL',1,Qice1,     7,myThid)
        CALL MDSREADFIELD(fn,prec,'RL',1,Qice2,     8,myThid)
        CALL MDSREADFIELD(fn,prec,'RL',1,snowAge,   9,myThid)

       IF ( stepFwd_oceMxL .AND. startIceModel.EQ.0 )  THEN
        CALL MDSREADFIELD(fn,prec,'RL',1,tOceMxL,  10,myThid)
        CALL MDSREADFIELD(fn,prec,'RL',1,sOceMxL,  11,myThid)
       ENDIF

#endif  /* ALLOW_THSICE */
      RETURN
      END

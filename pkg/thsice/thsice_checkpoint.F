C $Header: 
C $Name: 

#include "THSICE_OPTIONS.h"

CStartofinterface
      SUBROUTINE THSICE_READ_CHECKPOINT( prec, myIter, myThid )
C     /==========================================================\
C     | SUBROUTINE THSICE_READ_CHECKPOINT                           |
C     | o Read ice checkpoint arrays                             |
C     |==========================================================|
C     |                                                          |
C     \==========================================================/
      IMPLICIT NONE

C     === Global variables ===
#include "SIZE.h"
#include "EEPARAMS.h"
#include "PARAMS.h"
#include "THSICE.h"

C     == Routine arguments ==
      INTEGER prec
      INTEGER myIter
      INTEGER myThid
CEndofinterface


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
        CALL MDSREADFIELD(fn,prec,'RL',1,sage,      9,myThid)

#endif  /* ALLOW_THSICE */
      RETURN
      END


CStartofinterface
      SUBROUTINE THSICE_WRITE_CHECKPOINT( prec, lgf, permCheckPoint, 
     &                                  myIter, myThid )
C     /==========================================================\
C     | SUBROUTINE THSICE_WRITE_CHECKPOINT                         |
C     | o Write ice checkpoint arrays                           |
C     |==========================================================|
C     |                                                          |
C     \==========================================================/
      IMPLICIT NONE

C     === Global variables ===
#include "SIZE.h"
#include "EEPARAMS.h"
#include "PARAMS.h"
#include "THSICE.h"

C     == Common blocks ==
      COMMON /PCKP_GBLFLS/ globalFile
      LOGICAL globalFile

C     == Routine arguments ==
      INTEGER prec
      LOGICAL lgf
      LOGICAL permCheckPoint
      INTEGER myIter
      INTEGER myThid
CEndofinterface

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
        CALL MDSWRITEFIELD(fn,prec,lgf,'RL',1,Tsrf, 4,myIter,myThid)
        CALL MDSWRITEFIELD(fn,prec,lgf,'RL',1,Tice1,5,myIter,myThid)
        CALL MDSWRITEFIELD(fn,prec,lgf,'RL',1,Tice2,6,myIter,myThid)
        CALL MDSWRITEFIELD(fn,prec,lgf,'RL',1,Qice1,7,myIter,myThid)
        CALL MDSWRITEFIELD(fn,prec,lgf,'RL',1,Qice2,8,myIter,myThid)
        CALL MDSWRITEFIELD(fn,prec,lgf,'RL',1,sage ,9,myIter,myThid)

#endif  /* ALLOW_THSICE */
      RETURN
      END

C $Header: /u/gcmpack/MITgcm/pkg/cd_code/Attic/cd_code_checkpoint.F,v 1.1 2003/10/30 18:44:26 heimbach Exp $
C $Name:  $

#include "CD_CODE_OPTIONS.h"

      SUBROUTINE CD_CODE_READ_CHECKPOINT( myIter, myThid )

      IMPLICIT NONE
C     === Global variables ===
#include "SIZE.h"
#include "EEPARAMS.h"
#include "PARAMS.h"
#include "CD_CODE_VARS.h"

C     == Routine arguments ==
C     myThid -  Number of this instance of INI_UVEL 
      INTEGER myIter
      INTEGER myThid

C     !LOCAL VARIABLES:
C     == Local variables ==
C     bi,bj  - Loop counters
C     I,J,K
      INTEGER bi, bj
      INTEGER  I,  J, K
      CHARACTER*(MAX_LEN_FNAM) fn
      CHARACTER*(10) suff
      INTEGER prec
CEOP

#ifdef ALLOW_CD_CODE

C--   Suffix for pickup files
      IF (pickupSuff.EQ.' ') THEN
         WRITE(suff,'(I10.10)') myIter
      ELSE
         WRITE(suff,'(A10)') pickupSuff
      ENDIF
         
      _BARRIER
      _BEGIN_MASTER( myThid )

      prec = precFloat64
      
      WRITE(fn,'(A,A10)') 'pickup_cd.',suff
      CALL MDSREADFIELD(fn,prec,'RL',Nr,uVelD,    1,myThid)
      CALL MDSREADFIELD(fn,prec,'RL',Nr,vVelD,    2,myThid)
      CALL MDSREADFIELD(fn,prec,'RL',Nr,uNM1,     3,myThid)
      CALL MDSREADFIELD(fn,prec,'RL',Nr,vNM1,     4,myThid)
      CALL MDSREADFIELD(fn,prec,'RL', 1,etaNm1,6*Nr+1,myThid)
      
      _EXCH_XYZ_R8( uVelD,    myThid )
      _EXCH_XYZ_R8( vVelD,    myThid )
      CALL EXCH_UV_XYZ_RL(uNM1,vNM1,.TRUE.,myThid)
      _EXCH_XY_R8( etaNm1, myThid )
      
      _END_MASTER( myThid )
      _BARRIER
      
#endif /*  ALLOW_CD_CODE  */

      return
      end



      SUBROUTINE CD_CODE_WRITE_CHECKPOINT( 
     &     prec, lgf, permCheckPoint, myIter, myThid )

      IMPLICIT NONE
C     === Global variables ===
#include "SIZE.h"
#include "EEPARAMS.h"
#include "PARAMS.h"
#include "CD_CODE_VARS.h"

C     == Routine arguments ==
C     myThid -  Number of this instance of INI_UVEL 
      INTEGER prec
      LOGICAL lgf
      LOGICAL permCheckPoint  
      INTEGER myIter
      INTEGER myThid

C     == Common blocks ==
      COMMON /PCKP_GBLFLS/ globalFile
      LOGICAL globalFile

C     !LOCAL VARIABLES:
C     == Local variables ==
      CHARACTER*(MAX_LEN_FNAM) fn
      CHARACTER*(MAX_LEN_MBUF) msgBuf
      CHARACTER*(10) suff
CEOP

#ifdef ALLOW_CD_CODE

        IF ( permCheckPoint ) THEN
         WRITE(fn,'(A,I10.10)') 'pickup_cd.',myIter
        ELSE
         WRITE(fn,'(A,A)') 'pickup_cd.',checkPtSuff(nCheckLev)
        ENDIF
        CALL MDSWRITEFIELD(fn,prec,lgf,'RL',Nr,uVelD,1,myIter,myThid)
        CALL MDSWRITEFIELD(fn,prec,lgf,'RL',Nr,vVelD,2,myIter,myThid)
        CALL MDSWRITEFIELD(fn,prec,lgf,'RL',Nr,uNM1, 3,myIter,myThid)
        CALL MDSWRITEFIELD(fn,prec,lgf,'RL',Nr,vNM1, 4,myIter,myThid)
C- jmc: guCD & gvCD no longer exist. 
C   write some stuff to maintain the same pickup size
c       CALL MDSWRITEFIELD(fn,prec,lgf,'RL',Nr,guCD, 5,myIter,myThid)
c       CALL MDSWRITEFIELD(fn,prec,lgf,'RL',Nr,gvCD, 6,myIter,myThid)
        CALL MDSWRITEFIELD(fn,prec,lgf,'RL',Nr,uNM1, 5,myIter,myThid)
        CALL MDSWRITEFIELD(fn,prec,lgf,'RL',Nr,vNM1, 6,myIter,myThid)
        CALL MDSWRITEFIELD(fn,prec,lgf,'RL', 1,etaNm1,6*Nr+1,
     &                     myIter,myThid)

#endif /*  ALLOW_CD_CODE  */

      RETURN
      END

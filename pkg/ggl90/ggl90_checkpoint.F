C $Header: /u/gcmpack/MITgcm/pkg/ggl90/Attic/ggl90_checkpoint.F,v 1.1 2004/09/16 11:27:18 mlosch Exp $
C $Name:  $

#include "GGL90_OPTIONS.h"

      SUBROUTINE GGL90_READ_CHECKPOINT( myIter, myThid )

      IMPLICIT NONE
C     === Global variables ===
#include "SIZE.h"
#include "EEPARAMS.h"
#include "PARAMS.h"
#include "GGL90.h"

C     == Routine arguments ==
C     myThid -  Number of this instance of GGL90_READ_CHECKPOINT
      INTEGER myIter
      INTEGER myThid

#ifdef ALLOW_GGL90

C     !LOCAL VARIABLES:
C     == Local variables ==
      CHARACTER*(MAX_LEN_FNAM) fn
      CHARACTER*(10) suff
      INTEGER prec
CEOP

C--   Suffix for pickup files
      IF (pickupSuff.EQ.' ') THEN
         WRITE(suff,'(I10.10)') myIter
      ELSE
         WRITE(suff,'(A10)') pickupSuff
      ENDIF
         
      _BARRIER
      _BEGIN_MASTER( myThid )

      prec = precFloat64
      
      WRITE(fn,'(A,A10)') 'pickup_ggl90.',suff
      CALL MDSREADFIELD(fn,prec,'RL',Nr,GGL90TKE,1,myThid)
      
      _END_MASTER( myThid )
      _BARRIER
      
      _EXCH_XYZ_R8( GGL90TKE, myThid )
      
#endif /*  ALLOW_GGL90  */

      RETURN
      END



      SUBROUTINE GGL90_WRITE_CHECKPOINT( 
     &     prec, lgf, permCheckPoint, myIter, myThid )

      IMPLICIT NONE
C     === Global variables ===
#include "SIZE.h"
#include "EEPARAMS.h"
#include "PARAMS.h"
#include "GGL90.h"

C     == Routine arguments ==
C     myThid -  Number of this instance of GGL90_WRITE_CHECKPOINT
      INTEGER prec
      LOGICAL lgf
      LOGICAL permCheckPoint  
      INTEGER myIter
      INTEGER myThid

#ifdef ALLOW_GGL90

C     !LOCAL VARIABLES:
C     == Local variables ==
      CHARACTER*(MAX_LEN_FNAM) fn
c     CHARACTER*(MAX_LEN_MBUF) msgBuf
c     CHARACTER*(10) suff
CEOP

        IF ( permCheckPoint ) THEN
         WRITE(fn,'(A,I10.10)') 'pickup_ggl90.',myIter
        ELSE
         WRITE(fn,'(A,A)') 'pickup_ggl90.',checkPtSuff(nCheckLev)
        ENDIF
        CALL MDSWRITEFIELD(fn,prec,lgf,'RL',Nr,GGL90TKE,1,myIter,myThid)

#endif /*  ALLOW_GGL90  */

      RETURN
      END

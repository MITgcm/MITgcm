#include "CPP_OPTIONS.h"
#include "GCHEM_OPTIONS.h"

CStartOfInterFace
      SUBROUTINE GCHEM_FIELDS_LOAD (
     I           myTime,myIter, myThid)

C     /==========================================================\
C     | SUBROUTINE GCHEM_FIELDS_LOAD  i                          |
C     | o calls routines to read in fields needed for gchem pkgs |
C     |==========================================================|
      IMPLICIT NONE
      INTEGER myThid, myIter
      _RL myTime

#ifdef ALLOW_PTRACERS
#ifdef ALLOW_GCHEM

         _BEGIN_MASTER( mythid )
c
c
cccccccccccccccccccccccccc
c load external data     c
cccccccccccccccccccccccccc
#ifdef DIC_BIOTIC
       call  DIC_FIELDS_LOAD (myIter,myTime,myThid)
#endif

c
#ifdef ALLOW_CFC
       call CFC_FIELDS_LOAD(myIter,myTime,myThid)
#endif
         _BARRIER


         _END_MASTER(mythid)

#endif /* ALLOW_GCHEM */
#endif /* ALLOW_PTRACERS */

      RETURN
      END


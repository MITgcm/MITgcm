#include "CPP_OPTIONS.h"
#include "GCHEM_OPTIONS.h"

CBOP
C !ROUTINE: GCHEM_FIELDS_LOAD

C !INTERFACE: ==========================================================
      SUBROUTINE GCHEM_FIELDS_LOAD (
     I           myTime,myIter, myThid)

C !DESCRIPTION:
C  calls routines which read in fields needed for any tracer experiment
C !USES: ===============================================================
      IMPLICIT NONE
      INTEGER myThid, myIter
      _RL myTime 

C !INPUT PARAMETERS: ===================================================
C  myThid               :: thread number
C  myIter               :: current timestep
C  myTime               :: current timestep

C !LOCAL VARIABLES: ====================================================
CEOP

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


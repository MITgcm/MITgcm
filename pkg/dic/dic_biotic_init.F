C $Header: /u/gcmpack/MITgcm/pkg/dic/dic_biotic_init.F,v 1.1 2003/10/06 20:11:10 stephd Exp $
C $Name:  $

#include "CPP_OPTIONS.h"
#include "GCHEM_OPTIONS.h"

CStartOfInterface
      SUBROUTINE DIC_BIOTIC_INIT( myThid )
C     /==========================================================\
C     | SUBROUTINE DIC_BIOTIC_INIT                              |
C     | o Do DIC diagnostic initialization                   |
C     |==========================================================|
C     \==========================================================/
      IMPLICIT NONE

C     === Global variables ===
#include "SIZE.h"
#include "EEPARAMS.h"
#include "PARAMS.h"
#ifdef DIC_BIOTIC
#include "DIC_BIOTIC.h"
#endif

C     == Routine arguments ==
C     myThid -  Number of this instance of INI_FORCING
      INTEGER myThid
CEndOfInterface


C     == Local variables ==
      LOGICAL  DIFFERENT_MULTIPLE
      EXTERNAL DIFFERENT_MULTIPLE
      INTEGER bi, bj, k
      CHARACTER*(MAX_LEN_MBUF) suff

#ifdef DIC_BIOTIC


#ifdef ALLOW_TIMEAVE

c set arrays to zero if first timestep
        DO bj = myByLo(myThid), myByHi(myThid)
         DO bi = myBxLo(myThid), myBxHi(myThid)
          CALL TIMEAVE_RESET(BIOave,   Nr,  bi, bj, myThid)
          CALL TIMEAVE_RESET(CARave,   Nr,  bi, bj, myThid)
          CALL TIMEAVE_RESET(SURave,   1,  bi, bj, myThid)
          CALL TIMEAVE_RESET(SUROave,   1,  bi, bj, myThid)
          CALL TIMEAVE_RESET(pCO2ave,   1,  bi, bj, myThid)
          CALL TIMEAVE_RESET(pHave,   1,  bi, bj, myThid)
          CALL TIMEAVE_RESET(fluxCO2ave,   1,  bi, bj, myThid)
          print*,'QQ start timeave'
          do k=1,Nr
            DIC_TimeAve(bi,bj,k)=0.d0
          enddo
         ENDDO
        ENDDO

#endif /* ALLOW_TIMEAVE */
      
#endif /* DIC_BIOTIC   */
      
      RETURN
      END

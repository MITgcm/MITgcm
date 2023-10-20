#include "DIC_OPTIONS.h"
#ifdef ALLOW_CTRL
# include "CTRL_OPTIONS.h"
#endif

cphc$taf COMMON DIC_XX        adname = addic_xx
cphc$taf COMMON DIC_COST_CTRL adname = ADDIC_COST_CTRL

C !INTERFACE: ==========================================================
      SUBROUTINE DIC_SET_CONTROL( myThid )

C !DESCRIPTION:

C !USES: ===============================================================
      IMPLICIT NONE

C     == GLobal variables ==
#include "SIZE.h"
#include "EEPARAMS.h"
#include "PARAMS.h"
#ifdef DIC_BIOTIC
# include "DIC_VARS.h"
# include "DIC_CTRL.h"
#endif
#ifdef ALLOW_CTRL
#include "CTRL_SIZE.h"
#include "CTRL.h"
#include "CTRL_DUMMY.h"
#include "optim.h"
#endif
C     == Routine arguments ==
      INTEGER myThid

#ifdef ALLOW_CTRL
cph#ifdef DIC_BIOTIC
C     == Local arguments ==
#ifdef ALLOW_DIC_CONTROL
      INTEGER bi, bj
      INTEGER i, j
#endif

c     == end of interface ==
CEOP

#ifdef ALLOW_DIC_CONTROL

      DO bj=myByLo(myThid),myByHi(myThid)
       DO bi=myBxLo(myThid),myBxHi(myThid)
         DO j = 1, sNy
          DO i = 1, sNx
c           feload(i,j,bi,bj) = feload(i,j,bi,bj)*(1. _d 0 +xx_dic(1))
            rain_ratio(i,j,bi,bj) =
     &                      rain_ratio(i,j,bi,bj)*(1. _d 0 +xx_dic(2))
          ENDDO
         ENDDO
       ENDDO
      ENDDO

      _EXCH_XY_RL( alpha, myThid )
      _EXCH_XY_RL( rain_ratio, myThid )
c     _EXCH_XY_RL( feload, myThid )

      KScav       = KScav       * ( 1. _d 0 + xx_dic(3)*1. _d 6 )
      ligand_stab = ligand_stab * ( 1. _d 0 + xx_dic(4)*1. _d 6 )
      ligand_tot  = ligand_tot  * ( 1. _d 0 + xx_dic(5)*1. _d 6 )

      print *,'COST KScav = ', KScav
      print *,'COST ligand_stab = ', ligand_stab
      print *,'COST ligand_tot = ', ligand_tot

#endif /* ALLOW_DIC_CONTROL */

cph#endif /* DIC_BIOTIC */
#endif /* ALLOW_CTRL */

      RETURN
      END

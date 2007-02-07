C $Header: /u/gcmpack/MITgcm/model/src/ini_mixing.F,v 1.4 2007/02/07 13:38:28 heimbach Exp $
C $Name:  $

#include "CPP_OPTIONS.h"
# if (defined (ALLOW_GMREDI) && defined (ALLOW_KAPGM_CONTROL))
# include "GMREDI_OPTIONS.h"
#endif


CBOP
C     !ROUTINE: INI_MIXING
C     !INTERFACE:
      SUBROUTINE INI_MIXING( myThid )
C     !DESCRIPTION: \bv
C     *==========================================================*
C     | SUBROUTINE INI_MIXING                                     
C     | o Initialise diapycnal diffusivity to default constant val.
C     *==========================================================*
C     \ev

C     !USES:
      IMPLICIT NONE
C     === Global variables ===
#include "SIZE.h"
#include "EEPARAMS.h"
#include "PARAMS.h"
#include "GRID.h"
#include "DYNVARS.h"
# if (defined (ALLOW_GMREDI) && defined (ALLOW_KAPGM_CONTROL))
# include "GMREDI.h"
#endif

C     !INPUT/OUTPUT PARAMETERS:
C     == Routine arguments ==
C     myThid -  Number of this instance
      INTEGER myThid

C     !LOCAL VARIABLES:
C     == Local variables ==
C     bi,bj  - Loop counters
C     I,J
      INTEGER bi, bj
      INTEGER i, j, k
CEOP

       DO bj = myByLo(myThid), myByHi(myThid)
        DO bi = myBxLo(myThid), myBxHi(myThid)
         DO k=1,Nr
          DO j=1,sNy
           DO i=1,sNx
#if (defined (ALLOW_AUTODIFF_TAMC) && defined (ALLOW_DIFFKR_CONTROL))
            diffKr(i,j,k,bi,bj) = diffKrNrS(k)
#endif
#if (defined (ALLOW_AUTODIFF_TAMC) && defined (ALLOW_KAPGM_CONTROL))
            KapGM(i,j,k,bi,bj) = GM_background_K
#endif
           ENDDO
          ENDDO
         ENDDO
        ENDDO
       ENDDO

#if (defined (ALLOW_AUTODIFF_TAMC) && defined (ALLOW_DIFFKR_CONTROL))
      _EXCH_XYZ_R8(diffKr , myThid )
#endif
#if (defined (ALLOW_AUTODIFF_TAMC) && defined (ALLOW_KAPGM_CONTROL))
      _EXCH_XYZ_R8(KapGM,   myThid )
#endif

      RETURN
      END

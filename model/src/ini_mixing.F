C $Header: /u/gcmpack/MITgcm/model/src/ini_mixing.F,v 1.2 2001/09/26 18:09:15 cnh Exp $
C $Name:  $

#include "CPP_OPTIONS.h"

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
         DO j=1,sNy
          DO i=1,sNx
           DO k=1,Nr
#if (defined (ALLOW_AUTODIFF_TAMC) && defined (ALLOW_DIFFKR_CONTROL))
            diffKr(i,j,k,bi,bj) = diffKrS
#endif
#if (defined (ALLOW_AUTODIFF_TAMC) && defined (ALLOW_KAPGM_CONTROL))
            KapGM(i,j,k,bi,bj) = GMkbackground
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

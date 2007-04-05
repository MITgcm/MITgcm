C $Header: /u/gcmpack/MITgcm/model/src/ini_mixing.F,v 1.5 2007/04/05 00:02:49 dimitri Exp $
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
            diffKr3dS(i,j,k,bi,bj) = diffKrNrS(k)
            diffKr3dT(i,j,k,bi,bj) = diffKrNrT(k)
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

       IF ( diffKr3dSfile .NE. ' ' ) THEN
          CALL READ_FLD_XYZ_RL(diffKr3dSfile,' ',diffKr3dS,0,myThid)
       ENDIF
       IF ( diffKr3dTfile .NE. ' ' ) THEN
          CALL READ_FLD_XYZ_RL(diffKr3dTfile,' ',diffKr3dT,0,myThid)
       ENDIF

      _EXCH_XYZ_R8(diffKr3dS , myThid )
      _EXCH_XYZ_R8(diffKr3dT , myThid )
#if (defined (ALLOW_AUTODIFF_TAMC) && defined (ALLOW_DIFFKR_CONTROL))
      _EXCH_XYZ_R8(diffKr , myThid )
#endif
#if (defined (ALLOW_AUTODIFF_TAMC) && defined (ALLOW_KAPGM_CONTROL))
      _EXCH_XYZ_R8(KapGM,   myThid )
#endif

      RETURN
      END

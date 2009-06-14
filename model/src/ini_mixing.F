C $Header: /u/gcmpack/MITgcm/model/src/ini_mixing.F,v 1.10 2009/06/14 21:49:39 jmc Exp $
C $Name:  $

#include "PACKAGES_CONFIG.h"
#include "CPP_OPTIONS.h"
# if (defined (ALLOW_GMREDI) && (defined (ALLOW_KAPGM_CONTROL)||defined (ALLOW_KAPREDI_CONTROL)))
# include "GMREDI_OPTIONS.h"
#endif


CBOP
C     !ROUTINE: INI_MIXING
C     !INTERFACE:
      SUBROUTINE INI_MIXING( myThid )
C     !DESCRIPTION: \bv
C     *==========================================================*
C     | SUBROUTINE INI_MIXING
C     | o Initialise diffusivity to default constant value.
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
# if (defined (ALLOW_GMREDI) && (defined (ALLOW_KAPGM_CONTROL)||defined (ALLOW_KAPREDI_CONTROL)))
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
          DO j = 1-Oly, sNy+Oly
           DO i = 1-Olx, sNx+Olx
#ifdef ALLOW_3D_DIFFKR
            diffKr(i,j,k,bi,bj) = diffKrNrS(k)
#endif
#if (defined (ALLOW_AUTODIFF_TAMC) && defined (ALLOW_KAPGM_CONTROL))
            KapGM(i,j,k,bi,bj) = GM_background_K
#endif
#if (defined (ALLOW_AUTODIFF_TAMC) && defined (ALLOW_KAPREDI_CONTROL))
            kapredi(i,j,k,bi,bj) = GM_isopycK
#endif
           ENDDO
          ENDDO
         ENDDO
        ENDDO
       ENDDO

#ifdef ALLOW_BL79_LAT_VARY
       DO bj = myByLo(myThid), myByHi(myThid)
        DO bi = myBxLo(myThid), myBxHi(myThid)
         DO j = 1-Oly, sNy+Oly
          DO i = 1-Olx, sNx+Olx
           BL79LatArray(i,j,bi,bj) = 0.0 _d 0
           IF ( abs(YC(i,j,bi,bj)) .LT. BL79LatVary )
     &          BL79LatArray(i,j,bi,bj) =
     &          ( 1 + cos(YC(i,j,bi,bj)*pi/BL79LatVary) ) / 2
          ENDDO
         ENDDO
        ENDDO
       ENDDO
#endif

#ifdef ALLOW_3D_DIFFKR
       IF ( diffKrFile .NE. ' ' ) THEN
          CALL READ_FLD_XYZ_RL(diffKrFile,' ',diffKr,0,myThid)
          _EXCH_XYZ_RL(diffKr , myThid )
       ENDIF
#endif
#if (defined (ALLOW_AUTODIFF_TAMC) && defined (ALLOW_KAPGM_CONTROL))
      _EXCH_XYZ_RL(KapGM,   myThid )
#endif
#if (defined (ALLOW_AUTODIFF_TAMC) && defined (ALLOW_KAPREDI_CONTROL))
      _EXCH_XYZ_RL(kapredi,   myThid )
#endif
      RETURN
      END

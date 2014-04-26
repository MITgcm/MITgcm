C $Header: /u/gcmpack/MITgcm/model/src/ini_mixing.F,v 1.13 2014/04/26 19:26:36 jmc Exp $
C $Name:  $

#include "PACKAGES_CONFIG.h"
#include "CPP_OPTIONS.h"
#ifdef ALLOW_CTRL
# include "CTRL_OPTIONS.h"
#endif
#ifdef ALLOW_GMREDI
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
#ifdef ALLOW_CTRL
# include "CTRL_FIELDS.h"
# ifdef ALLOW_GMREDI
#  include "GMREDI.h"
# endif
#endif

C     !INPUT/OUTPUT PARAMETERS:
C     == Routine arguments ==
C     myThid ::  Number of this instance
      INTEGER myThid

C     !LOCAL VARIABLES:
C     == Local variables ==
C     bi,bj  :: tile indices
C     i,j,k  :: Loop counters
      INTEGER bi, bj
      INTEGER i, j, k
CEOP

       DO bj = myByLo(myThid), myByHi(myThid)
        DO bi = myBxLo(myThid), myBxHi(myThid)
         DO k=1,Nr
          DO j = 1-OLy, sNy+OLy
           DO i = 1-OLx, sNx+OLx
#ifdef ALLOW_3D_DIFFKR
            diffKr(i,j,k,bi,bj) = diffKrNrS(k)
#endif
#if (defined (ALLOW_CTRL) && defined (ALLOW_GMREDI))
# ifdef ALLOW_KAPGM_CONTROL
            KapGM(i,j,k,bi,bj) = GM_background_K
# endif
# ifdef ALLOW_KAPREDI_CONTROL
            kapRedi(i,j,k,bi,bj) = GM_isopycK
# endif
#endif /* ALLOW_CTRL & ALLOW_GMREDI */
           ENDDO
          ENDDO
         ENDDO
        ENDDO
       ENDDO

#ifdef ALLOW_BL79_LAT_VARY
       DO bj = myByLo(myThid), myByHi(myThid)
        DO bi = myBxLo(myThid), myBxHi(myThid)
         DO j = 1-OLy, sNy+OLy
          DO i = 1-OLx, sNx+OLx
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
          _EXCH_XYZ_RL( diffKr, myThid )
       ENDIF
#endif /* ALLOW_3D_DIFFKR */
#ifdef ALLOW_CTRL
# ifdef ALLOW_KAPGM_CONTROL
      _EXCH_XYZ_RL( KapGM, myThid )
# endif
# ifdef ALLOW_KAPREDI_CONTROL
      _EXCH_XYZ_RL( kapRedi, myThid )
# endif
#endif /* ALLOW_CTRL */
      RETURN
      END

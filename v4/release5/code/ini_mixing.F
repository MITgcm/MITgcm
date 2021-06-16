C $Header: /u/gcmpack/MITgcm/model/src/ini_mixing.F,v 1.16 2017/04/03 17:09:01 ou.wang Exp $
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
            kapGM(i,j,k,bi,bj) = GM_background_K*
     &        GM_bolFac2d(i,j,bi,bj)*GM_bolFac1d(k)
# endif
# ifdef ALLOW_KAPREDI_CONTROL
            kapRedi(i,j,k,bi,bj) = GM_isopycK*
     &        GM_isoFac2d(i,j,bi,bj)*GM_isoFac1d(k)
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
           IF ( ABS(YC(i,j,bi,bj)) .LT. BL79LatVary )
     &          BL79LatArray(i,j,bi,bj) =
     &          ( 1 + COS(YC(i,j,bi,bj)*pi/BL79LatVary) ) / 2
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
#ifdef ALLOW_KAPGM_CONTROL
#ifdef ALLOW_KAPGM_3DFILE
       IF ( useGMRedi.and.GM_background_K3dFile .NE. ' ' ) THEN
          CALL READ_FLD_XYZ_RL(GM_background_K3dFile,' ',KapGM,0,myThid)
       ENDIF
#endif
       _EXCH_XYZ_RL( kapGM, myThid )
       CALL WRITE_FLD_XYZ_RL( 'KapGM', ' ', kapGM, 0, myThid )
#endif
#ifdef ALLOW_KAPREDI_CONTROL
#ifdef ALLOW_KAPREDI_3DFILE
       IF ( useGMRedi.and.GM_isopycK3dFile .NE. ' ' ) THEN
          CALL READ_FLD_XYZ_RL(GM_isopycK3dFile,' ',KapRedi,0,myThid)
       ENDIF
#endif
       _EXCH_XYZ_RL( kapRedi, myThid )
       CALL WRITE_FLD_XYZ_RL( 'KapRedi', ' ', kapRedi, 0, myThid )
#endif
#endif /* ALLOW_CTRL */
      RETURN
      END

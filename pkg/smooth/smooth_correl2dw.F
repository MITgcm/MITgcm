#include "SMOOTH_OPTIONS.h"
#ifdef ALLOW_CTRL
# include "CTRL_OPTIONS.h"
#endif
#ifdef ALLOW_ECCO
# include "ECCO_OPTIONS.h"
#endif

      SUBROUTINE SMOOTH_CORREL2DW(
     &           fld_in, mask_in, xx_gen_file, myThid )

C     *==========================================================*
C     | SUBROUTINE smooth_correl2Dw
C     | o Routine that maps a 2D control field to physical units
C     |   by mutliplying it with 1/sqrt(weight)
C     |   after smooth_correl2D has been applied
C     *==========================================================*

      IMPLICIT NONE
#include "SIZE.h"
#include "EEPARAMS.h"
#include "GRID.h"
#include "PARAMS.h"
#include "SMOOTH.h"

      _RL fld_in (1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      _RS mask_in(1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      CHARACTER*(MAX_LEN_FNAM) xx_gen_file
      INTEGER myThid

      RETURN
      END

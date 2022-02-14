#include "SMOOTH_OPTIONS.h"

      SUBROUTINE SMOOTH2D(
     &           fld_in, mask_in, smoothOpNb, myThid )

C     *==========================================================*
C     | SUBROUTINE smooth2D
C     | o Routine that applies 2D spatial smoothing
C     *==========================================================*

      IMPLICIT NONE
#include "SIZE.h"
#include "EEPARAMS.h"
#include "GRID.h"
#include "PARAMS.h"
#include "SMOOTH.h"

      _RL fld_in (1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      _RS mask_in(1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      INTEGER smoothOpNb
      INTEGER myThid

      INTEGER nbt_in
      CHARACTER*(80) fnamegeneric

C read smoothing [i.e diffusion] operator:
      WRITE(fnamegeneric(1:80),'(1A,I3.3)')
     &    'smooth2Doperator',smoothOpNb
      CALL READ_REC_3D_RL( fnamegeneric, smoothprec,
     &                     1, smooth2D_Kux, 1, 1, myThid )
      CALL READ_REC_3D_RL( fnamegeneric, smoothprec,
     &                     1, smooth2D_Kvy, 2, 1, myThid )
      CALL EXCH_XY_RL( smooth2D_Kux, myThid )
      CALL EXCH_XY_RL( smooth2D_Kvy, myThid )

C do the smoothing:
      nbt_in = smooth2Dnbt(smoothOpNb)
      CALL SMOOTH_DIFF2D( fld_in, mask_in, nbt_in, myThid )

      RETURN
      END

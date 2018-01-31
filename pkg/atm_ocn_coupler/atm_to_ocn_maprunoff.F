#include "CPP_OPTIONS.h"

CBOP 0
C !ROUTINE: ATM_TO_OCN_MAPRUNOFF

C !INTERFACE:
      SUBROUTINE ATM_TO_OCN_MAPRUNOFF(
     I           fld_atm,
     O           fld_ocn,
     U           add2ocn )

C !DESCRIPTION:
C     *==========================================================*
C     | SUBROUTINE ATM_TO_OCN_MAPRUNOFF
C     | o maps RunOff from atmos. grid (land) to ocean grid
C     |    and Incorporate (substract) to ocean FreshWater flux
C     *==========================================================*

C !USES:
      IMPLICIT NONE
C     == Global variables ==
#include "ATMSIZE.h"
#include "OCNSIZE.h"
#include "CPL_MAP2GRIDS.h"

C !INPUT/OUTPUT PARAMETERS:
C     fld_atm :: Atmos. grid Run-Off field
C     fld_ocn :: Ocean grid Run-Off field (atmos field will be mapped to this)
C     add2ocn :: Ocean grid FreshWater field to update by substracting Run-Off
      _RL  fld_atm(Nx_atm*Ny_atm)
      _RL  fld_ocn(Nx_ocn*Ny_ocn)
      _RL  add2ocn(Nx_ocn*Ny_ocn)
CEOP

C !LOCAL VARIABLES:
      INTEGER n, ijo, ija

C       Set to zero everywhere
        DO ijo=1,Nx_ocn*Ny_ocn
          fld_ocn(ijo) = 0.
        ENDDO

C       Maps Runoff from land grid point to ocean grid point
C       and update oceanic FreshWater flux (substract RunOff)
        DO n=1,nROmap
          ijo=ijROocn(n)
          ija=ijROatm(n)
          fld_ocn(ijo) = fld_ocn(ijo)
     &                 + fld_atm(ija)*arROmap(n)
          add2ocn(ijo) = add2ocn(ijo)
     &                 - fld_atm(ija)*arROmap(n)
        ENDDO

      RETURN
      END

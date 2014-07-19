C $Header: /u/gcmpack/MITgcm/pkg/atm_ocn_coupler/atm_to_ocn_maprunoff.F,v 1.4 2014/07/19 23:01:48 jmc Exp $
C $Name:  $

#include "CPP_OPTIONS.h"

CStartOfInterface
      SUBROUTINE ATM_TO_OCN_MAPRUNOFF(
     I           fld_atm,
     U           fld_ocn )
C     *==========================================================*
C     | SUBROUTINE ATM_TO_OCN_MAPRUNOFF
C     | o maps RunOff from atmos. grid (land) to ocean grid
C     |    and Incorporate to ocean FreshWater flux
C     *==========================================================*
      IMPLICIT NONE

#include "ATMSIZE.h"
#include "OCNSIZE.h"
#include "CPL_MAP2GRIDS.h"

C     == Routine arguments ==
C     fld_atm - Atmos. grid field
C     fld_ocn - Ocean grid field (atmos field will be mapped to this field)
      _RL  fld_atm(Nx_atm*Ny_atm)
      _RL  fld_ocn(Nx_ocn*Ny_ocn)
CEndOfInterface

C     == Local variables ==
      INTEGER n, ijo, ija

C       Set to zero everywhere
c       DO ijo=1,Nx_ocn*Ny_ocn
c         fld_ocn(ijo)=0.
c       ENDDO

C       Maps Runoff from land grid point to ocean grid point
C       and update oceanic FreshWater flux (substract RunOff)
        DO n=1,nROmap
          ijo=ijROocn(n)
          ija=ijROatm(n)
          fld_ocn(ijo)=fld_ocn(ijo)
     &                -fld_atm(ija)*arROmap(n)
        ENDDO

      RETURN
      END

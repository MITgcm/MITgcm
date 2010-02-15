C $Header: /u/gcmpack/MITgcm/pkg/smooth/smooth_init_fixed.F,v 1.1 2010/02/15 23:46:04 gforget Exp $
C $Name:  $

#include "SMOOTH_OPTIONS.h"

      subroutine smooth_init_fixed (mythid)

C     *==========================================================*
C     | SUBROUTINE smooth_init_fixed
C     | o Routine that initializes smoothing/correlation operators
C     *==========================================================*

      IMPLICIT NONE
#include "SIZE.h"
#include "EEPARAMS.h"
#include "EESUPPORT.h"
#include "PARAMS.h"
#include "DYNVARS.h"
#include "GRID.h"
#ifdef ALLOW_AUTODIFF_TAMC
#include "tamc.h"
#include "tamc_keys.h"
#endif /* ALLOW_AUTODIFF_TAMC */
# include "FFIELDS.h"
# include "EOS.h"
#include "SMOOTH.h"
#ifdef ALLOW_SMOOTH_INV
#include "ctrl.h"
#endif
        integer k
        integer myThid
        integer ikey_bak
        integer smoothOpNb


#ifdef ALLOW_SMOOTH2D
      DO smoothOpNb=1,smoothOpNbMax
      if (smooth2Dtype(smoothOpNb).NE.0) then
      call smooth_init2D(smoothOpNb,mythid)
      endif
      ENDDO 
#endif

#ifdef ALLOW_SMOOTH_CORREL2D
      DO smoothOpNb=1,smoothOpNbMax
      if (smooth2Dtype(smoothOpNb).NE.0) then
      call smooth_filtervar2D(smoothOpNb,mythid)
      endif
      ENDDO
#endif

#ifdef ALLOW_SMOOTH3D
      DO smoothOpNb=1,smoothOpNbMax
      if ((smooth3DtypeZ(smoothOpNb).NE.0).OR.
     & (smooth3DtypeH(smoothOpNb).NE.0)) then
      call smooth_init3D(smoothOpNb,mythid)
      endif
      ENDDO
#endif

#ifdef ALLOW_SMOOTH_CORREL3D
      DO smoothOpNb=1,smoothOpNbMax
      if ((smooth3DtypeZ(smoothOpNb).NE.0).OR.
     & (smooth3DtypeH(smoothOpNb).NE.0)) then
      call smooth_filtervar3D(smoothOpNb,mythid)
      endif
      ENDDO
#endif

        END 



C $Header: /u/gcmpack/MITgcm/pkg/smooth/smooth_init_fixed.F,v 1.2 2012/08/21 13:46:30 gforget Exp $
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

      integer myThid
      integer ikey_bak
      integer smoothOpNb

      integer i,j,k, bi, bj, imin, imax, jmin, jmax
      integer itlo,ithi
      integer jtlo,jthi

      jtlo = mybylo(mythid)
      jthi = mybyhi(mythid)
      itlo = mybxlo(mythid)
      ithi = mybxhi(mythid)

      DO bj=jtlo,jthi
       DO bi=itlo,ithi
        DO k=1,Nr
         DO j=1-OLy,sNy+OLy
          DO i=1-OLx,sNx+OLx
          smooth_recip_hFacC(i,j,k,bi,bj)=_recip_hFacC(i,j,k,bi,bj)
          smooth_hFacW(i,j,k,bi,bj)=_hFacW(i,j,k,bi,bj)
          smooth_hFacS(i,j,k,bi,bj)=_hFacS(i,j,k,bi,bj)
          ENDDO
         ENDDO
        ENDDO
       ENDDO
      ENDDO

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



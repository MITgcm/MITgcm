C $Header: /u/gcmpack/MITgcm/pkg/smooth/smooth_init_fixed.F,v 1.5 2014/04/01 08:05:37 atn Exp $
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
#include "PARAMS.h"
#include "GRID.h"
#include "SMOOTH.h"

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

      DO smoothOpNb=1,smoothOpNbMax
      if (smooth2Dtype(smoothOpNb).NE.0) then
      call smooth_init2D(smoothOpNb,mythid)
      endif
      ENDDO 

      DO smoothOpNb=1,smoothOpNbMax
      if (smooth2Dtype(smoothOpNb).NE.0) then
      call smooth_filtervar2D(smoothOpNb,mythid)
      endif
      ENDDO

      DO smoothOpNb=1,smoothOpNbMax
      if ((smooth3DtypeZ(smoothOpNb).NE.0).OR.
     & (smooth3DtypeH(smoothOpNb).NE.0)) then
      call smooth_init3D(smoothOpNb,mythid)
      endif
      ENDDO

      DO smoothOpNb=1,smoothOpNbMax
      if ((smooth3DtypeZ(smoothOpNb).NE.0).OR.
     & (smooth3DtypeH(smoothOpNb).NE.0)) then
      call smooth_filtervar3D(smoothOpNb,mythid)
      endif
      ENDDO

        END 



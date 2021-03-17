#include "SMOOTH_OPTIONS.h"

      subroutine smooth2D (
     U     fld_in,mask_in,smoothOpNb,mythid)

C     *==========================================================*
C     | SUBROUTINE smooth2D
C     | o Routine that applies 2D spatial smoothing
C     *==========================================================*

      IMPLICIT NONE
#include "SIZE.h"
#include "EEPARAMS.h"
#include "GRID.h"
#include "PARAMS.h"
c#include "tamc.h"
#include "SMOOTH.h"

      _RS mask_in(1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      _RL fld_in(1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      integer smoothOpNb
      integer nbt_in
      character*( 80) fnamegeneric
      integer i,j,bi,bj
      integer itlo,ithi
      integer jtlo,jthi
      integer myThid

      jtlo = mybylo(mythid)
      jthi = mybyhi(mythid)
      itlo = mybxlo(mythid)
      ithi = mybxhi(mythid)

c read smoothing [i.e diffusion] operator:
      write(fnamegeneric(1:80),'(1a,i3.3)')
     &    'smooth2Doperator',smoothOpNb
      CALL READ_REC_3D_RL(fnamegeneric,smoothprec,
     &           1, smooth2D_Kux,1,1,mythid)
      CALL READ_REC_3D_RL(fnamegeneric,smoothprec,
     &           1, smooth2D_Kvy,2,1,mythid)
      CALL EXCH_XY_RL ( smooth2D_Kux, myThid )
      CALL EXCH_XY_RL ( smooth2D_Kvy, myThid )

c do the smoothing:
      nbt_in=smooth2Dnbt(smoothOpNb)
      call smooth_diff2D(fld_in,mask_in,nbt_in,mythid)

      end

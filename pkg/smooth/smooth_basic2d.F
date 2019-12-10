#include "SMOOTH_OPTIONS.h"

      subroutine smooth_basic2D (
     U     fld_in,mask_in,dist_in,nbt_in,mythid)

C     *==========================================================*
C     | SUBROUTINE smooth_basic2D
C     | o this routine smoothes a 2D field isotropically (except for
C     |   the land mask) to a constant length scale. It mostly 
C     |   calls smooth_diff2D.F.
C     *==========================================================*

      IMPLICIT NONE
#include "SIZE.h"
#include "EEPARAMS.h"
#include "GRID.h"
#include "PARAMS.h"
c#include "tamc.h"
#include "SMOOTH.h"

      _RL mask_in(1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      _RL fld_in(1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      _RL dist_in
      integer nbt_in
      integer i,j,bi,bj
      integer itlo,ithi
      integer jtlo,jthi
      integer myThid


      jtlo = mybylo(mythid)
      jthi = mybyhi(mythid)
      itlo = mybxlo(mythid)
      ithi = mybxhi(mythid)

      smooth2DtotTime=nbt_in*smooth2DdelTime

      DO bj=myByLo(myThid),myByHi(myThid)
       DO bi=myBxLo(myThid),myBxHi(myThid)
         DO j=1-OLy,sNy+OLy
          DO i=1-OLx,sNx+OLx
          smooth2D_Lx(i,j,bi,bj)=dist_in
          smooth2D_Ly(i,j,bi,bj)=dist_in
          ENDDO
         ENDDO
       ENDDO
      ENDDO

      DO bj=myByLo(myThid),myByHi(myThid)
       DO bi=myBxLo(myThid),myBxHi(myThid)
         DO j=1-OLy,sNy+OLy
          DO i=1-OLx,sNx+OLx
          smooth2D_Kux(i,j,bi,bj)=smooth2D_Lx(i,j,bi,bj)*
     & smooth2D_Lx(i,j,bi,bj)/smooth2DtotTime/2
          smooth2D_Kvy(i,j,bi,bj)=smooth2D_Ly(i,j,bi,bj)*
     & smooth2D_Ly(i,j,bi,bj)/smooth2DtotTime/2
          ENDDO
         ENDDO
       ENDDO
      ENDDO

      CALL EXCH_XY_RL ( smooth2D_Kux , myThid )
      CALL EXCH_XY_RL ( smooth2D_Kvy , myThid )

      call smooth_diff2D(fld_in,mask_in,nbt_in,mythid)

      CALL EXCH_XY_RL ( fld_in , myThid )
       
      end

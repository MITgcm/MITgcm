#include "SMOOTH_OPTIONS.h"

      subroutine smooth_init2D (smoothOpNb, mythid )

C     *==========================================================*
C     | SUBROUTINE smooth_init2D
C     | o Routine that initializes one 2D smoothing/correlation operator
C     |   by computing/writing the corresponding diffusion operator
C     *==========================================================*

cgf the choices of smooth2Dtype and smooth2Dsize need comments...

      IMPLICIT NONE
#include "SIZE.h"
#include "EEPARAMS.h"
#include "PARAMS.h"
#include "GRID.h"
#include "SMOOTH.h"

      integer i,j,k, bi, bj
      integer itlo,ithi
      integer jtlo,jthi
      integer myThid
      character*( 80) fnamegeneric
      integer smoothOpNb

      jtlo = mybylo(mythid)
      jthi = mybyhi(mythid)
      itlo = mybxlo(mythid)
      ithi = mybxhi(mythid)


      smooth2DtotTime=smooth2Dnbt(smoothOpNb)*smooth2DdelTime

      if ((smooth2Dtype(smoothOpNb).NE.0).AND.
     & (smooth2Dsize(smoothOpNb).EQ.2)) then
      write(fnamegeneric(1:80),'(1a,i3.3)')
     &    'smooth2Dscales',smoothOpNb
      CALL READ_REC_3D_RL(fnamegeneric,smoothprec,
     &           1, smooth2D_Lx,1,1,mythid)
      CALL READ_REC_3D_RL(fnamegeneric,smoothprec,
     &           1, smooth2D_Ly,2,1,mythid)
      CALL EXCH_XY_RL ( smooth2D_Lx, myThid )
      CALL EXCH_XY_RL ( smooth2D_Ly, myThid )
      else
      DO bj=myByLo(myThid),myByHi(myThid)
       DO bi=myBxLo(myThid),myBxHi(myThid)
         DO j=1-OLy,sNy+OLy
          DO i=1-OLx,sNx+OLx
          smooth2D_Lx(i,j,bi,bj)=smooth2D_Lx0(smoothOpNb)
          smooth2D_Ly(i,j,bi,bj)=smooth2D_Ly0(smoothOpNb)
          ENDDO
         ENDDO
       ENDDO
      ENDDO
      endif

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

c write diffusion operator to file
      write(fnamegeneric(1:80),'(1a,i3.3)')
     &    'smooth2Doperator',smoothOpNb
      CALL WRITE_REC_3D_RL(fnamegeneric,smoothprec,
     &            1,smooth2D_Kux,1,1,mythid)
      CALL WRITE_REC_3D_RL(fnamegeneric,smoothprec,
     &            1,smooth2D_Kvy,2,1,mythid)

      end

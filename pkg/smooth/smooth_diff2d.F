#include "SMOOTH_OPTIONS.h"

      SUBROUTINE SMOOTH_DIFF2D(
     &           fld_in, smooth2Dmask, nbt_in, myThid )

C     *==========================================================*
C     | SUBROUTINE smooth_diff2D
C     | o Routine that smoothes a 2D field, using diffusion
C     *==========================================================*

      IMPLICIT NONE
#include "SIZE.h"
#include "EEPARAMS.h"
#include "GRID.h"
#include "PARAMS.h"
#include "SMOOTH.h"

      _RL fld_in      (1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      _RS smooth2Dmask(1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      INTEGER nbt_in
      INTEGER myThid

      INTEGER i, j, bi, bj
      INTEGER myIter(nSx,nSy), iloop
      _RL ab15, ab05
      _RL gt_tmp
      _RL gt_in  (1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      _RL gtm1_in(1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)

      DO bj=myByLo(myThid),myByHi(myThid)
       DO bi=myBxLo(myThid),myBxHi(myThid)
        DO j = 1,sNy
         DO i = 1,sNx
           gt_in(i,j,bi,bj)   = 0. _d 0
           gtm1_in(i,j,bi,bj) = 0. _d 0
         ENDDO
        ENDDO
       ENDDO
      ENDDO

      CALL EXCH_XY_RL( fld_in,  myThid )
      CALL EXCH_XY_RL( gt_in,   myThid )
      CALL EXCH_XY_RL( gtm1_in, myThid )

      DO iloop=1,nbt_in

        DO bj=myByLo(myThid),myByHi(myThid)
         DO bi=myBxLo(myThid),myBxHi(myThid)
          DO j = 1,sNy
           DO i = 1,sNx

            gt_in(i,j,bi,bj)=0.

            IF (smooth2Dmask(i,j,bi,bj).NE.0.) THEN

             gt_in(i,j,bi,bj) = gt_in(i,j,bi,bj)
     &         + smooth2D_Kux(i,j,bi,bj)*dyG(i,j,bi,bj)
     &          *smooth2Dmask(i,j,bi,bj)*smooth2Dmask(i-1,j,bi,bj)
     &          *( fld_in(i,j,bi,bj) - fld_in(i-1,j,bi,bj) )
     &          *recip_dxC(i,j,bi,bj)

             gt_in(i,j,bi,bj) = gt_in(i,j,bi,bj)
     &         + smooth2D_Kux(i+1,j,bi,bj)*dyG(i+1,j,bi,bj)
     &          *smooth2Dmask(i,j,bi,bj)*smooth2Dmask(i+1,j,bi,bj)
     &          *( fld_in(i,j,bi,bj) - fld_in(i+1,j,bi,bj) )
     &          *recip_dxC(i+1,j,bi,bj)

             gt_in(i,j,bi,bj) = gt_in(i,j,bi,bj)
     &         + smooth2D_Kvy(i,j,bi,bj)*dxG(i,j,bi,bj)
     &          *smooth2Dmask(i,j,bi,bj)*smooth2Dmask(i,j-1,bi,bj)
     &          *( fld_in(i,j,bi,bj) - fld_in(i,j-1,bi,bj) )
     &          *recip_dyC(i,j,bi,bj)

             gt_in(i,j,bi,bj) = gt_in(i,j,bi,bj)
     &         + smooth2D_Kvy(i,j+1,bi,bj)*dxG(i,j+1,bi,bj)
     &          *smooth2Dmask(i,j,bi,bj)*smooth2Dmask(i,j+1,bi,bj)
     &          *( fld_in(i,j,bi,bj) - fld_in(i,j+1,bi,bj) )
     &          *recip_dyC(i,j+1,bi,bj)

            ENDIF

           ENDDO
          ENDDO
         ENDDO
        ENDDO

        DO bj=myByLo(myThid),myByHi(myThid)
         DO bi=myBxLo(myThid),myBxHi(myThid)
c Adams-Bashforth timestepping
          myIter(bi,bj)=iloop-1
          IF ( myIter(bi,bj).EQ.0 ) THEN
           ab15 = 1.0
           ab05 = 0.0
          ELSE
           ab15 =   1.5 + abEps
           ab05 = -(0.5 + abEps)
          ENDIF
          DO j = 1,sNy
           DO i = 1,sNx
c Compute effective G-term with Adams-Bashforth
            gt_tmp = ab15*gt_in(i,j,bi,bj) + ab05*gtm1_in(i,j,bi,bj)
            gtm1_in(i,j,bi,bj) = gt_in(i,j,bi,bj)
            gt_in(i,j,bi,bj) = gt_tmp
c time step:
            fld_in(i,j,bi,bj) = fld_in(i,j,bi,bj)
     &        - gt_in(i,j,bi,bj)*recip_rA(i,j,bi,bj)*smooth2DdelTime
            gt_in(i,j,bi,bj) = 0.
           ENDDO
          ENDDO
         ENDDO
        ENDDO

        CALL EXCH_XY_RL( gt_in,   myThid )
        CALL EXCH_XY_RL( fld_in,  myThid )
        CALL EXCH_XY_RL( gtm1_in, myThid )

      ENDDO

      RETURN
      END

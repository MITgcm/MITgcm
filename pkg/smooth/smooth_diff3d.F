#include "SMOOTH_OPTIONS.h"

      SUBROUTINE SMOOTH_DIFF3D( fld_in, nbt_in, myThid )

C     *==========================================================*
C     | SUBROUTINE smooth_diff3D
C     | o Routine that smoothes a 3D field, using diffusion
C     *==========================================================*

      IMPLICIT NONE
#include "EEPARAMS.h"
#include "SIZE.h"
#include "PARAMS.h"
#include "GRID.h"
#include "SMOOTH.h"

      _RL fld_in(1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nr,nSx,nSy)
      INTEGER nbt_in
      INTEGER myThid

      INTEGER i, j, k, bi, bj
      INTEGER myIter(nSx,nSy), iloop
      _RL gT_in  (1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nr,nSx,nSy)
      _RL gTm1_in(1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nr,nSx,nSy)
      _RL gt_AB  (1-OLx:sNx+OLx,1-OLy:sNy+OLy)

      DO bj=myByLo(myThid),myByHi(myThid)
       DO bi=myBxLo(myThid),myBxHi(myThid)
        DO k=1,Nr
         DO j=1-OLy,sNy+OLy
          DO i=1-OLx,sNx+OLx
           gT_in(i,j,k,bi,bj)   = 0. _d 0
           gTm1_in(i,j,k,bi,bj) = 0. _d 0
          ENDDO
         ENDDO
        ENDDO
       ENDDO
      ENDDO

      CALL EXCH_XYZ_RL( fld_in,  myThid )
      CALL EXCH_XYZ_RL( gt_in,   myThid )
      CALL EXCH_XYZ_RL( gtm1_in, myThid )

      DO iloop=1,nbt_in

       DO bj=myByLo(myThid),myByHi(myThid)
        DO bi=myBxLo(myThid),myBxHi(myThid)
         DO k=1,Nr
          DO j=1,sNy
           DO i=1,sNx
            gT_in(i,j,k,bi,bj) = 0. _d 0
           ENDDO
          ENDDO
         ENDDO
        ENDDO
       ENDDO

       CALL EXCH_XYZ_RL( gt_in, myThid )

c compute gT_in:
       CALL SMOOTH_RHS( fld_in, gT_in, myThid )

       DO bj=myByLo(myThid),myByHi(myThid)
        DO bi=myBxLo(myThid),myBxHi(myThid)
c adams bashfort on gT_in:
         myIter(bi,bj) = iloop-1
         DO k=1,Nr
          CALL ADAMS_BASHFORTH2(
     I                        bi, bj, k, Nr,
     U                        gT_in(1-OLx,1-OLy,1,bi,bj),
     U                        gTm1_in(1-OLx,1-OLy,1,bi,bj), gt_AB,
     I                        0, myIter(bi,bj), myThid )
         ENDDO
c time stepping:
         DO k=1,Nr
          DO j=1-OLy,sNy+OLy
           DO i=1-OLx,sNx+OLx
            IF (maskC(i,j,k,bi,bj).NE.0.) THEN
             fld_in(i,j,k,bi,bj) = fld_in(i,j,k,bi,bj)
     &            + smooth3DdelTime*gT_in(i,j,k,bi,bj)
             gT_in(i,j,k,bi,bj) = 0
            ENDIF
           ENDDO
          ENDDO
         ENDDO

         IF ( smooth3DdoImpldiff ) THEN
          CALL SMOOTH_IMPLDIFF(
     I         bi, bj, 1, sNx, 1, sNy ,
     I         smooth3DdelTime, smooth3D_kappaR(1-OLx,1-OLy,1,bi,bj),
     I         recip_hFacC,
     U         fld_in,
     I         myThid )
         ENDIF

        ENDDO
       ENDDO

       CALL EXCH_XYZ_RL ( fld_in , myThid )
       CALL EXCH_XYZ_RL ( gt_in , myThid )
       CALL EXCH_XYZ_RL ( gtm1_in , myThid )

      ENDDO

      RETURN
      END

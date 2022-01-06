#include "SMOOTH_OPTIONS.h"
#ifdef ALLOW_AUTODIFF
# include "AUTODIFF_OPTIONS.h"
#endif

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
#ifdef ALLOW_AUTODIFF_TAMC
#include "tamc.h"
#endif /* ALLOW_AUTODIFF_TAMC */
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
#ifdef ALLOW_TAMC_CHECKPOINTING
      INTEGER ilev_1, ilev_2, ilev_3
      INTEGER max_lev2, max_lev3, key_in
#endif

#ifdef ALLOW_TAMC_CHECKPOINTING
c for now: useless, because level 3 is recomputed anyway
c but : if level3 was computed during the fwd loop by calling
c       mdsmooth_diff3D (assumes that it would be called
c       directly by the_main_loop) then I would need to pass key_in
c       as a parameter, with different values for T, S, ...
c       in order not to overwrite the same tape
      key_in = 0
#endif

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

#ifdef ALLOW_TAMC_CHECKPOINTING

c checkpointing:
      max_lev3 = nbt_in/(nchklev_1*nchklev_2)+1
      max_lev2 = nbt_in/nchklev_1+1
#ifdef ALLOW_AUTODIFF_TAMC
CADJ INIT tape_smooth2D_lev3 = USER
#endif /* ALLOW_AUTODIFF_TAMC */
      DO ilev_3 = 1,nchklev_3
       IF (ilev_3.LE.max_lev3) THEN
#ifdef ALLOW_AUTODIFF_TAMC
CADJ STORE fld_in = tape_smooth2D_lev3 ,
CADJ & key = key_in*max_lev3 + ilev_3
CADJ STORE gTm1_in = tape_smooth2D_lev3 ,
CADJ & key = key_in*max_lev3 + ilev_3
#endif /* ALLOW_AUTODIFF_TAMC */
#ifdef ALLOW_AUTODIFF_TAMC
CADJ INIT tape_smooth2D_lev2 = USER
#endif /* ALLOW_AUTODIFF_TAMC */

        DO ilev_2 = 1,nchklev_2
         IF (ilev_2.LE.max_lev2) THEN
#ifdef ALLOW_AUTODIFF_TAMC
CADJ STORE fld_in = tape_smooth2D_lev2 ,
CADJ & key = key_in*nchklev_2 + ilev_2
CADJ STORE gTm1_in = tape_smooth2D_lev2 ,
CADJ & key = key_in*nchklev_2 + ilev_2
#endif /* ALLOW_AUTODIFF_TAMC */
#ifdef ALLOW_AUTODIFF_TAMC
CADJ INIT tape_smooth2D_lev1  = COMMON,
CADJ & nchklev_1*nSx*nSy*nthreads_chkpt
#endif /* ALLOW_AUTODIFF_TAMC */

          DO ilev_1 = 1,nchklev_1
           iloop = (ilev_2 - 1)*nchklev_1 + ilev_1
     &           + (ilev_3 - 1)*nchklev_2*nchklev_1
           IF ( iloop .LE. nbt_in ) THEN
#ifdef ALLOW_AUTODIFF_TAMC
c needed?? CADJ STORE fld_in = tape_smooth2D_lev1 ,
c CADJ & key = key_in*nchklev_1 + ilev_1
CADJ STORE gtm1_in = tape_smooth2D_lev1 ,
CADJ & key = key_in*nchklev_1 + ilev_1
#endif /* ALLOW_AUTODIFF_TAMC */

#else /* ALLOW_TAMC_CHECKPOINTING */
      DO iloop=1,nbt_in
#endif

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

#ifdef ALLOW_TAMC_CHECKPOINTING
           ENDIF
          ENDDO
         ENDIF
        ENDDO
       ENDIF
      ENDDO
#else /* ALLOW_TAMC_CHECKPOINTING */
      ENDDO
#endif

      RETURN
      END

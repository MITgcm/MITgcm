#include "SMOOTH_OPTIONS.h"
#ifdef ALLOW_AUTODIFF
# include "AUTODIFF_OPTIONS.h"
#endif

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
#ifdef ALLOW_AUTODIFF_TAMC
#include "tamc.h"
#endif /* ALLOW_AUTODIFF_TAMC */
#include "SMOOTH.h"

      _RL fld_in(1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nr,nSx,nSy)
      INTEGER nbt_in
      INTEGER myThid

      INTEGER i, j, k, bi, bj
      INTEGER myIter(nSx,nSy), iloop
      _RL gT_in  (1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nr,nSx,nSy)
      _RL gTm1_in(1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nr,nSx,nSy)
      _RL gt_AB  (1-OLx:sNx+OLx,1-OLy:sNy+OLy)
#ifdef ALLOW_TAMC_CHECKPOINTING
      INTEGER ilev_1, ilev_2, ilev_3
      INTEGER max_lev2, max_lev3, key_in
#endif

#ifdef ALLOW_TAMC_CHECKPOINTING
c for now: useless, because level 3 is recomputed anyway
c but : if level3 was computed during the fwd loop by callung
c       mdsmooth_diff3D (assumes that it would be called
c       directly by the_main_loop) then I would need to pass key_in
c       as a parameter, with different values for T, S, ...
c       in order not to overwrite the same tape
      key_in = 0
#endif

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

#ifdef ALLOW_TAMC_CHECKPOINTING

c checkpointing:
      max_lev3=nbt_in/(nchklev_1*nchklev_2)+1
      max_lev2=nbt_in/nchklev_1+1
#ifdef ALLOW_AUTODIFF_TAMC
CADJ INIT tape_smooth_lev3 = USER
#endif /* ALLOW_AUTODIFF_TAMC */
      DO ilev_3 = 1,nchklev_3
       IF (ilev_3.LE.max_lev3) THEN
#ifdef ALLOW_AUTODIFF_TAMC
CADJ STORE fld_in = tape_smooth_lev3 ,
CADJ & key = key_in*max_lev3 + ilev_3
CADJ STORE gTm1_in = tape_smooth_lev3 ,
CADJ & key = key_in*max_lev3 + ilev_3
#endif /* ALLOW_AUTODIFF_TAMC */
#ifdef ALLOW_AUTODIFF_TAMC
CADJ INIT tape_smooth_lev2 = USER
#endif /* ALLOW_AUTODIFF_TAMC */
        DO ilev_2 = 1,nchklev_2
         IF (ilev_2.LE.max_lev2) THEN
#ifdef ALLOW_AUTODIFF_TAMC
CADJ STORE fld_in = tape_smooth_lev2 ,
CADJ & key = key_in*nchklev_2 + ilev_2
CADJ STORE gTm1_in = tape_smooth_lev2 ,
CADJ & key = key_in*nchklev_2 + ilev_2
#endif /* ALLOW_AUTODIFF_TAMC */
#ifdef ALLOW_AUTODIFF_TAMC
CADJ INIT tape_smooth_lev1  = COMMON,
CADJ & nchklev_1*nsx*nsy*nthreads_chkpt
#endif /* ALLOW_AUTODIFF_TAMC */
          DO ilev_1 = 1,nchklev_1
           iloop = (ilev_2 - 1)*nchklev_1 + ilev_1
     &            + (ilev_3 - 1)*nchklev_2*nchklev_1
           IF ( iloop .LE. nbt_in ) THEN

#ifdef ALLOW_AUTODIFF_TAMC
CADJ STORE gTm1_in(:,:,:,bi,bj) = tape_smooth_lev1 ,
CADJ & key = key_in*nchklev_1 + ilev_1
#endif /* ALLOW_AUTODIFF_TAMC */

#else /* ALLOW_TAMC_CHECKPOINTING */
      DO iloop=1,nbt_in
#endif

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
       CALL smooth_rhs( fld_in, gT_in, myThid )

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

#ifdef ALLOW_AUTODIFF_TAMC
CADJ STORE fld_in(:,:,:,bi,bj) = tape_smooth_lev1,
CADJ & key = key_in*nchklev_1 + ilev_1
#endif /* ALLOW_AUTODIFF_TAMC */

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

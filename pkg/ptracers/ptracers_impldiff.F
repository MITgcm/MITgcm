C $Header: /u/gcmpack/MITgcm/pkg/ptracers/Attic/ptracers_impldiff.F,v 1.8 2004/12/04 00:17:50 jmc Exp $
C $Name:  $

#include "PTRACERS_OPTIONS.h"

CBOP
C !ROUTINE: PTRACERS_IMPLDIFF

C !INTERFACE: ==========================================================
      SUBROUTINE PTRACERS_IMPLDIFF( bi,bj,kappaRk,myThid )

C !DESCRIPTION:
C     Calls the implicit vertical diffusion routine for each passive
C     tracer.

C !USES: ===============================================================
      IMPLICIT NONE
#include "SIZE.h"
#include "EEPARAMS.h"
#include "PTRACERS_SIZE.h"
#include "PTRACERS.h"
#include "PARAMS.h"
#include "GRID.h"
#include "GAD.h"
#ifdef ALLOW_AUTODIFF_TAMC
# include "tamc.h"
# include "tamc_keys.h"
#endif

C !INPUT PARAMETERS: ===================================================
C  bi,bj   :: tile indices
C  myThid  :: thread number
      INTEGER bi,bj
      INTEGER myThid

C !OUTPUT PARAMETERS: ==================================================
C  KappaRk :: vertical diffusion coefficient
      _RL kappaRk(1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nr)

#ifdef ALLOW_PTRACERS

C !LOCAL VARIABLES: ====================================================
C  iTracer :: tracer index
      INTEGER iTracer
      INTEGER iMin,iMax,jMin,jMax
      INTEGER GAD_TR
CEOP

      iMin=0
      iMax=sNx+1
      jMin=0
      jMax=sNy+1

C Loop over tracers
      DO iTracer=1,PTRACERS_numInUse

#ifdef ALLOW_AUTODIFF_TAMC
          act0 = iTracer - 1
          max0 = PTRACERS_num
          act1 = bi - myBxLo(myThid)
          max1 = myBxHi(myThid) - myBxLo(myThid) + 1
          act2 = bj - myByLo(myThid)
          max2 = myByHi(myThid) - myByLo(myThid) + 1
          act3 = myThid - 1
          max3 = nTx*nTy
          act4 = ikey_dynamics - 1
          iptrkey = (act0 + 1) 
     &                      + act1*max0
     &                      + act2*max0*max1
     &                      + act3*max0*max1*max2
     &                      + act4*max0*max1*max2*max3
#endif /* ALLOW_AUTODIFF_TAMC */

          GAD_TR = GAD_TR1 + iTracer - 1
          CALL CALC_3D_DIFFUSIVITY(
     I         bi,bj,iMin,iMax,jMin,jMax,
     I    GAD_TR,PTRACERS_useGMRedi(iTracer),PTRACERS_useKPP(iTracer),
     O         kappaRk,
     I         myThid)

#ifdef ALLOW_AUTODIFF_TAMC
CADJ STORE gPtr(:,:,:,bi,bj,iTracer) = comlev1_bibj_ptracers, 
CADJ &     key=iptrkey, byte=isbyte
#endif /* ALLOW_AUTODIFF_TAMC */

c #ifdef INCLUDE_IMPLVERTADV_CODE
c       IF ( ptracerImplVertAdv ) THEN
c       ELSEIF ( implicitDiffusion ) THEN
c #else

          CALL IMPLDIFF(
     I         bi, bj, iMin, iMax, jMin, jMax,
     I         dTtracerLev(1), kappaRk, recip_HFacC,
     U         gPtr(1-Olx,1-Oly,1,1,1,iTracer),
     I         myThid )

C End of tracer loop
      ENDDO

#endif /* ALLOW_PTRACERS */

      RETURN
      END

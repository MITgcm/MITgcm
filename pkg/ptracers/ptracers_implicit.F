C $Header: /u/gcmpack/MITgcm/pkg/ptracers/Attic/ptracers_implicit.F,v 1.4 2005/10/10 05:53:48 mlosch Exp $
C $Name:  $

#include "PTRACERS_OPTIONS.h"

CBOP
C !ROUTINE: PTRACERS_IMPLICIT

C !INTERFACE: ==========================================================
      SUBROUTINE PTRACERS_IMPLICIT( 
     U                              kappaRk,
     I                              bi, bj, myTime, myIter, myThid )

C !DESCRIPTION:
C     Calls the implicit vertical advection/diffusion routine
C     for each passive tracer.

C !USES: ===============================================================
      IMPLICIT NONE
#include "SIZE.h"
#include "EEPARAMS.h"
#include "PTRACERS_SIZE.h"
#include "PTRACERS.h"
#include "PARAMS.h"
#include "GRID.h"
#include "GAD.h"
#include "DYNVARS.h"
#ifdef ALLOW_AUTODIFF_TAMC
# include "tamc.h"
# include "tamc_keys.h"
#endif

C !INPUT PARAMETERS: ===================================================
C  bi,bj   :: tile indices
C  myTime  :: time in simulation
C  myIter  :: iteration number in simulation
C  myThid  :: thread number
      INTEGER bi,bj
      _RL     myTime
      INTEGER myIter
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
      INTEGER K
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
        IF ( implicitDiffusion ) THEN
          CALL CALC_3D_DIFFUSIVITY(
     I         bi,bj,iMin,iMax,jMin,jMax,
     I         GAD_TR,
     I         PTRACERS_useGMRedi(iTracer),PTRACERS_useKPP(iTracer),
     O         kappaRk,
     I         myThid)
        ENDIF

#ifdef ALLOW_AUTODIFF_TAMC
CADJ STORE kappaRk(:,:,:) = comlev1_bibj_ptracers, 
CADJ &     key=iptrkey, byte=isbyte
CADJ STORE gPtr(:,:,:,bi,bj,iTracer) = comlev1_bibj_ptracers, 
CADJ &     key=iptrkey, byte=isbyte
#endif /* ALLOW_AUTODIFF_TAMC */

#ifdef INCLUDE_IMPLVERTADV_CODE
        IF ( PTRACERS_ImplVertAdv(iTracer) ) THEN

          CALL GAD_IMPLICIT_R(
     I         PTRACERS_ImplVertAdv(iTracer), 
     I         PTRACERS_advScheme(iTracer), GAD_TR,
     I         kappaRk, wVel, pTracer(1-Olx,1-Oly,1,1,1,iTracer),
     U         gPtr(1-Olx,1-Oly,1,1,1,iTracer),
     I         bi, bj, myTime, myIter, myThid )

        ELSEIF ( implicitDiffusion ) THEN
#else /* INCLUDE_IMPLVERTADV_CODE */
        IF ( implicitDiffusion ) THEN
#endif /* INCLUDE_IMPLVERTADV_CODE */

          CALL IMPLDIFF(
     I         bi, bj, iMin, iMax, jMin, jMax,
     I         GAD_TR, kappaRk, recip_HFacC,
     U         gPtr(1-Olx,1-Oly,1,1,1,iTracer),
     I         myThid )

#ifdef   ALLOW_OBCS
C--      Apply open boundary conditions
        IF ( useOBCS ) THEN
         DO K=1,Nr
          CALL OBCS_APPLY_PTRACER( 
     I         bi, bj, K, iTracer,
     U         gPtr(1-Olx,1-Oly,K,bi,bj,iTracer),
     I         myThid )
         ENDDO
        ENDIF
#endif   /* ALLOW_OBCS */

        ENDIF
C End of tracer loop
      ENDDO

#endif /* ALLOW_PTRACERS */

      RETURN
      END

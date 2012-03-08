C $Header: /u/gcmpack/MITgcm/pkg/ptracers/Attic/ptracers_advection.F,v 1.17 2012/03/08 17:13:31 jmc Exp $
C $Name:  $

#include "PTRACERS_OPTIONS.h"
#include "GAD_OPTIONS.h"

CBOP
C !ROUTINE: PTRACERS_ADVECTION

C !INTERFACE: ==========================================================
      SUBROUTINE PTRACERS_ADVECTION( bi,bj,myTime,myIter,myThid )

C !DESCRIPTION:
C     Calculates tendency for passive tracers and integrates forward
C     in time.

C !USES: ===============================================================
#include "PTRACERS_MOD.h"
      IMPLICIT NONE
#include "SIZE.h"
#include "EEPARAMS.h"
#include "PARAMS.h"
#include "DYNVARS.h"
#ifdef ALLOW_LONGSTEP
#include "LONGSTEP_PARAMS.h"
#include "LONGSTEP.h"
#endif
#include "PTRACERS_SIZE.h"
#include "PTRACERS_PARAMS.h"
#include "PTRACERS_START.h"
#include "PTRACERS_FIELDS.h"
#include "GAD.h"
#ifdef ALLOW_AUTODIFF_TAMC
# include "tamc.h"
# include "tamc_keys.h"
#endif

C !INPUT PARAMETERS: ===================================================
C  bi,bj                :: tile indices
C  myTime               :: model time
C  myIter               :: time-step number
C  myThid               :: thread number
      INTEGER bi,bj
      _RL myTime
      INTEGER myIter
      INTEGER myThid

C !OUTPUT PARAMETERS: ==================================================
C  none

#ifdef ALLOW_PTRACERS

C !LOCAL VARIABLES: ====================================================
C  i,j,k,bi,bj,iTracer  :: loop indices
C  iMin,iMax,jMin,jMax  :: loop ranges
C  kUp,kDown            :: toggle indices for even/odd level fluxes
C  km1                  :: =min(1,k-1)
C  rFlx                 :: vertical flux
      INTEGER iTracer
CEOP

C Loop over tracers
      DO iTracer=1,PTRACERS_numInUse
      IF ( PTRACERS_StepFwd(iTracer) ) THEN

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

#ifdef ALLOW_AUTODIFF_TAMC
CADJ STORE pTracer(:,:,:,bi,bj,iTracer)
CADJ &      = comlev1_bibj_ptracers, key=iptrkey, byte=isbyte
#endif /* ALLOW_AUTODIFF_TAMC */

#ifdef PTRACERS_ALLOW_DYN_STATE
       IF ( PTRACERS_SOM_Advection(iTracer) ) THEN
#ifdef ALLOW_DEBUG
         IF (debugMode) CALL DEBUG_CALL('GAD_SOM_ADVECT',myThid)
#endif
         CALL GAD_SOM_ADVECT(
     I                       PTRACERS_ImplVertAdv(iTracer),
     I                       PTRACERS_advScheme(iTracer),
     I                       PTRACERS_advScheme(iTracer),
     I                       GAD_TR1+iTracer-1,
     I                       PTRACERS_dTLev,
#ifdef ALLOW_LONGSTEP
     I                       LS_uVel, LS_vVel, LS_wVel,
#else
     I                       uVel, vVel, wVel,
#endif
     I                       pTracer(1-OLx,1-OLy,1,1,1,iTracer),
     U                       _Ptracers_som(:,:,:,:,:,:,iTracer),
     O                       gPtr(1-OLx,1-OLy,1,1,1,iTracer),
     I                       bi,bj,myTime,myIter,myThid)
       ELSEIF ( PTRACERS_MultiDimAdv(iTracer) ) THEN
#else /* PTRACERS_ALLOW_DYN_STATE */
       IF ( PTRACERS_MultiDimAdv(iTracer) ) THEN
#endif /* PTRACERS_ALLOW_DYN_STATE */
            CALL GAD_ADVECTION(
     I                        PTRACERS_ImplVertAdv(iTracer),
     I                        PTRACERS_advScheme(iTracer),
     I                        PTRACERS_advScheme(iTracer),
     I                        GAD_TR1+iTracer-1,
     I                        PTRACERS_dTLev,
#ifdef ALLOW_LONGSTEP
     I                        LS_uVel, LS_vVel, LS_wVel,
#else
     I                        uVel, vVel, wVel,
#endif
     I                        pTracer(1-OLx,1-OLy,1,1,1,iTracer),
     O                        gPtr(1-OLx,1-OLy,1,1,1,iTracer),
     I                        bi,bj,myTime,myIter,myThid)
       ENDIF


C end of tracer loop
      ENDIF
      ENDDO

#endif /* ALLOW_PTRACERS */

      RETURN
      END

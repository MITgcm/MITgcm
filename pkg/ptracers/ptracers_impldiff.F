C $Header: /u/gcmpack/MITgcm/pkg/ptracers/Attic/ptracers_impldiff.F,v 1.4 2004/07/13 16:47:49 jmc Exp $
C $Name:  $

#include "PTRACERS_OPTIONS.h"

CBOP
C !ROUTINE: PTRACERS_IMPLDIFF

C !INTERFACE: ==========================================================
      SUBROUTINE PTRACERS_IMPLDIFF( bi,bj,KappaR,myThid )

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
#ifdef ALLOW_AUTODIFF_TAMC
# include "tamc.h"
# include "tamc_keys.h"
#endif

C !INPUT PARAMETERS: ===================================================
C  bi,bj   :: tile indices
C  KappaR  :: vertical diffusion coefficient
C  myThid  :: thread number
      INTEGER bi,bj
      _RL KappaR(1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nr)
      INTEGER myThid

C !OUTPUT PARAMETERS: ==================================================
C  none

#ifdef ALLOW_PTRACERS

C !LOCAL VARIABLES: ====================================================
C  iTracer :: tracer index
      INTEGER iTracer
      INTEGER iMin,iMax,jMin,jMax
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

#ifdef ALLOW_AUTODIFF_TAMC
CADJ STORE gPtr(:,:,:,bi,bj,iTracer) = comlev1_bibj_ptracers, 
CADJ &     key=iptrkey, byte=isbyte
#endif /* ALLOW_AUTODIFF_TAMC */

            CALL IMPLDIFF(
     I         bi, bj, iMin, iMax, jMin, jMax,
     I         deltaTtracer, KappaR, recip_HFacC,
     U         gPtr(1-Olx,1-Oly,1,bi,bj,iTracer),
     I         myThid )

C End of tracer loop
      ENDDO

#endif /* ALLOW_PTRACERS */

      RETURN
      END

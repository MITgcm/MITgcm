#include "PTRACERS_OPTIONS.h"
#ifdef ALLOW_AUTODIFF
# include "AUTODIFF_OPTIONS.h"
#endif

CBOP
C !ROUTINE: PTRACERS_CONVECT

C !INTERFACE: ==========================================================
      SUBROUTINE PTRACERS_CONVECT( bi,bj,k,weightA,weightB,myThid )

C !DESCRIPTION:
C     do passive tracers convection

C !USES: ===============================================================
      IMPLICIT NONE
#include "SIZE.h"
#include "EEPARAMS.h"
#include "PTRACERS_SIZE.h"
#include "PTRACERS_PARAMS.h"
#include "PTRACERS_START.h"
#include "PTRACERS_FIELDS.h"
#ifdef ALLOW_AUTODIFF_TAMC
# include "tamc.h"
#endif

C !INPUT PARAMETERS: ===================================================
C  bi,bj,k :: tile indices
C  weightA :: weight for level K-1
C  weightB :: weight for level K
C  myThid  :: thread number
      INTEGER bi,bj,k
      _RL weightA(1-OLx:sNx+OLx,1-OLy:sNy+OLy)
      _RL weightB(1-OLx:sNx+OLx,1-OLy:sNy+OLy)
      INTEGER myThid

C !OUTPUT PARAMETERS: ==================================================
C  none

#ifdef ALLOW_PTRACERS

C !LOCAL VARIABLES: ====================================================
C  iTracer :: tracer index
      INTEGER iTracer
#ifdef ALLOW_AUTODIFF_TAMC
      INTEGER act0, act1, act2, act3, act4
      INTEGER max0, max1, max2, max3
      INTEGER iptrkey, kkey
#endif
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
        iptrkey = (act0 + 1) + act1*max0
     &                       + act2*max0*max1
     &                       + act3*max0*max1*max2
     &                       + act4*max0*max1*max2*max3
        kkey = (iptrkey-1)*Nr + k
CADJ STORE pTracer(:,:,k,bi,bj,iTracer)
CADJ &      = comlev1_bibj_k_ptracers, key=kkey, byte=isbyte
#endif /* ALLOW_AUTODIFF_TAMC */

        CALL CONVECTIVELY_MIXTRACER(
     I           bi,bj,k,weightA,weightB,
     U           pTracer(1-OLx,1-OLy,1,1,1,iTracer),
     I           myThid )

C End of tracer loop
       ENDIF
      ENDDO

#endif /* ALLOW_PTRACERS */

      RETURN
      END

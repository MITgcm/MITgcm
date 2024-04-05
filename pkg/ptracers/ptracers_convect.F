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
#if ( defined NONLIN_FRSURF && defined ALLOW_AUTODIFF_TAMC )
C     kkey :: tape key (depends on levels, tracer, and tiles)
      INTEGER kkey
#endif
CEOP

C Loop over tracers
      DO iTracer=1,PTRACERS_numInUse
       IF ( PTRACERS_StepFwd(iTracer) ) THEN

#if ( defined NONLIN_FRSURF && defined ALLOW_AUTODIFF_TAMC )
        kkey = bi + (bj-1)*nSx + (ikey_dynamics-1)*nSx*nSy
        kkey = iTracer + (kkey-1)*PTRACERS_num
        kkey = (kkey-1)*Nr + k
CADJ STORE pTracer(:,:,k,bi,bj,iTracer)
CADJ &      = comlev1_bibj_k_ptracers, key=kkey, byte=isbyte
#endif

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

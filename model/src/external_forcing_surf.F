C $Header: /u/gcmpack/MITgcm/model/src/external_forcing_surf.F,v 1.6 2002/02/10 00:39:22 jmc Exp $
C $Name:  $

#include "CPP_OPTIONS.h"
 
CBOP
C     !ROUTINE: EXTERNAL_FORCING_SURF
C     !INTERFACE:
      SUBROUTINE EXTERNAL_FORCING_SURF( 
     I             bi, bj, iMin, iMax, jMin, jMax,
     I             myThid )
C     !DESCRIPTION: \bv
C     *==========================================================*
C     | SUBROUTINE EXTERNAL_FORCING_SURF                          
C     | o Determines forcing terms based on external fields       
C     |   relaxation terms etc.                                   
C     *==========================================================*
C     \ev

C     !USES:
      IMPLICIT NONE
C     === Global variables ===
#include "SIZE.h"
#include "EEPARAMS.h"
#include "PARAMS.h"
#include "FFIELDS.h"
#include "DYNVARS.h"
#include "GRID.h"
#ifdef NONLIN_FRSURF
#include "SURFACE.h"
#endif
 
C     !INPUT/OUTPUT PARAMETERS:
C     === Routine arguments ===
C     myThid :: Thread no. that called this routine.
      INTEGER myThid
      INTEGER bi,bj
      INTEGER iMin, iMax
      INTEGER jMin, jMax

C     !LOCAL VARIABLES:
C     === Local variables ===
      INTEGER i,j
CEOP

      DO j = jMin, jMax
         DO i = iMin, iMax

c     Zonal wind stress fu:
          surfaceTendencyU(i,j,bi,bj) = 
     &      fu(i,j,bi,bj)*horiVertRatio*recip_rhoNil
     &           *recip_drF(1)*recip_hFacW(i,j,1,bi,bj)
c     Meridional wind stress fv:
          surfaceTendencyV(i,j,bi,bj) = 
     &      fv(i,j,bi,bj)*horiVertRatio*recip_rhoNil
     &           *recip_drF(1)*recip_hFacS(i,j,1,bi,bj)
c     Net heat flux Qnet:
          surfaceTendencyT(i,j,bi,bj) = 
     &      -Qnet(i,j,bi,bj)*recip_Cp*recip_rhoNil
     &           *recip_drF(1)*recip_hFacC(i,j,1,bi,bj)
     &      -lambdaThetaClimRelax*(theta(i,j,1,bi,bj)-SST(i,j,bi,bj))
C     Salt Flux (restoring term) : 
C         surfaceTendencyS(i,j,bi,bj) = 
C    &      -lambdaSaltClimRelax*(salt(i,j,1,bi,bj)-SSS(i,j,bi,bj))
C notes : because truncation is different when this tendency is splitted 
C   in 2 parts, keep this salt flux with freshwater flux (see below)

#ifdef ALLOW_PASSIVE_TRACER
c ***  define the tracer surface tendency here ***
#endif /* ALLOW_PASSIVE_TRACER */

         ENDDO
      ENDDO

c---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|
C     Surface salinity tendency and freshwater flux EmPmR: 

      IF (.NOT.useRealFreshWaterFlux .OR. nonlinFreeSurf .LE. 0 ) THEN

c- EmPmR does not really affect the water column height (for tracer budget)
c   and is converted to a salt tendency.

       IF (convertFW2Salt .EQ. -1.) THEN
c- converts EmPmR to salinity tendency using surface local salinity
        DO j = jMin, jMax
         DO i = iMin, iMax
          surfaceTendencyS(i,j,bi,bj) =
     &      + EmPmR(i,j,bi,bj)*salt(i,j,1,bi,bj)
     &           *recip_drF(1)*recip_hFacC(i,j,1,bi,bj)
     &      -lambdaSaltClimRelax*(salt(i,j,1,bi,bj)-SSS(i,j,bi,bj))
         ENDDO
        ENDDO
       ELSE 
c- converts EmPmR to virtual salt flux using uniform salinity (default=35)
        DO j = jMin, jMax
         DO i = iMin, iMax
          surfaceTendencyS(i,j,bi,bj) = 
     &      + EmPmR(i,j,bi,bj)*convertFW2Salt
     &           *recip_drF(1)*recip_hFacC(i,j,1,bi,bj)
     &      -lambdaSaltClimRelax*(salt(i,j,1,bi,bj)-SSS(i,j,bi,bj))
         ENDDO
        ENDDO
       ENDIF

#ifdef NONLIN_FRSURF
c---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|
      ELSE

c     Salt Flux (restoring term) : 
       DO j = jMin, jMax
         DO i = iMin, iMax
          surfaceTendencyS(i,j,bi,bj) = 
     &      -lambdaSaltClimRelax*(salt(i,j,1,bi,bj)-SSS(i,j,bi,bj))
         ENDDO
       ENDDO

c-  NonLin_FrSurf and RealFreshWaterFlux : PmEpR effectively changes 
c   the water column height ; temp., salt, (tracer) flux associated 
c   with this input/output of water is added here to the surface tendency.
c
c NB: PmEpR lag 1 time step behind EmPmR ( PmEpR_n = - EmPmR_n-1 ) to stay 
c     consitent with volume change (=d/dt etaN).

       IF (temp_EvPrRn.NE.UNSET_RL) THEN
        DO j = jMin, jMax
         DO i = iMin, iMax
          surfaceTendencyT(i,j,bi,bj) = surfaceTendencyT(i,j,bi,bj)
     &      + PmEpR(i,j,bi,bj)*( temp_EvPrRn - theta(i,j,1,bi,bj) )
     &           *recip_drF(1)*recip_hFacC(i,j,1,bi,bj)
         ENDDO
        ENDDO
       ENDIF

       IF (salt_EvPrRn.NE.UNSET_RL) THEN
        DO j = jMin, jMax
         DO i = iMin, iMax
          surfaceTendencyS(i,j,bi,bj) = surfaceTendencyS(i,j,bi,bj)
     &      + PmEpR(i,j,bi,bj)*( salt_EvPrRn - salt(i,j,1,bi,bj) )
     &           *recip_drF(1)*recip_hFacC(i,j,1,bi,bj)
         ENDDO
        ENDDO
       ENDIF

#ifdef ALLOW_PASSIVE_TRACER
c  *** add the tracer flux associated with P-E+R here *** 
c      IF (trac_EvPrRn.NE.UNSET_RL) THEN
c    &      + PmEpR(i,j,bi,bj)*( trac_EvPrRn - tr1(i,j,1,bi,bj) )
c    &           *recip_drF(1)*recip_hFacC(i,j,1,bi,bj)
c      ENDIF
#endif /* ALLOW_PASSIVE_TRACER */

#endif /* NONLIN_FRSURF */
      ENDIF

      RETURN
      END

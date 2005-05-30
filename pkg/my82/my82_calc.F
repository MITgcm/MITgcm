C $Header: /u/gcmpack/MITgcm/pkg/my82/my82_calc.F,v 1.4 2005/05/30 07:41:45 mlosch Exp $
C $Name:  $

#include "MY82_OPTIONS.h"

CBOP
C !ROUTINE: MY82_CALC

C !INTERFACE: ======================================================
      subroutine MY82_CALC(
     I     bi, bj, myTime, myThid )

C !DESCRIPTION: \bv
C     /==========================================================\
C     | SUBROUTINE MY82_CALC                                     |
C     | o Compute all MY82 fields defined in MY82.h              |
C     |==========================================================|
C     | This subroutine is based on SPEM code                    |
C     \==========================================================/
      IMPLICIT NONE
C
C--------------------------------------------------------------------

C global parameters updated by pp_calc
C     PPviscAz   - PP eddy viscosity coefficient              (m^2/s)
C     PPdiffKzT  - PP diffusion coefficient for temperature   (m^2/s)
C
C \ev

C !USES: ============================================================
#include "SIZE.h"
#include "EEPARAMS.h"
#include "PARAMS.h"
#include "DYNVARS.h"
#include "MY82.h"
#include "FFIELDS.h"
#include "GRID.h"
#ifdef ALLOW_AUTODIFF_TAMC
#include "tamc.h"
#include "tamc_keys.h"
#else /* ALLOW_AUTODIFF_TAMC */
      integer ikppkey
#endif /* ALLOW_AUTODIFF_TAMC */

C !INPUT PARAMETERS: ===================================================
c Routine arguments
c     bi, bj - array indices on which to apply calculations
c     myTime - Current time in simulation

      INTEGER bi, bj
      INTEGER myThid
      _RL     myTime

#ifdef ALLOW_MY82

C !LOCAL VARIABLES: ====================================================
c Local constants
C     imin, imax, jmin, jmax  - array computation indices
C     RiNumber - Richardson Number = -GH/GM
C     GH       - buoyancy freqency after call to Ri_number, later
C                GH of M. Satoh, Eq. (11.3.45)
C     GM       - vertical shear of velocity after call Ri_number,
C                later GM of M. Satoh, Eq. (11.3.44)
      INTEGER I, J, K
      INTEGER   iMin ,iMax ,jMin ,jMax
      _RL     RiFlux, RiTmp
      _RL     SHtmp, bTmp, tkesquare, tkel
      _RL     RiNumber(1-OLx:sNx+OLx,1-OLy:sNy+OLy)
      _RL     GH(1-OLx:sNx+OLx,1-OLy:sNy+OLy)
      _RL     GM(1-OLx:sNx+OLx,1-OLy:sNy+OLy)
      _RL     SH(1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nr)
      _RL     SM(1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nr)
      _RL     tke(1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nr)
CEOP
      iMin = 2-OLx
      iMax = sNx+OLx-1
      jMin = 2-OLy
      jMax = sNy+OLy-1

C     Initialize local fields
      DO J=1-Oly,sNy+Oly
       DO I=1-Olx,sNx+Olx
	GH(I,J) = 0. _d 0
	GM(I,J) = 0. _d 0
       ENDDO
      ENDDO
      DO K = 1, Nr
       DO J=1-Oly,sNy+Oly
        DO I=1-Olx,sNx+Olx
         SH(I,J,K)  = 0. _d 0
         SM(I,J,K)  = 0. _d 0
         tke(I,J,K) = 0. _d 0
        ENDDO
       ENDDO	
      ENDDO
C     first k-loop
C     compute turbulent kinetic energy from richardson number
      DO K = 2, Nr
       CALL MY82_RI_NUMBER(
     I      bi, bj, K, iMin, iMax, jMin, jMax, 
     O      RiNumber, GH, GM,
     I      myTime, myThid )
       DO J=jMin,jMax
        DO I=iMin,iMax
         RiTmp  = MIN(RiNumber(I,J),RiMax)
         btmp   = beta1+beta4*RiTmp
C     M. Satoh, Atmospheric Circulation Dynamics and General
C     Circulation models, Springer, 2004: Eq. (11.3.60)
         RiFlux = ( btmp - SQRT(btmp*btmp - 4.*beta2*beta3*RiTmp) )
     &        /(2.*beta2)
C     M. Satoh: Eq. (11.3.58)
         SHtmp       = (alpha1-alpha2*RiFlux)/(1.-RiFlux)
         SH(I,J,K)   = SHtmp
         SM(I,J,K)   = SHtmp*(beta1-beta2*RiFlux)/(beta3-beta4*RiFlux)
C     M. Satoh: Eq. (11.3.53/55)
         tkesquare = MAX( 0. _d 0,
     &        b1*(SH(I,J,K)*GH(I,J) + SM(I,J,K)*GM(I,J)) )
         tke(I,J,K) = sqrt(tkesquare)
CML         if ( k.eq.2 .and. i.ge.1 .and. i.le.sNx .and. j.eq.1)
CML     &   print '(A,3I3,8E13.5)', 'ml-my82', I,J,K, RiNumber(I,J),
CML     &        RiTmp,RiFlux,
CML     &      SH(I,J,K), SM(I,J,K), GH(I,J), GM(I,J), tke(I,J,K)
        ENDDO
       ENDDO
C     end of first k-loop
      ENDDO	

C     re-initilialize GM and GH for abuse, they no longer have 
C     the meaning of shear and negative  buoyancy frequency
      DO J=jMin,jMax
       DO I=iMin,iMax
        GH(I,J) = 0. _d 0
        GM(I,J) = 0. _d 0
       ENDDO
      ENDDO
C     Find boundary length scale from energy weighted mean.
C     This is something like "center of mass" of the vertical
C     tke-distribution
C     begin second k-loop
      DO K = 2, Nr
       DO J=jMin,jMax
        DO I=iMin,iMax
         GM(I,J) = GM(I,J) + tke(I,J,K)*rF(K)
         GH(I,J) = GH(I,J) + tke(I,J,K)      
        ENDDO
       ENDDO
C     end of second k-loop
      END DO
C     compute boundary length scale MYhbl
      DO J=jMin,jMax
       DO I=iMin,iMax
        IF ( GH(I,J) .EQ. 0. _d 0 ) THEN
         MYhbl(I,J,bi,bj) = 0. _d 0
        ELSE
         MYhbl(I,J,bi,bj) = -GM(I,J)/GH(I,J)*MYhblScale
        ENDIF
       ENDDO
      ENDDO 
C     begin third k-loop
      DO K = 1, Nr
C     integrate tke to find integral length scale
       DO J=jMin,jMax
        DO I=iMin,iMax
         tkel = MYhbl(I,J,bi,bj)*tke(I,J,K)
C     M. Satoh: Eq. (11.3.43)
         MYviscAr(I,J,K,bi,bj) = MYhbl(I,J,bi,bj)*tkel*SM(I,J,K)
         MYdiffKr(I,J,K,bi,bj) = MYhbl(I,J,bi,bj)*tkel*SH(I,J,K)
C     Set a minium (= background) value
         MYviscAr(I,J,K,bi,bj) = MAX(MYviscAr(I,J,K,bi,bj),viscAr)
         MYdiffKr(I,J,K,bi,bj) = MAX(MYdiffKr(I,J,K,bi,bj),
     &                               diffKrNrT(k) )
C     Set a maximum and mask land point
         MYviscAr(I,J,K,bi,bj) = MIN(MYviscAr(I,J,K,bi,bj),MYviscMax)
     &        * maskC(I,J,K,bi,bj)
         MYdiffKr(I,J,K,bi,bj) = MIN(MYdiffKr(I,J,K,bi,bj),MYdiffMax)
     &        * maskC(I,J,K,bi,bj)
        ENDDO
       ENDDO 
C     end third k-loop
      ENDDO	

#endif /* ALLOW_MY82 */

      RETURN
      END


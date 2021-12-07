#include "MY82_OPTIONS.h"

CBOP
C !ROUTINE: MY82_CALC

C !INTERFACE: ======================================================
      subroutine MY82_CALC(
     I                bi, bj, sigmaR, myTime, myIter, myThid )

C !DESCRIPTION: \bv
C     *==========================================================*
C     | SUBROUTINE MY82_CALC                                     |
C     | o Compute all MY82 fields defined in MY82.h              |
C     *==========================================================*
C     | This subroutine is based on SPEM code                    |
C     *==========================================================*
      IMPLICIT NONE
C
C--------------------------------------------------------------------

C global parameters updated by pp_calc
C     MYviscAz  :: MY82 eddy viscosity coefficient              (m^2/s)
C     MYdiffKzT :: MY82 diffusion coefficient for temperature   (m^2/s)
C
C \ev

C !USES: ============================================================
#include "SIZE.h"
#include "EEPARAMS.h"
#include "PARAMS.h"
#include "GRID.h"
#include "MY82.h"
#ifdef ALLOW_3D_DIFFKR
# include "DYNVARS.h"
#endif

C !INPUT PARAMETERS: ===================================================
C Routine arguments
C     bi, bj :: Current tile indices
C     sigmaR :: Vertical gradient of iso-neutral density
C     myTime :: Current time in simulation
C     myIter :: Current time-step number
C     myThid :: My Thread Id number
      INTEGER bi, bj
      _RL     sigmaR(1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nr)
      _RL     myTime
      INTEGER myIter
      INTEGER myThid

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
      DO J=1-OLy,sNy+OLy
       DO I=1-OLx,sNx+OLx
        GH(I,J) = 0. _d 0
        GM(I,J) = 0. _d 0
       ENDDO
      ENDDO
      DO K = 1, Nr
       DO J=1-OLy,sNy+OLy
        DO I=1-OLx,sNx+OLx
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
         RiFlux =( btmp - SQRT(btmp*btmp - 4. _d 0 *beta2*beta3*RiTmp) )
     &        /(2. _d 0*beta2)
C     M. Satoh: Eq. (11.3.58)
         SHtmp       = (alpha1-alpha2*RiFlux)/(1. _d 0-RiFlux)
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
         MYviscAr(I,J,K,bi,bj) = MAX(MYviscAr(I,J,K,bi,bj),
     &                               viscArnr(k) )
         MYdiffKr(I,J,K,bi,bj) = MAX(MYdiffKr(I,J,K,bi,bj),
#ifdef ALLOW_3D_DIFFKR
     &                               diffKr(i,j,k,bi,bj) )
#else
     &                               diffKrNrS(k) )
#endif
C     Set a maximum and mask land point
         MYviscAr(I,J,K,bi,bj) = MIN(MYviscAr(I,J,K,bi,bj),MYviscMax)
     &        * maskC(I,J,K,bi,bj)
         MYdiffKr(I,J,K,bi,bj) = MIN(MYdiffKr(I,J,K,bi,bj),MYdiffMax)
     &        * maskC(I,J,K,bi,bj)
        ENDDO
       ENDDO
C     end third k-loop
      ENDDO

#ifdef ALLOW_DIAGNOSTICS
      IF ( useDiagnostics ) THEN
       CALL DIAGNOSTICS_FILL(MYviscAr,'MYVISCAR',0,Nr,1,bi,bj,myThid)
       CALL DIAGNOSTICS_FILL(MYdiffKr,'MYDIFFKR',0,Nr,1,bi,bj,myThid)
       CALL DIAGNOSTICS_FILL(MYhbl   ,'MYHBL   ',0,1 ,1,bi,bj,myThid)
      ENDIF
#endif /* ALLOW_DIAGNOSTICS */

      RETURN
      END

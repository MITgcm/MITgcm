C $Header: /u/gcmpack/MITgcm/pkg/pp81/pp81_calc.F,v 1.3 2005/04/06 18:45:20 jmc Exp $
C $Name:  $

#include "PP81_OPTIONS.h"

CBOP
C !ROUTINE: PP81_CALC

C !INTERFACE: =======================================================
      subroutine PP81_CALC(
     I     bi, bj, myTime, myThid )

C !DESCRIPTION: \bv
C     /==========================================================\
C     | SUBROUTINE PP81_CALC                                     |
C     | o Compute all PP81 fields defined in PP81.h              |
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
#include "PP81.h"
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

#ifdef ALLOW_PP81

C !LOCAL VARIABLES: ====================================================
c Local constants
C     imin, imax, jmin, jmax  - array computation indices
C     RiNumber                - Richardson Number
      INTEGER I, J, K
      INTEGER   iMin ,iMax ,jMin ,jMax
      _RL     denom, PPviscTmp
      _RL     RiNumber(1-OLx:sNx+OLx,1-OLy:sNy+OLy)
CEOP
      iMin = 2-OLx
      iMax = sNx+OLx-1
      jMin = 2-OLy
      jMax = sNy+OLy-1

      DO K = 2, Nr
       CALL PP81_RI_NUMBER(
     I      bi, bj, K, iMin, iMax, jMin, jMax, 
     O      RiNumber, 
     I      myTime, myThid )
       DO J=jMin,jMax
        DO I=iMin,iMax
         IF ( RiNumber(I,J) .LT. RiLimit ) THEN
          denom = 1.0 + PPalpha*RiLimit
          PPviscTmp = PPviscMax
         ELSE
          denom = 1.0 + PPalpha*RiNumber(I,J)
          PPviscTmp = PPnu0/(denom**PPnRi)
         ENDIF
C     assign a minium ( = background ) value
         PPviscAr(I,J,K,bi,bj) = MAX(PPviscTmp,viscAr)
         PPdiffKr(I,J,K,bi,bj) = MAX(PPviscAr(I,J,K,bi,bj)/denom,
     &        diffKrNrT(k))
CML         if ( k.eq.2 .and. i.ge.1 .and. i.le.sNx .and. j.eq.1)
CML     &        print '(A,3I3,5E14.5)', 'ml-pp81', I,J,K, RiLimit,
CML     &        RiNumber(I,J),denom,
CML     &        PPviscAr(I,J,K,bi,bj), PPdiffKr(I,J,K,bi,bj)
        ENDDO
       ENDDO
#ifdef ALLOW_PP81_LOWERBOUND
CRT   This is where the lower limit for subsurface layers 
CRT   (BRIOS special) is set. 
       IF ( (buoyancyRelation .EQ. 'OCEANIC' .AND. K .EQ. 2) .OR.
     &      (buoyancyRelation .EQ. 'OCEANICP' .AND. K .EQ. Nr) ) THEN
        DO J=jMin,jMax
         DO I=iMin,iMax
          PPviscAr(I,J,K,bi,bj) = MAX(PPviscMin,PPviscAr(I,J,K,bi,bj))
          PPdiffKr(I,J,K,bi,bj) = MAX(PPdiffMin,PPdiffKr(I,J,K,bi,bj))
         ENDDO 
        ENDDO
       ENDIF
#endif /* ALLOW_PP81_LOWERBOUND */
C     Mask land points
       DO J=jMin,jMax
        DO I=iMin,iMax
         PPviscAr(I,J,K,bi,bj) = PPviscAr(I,J,K,bi,bj)
     &        * maskC(I,J,K,bi,bj)
         PPdiffKr(I,J,K,bi,bj) = PPdiffKr(I,J,K,bi,bj)
     &        * maskC(I,J,K,bi,bj)
        ENDDO 
       ENDDO
C     end K-loop
      ENDDO

#endif /* ALLOW_PP81 */

      RETURN
      END


#include "WETDRY_OPTIONS.h"
#ifdef ALLOW_GMREDI
# include "GMREDI_OPTIONS.h"
#endif

C---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|
CBOP
C !ROUTINE: WETDRY_GMREDI_TAPER

C !INTERFACE:
      SUBROUTINE WETDRY_GMREDI_TAPER( bi, bj, myThid )

C !DESCRIPTION:
C     Taper the GM/Redi tensor (pkg/gmredi) to zero in shallow columns:
C     isoneutral slopes are meaningless in r* layers a few cm thick, and
C     GM there creates large tracer extrema (and can make the implicit
C     vertical diffusion singular). Column factor
C       fC = MIN(1, MAX(0, (D - wdCritDepth)/(wdGMDepth - wdCritDepth)))
C     applied to Kwx, Kwy, Kwz ; MIN of the two columns at U/V points for
C     Kux, Kvy (and Kuz, Kvz, GM_PsiX/Y when compiled).
C     Called at the end of S/R GMREDI_CALC_TENSOR.

C !USES:
      IMPLICIT NONE
#include "SIZE.h"
#include "EEPARAMS.h"
#include "PARAMS.h"
#include "GRID.h"
#include "DYNVARS.h"
#include "WETDRY.h"
#ifdef ALLOW_GMREDI
# include "GMREDI.h"
#endif

C !INPUT PARAMETERS:
      INTEGER bi, bj
      INTEGER myThid
CEOP

#if (defined ALLOW_WETDRY) && (defined ALLOW_GMREDI)
C !LOCAL VARIABLES:
      INTEGER i, j, k
      _RL fC(1-OLx:sNx+OLx,1-OLy:sNy+OLy)
      _RL gmRange, fU, fV

      IF ( .NOT.useWETDRY ) RETURN
      gmRange = wdGMDepth - wdCritDepth
      IF ( gmRange.LE.zeroRL ) RETURN

      DO j=1-OLy,sNy+OLy
       DO i=1-OLx,sNx+OLx
        fC(i,j) = MIN( oneRL, MAX( zeroRL,
     &     ( etaH(i,j,bi,bj) + Ro_surf(i,j,bi,bj) - R_low(i,j,bi,bj)
     &       - wdCritDepth )/gmRange ) )
       ENDDO
      ENDDO

      DO k=1,Nr
       DO j=1-OLy,sNy+OLy
        DO i=1-OLx,sNx+OLx
         Kwx(i,j,k,bi,bj) = Kwx(i,j,k,bi,bj)*fC(i,j)
         Kwy(i,j,k,bi,bj) = Kwy(i,j,k,bi,bj)*fC(i,j)
         Kwz(i,j,k,bi,bj) = Kwz(i,j,k,bi,bj)*fC(i,j)
        ENDDO
       ENDDO
       DO j=1-OLy+1,sNy+OLy
        DO i=1-OLx+1,sNx+OLx
         fU = MIN( fC(i-1,j), fC(i,j) )
         fV = MIN( fC(i,j-1), fC(i,j) )
         Kux(i,j,k,bi,bj) = Kux(i,j,k,bi,bj)*fU
         Kvy(i,j,k,bi,bj) = Kvy(i,j,k,bi,bj)*fV
#ifdef GM_EXTRA_DIAGONAL
         Kuz(i,j,k,bi,bj) = Kuz(i,j,k,bi,bj)*fU
         Kvz(i,j,k,bi,bj) = Kvz(i,j,k,bi,bj)*fV
#endif
#ifdef GM_BOLUS_ADVEC
         GM_PsiX(i,j,k,bi,bj) = GM_PsiX(i,j,k,bi,bj)*fU
         GM_PsiY(i,j,k,bi,bj) = GM_PsiY(i,j,k,bi,bj)*fV
#endif
        ENDDO
       ENDDO
      ENDDO
#endif /* ALLOW_WETDRY and ALLOW_GMREDI */

      RETURN
      END

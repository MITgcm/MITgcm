C $Header: /u/gcmpack/MITgcm/pkg/ggl90/ggl90_calc_visc.F,v 1.4 2009/01/30 02:23:56 dfer Exp $
C $Name:  $

#include "GGL90_OPTIONS.h"

      SUBROUTINE GGL90_CALC_VISC( 
     I        bi,bj,iMin,iMax,jMin,jMax,K,
     U        KappaRU,KappaRV,
     I        myThid)

CBOP
C     /==========================================================\
C     | SUBROUTINE GGL90_CALC_VISC                               |
C     | o Add contrubution to net viscosity from GGL90 mixing    |
C     \==========================================================/
      IMPLICIT NONE

C     == GLobal variables ==
#include "SIZE.h"
#include "EEPARAMS.h"
#include "PARAMS.h"
#include "DYNVARS.h"
#include "GRID.h"

C     == Routine arguments ==
C     bi, bj, iMin, iMax, jMin, jMax - Range of points for which calculation
C     myThid - Instance number for this innvocation of GGL90_CALC_VISC
C
      INTEGER bi,bj,iMin,iMax,jMin,jMax,K
      _RL KappaRU(1-Olx:sNx+Olx,1-Oly:sNy+Oly,Nr)
      _RL KappaRV(1-Olx:sNx+Olx,1-Oly:sNy+Oly,Nr)
      INTEGER myThid

#ifdef ALLOW_GGL90
#include "GGL90.h"

C     == Local variables ==
C     I, J, K - Loop counters
      INTEGER i,j
      _RL p4, p8, p16

      p4=0.25 _d 0
      p8=0.125 _d 0
      p16=0.0625 _d 0
CEOP

      DO j=jMin,jMax
       DO i=iMin,iMax
#ifdef ALLOW_GGL90_SMOOTH
        KappaRU(i,j,k) = KappaRU(i,j,k) +
     & ((
     &   p4 *(GGL90viscAr(i  ,j  ,k,bi,bj) * mskCor(i  ,j  ,bi,bj)
     &       +GGL90viscAr(i-1,j  ,k,bi,bj) * mskCor(i-1,j  ,bi,bj))
     &  +p8 *(GGL90viscAr(i-1,j-1,k,bi,bj) * mskCor(i-1,j-1,bi,bj)
     &       +GGL90viscAr(i-1,j+1,k,bi,bj) * mskCor(i-1,j+1,bi,bj)
     &       +GGL90viscAr(i  ,j-1,k,bi,bj) * mskCor(i  ,j-1,bi,bj)
     &       +GGL90viscAr(i  ,j+1,k,bi,bj) * mskCor(i  ,j+1,bi,bj))
     &  )
     & /(p4 * 2. _d 0
     &  +p8 *(      maskC(i-1,j-1,k,bi,bj) * mskCor(i-1,j-1,bi,bj)
     &       +      maskC(i-1,j+1,k,bi,bj) * mskCor(i-1,j+1,bi,bj)
     &       +      maskC(i  ,j-1,k,bi,bj) * mskCor(i  ,j-1,bi,bj)
     &       +      maskC(i  ,j+1,k,bi,bj) * mskCor(i  ,j+1,bi,bj))
     &  ) - viscAr )
     &  *maskC(i  ,j,k,bi,bj)*mskCor(i  ,j,bi,bj)
     &  *maskC(i-1,j,k,bi,bj)*mskCor(i-1,j,bi,bj)
#else
        KappaRU(i,j,k) = KappaRU(i,j,k) + _maskW(i,j,k,bi,bj) *
     &                   (.5 _d 0*(GGL90viscAr(i,j,k,bi,bj)
     &                            +GGL90viscAr(i-1,j,k,bi,bj))
     &                    -viscAr)
#endif
       ENDDO
      ENDDO

      DO j=jMin,jMax
       DO i=iMin,iMax
#ifdef ALLOW_GGL90_SMOOTH
        KappaRV(i,j,k) = KappaRV(i,j,k) +
     & ((
     &   p4 *(GGL90viscAr(i  ,j  ,k,bi,bj) * mskCor(i  ,j  ,bi,bj)
     &       +GGL90viscAr(i  ,j-1,k,bi,bj) * mskCor(i  ,j-1,bi,bj))
     &  +p8 *(GGL90viscAr(i-1,j  ,k,bi,bj) * mskCor(i-1,j  ,bi,bj)
     &       +GGL90viscAr(i-1,j-1,k,bi,bj) * mskCor(i-1,j-1,bi,bj)
     &       +GGL90viscAr(i+1,j  ,k,bi,bj) * mskCor(i+1,j  ,bi,bj)
     &       +GGL90viscAr(i+1,j-1,k,bi,bj) * mskCor(i+1,j-1,bi,bj))
     &  )
     & /(p4 * 2. _d 0
     &  +p8 *(      maskC(i-1,j  ,k,bi,bj) * mskCor(i-1,j  ,bi,bj)
     &       +      maskC(i-1,j-1,k,bi,bj) * mskCor(i-1,j-1,bi,bj)
     &       +      maskC(i+1,j  ,k,bi,bj) * mskCor(i+1,j  ,bi,bj)
     &       +      maskC(i+1,j-1,k,bi,bj) * mskCor(i+1,j-1,bi,bj))
     &  ) - viscAr )
     &   *maskC(i,j  ,k,bi,bj)*mskCor(i,j  ,bi,bj)
     &   *maskC(i,j-1,k,bi,bj)*mskCor(i,j-1,bi,bj)
#else
        KappaRV(i,j,k) = KappaRV(i,j,k) + _maskS(i,j,k,bi,bj) *
     &                   (.5 _d 0*(GGL90viscAr(i,j,k,bi,bj)
     &                            +GGL90viscAr(i,j-1,k,bi,bj))
     &                    -viscAr)
#endif
       ENDDO
      ENDDO

#endif /* ALLOW_GGL90 */

      RETURN
      END

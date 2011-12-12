C $Header: /u/gcmpack/MITgcm/pkg/gmredi/gmredi_calc_diff.F,v 1.11 2011/12/12 15:34:08 mlosch Exp $
C $Name:  $

#include "GMREDI_OPTIONS.h"

      SUBROUTINE GMREDI_CALC_DIFF( 
     I        bi,bj,iMin,iMax,jMin,jMax,kArg,kSize,
     U        KappaRx,
     I        tracerIdentity,myThid)

C     /==========================================================\
C     | SUBROUTINE GMREDI_CALC_DIFF                              |
C     | o Add contribution to net diffusivity from GM/Redi       |
C     \==========================================================/
      IMPLICIT NONE

C     == GLobal variables ==
#include "SIZE.h"
#include "EEPARAMS.h"
#include "PARAMS.h"
#include "GRID.h"
#include "GAD.h"
#ifdef ALLOW_LONGSTEP
#include "LONGSTEP.h"
#endif
#ifdef ALLOW_GMREDI
#include "GMREDI.h"
#endif

C     == Routine arguments ==
C     bi, bj,   :: tile indices
C     iMin,iMax :: Range of points for which calculation is done
C     jMin,jMax :: Range of points for which calculation is done
C     kArg      :: = 0 -> do the k-loop here and treat all levels
C                  > 0 -> k-loop is done outside and treat only level k=kArg
C     kSize     :: 3rd Dimension of the vertical diffusivity array KappaRx
C     KappaRx   :: vertical diffusivity array 
C     myThid    :: Instance number for this innvocation of GMREDI_CALC_DIFF
C
      INTEGER bi,bj,iMin,iMax,jMin,jMax,kArg,kSize
      _RL KappaRx(1-Olx:sNx+Olx,1-Oly:sNy+Oly,kSize)
      INTEGER tracerIdentity
      INTEGER myThid

#ifdef ALLOW_GMREDI

C     == Local variables ==
C     i,j,k     :: Loop counters
      INTEGER i,j,k

C--   Add Vertical diffusivity contribution from GM/Redi
      IF ( kArg .EQ. 0 ) THEN
C-    do all levels :
        DO k=1,MIN(Nr,kSize)
         DO j=jMin,jMax
          DO i=iMin,iMax
           IF (tracerIdentity .LT. GAD_TR1) THEN
            KappaRx(i,j,k) = KappaRx(i,j,k)+Kwz(i,j,k,bi,bj)
     &           *maskInC(i,j,bi,bj)
           ELSE
#ifdef ALLOW_LONGSTEP
            KappaRx(i,j,k) = KappaRx(i,j,k)+LS_Kwz(i,j,k,bi,bj)
#else
            KappaRx(i,j,k) = KappaRx(i,j,k)+Kwz(i,j,k,bi,bj)
#endif
     &           *maskInC(i,j,bi,bj)
           ENDIF
          ENDDO
         ENDDO
        ENDDO
      ELSE
C-    do level k=kArg only :
         k = MIN(kArg,kSize)
         DO j=jMin,jMax
          DO i=iMin,iMax
           IF (tracerIdentity .LT. GAD_TR1) THEN
            KappaRx(i,j,k) = KappaRx(i,j,k)+Kwz(i,j,kArg,bi,bj)
     &           *maskInC(i,j,bi,bj)
           ELSE
#ifdef ALLOW_LONGSTEP
            KappaRx(i,j,k) = KappaRx(i,j,k)+LS_Kwz(i,j,kArg,bi,bj)
#else
            KappaRx(i,j,k) = KappaRx(i,j,k)+Kwz(i,j,kArg,bi,bj)
#endif
     &           *maskInC(i,j,bi,bj)
           ENDIF
          ENDDO
         ENDDO
      ENDIF

#endif /* ALLOW_GMREDI */

      RETURN
      END

C $Header: /u/gcmpack/MITgcm/pkg/gmredi/gmredi_ytransport.F,v 1.4 2001/02/04 14:38:49 cnh Exp $
C $Name:  $

#include "GMREDI_OPTIONS.h"

      subroutine GMREDI_YTRANSPORT(
     I     iMin,iMax,jMin,jMax,bi,bj,K,
     I     yA,Tracer,
     U     df,
     I     myThid)
C     /==========================================================\
C     | o SUBROUTINE GMREDI_YTRANSPORT                           |
C     |   Add horizontal y transport terms from GM/Redi          |
C     |   parameterization.                                      |
C     |==========================================================|
C     \==========================================================/
      IMPLICIT NONE

C     == GLobal variables ==
#include "SIZE.h"
#include "EEPARAMS.h"
#include "PARAMS.h"
#include "GRID.h"
#include "GMREDI.h"

C     == Routine arguments ==
C     iMin,iMax,jMin,  - Range of points for which calculation
C     jMax,bi,bj,k       results will be set.
C     xA               - Area of X face
C     Tracer           - 3D Tracer field
C     df               - Diffusive flux component work array.
      INTEGER iMin,iMax,jMin,jMax,bi,bj,k
      _RS yA(1-OLx:sNx+OLx,1-OLy:sNy+OLy)
      _RL Tracer(1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nr,nSx,nSy)
      _RL df    (1-OLx:sNx+OLx,1-OLy:sNy+OLy)
      INTEGER myThid

#ifdef ALLOW_GMREDI

C     == Local variables ==
C     I, J - Loop counters
      INTEGER I, J

      IF (useGMRedi) THEN

C--   Area integrated meridional flux
      DO j=jMin,jMax
       DO i=iMin,iMax
        df(i,j) = df(i,j)
     &    -yA(i,j)
     &    *(GM_background_K
#ifdef GM_VISBECK_VARIABLE_K
     &    +0.5*(VisbeckK(i,j,bi,bj)+VisbeckK(i,j-1,bi,bj))
#endif
     &    )
#ifdef GM_NON_UNITY_DIAGONAL
     &    *Kvy(i,j,k,bi,bj)
#endif
     &    *_recip_dyC(i,j,bi,bj)
     &    *(Tracer(i,j,k,bi,bj)-Tracer(i,j-1,k,bi,bj))
       ENDDO
      ENDDO

      ENDIF
#endif /* ALLOW_GMREDI */

      RETURN
      END

C $Header: /u/gcmpack/MITgcm/pkg/shap_filt/shap_filt_v.F,v 1.4 2001/05/29 14:01:40 adcroft Exp $
C $Name:  $

#include "SHAP_FILT_OPTIONS.h"

      SUBROUTINE SHAP_FILT_V( vVel,bi,bj,K,myCurrentTime,myThid )
C     /==========================================================\
C     | S/R SHAP_FILT_V                                          |
C     | Applies Shapiro filter to V field over one XY slice      |
C     | of one tile at a time.                                   |
C     \==========================================================/
      IMPLICIT NONE

C     == Global variables ===
#include "SIZE.h"
#include "EEPARAMS.h"
#include "PARAMS.h"
#include "GRID.h"
#include "SHAP_FILT.h"

C     == Routine arguments
      _RL vVel(1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nr,nSx,nSy)
      INTEGER myThid
      _RL     myCurrentTime
      INTEGER bi, bj, K

#ifdef ALLOW_SHAP_FILT

C     == Local variables ==
      _RL tmpFldX(1-OLx:sNx+OLx,1-OLy:sNy+OLy,2)
      _RL tmpFldY(1-OLx:sNx+OLx,1-OLy:sNy+OLy,2)
      _RS maskZ(1-OLx:sNx+OLx,1-OLy:sNy+OLy)
      INTEGER I,J,N,N1,N2

C     Create temporary Zeta mask (accounting for thin walls)
      DO J=1-OLy+1,sNy+OLy
       DO I=1-OLx,sNx+OLx
        maskZ(i,j) = _maskW(i,j-1,k,bi,bj)
     &              *_maskW(i, j ,k,bi,bj)
       ENDDO
      ENDDO

      DO J=1-OLy,sNy+OLy
       DO I=1-OLx,sNx+OLx
        tmpFldX(i,j,1) = vVel(i,j,k,bi,bj)
     &                   *_maskS(i,j,k,bi,bj)
       ENDDO
      ENDDO

C     Extract small-scale noise from tmpFldX (delta_ii^n)
      DO N=1,nShapUV
       N1=1+mod(N+1,2)
       N2=1+mod( N ,2)
       DO J=1-OLy+1,sNy+OLy
        DO I=1-OLx+1,sNx+OLx-1
         tmpFldX(i,j,N2) = -0.25*(
     &    (tmpFldX(i+1,j,N1)-tmpFldX( i ,j,N1))*maskZ(i+1,j)
     &   -(tmpFldX( i ,j,N1)-tmpFldX(i-1,j,N1))*maskZ( i ,j)
#ifdef NO_SLIP_SHAP
     &   -2.*(2.-maskZ(i,j)-maskZ(i+1,j))*tmpFldX(i,j,N1)
#endif
     &         )*_maskS(i,j,k,bi,bj)
        ENDDO
       ENDDO
      ENDDO

#ifdef SEQUENTIAL_2D_SHAP
      DO J=1-OLy,sNy+OLy
       DO I=1-OLx,sNx+OLx
        tmpFldX(i,j,N2) = vVel(i,j,k,bi,bj) - tmpFldX(i,j,N2)
        tmpFldY(i,j,1) = tmpFldX(i,j,N2)
       ENDDO
      ENDDO
#else
      DO J=1-OLy,sNy+OLy
       DO I=1-OLx,sNx+OLx
        tmpFldY(i,j,1) = vVel(i,j,k,bi,bj)
     &                   *_maskS(i,j,k,bi,bj)
       ENDDO
      ENDDO
#endif /* SEQUENTIAL_2D_SHAP */

C     Extract small-scale noise from tmpFldY (delta_jj^n)
      DO N=1,nShapUV
       N1=1+mod(N+1,2)
       N2=1+mod( N ,2)
       DO J=1-OLy+1,sNy+OLy-1
        DO I=1-OLx,sNx+OLx
         tmpFldY(i,j,N2) = -0.25*(
     &          tmpFldY(i,j-1,N1) + tmpFldY(i,j+1,N1)
     &             - 2.*tmpFldY(i,j,N1) 
     &            )*_maskS(i,j,k,bi,bj)
        ENDDO
       ENDDO
      ENDDO

C     Subtract small-scale noise from field
#ifdef SEQUENTIAL_2D_SHAP
      DO J=1-OLy,sNy+OLy
       DO I=1-OLx,sNx+OLx
        vVel(i,j,k,bi,bj) = tmpFldX(i,j,N2) - tmpFldY(i,j,N2)
       ENDDO
      ENDDO
#else
      DO J=1-OLy,sNy+OLy
       DO I=1-OLx,sNx+OLx
        vVel(i,j,k,bi,bj) = vVel(i,j,k,bi,bj)
     &    -0.5*( tmpFldX(i,j,N2)+tmpFldY(i,j,N2) )
       ENDDO
      ENDDO
#endif /* SEQUENTIAL_2D_SHAP */

#endif /* ALLOW_SHAP_FILT */

      RETURN
      END

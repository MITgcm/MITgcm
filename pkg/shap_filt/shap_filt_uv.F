C $Header: /u/gcmpack/MITgcm/pkg/shap_filt/Attic/shap_filt_uv.F,v 1.3 2001/02/04 14:38:50 cnh Exp $
C $Name:  $

#include "SHAP_FILT_OPTIONS.h"

      SUBROUTINE SHAP_FILT_UV( 
     U           uFld, vFld,
     I           myTime, myThid )
C     /==========================================================\
C     | S/R SHAP_FILT_UV                                         |
C     | Applies Shapiro filter to tracer field over one XY slice |
C     | of one tile at a time.                                   |
C     \==========================================================/
      IMPLICIT NONE

C     == Global variables ===
#include "SIZE.h"
#include "EEPARAMS.h"
#include "PARAMS.h"
#include "GRID.h"

C     == Routine arguments
      _RL uFld(1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nr,nSx,nSy)
      _RL vFld(1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nr,nSx,nSy)
      _RL     myTime
      INTEGER myThid

#ifdef ALLOW_SHAP_FILT

C     == Local variables ==
      INTEGER bi,bj,K,I,J,N
      _RL tmpFldU(1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nr,nSx,nSy)
      _RL tmpFldV(1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nr,nSx,nSy)
      _RL tmpGrdU(1-OLx:sNx+OLx,1-OLy:sNy+OLy)
      _RL tmpGrdV(1-OLx:sNx+OLx,1-OLy:sNy+OLy)
      _RL maskZj,maskZp
#ifndef SEQUENTIAL_2D_SHAP
      _RL tmpScal
#endif

      IF (nShap.gt.0) THEN

        DO bj=myByLo(myThid),myByHi(myThid)
         DO bi=myBxLo(myThid),myBxHi(myThid)
          DO K=1,Nr
           DO J=1,sNy
            DO I=1,sNx+1
             tmpFldU(i,j,k,bi,bj)=uFld(i,j,k,bi,bj)
     &                *_maskW(i,j,k,bi,bj)
            ENDDO
           ENDDO
           DO J=1,sNy+1
            DO I=1,sNx
             tmpFldV(i,j,k,bi,bj)=vFld(i,j,k,bi,bj)
     &                *_maskS(i,j,k,bi,bj)
            ENDDO
           ENDDO
          ENDDO
         ENDDO
        ENDDO


C      d_xx^n tmpFld 

       DO N=1,nShap

Ccube   _EXCHUV_XYZ_R8( tmpFldU, tmpFldV, .TRUE., myThid )
        _EXCH_XYZ_R8( tmpFldU, myThid )
        _EXCH_XYZ_R8( tmpFldV, myThid )

        DO bj=myByLo(myThid),myByHi(myThid)
         DO bi=myBxLo(myThid),myBxHi(myThid)
          DO K=1,Nr

C          Uxx
           DO J=1,sNy
            DO I=1,sNx+1
             tmpGrdU(i,j) = -0.25*(
     &          tmpFldU(i-1,j,k,bi,bj) + tmpFldU(i+1,j,k,bi,bj)
     &             - 2.*tmpFldU(i,j,k,bi,bj)
     &            )*_maskW(i,j,k,bi,bj)
            ENDDO
           ENDDO

           DO J=1,sNy
            DO I=1,sNx+1
             tmpFldU(i,j,k,bi,bj) = tmpGrdU(i,j)
            ENDDO
           ENDDO

C          Vyy
           DO J=1,sNy+1
            DO I=1,sNx
             tmpGrdV(i,j) = -0.25*(
     &          tmpFldV(i,j-1,k,bi,bj) + tmpFldV(i,j+1,k,bi,bj)
     &             - 2.*tmpFldV(i,j,k,bi,bj)
     &            )*_maskS(i,j,k,bi,bj)
            ENDDO
           ENDDO

           DO J=1,sNy+1
            DO I=1,sNx
             tmpFldV(i,j,k,bi,bj) = tmpGrdV(i,j)
            ENDDO
           ENDDO

          ENDDO
         ENDDO
        ENDDO

       ENDDO

C      F <-  [1-d_xx^n]F
       DO bj=myByLo(myThid),myByHi(myThid)
        DO bi=myBxLo(myThid),myBxHi(myThid)
         DO K=1,Nr
          DO J=1,sNy
           DO I=1,sNx+1
#ifdef      SEQUENTIAL_2D_SHAP
            tmpFldU(i,j,k,bi,bj)=uFld(i,j,k,bi,bj)-tmpFldU(i,j,k,bi,bj)
            uFld(i,j,k,bi,bj)=tmpFldU(i,j,k,bi,bj)
#else
            tmpScal=tmpFldU(i,j,k,bi,bj)
            tmpFldU(i,j,k,bi,bj)=uFld(i,j,k,bi,bj)
     &                *_maskW(i,j,k,bi,bj)
            uFld(i,j,k,bi,bj)=uFld(i,j,k,bi,bj)-0.5*tmpScal
#endif   /* SEQUENTIAL_2D_SHAP */
           ENDDO
          ENDDO
          DO J=1,sNy+1
           DO I=1,sNx
#ifdef      SEQUENTIAL_2D_SHAP
            tmpFldV(i,j,k,bi,bj)=vFld(i,j,k,bi,bj)-tmpFldV(i,j,k,bi,bj)
            vFld(i,j,k,bi,bj)=tmpFldV(i,j,k,bi,bj)
#else
            tmpScal=tmpFldV(i,j,k,bi,bj)
            tmpFldV(i,j,k,bi,bj)=vFld(i,j,k,bi,bj)
     &                *_maskW(i,j,k,bi,bj)
            vFld(i,j,k,bi,bj)=vFld(i,j,k,bi,bj)-0.5*tmpScal
#endif   /* SEQUENTIAL_2D_SHAP */
           ENDDO
          ENDDO
         ENDDO
        ENDDO
       ENDDO


C      d_yy^n tmpFld 

       DO N=1,nShap

Ccube   _EXCHUV_XYZ_R8( tmpFldU, tmpFldV, .TRUE., myThid )
        _EXCH_XYZ_R8( tmpFldU, myThid )
        _EXCH_XYZ_R8( tmpFldV, myThid )

        DO bj=myByLo(myThid),myByHi(myThid)
         DO bi=myBxLo(myThid),myBxHi(myThid)
          DO K=1,Nr

C          Uyy
           DO J=1,sNy
            DO I=1,sNx+1
             maskZj=_maskS(i-1, j ,k,bi,bj)
     &             *_maskS( i , j ,k,bi,bj)
             maskZp=_maskS(i-1,j+1,k,bi,bj)
     &             *_maskS( i ,j+1,k,bi,bj)
             tmpGrdU(i,j) = -0.25*(
     &        (tmpFldU(i,j+1,k,bi,bj)-tmpFldU(i, j ,k,bi,bj))*maskZp
     &       -(tmpFldU(i, j ,k,bi,bj)-tmpFldU(i,j-1,k,bi,bj))*maskZj
#ifdef NO_SLIP_SHAP
     &       -2.*(2.-maskZj-maskZp)*tmpFldU(i,j,k,bi,bj)
#endif
     &             )*_maskW(i,j,k,bi,bj)
            ENDDO
           ENDDO

           DO J=1,sNy
            DO I=1,sNx+1
             tmpFldU(i,j,k,bi,bj) = tmpGrdU(i,j)
            ENDDO
           ENDDO

C          Vxx
           DO J=1,sNy+1
            DO I=1,sNx
             maskZj=_maskW( i ,j-1,k,bi,bj)
     &             *_maskW( i , j ,k,bi,bj)
             maskZp=_maskW(i+1,j-1,k,bi,bj)
     &             *_maskW(i+1, j ,k,bi,bj)
             tmpGrdV(i,j) = -0.25*(
     &        (tmpFldV(i+1,j,k,bi,bj)-tmpFldV( i ,j,k,bi,bj))*maskZp
     &       -(tmpFldV( i ,j,k,bi,bj)-tmpFldV(i-1,j,k,bi,bj))*maskZj
#ifdef NO_SLIP_SHAP
     &       -2.*(2.-maskZj-maskZp)*tmpFldV(i,j,k,bi,bj)
#endif
     &             )*_maskS(i,j,k,bi,bj)
            ENDDO
           ENDDO

           DO J=1,sNy+1
            DO I=1,sNx
             tmpFldV(i,j,k,bi,bj) = tmpGrdV(i,j)
            ENDDO
           ENDDO

          ENDDO
         ENDDO
        ENDDO

       ENDDO

C      F <-  [1-d_yy^n]F
       DO bj=myByLo(myThid),myByHi(myThid)
        DO bi=myBxLo(myThid),myBxHi(myThid)
         DO K=1,Nr
          DO J=1,sNy
           DO I=1,sNx+1
#ifdef      SEQUENTIAL_2D_SHAP
            uFld(i,j,k,bi,bj)=uFld(i,j,k,bi,bj)-tmpFldU(i,j,k,bi,bj)
#else
            tmpScal=tmpFldU(i,j,k,bi,bj)
            uFld(i,j,k,bi,bj)=uFld(i,j,k,bi,bj)-0.5*tmpScal
#endif   /* SEQUENTIAL_2D_SHAP */
           ENDDO
          ENDDO
          DO J=1,sNy+1
           DO I=1,sNx
#ifdef      SEQUENTIAL_2D_SHAP
            vFld(i,j,k,bi,bj)=vFld(i,j,k,bi,bj)-tmpFldV(i,j,k,bi,bj)
#else
            tmpScal=tmpFldV(i,j,k,bi,bj)
            vFld(i,j,k,bi,bj)=vFld(i,j,k,bi,bj)-0.5*tmpScal
#endif   /* SEQUENTIAL_2D_SHAP */
           ENDDO
          ENDDO
         ENDDO
        ENDDO
       ENDDO

Ccube _EXCHUV_XYZ_R8( uFld, vFld, .TRUE., myThid )
      _EXCH_XYZ_R8( tmpFldU, myThid )
      _EXCH_XYZ_R8( tmpFldV, myThid )

      ENDIF
#endif /* ALLOW_SHAP_FILT */

      RETURN
      END

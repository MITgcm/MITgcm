C $Header: /u/gcmpack/MITgcm/pkg/shap_filt/shap_filt_uv_s1.F,v 1.3 2002/03/04 01:32:55 jmc Exp $
C $Name:  $

#include "SHAP_FILT_OPTIONS.h"

CBOP
C     !ROUTINE: SHAP_FILT_UV_S1
C     !INTERFACE:
      SUBROUTINE SHAP_FILT_UV_S1(
     U           uFld, vFld, tmpFldU, tmpFldV,
     I           kSize, myTime, myThid )
C     !DESCRIPTION: \bv
C     *==========================================================*
C     | S/R SHAP_FILT_UV_S1
C     | o Applies Shapiro filter to velocity field (u & v).
C     | o use filtering function "S1" = [1 - d_xx^n - d_yy^n]
C     |     with no grid spacing (computational Filter) ;
C     |     include No-Slip option.
C     *==========================================================*
C     \ev

C     !USES:
      IMPLICIT NONE

C     == Global variables ===
#include "SIZE.h"
#include "EEPARAMS.h"
#include "PARAMS.h"
#include "GRID.h"
#include "SHAP_FILT.h"

C     !INPUT/OUTPUT PARAMETERS:
C     == Routine arguments
C     uFld :: velocity field (U component) on which filter applies
C     vFld :: velocity field (V component) on which filter applies
C     tmpFldU :: working temporary array
C     tmpFldV :: working temporary array
C     kSize :: length of 3rd Dim : either =1 (2D field) or =Nr (3D field)
C     myTime :: Current time in simulation
C     myThid :: Thread number for this instance of SHAP_FILT_UV_S1
      INTEGER kSize
      _RL uFld(1-OLx:sNx+OLx,1-OLy:sNy+OLy,kSize,nSx,nSy)
      _RL vFld(1-OLx:sNx+OLx,1-OLy:sNy+OLy,kSize,nSx,nSy)
      _RL tmpFldU(1-OLx:sNx+OLx,1-OLy:sNy+OLy,kSize,nSx,nSy)
      _RL tmpFldV(1-OLx:sNx+OLx,1-OLy:sNy+OLy,kSize,nSx,nSy)
      _RL     myTime
      INTEGER myThid

#ifdef ALLOW_SHAP_FILT

C     !LOCAL VARIABLES: 
C     == Local variables ==
      INTEGER bi,bj,k,i,j,N
      _RL tmpGrdU(1-OLx:sNx+OLx,1-OLy:sNy+OLy)
      _RL tmpGrdV(1-OLx:sNx+OLx,1-OLy:sNy+OLy)
      _RL maskZj,maskZp
      _RL noSlipFact
      _RL tmpScal
CEOP

      noSlipFact = Shap_noSlip*2. _d 0

      IF (nShapUV.gt.0) THEN

        DO bj=myByLo(myThid),myByHi(myThid)
         DO bi=myBxLo(myThid),myBxHi(myThid)
          DO K=1,kSize
           DO J=1-OLy,sNy+OLy
            DO I=1-OLx,sNx+OLx
             tmpFldU(i,j,k,bi,bj)=uFld(i,j,k,bi,bj)
     &                *_maskW(i,j,k,bi,bj)
             tmpFldV(i,j,k,bi,bj)=vFld(i,j,k,bi,bj)
     &                *_maskS(i,j,k,bi,bj)
            ENDDO
           ENDDO
          ENDDO
         ENDDO
        ENDDO


C      d_xx^n tmpFld 

       DO N=1,nShapUV

        IF (kSize.EQ.Nr) THEN
          CALL EXCH_UV_XYZ_RL(tmpFldU,tmpFldV,.TRUE.,myThid)
        ELSE
          CALL EXCH_UV_XY_RL(tmpFldU,tmpFldV,.TRUE.,myThid)
        ENDIF

        DO bj=myByLo(myThid),myByHi(myThid)
         DO bi=myBxLo(myThid),myBxHi(myThid)
          DO K=1,kSize

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

C      F <-  [1 - d_xx^n *deltaT/tau].F
       DO bj=myByLo(myThid),myByHi(myThid)
        DO bi=myBxLo(myThid),myBxHi(myThid)
         DO K=1,kSize
          DO J=1,sNy
           DO I=1,sNx+1
            tmpScal = uFld(i,j,k,bi,bj)
            uFld(i,j,k,bi,bj) = uFld(i,j,k,bi,bj)
     &         -0.5*tmpFldU(i,j,k,bi,bj)*deltaTmom/Shap_uvtau
            tmpFldU(i,j,k,bi,bj) = tmpScal*_maskW(i,j,k,bi,bj)
           ENDDO
          ENDDO
          DO J=1,sNy+1
           DO I=1,sNx
            tmpScal = vFld(i,j,k,bi,bj)
            vFld(i,j,k,bi,bj) = vFld(i,j,k,bi,bj)
     &         -0.5*tmpFldV(i,j,k,bi,bj)*deltaTmom/Shap_uvtau
            tmpFldV(i,j,k,bi,bj) = tmpScal*_maskS(i,j,k,bi,bj)
           ENDDO
          ENDDO
         ENDDO
        ENDDO
       ENDDO


C      d_yy^n tmpFld 

       DO N=1,nShapUV

        IF (kSize.EQ.Nr) THEN
          CALL EXCH_UV_XYZ_RL(tmpFldU,tmpFldV,.TRUE.,myThid)
        ELSE
          CALL EXCH_UV_XY_RL(tmpFldU,tmpFldV,.TRUE.,myThid)
        ENDIF

        DO bj=myByLo(myThid),myByHi(myThid)
         DO bi=myBxLo(myThid),myBxHi(myThid)
          DO K=1,kSize

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
     &       -noSlipFact*(2.-maskZj-maskZp)*tmpFldU(i,j,k,bi,bj)
     &             )*_maskW(i,j,k,bi,bj)
            ENDDO
           ENDDO

           IF (useCubedSphereExchange) THEN
            DO I=1,sNx+1,sNx
            J=1
             maskZj=maskS(i-1, j ,k,bi,bj)*maskS( i , j ,k,bi,bj)
             maskZp=maskS(i-1,j+1,k,bi,bj)*maskS( i ,j+1,k,bi,bj)
             tmpGrdU(i,j) = -0.25*(
     &        (tmpFldU(i,j+1,k,bi,bj)-tmpFldU(i, j ,k,bi,bj))*maskZp
     &       -(tmpFldU(i, j ,k,bi,bj)-0*tmpFldU(i,j-1,k,bi,bj))*maskZj
     &       -noSlipFact*(2.-maskZj-maskZp)*tmpFldU(i,j,k,bi,bj)
     &             )*_maskW(i,j,k,bi,bj)
            J=sNy
             maskZj=maskS(i-1, j ,k,bi,bj)*maskS( i , j ,k,bi,bj)
             maskZp=maskS(i-1,j+1,k,bi,bj)*maskS( i ,j+1,k,bi,bj)
             tmpGrdU(i,j) = -0.25*(
     &        (0*tmpFldU(i,j+1,k,bi,bj)-tmpFldU(i, j ,k,bi,bj))*maskZp
     &       -(tmpFldU(i, j ,k,bi,bj)-tmpFldU(i,j-1,k,bi,bj))*maskZj
     &       -noSlipFact*(2.-maskZj-maskZp)*tmpFldU(i,j,k,bi,bj)
     &             )*_maskW(i,j,k,bi,bj)
            ENDDO
           ENDIF

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
     &       -noSlipFact*(2.-maskZj-maskZp)*tmpFldV(i,j,k,bi,bj)
     &             )*_maskS(i,j,k,bi,bj)
            ENDDO
           ENDDO

           IF (useCubedSphereExchange) THEN
            DO J=1,sNy+1,sNy
            I=1
             maskZj=maskW( i ,j-1,k,bi,bj)*maskW( i , j ,k,bi,bj)
             maskZp=maskW(i+1,j-1,k,bi,bj)*maskW(i+1, j ,k,bi,bj)
             tmpGrdV(i,j) = -0.25*(
     &        (tmpFldV(i+1,j,k,bi,bj)-tmpFldV( i ,j,k,bi,bj))*maskZp
     &       -(tmpFldV( i ,j,k,bi,bj)-0*tmpFldV(i-1,j,k,bi,bj))*maskZj
     &       -noSlipFact*(2.-maskZj-maskZp)*tmpFldV(i,j,k,bi,bj)
     &             )*_maskS(i,j,k,bi,bj)
            I=sNx
             maskZj=maskW( i ,j-1,k,bi,bj)*maskW( i , j ,k,bi,bj)
             maskZp=maskW(i+1,j-1,k,bi,bj)*maskW(i+1, j ,k,bi,bj)
             tmpGrdV(i,j) = -0.25*(
     &        (0*tmpFldV(i+1,j,k,bi,bj)-tmpFldV( i ,j,k,bi,bj))*maskZp
     &       -(tmpFldV( i ,j,k,bi,bj)-tmpFldV(i-1,j,k,bi,bj))*maskZj
     &       -noSlipFact*(2.-maskZj-maskZp)*tmpFldV(i,j,k,bi,bj)
     &             )*_maskS(i,j,k,bi,bj)
            ENDDO
           ENDIF

           DO J=1,sNy+1
            DO I=1,sNx
             tmpFldV(i,j,k,bi,bj) = tmpGrdV(i,j)
            ENDDO
           ENDDO

          ENDDO
         ENDDO
        ENDDO

       ENDDO

C      F <-  [1 - d_yy^n *deltaT/tau].F
       DO bj=myByLo(myThid),myByHi(myThid)
        DO bi=myBxLo(myThid),myBxHi(myThid)
         DO K=1,kSize
          DO J=1,sNy
           DO I=1,sNx+1
            uFld(i,j,k,bi,bj) = uFld(i,j,k,bi,bj)
     &         -0.5*tmpFldU(i,j,k,bi,bj)*deltaTmom/Shap_uvtau
           ENDDO
          ENDDO
          DO J=1,sNy+1
           DO I=1,sNx
            vFld(i,j,k,bi,bj) = vFld(i,j,k,bi,bj)
     &         -0.5*tmpFldV(i,j,k,bi,bj)*deltaTmom/Shap_uvtau
           ENDDO
          ENDDO
         ENDDO
        ENDDO
       ENDDO

        IF (kSize.EQ.Nr) THEN
          CALL EXCH_UV_XYZ_RL(uFld,vFld,.TRUE.,myThid)
        ELSEIF (kSize.EQ.1) THEN
          CALL EXCH_UV_XY_RL(uFld,vFld,.TRUE.,myThid)
        ELSE
          STOP 'S/R SHAP_FILT_UV_S1: kSize is wrong'
        ENDIF

      ENDIF
#endif /* ALLOW_SHAP_FILT */

      RETURN
      END

C $Header: /u/gcmpack/MITgcm/pkg/shap_filt/Attic/shap_filt_uv_s2g.F,v 1.6 2004/05/24 20:03:49 adcroft Exp $
C $Name:  $

#include "SHAP_FILT_OPTIONS.h"

CBOP
C     !ROUTINE: SHAP_FILT_UV_S2G
C     !INTERFACE:
      SUBROUTINE SHAP_FILT_UV_S2G(
     U           uFld, vFld, tmpFldU, tmpFldV,
     I           kSize, myTime, myThid )
C     !DESCRIPTION: \bv
C     *==========================================================*
C     | S/R SHAP_FILT_UV_S2G
C     | o Applies Shapiro filter to velocity field (u & v).
C     | o use filtering function "S2" = [1 - (d_xx+d_yy)^n]
C     |     with grid spacing (physical space filter) ;
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
C     myThid :: Thread number for this instance of SHAP_FILT_UV_S2G
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
      _RS hFacZ(1-OLx:sNx+OLx,1-OLy:sNy+OLy)
      _RS r_hFacZ(1-OLx:sNx+OLx,1-OLy:sNy+OLy) 
      _RL hDiv(1-OLx:sNx+OLx,1-OLy:sNy+OLy)
      _RL vort3(1-OLx:sNx+OLx,1-OLy:sNy+OLy)
      _RL tmpGrdU(1-OLx:sNx+OLx,1-OLy:sNy+OLy)
      _RL tmpGrdV(1-OLx:sNx+OLx,1-OLy:sNy+OLy)
CEOP

      IF (nShapUV.gt.0 .AND. Shap_uvtau.GT.0.) THEN

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


C    [d_xx+d_yy]^n tmpFld

       DO N=1,nShapUV

        IF (kSize.EQ.Nr) THEN
          CALL EXCH_UV_XYZ_RL(tmpFldU,tmpFldV,.TRUE.,myThid)
        ELSE
          CALL EXCH_UV_XY_RL(tmpFldU,tmpFldV,.TRUE.,myThid)
        ENDIF

        DO bj=myByLo(myThid),myByHi(myThid)
         DO bi=myBxLo(myThid),myBxHi(myThid)
          DO K=1,kSize

C    [d_xx+d_yy] tmpFld
           CALL MOM_CALC_HFACZ(bi,bj,k,hFacZ,r_hFacZ,myThid)
           CALL MOM_CALC_HDIV(bi,bj,k,2,
     I                    tmpFldU(1-OLx,1-OLy,k,bi,bj),
     I                    tmpFldV(1-OLx,1-OLy,k,bi,bj),
     &                    hDiv,myThid)
           CALL MOM_CALC_RELVORT3(bi,bj,k,
     I                    tmpFldU(1-OLx,1-OLy,k,bi,bj),
     I                    tmpFldV(1-OLx,1-OLy,k,bi,bj),
     &                    hFacZ,vort3,myThid)
           CALL MOM_VI_DEL2UV(
     I                    bi,bj,k,hDiv,vort3,hFacZ,
     O                    tmpGrdU,tmpGrdV,
     &                    myThid) 

           IF (Shap_uvLength.EQ.0.) THEN
            DO J=1,sNy+1
             DO I=1,sNx+1
              tmpFldU(i,j,k,bi,bj) = -0.125
     &           *rAw(i,j,bi,bj)*tmpGrdU(i,j)*_maskW(i,j,k,bi,bj)
              tmpFldV(i,j,k,bi,bj) = -0.125
     &           *rAs(i,j,bi,bj)*tmpGrdV(i,j)*_maskS(i,j,k,bi,bj)
             ENDDO
            ENDDO
           ELSE
            DO J=1,sNy+1
             DO I=1,sNx+1
              tmpFldU(i,j,k,bi,bj) = -0.125
     &           *Shap_uvLength*Shap_uvLength
     &           *tmpGrdU(i,j)*_maskW(i,j,k,bi,bj)
              tmpFldV(i,j,k,bi,bj) = -0.125
     &           *Shap_uvLength*Shap_uvLength
     &           *tmpGrdV(i,j)*_maskS(i,j,k,bi,bj)
             ENDDO
            ENDDO
           ENDIF


          ENDDO
         ENDDO
        ENDDO

       ENDDO

C      F <-  [1 - (d_xx+d_yy)^n *deltaT/tau].F
       DO bj=myByLo(myThid),myByHi(myThid)
        DO bi=myBxLo(myThid),myBxHi(myThid)
         DO K=1,kSize
          DO J=1,sNy+1
           DO I=1,sNx+1
            uFld(i,j,k,bi,bj)=uFld(i,j,k,bi,bj)
     &             -tmpFldU(i,j,k,bi,bj)*deltaTmom/Shap_uvtau
            vFld(i,j,k,bi,bj)=vFld(i,j,k,bi,bj)
     &             -tmpFldV(i,j,k,bi,bj)*deltaTmom/Shap_uvtau
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
          STOP 'S/R SHAP_FILT_UV_S2G: kSize is wrong'
        ENDIF

      ENDIF
#endif /* ALLOW_SHAP_FILT */

      RETURN
      END

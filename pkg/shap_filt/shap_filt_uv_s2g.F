C $Header: /u/gcmpack/MITgcm/pkg/shap_filt/Attic/shap_filt_uv_s2g.F,v 1.2 2001/05/29 14:01:40 adcroft Exp $
C $Name:  $

#include "SHAP_FILT_OPTIONS.h"

      SUBROUTINE SHAP_FILT_UV_S2G( 
     U           uFld, vFld,
     I           myTime, myThid )
C     /==========================================================\
C     | S/R SHAP_FILT_UV_S2                                      |
C     | Applies Shapiro filter to U,V field over one XY slice    |
C     | of one tile at a time.                                   |
C     \==========================================================/
      IMPLICIT NONE

C     == Global variables ===
#include "SIZE.h"
#include "EEPARAMS.h"
#include "PARAMS.h"
#include "GRID.h"
#include "SHAP_FILT.h"
#include "SHAP_FILT_UV.h"

C     == Routine arguments
      _RL uFld(1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nr,nSx,nSy)
      _RL vFld(1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nr,nSx,nSy)
      _RL     myTime
      INTEGER myThid

#ifdef ALLOW_SHAP_FILT

C     == Local variables ==
      INTEGER bi,bj,K,I,J,N
      _RS hFacZ(1-OLx:sNx+OLx,1-OLy:sNy+OLy)
      _RS r_hFacZ(1-OLx:sNx+OLx,1-OLy:sNy+OLy) 
      _RL hDiv(1-OLx:sNx+OLx,1-OLy:sNy+OLy)
      _RL vort3(1-OLx:sNx+OLx,1-OLy:sNy+OLy)
      _RL tmpGrdU(1-OLx:sNx+OLx,1-OLy:sNy+OLy)
      _RL tmpGrdV(1-OLx:sNx+OLx,1-OLy:sNy+OLy)

      IF (nShapUV.gt.0 .AND. Shap_uvtau.GT.0.) THEN

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


C    [d_xx+d_yy]^n tmpFld

       DO N=1,nShapUV

        CALL EXCH_UV_XYZ_RL(tmpFldU,tmpFldV,.TRUE.,myThid)

        DO bj=myByLo(myThid),myByHi(myThid)
         DO bi=myBxLo(myThid),myBxHi(myThid)
          DO K=1,Nr

C    [d_xx+d_yy] tmpFld
           CALL MOM_CALC_HFACZ(bi,bj,k,hFacZ,r_hFacZ,myThid)
           CALL MOM_VI_CALC_HDIV(bi,bj,k,
     I                    tmpFldU(1-OLx,1-OLy,k,bi,bj),
     I                    tmpFldV(1-OLx,1-OLy,k,bi,bj),
     &                    hDiv,myThid)
           CALL MOM_VI_CALC_RELVORT3(bi,bj,k,
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

C      F <-  [1 - (d_xx+d_yy)^n *deltat/tau].F
       DO bj=myByLo(myThid),myByHi(myThid)
        DO bi=myBxLo(myThid),myBxHi(myThid)
         DO K=1,Nr
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

      CALL EXCH_UV_XYZ_RL(uFld,vFld,.TRUE.,myThid)

      ENDIF
#endif /* ALLOW_SHAP_FILT */

      RETURN
      END

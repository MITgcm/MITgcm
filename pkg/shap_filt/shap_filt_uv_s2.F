C $Header: /u/gcmpack/MITgcm/pkg/shap_filt/shap_filt_uv_s2.F,v 1.7 2003/08/03 03:05:19 jmc Exp $
C $Name:  $

#include "SHAP_FILT_OPTIONS.h"

CBOP
C     !ROUTINE: SHAP_FILT_UV_S2
C     !INTERFACE:
      SUBROUTINE SHAP_FILT_UV_S2(
     U           uFld, vFld, tmpFldU, tmpFldV,
     I           kSize, myTime, myThid )
C     !DESCRIPTION: \bv
C     *==========================================================*
C     | S/R SHAP_FILT_UV_S2
C     | o Applies Shapiro filter to velocity field (u & v).
C     | o use filtering function "S2" = [1 - (d_xx+d_yy)^n]
C     | o Options for computational filter (no grid spacing)
C     |   or physical space filter (with grid spacing) or both.
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
C     myThid :: Thread number for this instance of SHAP_FILT_UV_S2
      INTEGER kSize
      _RL uFld(1-OLx:sNx+OLx,1-OLy:sNy+OLy,kSize,nSx,nSy)
      _RL vFld(1-OLx:sNx+OLx,1-OLy:sNy+OLy,kSize,nSx,nSy)
      _RL tmpFldU(1-OLx:sNx+OLx,1-OLy:sNy+OLy,kSize,nSx,nSy) 
      _RL tmpFldV(1-OLx:sNx+OLx,1-OLy:sNy+OLy,kSize,nSx,nSy) 
      _RL     myTime
      INTEGER myThid

#ifdef ALLOW_SHAP_FILT

C------
C  Combine computational Filter of Div & Vorticity 
C   and Physical Filter of U,V field 
C   nShapUVPhys = 0  ==> use only computational Filter
C   nShapUVPhys = 1  ==> compute Div & Vort. with  Grid factors, 
C                        Filter Div & Vort. Numerically (power nShapUV-1)
C                        and return filtered U.V in physical space 
C   nShapUVPhys = nShapUV  ==> Filter in Physical space only (power nShapUV)
C------  

C     !LOCAL VARIABLES: 
C     == Local variables ==
      INTEGER bi,bj,k,i,j,N
      _RS hFacZ(1-OLx:sNx+OLx,1-OLy:sNy+OLy)
      _RS r_hFacZ(1-OLx:sNx+OLx,1-OLy:sNy+OLy) 
      _RL hDiv(1-OLx:sNx+OLx,1-OLy:sNy+OLy)
      _RL vort3(1-OLx:sNx+OLx,1-OLy:sNy+OLy)
      _RS maskZ
CEOP

      IF (nShapUV.GT.0 .AND. Shap_uvtau.GT.0.) THEN

        DO bj=myByLo(myThid),myByHi(myThid)
         DO bi=myBxLo(myThid),myBxHi(myThid)
          DO K=1,kSize
           DO J=1-Oly,sNy+Oly
            DO I=1-Olx,sNx+Olx 
             tmpFldU(i,j,k,bi,bj)=uFld(i,j,k,bi,bj)
     &                *_maskW(i,j,k,bi,bj)
             tmpFldV(i,j,k,bi,bj)=vFld(i,j,k,bi,bj)
     &                *_maskS(i,j,k,bi,bj)
            ENDDO
           ENDDO
          ENDDO
         ENDDO
        ENDDO

c---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|

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
         IF (N.LE.nShapUVPhys) THEN
           CALL MOM_CALC_HFACZ(bi,bj,k,hFacZ,r_hFacZ,myThid)
           CALL MOM_VI_CALC_HDIV(bi,bj,k,
     I                    tmpFldU(1-OLx,1-OLy,k,bi,bj),
     I                    tmpFldV(1-OLx,1-OLy,k,bi,bj),
     &                    hDiv,myThid)
           CALL SHAP_FILT_RELVORT3(bi,bj,k,
     I                    tmpFldU(1-OLx,1-OLy,k,bi,bj),
     I                    tmpFldV(1-OLx,1-OLy,k,bi,bj),
     &                    hFacZ,vort3,myThid)
         ELSE
C-  replace Physical calc Div & Vort by computational one :
           DO J=0,sNy+1
            DO I=0,sNx+1
             hDiv(i,j)=tmpFldU(i+1,j,k,bi,bj)-tmpFldU(i,j,k,bi,bj)
     &                +tmpFldV(i,j+1,k,bi,bj)-tmpFldV(i,j,k,bi,bj)
            ENDDO
           ENDDO
           DO J=1,sNy+1
            DO I=1,sNx+1
             vort3(i,j)=(tmpFldV(i,j,k,bi,bj)-tmpFldV(i-1,j,k,bi,bj)
     &                  -tmpFldU(i,j,k,bi,bj)+tmpFldU(i,j-1,k,bi,bj)
     &                  )
             maskZ = (maskW(i,j,k,bi,bj)+maskW(i,j-1,k,bi,bj))
     &              *(maskS(i,j,k,bi,bj)+maskS(i-1,j,k,bi,bj))
             IF (maskZ.LT.1.) vort3(i,j)=0.
            ENDDO
           ENDDO

C     Special stuff for Cubed Sphere
           IF (useCubedSphereExchange) THEN
c---
             I=1
             J=1
             maskZ = maskW(i,j,k,bi,bj)+maskW(i,j-1,k,bi,bj)
     &              +maskS(i,j,k,bi,bj)
            IF (maskZ.GE.2.) THEN
             vort3(I,J)=
     &          tmpFldV(I,J,k,bi,bj)
     &         -tmpFldU(I,J,k,bi,bj)
     &         +tmpFldU(I,J-1,k,bi,bj)
             vort3(I,J)=vort3(I,J)*4.d0/3.d0
            ELSE
             vort3(I,J)=0.
            ENDIF
c---
             I=sNx+1
             J=1
             maskZ = maskW(i,j,k,bi,bj)+maskW(i,j-1,k,bi,bj)
     &                                 +maskS(i-1,j,k,bi,bj)
            IF (maskZ.GE.2.) THEN
             vort3(I,J)=
     &         -tmpFldV(I-1,J,k,bi,bj)
     &         -tmpFldU(I,J,k,bi,bj)
     &         +tmpFldU(I,J-1,k,bi,bj)
             vort3(I,J)=vort3(I,J)*4.d0/3.d0
            ELSE
             vort3(I,J)=0.
            ENDIF
c---
             I=1
             J=sNy+1
             maskZ = maskW(i,j,k,bi,bj)+maskW(i,j-1,k,bi,bj)
     &              +maskS(i,j,k,bi,bj)
            IF (maskZ.GE.2.) THEN
             vort3(I,J)=
     &          tmpFldV(I,J,k,bi,bj)
     &         -tmpFldU(I,J,k,bi,bj)
     &         +tmpFldU(I,J-1,k,bi,bj)
             vort3(I,J)=vort3(I,J)*4.d0/3.d0
            ELSE
             vort3(I,J)=0.
            ENDIF
c---
             I=sNx+1
             J=sNy+1
             maskZ = maskW(i,j,k,bi,bj)+maskW(i,j-1,k,bi,bj)
     &                                 +maskS(i-1,j,k,bi,bj)
            IF (maskZ.GE.2.) THEN
             vort3(I,J)=
     &         -tmpFldV(I-1,J,k,bi,bj)
     &         -tmpFldU(I,J,k,bi,bj)
     &         +tmpFldU(I,J-1,k,bi,bj)
             vort3(I,J)=vort3(I,J)*4.d0/3.d0
            ELSE
             vort3(I,J)=0.
            ENDIF
c---
           ENDIF
         ENDIF

c---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|

         IF (N.GT.nShapUV-nShapUVPhys) THEN
           CALL MOM_CALC_HFACZ(bi,bj,k,hFacZ,r_hFacZ,myThid)
           CALL MOM_VI_DEL2UV(
     I                    bi,bj,k,hDiv,vort3,hFacZ,
     O                    tmpFldU(1-OLx,1-OLy,k,bi,bj),
     O                    tmpFldV(1-OLx,1-OLy,k,bi,bj),
     I                    myThid) 
           IF (Shap_uvLength.EQ.0.) THEN
            DO J=1,sNy+1
             DO I=1,sNx+1
              tmpFldU(i,j,k,bi,bj) = -0.125*tmpFldU(i,j,k,bi,bj)
     &                       *rAw(i,j,bi,bj)*_maskW(i,j,k,bi,bj)
              tmpFldV(i,j,k,bi,bj) = -0.125*tmpFldV(i,j,k,bi,bj)
     &                       *rAs(i,j,bi,bj)*_maskS(i,j,k,bi,bj)
             ENDDO
            ENDDO
           ELSE
            DO J=1,sNy+1
             DO I=1,sNx+1
              tmpFldU(i,j,k,bi,bj) = -0.125*tmpFldU(i,j,k,bi,bj)
     &           *Shap_uvLength*Shap_uvLength*_maskW(i,j,k,bi,bj)
              tmpFldV(i,j,k,bi,bj) = -0.125*tmpFldV(i,j,k,bi,bj)
     &           *Shap_uvLength*Shap_uvLength*_maskS(i,j,k,bi,bj)
             ENDDO
            ENDDO
           ENDIF 
         ELSE
           DO J=1,sNy
            DO I=1,sNx+1
             tmpFldU(i,j,k,bi,bj) = -0.125*
     &                   ( hDiv(i,j)-hDiv(i-1,j)
     &                    -vort3(i,j+1)+vort3(i,j)
     &                   )*maskW(i,j,k,bi,bj)
            ENDDO
           ENDDO
           DO J=1,sNy+1
            DO I=1,sNx
             tmpFldV(i,j,k,bi,bj) = -0.125*
     &                   ( vort3(i+1,j)-vort3(i,j)
     &                    +hDiv(i,j)-hDiv(i,j-1)
     &                   )*maskS(i,j,k,bi,bj)
            ENDDO
           ENDDO

         ENDIF

          ENDDO
         ENDDO
        ENDDO
C end loop N=1,nShapUV
       ENDDO

c---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|

C      F <-  [1 - (d_xx+d_yy)^n *deltaT/tau].F
       DO bj=myByLo(myThid),myByHi(myThid)
        DO bi=myBxLo(myThid),myBxHi(myThid)
         DO K=1,kSize
          DO J=1,sNy+1
           DO I=1,sNx
            uFld(i,j,k,bi,bj)=uFld(i,j,k,bi,bj)
     &             -tmpFldU(i,j,k,bi,bj)*deltaTmom/Shap_uvtau
           ENDDO
          ENDDO
          DO J=1,sNy+1
           DO I=1,sNx
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
          STOP 'S/R SHAP_FILT_UV_S2: kSize is wrong'
        ENDIF

      ENDIF
#endif /* ALLOW_SHAP_FILT */

      RETURN
      END

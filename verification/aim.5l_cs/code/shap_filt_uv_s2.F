C $Header: /u/gcmpack/MITgcm/verification/aim.5l_cs/code/Attic/shap_filt_uv_s2.F,v 1.1 2002/01/09 00:28:55 jmc Exp $
C $Name:  $

#include "SHAP_FILT_OPTIONS.h"

      SUBROUTINE SHAP_FILT_UV_S2( 
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
C------
C  Combine computational Filter of Div & Vorticity 
C   and Physical Filter of U,V field 
C   nShapUVPhys = 0  ==> use only computational Filter
C   nShapUVPhys = 1  ==> compute Div & Vort. with  Grid factors, 
C                        Filter Div & Vort. Numerically (power nShapUV-1)
C                        and return filtered U.V in physical space 
C   nShapUVPhys = nShapUV  ==> Filter in Physical space only (power nShapUV)
C------  

C     == Local variables ==
      INTEGER bi,bj,K,I,J,N
      _RS hFacZ(1-OLx:sNx+OLx,1-OLy:sNy+OLy)
      _RS r_hFacZ(1-OLx:sNx+OLx,1-OLy:sNy+OLy) 
      _RL hDiv(1-OLx:sNx+OLx,1-OLy:sNy+OLy)
      _RL vort3(1-OLx:sNx+OLx,1-OLy:sNy+OLy)
      _RS maskZ

      IF (nShapUV.GT.0 .AND. Shap_uvtau.GT.0.) THEN

        DO bj=myByLo(myThid),myByHi(myThid)
         DO bi=myBxLo(myThid),myBxHi(myThid)
          DO K=1,Nr
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

        CALL EXCH_UV_XYZ_RL(tmpFldU,tmpFldV,.TRUE.,myThid)

        DO bj=myByLo(myThid),myByHi(myThid)
         DO bi=myBxLo(myThid),myBxHi(myThid)
          DO K=1,Nr

C    [d_xx+d_yy] tmpFld
         IF (N.LE.nShapUVPhys) THEN
           CALL MOM_CALC_HFACZ(bi,bj,k,hFacZ,r_hFacZ,myThid)
           CALL MOM_VI_CALC_HDIV(bi,bj,k,
     I                    tmpFldU(1-OLx,1-OLy,k,bi,bj),
     I                    tmpFldV(1-OLx,1-OLy,k,bi,bj),
     &                    hDiv,myThid)
c          CALL MOM_VI_CALC_RELVORT3(bi,bj,k,
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
c    &         -tmpFldV(I-1,J,k,bi,bj)
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
c    &          tmpFldV(I,J,k,bi,bj)
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
c    &         -tmpFldV(I-1,J,k,bi,bj)
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
c    &          tmpFldV(I,J,k,bi,bj)
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

C      F <-  [1 - (d_xx+d_yy)^n *deltat/tau].F
       DO bj=myByLo(myThid),myByHi(myThid)
        DO bi=myBxLo(myThid),myBxHi(myThid)
         DO K=1,Nr
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

      CALL EXCH_UV_XYZ_RL(uFld,vFld,.TRUE.,myThid)

      ENDIF
#endif /* ALLOW_SHAP_FILT */

      RETURN
      END

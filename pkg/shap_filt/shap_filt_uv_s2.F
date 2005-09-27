C $Header: /u/gcmpack/MITgcm/pkg/shap_filt/shap_filt_uv_s2.F,v 1.11 2005/09/27 22:11:06 jmc Exp $
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
#ifdef ALLOW_EXCH2
#include "W2_EXCH2_TOPOLOGY.h"
#include "W2_EXCH2_PARAMS.h"
#endif /* ALLOW_EXCH2 */

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
      LOGICAL northWestCorner, northEastCorner,
     &        southWestCorner, southEastCorner
#ifdef ALLOW_EXCH2
      INTEGER myTile
#endif /* ALLOW_EXCH2 */
CEOP

      IF (nShapUV.GT.0 .AND. Shap_uvtau.GT.0.) THEN

        DO bj=myByLo(myThid),myByHi(myThid)
         DO bi=myBxLo(myThid),myBxHi(myThid)
          DO k=1,kSize
           DO j=1-Oly,sNy+Oly
            DO i=1-Olx,sNx+Olx 
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

C--   [d_xx+d_yy]^n tmpFld

       DO N=1,nShapUV

        IF (kSize.EQ.Nr) THEN
          CALL EXCH_UV_XYZ_RL(tmpFldU,tmpFldV,.TRUE.,myThid)
        ELSE
          CALL EXCH_UV_XY_RL(tmpFldU,tmpFldV,.TRUE.,myThid)
        ENDIF

        DO bj=myByLo(myThid),myByHi(myThid)
         DO bi=myBxLo(myThid),myBxHi(myThid)
          DO k=1,kSize

C-    [d_xx+d_yy] tmpFld
         IF (N.LE.nShapUVPhys) THEN
           CALL MOM_CALC_HFACZ(bi,bj,k,hFacZ,r_hFacZ,myThid)
           CALL MOM_CALC_HDIV(bi,bj,k,2,
     I                    tmpFldU(1-OLx,1-OLy,k,bi,bj),
     I                    tmpFldV(1-OLx,1-OLy,k,bi,bj),
     &                    hDiv,myThid)
#ifdef USE_SHAP_CALC_VORTICITY
           CALL SHAP_FILT_RELVORT3(bi,bj,k,
     I                    tmpFldU(1-OLx,1-OLy,k,bi,bj),
     I                    tmpFldV(1-OLx,1-OLy,k,bi,bj),
     &                    hFacZ,vort3,myThid)
#else
           CALL MOM_CALC_RELVORT3(bi,bj,k,
     I                    tmpFldU(1-OLx,1-OLy,k,bi,bj),
     I                    tmpFldV(1-OLx,1-OLy,k,bi,bj),
     &                    hFacZ,vort3,myThid)
#endif
         ELSE
C-    replace Physical calc Div & Vort by computational one :
           DO j=0,sNy+1
            DO i=0,sNx+1
             hDiv(i,j)=tmpFldU(i+1,j,k,bi,bj)-tmpFldU(i,j,k,bi,bj)
     &                +tmpFldV(i,j+1,k,bi,bj)-tmpFldV(i,j,k,bi,bj)
            ENDDO
           ENDDO
           DO j=1,sNy+1
            DO i=1,sNx+1
             vort3(i,j)=(tmpFldV(i,j,k,bi,bj)-tmpFldV(i-1,j,k,bi,bj)
     &                  -tmpFldU(i,j,k,bi,bj)+tmpFldU(i,j-1,k,bi,bj)
     &                  )
             maskZ = (maskW(i,j,k,bi,bj)+maskW(i,j-1,k,bi,bj))
     &              *(maskS(i,j,k,bi,bj)+maskS(i-1,j,k,bi,bj))
             IF (maskZ.LT.1.) vort3(i,j)=0.
            ENDDO
           ENDDO

C-    Special stuff for Cubed Sphere
           IF (useCubedSphereExchange) THEN
#ifdef ALLOW_EXCH2
            myTile = W2_myTileList(bi)
            southWestCorner = exch2_isWedge(myTile).EQ.1
     &                  .AND. exch2_isSedge(myTile).EQ.1
            southEastCorner = exch2_isEedge(myTile).EQ.1
     &                  .AND. exch2_isSedge(myTile).EQ.1
            northEastCorner = exch2_isEedge(myTile).EQ.1
     &                  .AND. exch2_isNedge(myTile).EQ.1
            northWestCorner = exch2_isWedge(myTile).EQ.1
     &                  .AND. exch2_isNedge(myTile).EQ.1
#else
            southWestCorner = .TRUE.
            southEastCorner = .TRUE.
            northWestCorner = .TRUE.
            northEastCorner = .TRUE.
#endif /* ALLOW_EXCH2 */
C---
            IF ( southWestCorner ) THEN
             i=1
             j=1
             maskZ = maskW(i,j,k,bi,bj)+maskW(i,j-1,k,bi,bj)
     &              +maskS(i,j,k,bi,bj)
             IF (maskZ.GE.2.) THEN
              vort3(i,j)=
     &          tmpFldV(i,j,k,bi,bj)
     &         -tmpFldU(i,j,k,bi,bj)
     &         +tmpFldU(i,j-1,k,bi,bj)
              vort3(i,j)=vort3(i,j)*4. _d 0 / 3. _d 0
             ELSE
              vort3(i,j)=0.
             ENDIF
            ENDIF
C---
            IF ( southEastCorner ) THEN
             i=sNx+1
             j=1
             maskZ = maskW(i,j,k,bi,bj)+maskW(i,j-1,k,bi,bj)
     &                                 +maskS(i-1,j,k,bi,bj)
             IF (maskZ.GE.2.) THEN
              vort3(i,j)=
     &         -tmpFldV(i-1,j,k,bi,bj)
     &         -tmpFldU(i,j,k,bi,bj)
     &         +tmpFldU(i,j-1,k,bi,bj)
              vort3(i,j)=vort3(i,j)*4. _d 0 / 3. _d 0
             ELSE
              vort3(i,j)=0.
             ENDIF
            ENDIF
C---
            IF ( northWestCorner ) THEN
             i=1
             j=sNy+1
             maskZ = maskW(i,j,k,bi,bj)+maskW(i,j-1,k,bi,bj)
     &              +maskS(i,j,k,bi,bj)
             IF (maskZ.GE.2.) THEN
              vort3(i,j)=
     &          tmpFldV(i,j,k,bi,bj)
     &         -tmpFldU(i,j,k,bi,bj)
     &         +tmpFldU(i,j-1,k,bi,bj)
              vort3(i,j)=vort3(i,j)*4. _d 0 / 3. _d 0
             ELSE
              vort3(i,j)=0.
             ENDIF
            ENDIF
C---
            IF ( northEastCorner ) THEN
             i=sNx+1
             j=sNy+1
             maskZ = maskW(i,j,k,bi,bj)+maskW(i,j-1,k,bi,bj)
     &                                 +maskS(i-1,j,k,bi,bj)
             IF (maskZ.GE.2.) THEN
              vort3(i,j)=
     &         -tmpFldV(i-1,j,k,bi,bj)
     &         -tmpFldU(i,j,k,bi,bj)
     &         +tmpFldU(i,j-1,k,bi,bj)
              vort3(i,j)=vort3(i,j)*4. _d 0 / 3. _d 0
             ELSE
              vort3(i,j)=0.
             ENDIF
            ENDIF
C---  end if useCubedSphereExchange:
           ENDIF
         ENDIF

c---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|

         IF (N.GT.nShapUV-nShapUVPhys) THEN
           IF (Shap_uvLength.LT.0.) THEN
             DO j=0,sNy+1
              DO i=0,sNx+1
                hDiv(i,j) = hDiv(i,j) * rA(i,j,bi,bj)
                vort3(i,j)= vort3(i,j)*rAz(i,j,bi,bj)
              ENDDO
             ENDDO
           ENDIF
           CALL MOM_CALC_HFACZ(bi,bj,k,hFacZ,r_hFacZ,myThid)
           CALL MOM_VI_DEL2UV(
     I                    bi,bj,k,hDiv,vort3,hFacZ,
     O                    tmpFldU(1-OLx,1-OLy,k,bi,bj),
     O                    tmpFldV(1-OLx,1-OLy,k,bi,bj),
     I                    myThid) 
           IF (Shap_uvLength.LT.0.) THEN
            DO j=1,sNy+1
             DO i=1,sNx+1
              tmpFldU(i,j,k,bi,bj) = -0.125 _d 0*tmpFldU(i,j,k,bi,bj)
     &                                          *_maskW(i,j,k,bi,bj)
              tmpFldV(i,j,k,bi,bj) = -0.125 _d 0*tmpFldV(i,j,k,bi,bj)
     &                                          *_maskS(i,j,k,bi,bj)
             ENDDO
            ENDDO
           ELSEIF (Shap_uvLength.EQ.0.) THEN
            DO j=1,sNy+1
             DO i=1,sNx+1
              tmpFldU(i,j,k,bi,bj) = -0.125 _d 0*tmpFldU(i,j,k,bi,bj)
     &                            *rAw(i,j,bi,bj)*_maskW(i,j,k,bi,bj)
              tmpFldV(i,j,k,bi,bj) = -0.125 _d 0*tmpFldV(i,j,k,bi,bj)
     &                            *rAs(i,j,bi,bj)*_maskS(i,j,k,bi,bj)
             ENDDO
            ENDDO
           ELSE
            DO j=1,sNy+1
             DO i=1,sNx+1
              tmpFldU(i,j,k,bi,bj) = -0.125 _d 0*tmpFldU(i,j,k,bi,bj)
     &               *Shap_uvLength*Shap_uvLength*_maskW(i,j,k,bi,bj)
              tmpFldV(i,j,k,bi,bj) = -0.125 _d 0*tmpFldV(i,j,k,bi,bj)
     &               *Shap_uvLength*Shap_uvLength*_maskS(i,j,k,bi,bj)
             ENDDO
            ENDDO
           ENDIF 
         ELSE
           DO j=1,sNy
            DO i=1,sNx+1
             tmpFldU(i,j,k,bi,bj) = -0.125 _d 0*
     &                   ( hDiv(i,j)-hDiv(i-1,j)
     &                    -vort3(i,j+1)+vort3(i,j)
     &                   )*maskW(i,j,k,bi,bj)
            ENDDO
           ENDDO
           DO j=1,sNy+1
            DO i=1,sNx
             tmpFldV(i,j,k,bi,bj) = -0.125 _d 0*
     &                   ( vort3(i+1,j)-vort3(i,j)
     &                    +hDiv(i,j)-hDiv(i,j-1)
     &                   )*maskS(i,j,k,bi,bj)
            ENDDO
           ENDDO

         ENDIF

C end loops  k / bi / bj
          ENDDO
         ENDDO
        ENDDO
C end loop N=1,nShapUV
       ENDDO

c---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|

C      F <-  [1 - (d_xx+d_yy)^n *deltaT/tau].F
       DO bj=myByLo(myThid),myByHi(myThid)
        DO bi=myBxLo(myThid),myBxHi(myThid)
         DO k=1,kSize
          DO j=1,sNy+1
           DO i=1,sNx
            uFld(i,j,k,bi,bj)=uFld(i,j,k,bi,bj)
     &                  -tmpFldU(i,j,k,bi,bj)*deltaTmom/Shap_uvtau
            tmpFldU(i,j,k,bi,bj)= -tmpFldU(i,j,k,bi,bj)/Shap_uvtau
           ENDDO
          ENDDO
          DO j=1,sNy+1
           DO i=1,sNx
            vFld(i,j,k,bi,bj)=vFld(i,j,k,bi,bj)
     &                  -tmpFldV(i,j,k,bi,bj)*deltaTmom/Shap_uvtau
            tmpFldV(i,j,k,bi,bj)= -tmpFldV(i,j,k,bi,bj)/Shap_uvtau
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

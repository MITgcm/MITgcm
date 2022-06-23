#include "SHAP_FILT_OPTIONS.h"
#ifdef ALLOW_AUTODIFF
# include "AUTODIFF_OPTIONS.h"
#endif

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
c#ifdef ALLOW_EXCH2
c#include "W2_EXCH2_SIZE.h"
c#include "W2_EXCH2_TOPOLOGY.h"
c#endif /* ALLOW_EXCH2 */

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
      INTEGER bi,bj,k,i,j,n
      _RS hFacZ(1-OLx:sNx+OLx,1-OLy:sNy+OLy)
      _RS r_hFacZ(1-OLx:sNx+OLx,1-OLy:sNy+OLy)
      _RL hDiv(1-OLx:sNx+OLx,1-OLy:sNy+OLy)
      _RL vort3(1-OLx:sNx+OLx,1-OLy:sNy+OLy)
#ifdef ALLOW_AUTODIFF_TAMC
      INTEGER kkey, act1, max1, act2, max2, act3, max3
#endif
CEOP

#ifdef ALLOW_AUTODIFF_TAMC
CADJ INIT loctape_shapfilt_bibj_k = COMMON, nSx*nSy*Nr
#endif
      IF (nShapUV.GT.0 .AND. Shap_uvtau.GT.0.) THEN

        IF (useCubedSphereExchange) THEN
C-      need to initialise hDiv for MOM_VI_DEL2UV(call FILL_CS_CORNER_TR_RL)
          DO j=1-OLy,sNy+OLy
           DO i=1-OLx,sNx+OLx
            hDiv(i,j) = 0. _d 0
           ENDDO
          ENDDO
        ENDIF

        DO bj=myByLo(myThid),myByHi(myThid)
         DO bi=myBxLo(myThid),myBxHi(myThid)
          DO k=1,kSize
           DO j=1-OLy,sNy+OLy
            DO i=1-OLx,sNx+OLx
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

       DO n=1,nShapUV

        IF ( MOD(n,2).EQ.1 .OR. Shap_alwaysExchUV ) THEN
          CALL EXCH_UV_3D_RL( tmpFldU,tmpFldV, .TRUE., kSize, myThid )
        ENDIF

        DO bj=myByLo(myThid),myByHi(myThid)
         DO bi=myBxLo(myThid),myBxHi(myThid)
          DO k=1,kSize

#ifdef ALLOW_AUTODIFF_TAMC
C     This store uses a wrongly-sized tape and stores the same field
C     nShapUV times, but since this field does not change here, we
C     can do it like this and suppress the recomputation warning
           act1 = bi - myBxLo(myThid)
           max1 = myBxHi(myThid) - myBxLo(myThid) + 1
           act2 = bj - myByLo(myThid)
           max2 = myByHi(myThid) - myByLo(myThid) + 1
           act3 = myThid - 1
           max3 = nTx*nTy
           kkey= k + (act1 + act2*max1 + act3*max1*max2 -1 ) * Nr
CADJ STORE hFacZ = loctape_shapfilt_bibj_k, key = kkey
#endif
           IF ( n.LE.nShapUVPhys .OR.
     &          n.GT.nShapUV-nShapUVPhys )
     &     CALL MOM_CALC_HFACZ(bi,bj,k,hFacZ,r_hFacZ,myThid)

C-    [d_xx+d_yy] tmpFld
         IF (n.LE.nShapUVPhys) THEN
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
           DO j=1-OLy,sNy+OLy-1
            DO i=1-OLx,sNx+OLx-1
             hDiv(i,j)=(tmpFldU(i+1,j,k,bi,bj)-tmpFldU(i,j,k,bi,bj))
     &                +(tmpFldV(i,j+1,k,bi,bj)-tmpFldV(i,j,k,bi,bj))
            ENDDO
           ENDDO
           CALL SHAP_FILT_COMPUTVORT(
     I                    tmpFldU(1-OLx,1-OLy,k,bi,bj),
     I                    tmpFldV(1-OLx,1-OLy,k,bi,bj),
     O                    vort3,
     I                    k,bi,bj,myThid)
         ENDIF

c---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|

         IF (n.GT.nShapUV-nShapUVPhys) THEN
           IF (Shap_uvLength.LT.0.) THEN
             DO j=1-OLy,sNy+OLy-1
              DO i=1-OLx,sNx+OLx-1
                hDiv(i,j) = hDiv(i,j) * rA(i,j,bi,bj)
              ENDDO
             ENDDO
             DO j=2-OLy,sNy+OLy
              DO i=2-OLx,sNx+OLx
                vort3(i,j)= vort3(i,j)*rAz(i,j,bi,bj)
              ENDDO
             ENDDO
           ENDIF
           CALL MOM_VI_DEL2UV(
     I                    bi,bj,k,hDiv,vort3,hFacZ,
     O                    tmpFldU(1-OLx,1-OLy,k,bi,bj),
     O                    tmpFldV(1-OLx,1-OLy,k,bi,bj),
     I                    myThid)
           IF (Shap_uvLength.LT.0.) THEN
            DO j=2-OLy,sNy+OLy-1
             DO i=2-OLx,sNx+OLx-1
              tmpFldU(i,j,k,bi,bj) = -0.125 _d 0*tmpFldU(i,j,k,bi,bj)
     &                                          *_maskW(i,j,k,bi,bj)
              tmpFldV(i,j,k,bi,bj) = -0.125 _d 0*tmpFldV(i,j,k,bi,bj)
     &                                          *_maskS(i,j,k,bi,bj)
             ENDDO
            ENDDO
           ELSEIF (Shap_uvLength.EQ.0.) THEN
            DO j=2-OLy,sNy+OLy-1
             DO i=2-OLx,sNx+OLx-1
              tmpFldU(i,j,k,bi,bj) = -0.125 _d 0*tmpFldU(i,j,k,bi,bj)
     &                            *rAw(i,j,bi,bj)*_maskW(i,j,k,bi,bj)
              tmpFldV(i,j,k,bi,bj) = -0.125 _d 0*tmpFldV(i,j,k,bi,bj)
     &                            *rAs(i,j,bi,bj)*_maskS(i,j,k,bi,bj)
             ENDDO
            ENDDO
           ELSE
            DO j=2-OLy,sNy+OLy-1
             DO i=2-OLx,sNx+OLx-1
              tmpFldU(i,j,k,bi,bj) = -0.125 _d 0*tmpFldU(i,j,k,bi,bj)
     &               *Shap_uvLength*Shap_uvLength*_maskW(i,j,k,bi,bj)
              tmpFldV(i,j,k,bi,bj) = -0.125 _d 0*tmpFldV(i,j,k,bi,bj)
     &               *Shap_uvLength*Shap_uvLength*_maskS(i,j,k,bi,bj)
             ENDDO
            ENDDO
           ENDIF
         ELSE
C-       replace derivatives in physical space of Div & Vort by computational ones:
#ifndef ALLOW_AUTODIFF
           IF ( .NOT.Shap_alwaysExchUV
     &          .AND. useCubedSphereExchange ) THEN
C          to compute d/dx(hDiv), fill corners with appropriate values:
             CALL FILL_CS_CORNER_TR_RL( 1, .FALSE.,
     &                                  hDiv, bi,bj, myThid )
           ENDIF
#endif
           DO j=2-OLy,sNy+OLy-1
            DO i=2-OLx,sNx+OLx-1
             tmpFldU(i,j,k,bi,bj) = -0.125 _d 0*
     &                   ( (hDiv(i,j) - hDiv(i-1,j))
     &                    -(vort3(i,j+1)-vort3(i,j))
     &                   )*maskW(i,j,k,bi,bj)
            ENDDO
           ENDDO
#ifndef ALLOW_AUTODIFF
           IF ( .NOT.Shap_alwaysExchUV
     &          .AND. useCubedSphereExchange ) THEN
C          to compute d/dy(hDiv), fill corners with appropriate values:
             CALL FILL_CS_CORNER_TR_RL( 2, .FALSE.,
     &                                  hDiv, bi,bj, myThid )
           ENDIF
#endif
           DO j=2-OLy,sNy+OLy-1
            DO i=2-OLx,sNx+OLx-1
             tmpFldV(i,j,k,bi,bj) = -0.125 _d 0*
     &                   ( (hDiv(i,j) - hDiv(i,j-1))
     &                    +(vort3(i+1,j)-vort3(i,j))
     &                   )*maskS(i,j,k,bi,bj)
            ENDDO
           ENDDO

         ENDIF

C end loops  k / bi / bj
          ENDDO
         ENDDO
        ENDDO
C end loop n=1,nShapUV
       ENDDO

c---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|

C      F <-  [1 - (d_xx+d_yy)^n *deltaT/tau].F
       DO bj=myByLo(myThid),myByHi(myThid)
        DO bi=myBxLo(myThid),myBxHi(myThid)
         DO k=1,kSize
          DO j=1,sNy
           DO i=1,sNx+1
            uFld(i,j,k,bi,bj)=uFld(i,j,k,bi,bj)
     &                  -tmpFldU(i,j,k,bi,bj)*deltaTMom/Shap_uvtau
            tmpFldU(i,j,k,bi,bj)= -tmpFldU(i,j,k,bi,bj)/Shap_uvtau
           ENDDO
          ENDDO
          DO j=1,sNy+1
           DO i=1,sNx
            vFld(i,j,k,bi,bj)=vFld(i,j,k,bi,bj)
     &                  -tmpFldV(i,j,k,bi,bj)*deltaTMom/Shap_uvtau
            tmpFldV(i,j,k,bi,bj)= -tmpFldV(i,j,k,bi,bj)/Shap_uvtau
           ENDDO
          ENDDO
         ENDDO
        ENDDO
       ENDDO

       IF ( ( OLx.LE.2 .OR. OLy.LE.2 ) .AND.
     &       MOD(nShapUV,2).EQ.0 .AND. .NOT.Shap_alwaysExchUV )
     &  CALL EXCH_UV_3D_RL( uFld, vFld, .TRUE., kSize, myThid )

      ENDIF
#endif /* ALLOW_SHAP_FILT */

      RETURN
      END

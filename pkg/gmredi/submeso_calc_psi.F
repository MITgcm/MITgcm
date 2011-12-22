C $Header: /u/gcmpack/MITgcm/pkg/gmredi/submeso_calc_psi.F,v 1.2 2011/12/22 19:06:25 jmc Exp $
C $Name:  $

#include "GMREDI_OPTIONS.h"

CBOP
C     !ROUTINE: SUBMESO_CALC_PSI
C     !INTERFACE:
      SUBROUTINE SUBMESO_CALC_PSI(
     I             bi, bj, iMin, iMax, jMin, jMax,
     I             sigmaX, sigmaY, sigmaR,
     I             locMixLayer,
     I             myIter, myThid )

C     !DESCRIPTION: \bv
C     *==========================================================*
C     | SUBROUTINE SUBMESO_CALC_PSI
C     | o Calculate stream-functions for Sub-Meso bolus velocity
C     *==========================================================*
C     | Ref: B. Fox-Kemper etal, Oce.Model., 39:61-78, 2011
C     |      B. Fox-Kemper etal, JPO, 38(6):1145-1165, 2008
C     *==========================================================*
C     \ev

C     !USES:
      IMPLICIT NONE

C     == Global variables ==
#include "SIZE.h"
#include "GRID.h"
#include "EEPARAMS.h"
#include "PARAMS.h"
#include "GMREDI.h"

C     !INPUT/OUTPUT PARAMETERS:
C     == Routine arguments ==
      _RL sigmaX(1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nr)
      _RL sigmaY(1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nr)
      _RL sigmaR(1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nr)
      _RL locMixLayer(1-OLx:sNx+OLx,1-OLy:sNy+OLy)
      INTEGER bi,bj,iMin,iMax,jMin,jMax
      INTEGER myIter
      INTEGER myThid
CEOP

#ifndef GM_EXCLUDE_SUBMESO

C     !LOCAL VARIABLES:
C     == Local variables ==
      INTEGER i,j,k
      _RL mixLayerU   (1-OLx:sNx+OLx,1-OLy:sNy+OLy)
      _RL mixLayerV   (1-OLx:sNx+OLx,1-OLy:sNy+OLy)
      _RL dBuoyX_Hu   (1-OLx:sNx+OLx,1-OLy:sNy+OLy)
      _RL dBuoyY_Hv   (1-OLx:sNx+OLx,1-OLy:sNy+OLy)
      _RL NHmixLay    (1-OLx:sNx+OLx,1-OLy:sNy+OLy)
      _RL MsquareH    (1-OLx:sNx+OLx,1-OLy:sNy+OLy)
      _RL lengthScaleF(1-OLx:sNx+OLx,1-OLy:sNy+OLy)
      _RL fcorLoc     (1-OLx:sNx+OLx,1-OLy:sNy+OLy)
      _RL PsiLoc      (1-OLx:sNx+OLx,1-OLy:sNy+OLy)
      _RL dzLoc
#ifdef GM_BOLUS_ADVEC
      _RL z2H, mu_z
#endif
      _RL five_ov21
      PARAMETER( five_ov21 = 5. _d 0 / 21. _d 0 )

C--   parameter to move to GMREDI.h
c     _RL subMeso_invTau, subMeso_LfMin, subMeso_Ceff
c     _RS subMeso_Lmax

c     subMeso_invTau = 1.6 _d -6  ! ~ 1/(7.2 days)
c     subMeso_LfMin  = 1000. _d 0
c     subMeso_Ceff   = 0.07 _d 0
c     subMeso_Lmax   = 111. _d 3

C-    Initialization : <= done in S/R gmredi_init

c     IF ( GM_useSubMeso ) THEN
       DO j=1-OLy,sNy+OLy
        DO i=1-OLx+1,sNx+OLx
         mixLayerU(i,j) = op5*( locMixLayer(i-1,j)+locMixLayer(i,j) )
         mixLayerU(i,j) = MIN( mixLayerU(i,j), -rLowW(i,j,bi,bj) )
        ENDDO
       ENDDO
       DO j=1-OLy+1,sNy+OLy
        DO i=1-OLx,sNx+OLx
         mixLayerV(i,j)=op5*( locMixLayer(i,j-1)+locMixLayer(i,j) )
         mixLayerV(i,j) = MIN( mixLayerV(i,j), -rLowS(i,j,bi,bj) )
        ENDDO
       ENDDO

C--    Integrate buoyancy gradient over the Mixed-Layer
       DO j=1-OLy,sNy+OLy
        DO i=1-OLx,sNx+OLx
          dBuoyX_Hu(i,j)= 0.
          dBuoyY_Hv(i,j)= 0.
          NHmixLay(i,j) = 0.
          fcorLoc(i,j) = SQRT( fCori(i,j,bi,bj)*fCori(i,j,bi,bj)
     &                       + subMeso_invTau*subMeso_invTau )
        ENDDO
       ENDDO
       DO k=1,Nr
        DO j=1-OLy,sNy+OLy
         DO i=1-OLx+1,sNx+OLx
          dzLoc = MAX( 0. _d 0, MIN( drF(k), mixLayerU(i,j)+rF(k) ) )
          dBuoyX_Hu(i,j) = dBuoyX_Hu(i,j) + sigmaX(i,j,k)*dzLoc
         ENDDO
        ENDDO
        DO j=1-OLy+1,sNy+OLy
         DO i=1-OLx,sNx+OLx
          dzLoc = MAX( 0. _d 0, MIN( drF(k), mixLayerV(i,j)+rF(k) ) )
          dBuoyY_Hv(i,j) = dBuoyY_Hv(i,j) + sigmaY(i,j,k)*dzLoc
         ENDDO
        ENDDO
       ENDDO
       DO k=2,Nr
        DO j=1-OLy,sNy+OLy
         DO i=1-OLx,sNx+OLx
          dzLoc = 0.
          IF ( locMixLayer(i,j)+rC(k-1).GE.0. ) dzLoc = drC(k)
          NHmixLay(i,j) = NHmixLay(i,j)
     &                  + dzLoc*MAX( -sigmaR(i,j,k), 0. _d 0 )
         ENDDO
        ENDDO
       ENDDO
       DO j=1-OLy,sNy+OLy
        DO i=1-OLx,sNx+OLx
          dBuoyX_Hu(i,j)= -dBuoyX_Hu(i,j)*gravity*recip_rhoConst
          dBuoyY_Hv(i,j)= -dBuoyY_Hv(i,j)*gravity*recip_rhoConst
          NHmixLay(i,j) = SQRT( NHmixLay(i,j)*gravity*recip_rhoConst
     &                         *locMixLayer(i,j) )
        ENDDO
       ENDDO
       DO j=2-OLy,sNy+OLy-1
        DO i=2-OLx,sNx+OLx-1
          MsquareH(i,j)= SQRT( op25*(
     &            (dBuoyX_Hu(i,j) + dBuoyX_Hu(i+1,j))**2
     &          + (dBuoyY_Hv(i,j) + dBuoyY_Hv(i,j+1))**2
     &                     ) )
        ENDDO
       ENDDO
C-     Compute Lf at grid-cell center
       DO j=2-OLy,sNy+OLy-1
        DO i=2-OLx,sNx+OLx-1
          lengthScaleF(i,j)= MAX(
     &        MsquareH(i,j)/(fcorLoc(i,j)*fcorLoc(i,j)) ,
     &        NHmixLay(i,j)/fcorLoc(i,j) ,
     &        subMeso_LfMin )
        ENDDO
       ENDDO

C      Mix-Layer Eddies contribution to Bolus Transport in X dir.
       DO j=2-OLy,sNy+OLy-1
        DO i=3-OLx,sNx+OLx-1
         PsiLoc(i,j) = -subMeso_Ceff*dBuoyX_Hu(i,j)
     &                 *mixLayerU(i,j)
     &                 *MIN( dxC(i,j,bi,bj), subMeso_Lmax )
     &                 *2. _d 0/(lengthScaleF(i-1,j)+lengthScaleF(i,j))
     &                 *2. _d 0/(fcorLoc(i-1,j)+fcorLoc(i,j))
        ENDDO
       ENDDO
#ifdef GM_BOLUS_ADVEC
       DO k=2,Nr
        DO j=2-OLy,sNy+OLy-1
         DO i=3-OLx,sNx+OLx-1
          IF ( mixLayerU(i,j).GT.0. _d 0 ) THEN
            z2H = 2. _d 0*rF(k)/mixLayerU(i,j)
          ELSE
            z2H = 0. _d 0
          ENDIF
          mu_z = ( z2H + 1. _d 0 )*( z2H + 1. _d 0 )
          mu_z = ( 1. _d 0 - mu_z )*(1. _d 0 + mu_z*five_ov21 )
          mu_z = MAX( 0. _d 0, mu_z )
          GM_PsiX(i,j,k,bi,bj) = GM_PsiX(i,j,k,bi,bj)
     &                         + mu_z*PsiLoc(i,j)
         ENDDO
        ENDDO
       ENDDO
#endif /* GM_BOLUS_ADVEC */
#ifdef ALLOW_DIAGNOSTICS
       IF ( useDiagnostics ) THEN
         CALL DIAGNOSTICS_FILL( lengthScaleF, 'SubMesLf',
     &                           0, 1, 2, bi, bj, myThid )
         CALL DIAGNOSTICS_FILL( PsiLoc, 'SubMpsiX',
     &                          0, 1, 2, bi, bj, myThid )
       ENDIF
#endif
       IF ( debugLevel.GE.debLevD ) THEN
         CALL WRITE_LOCAL_RL( 'subMeso_Lf','I10',1,lengthScaleF,
     &                         bi,bj,1,myIter,myThid )
         CALL WRITE_LOCAL_RL( 'subMeso_psiX','I10',1,PsiLoc,
     &                         bi,bj,1,myIter,myThid )
       ENDIF

C      Mix-Layer Eddies contribution to Bolus Transport in Y dir.
       DO j=3-OLy,sNy+OLy-1
        DO i=2-OLx,sNx+OLx-1
         PsiLoc(i,j) = -subMeso_Ceff*dBuoyY_Hv(i,j)
     &                 *mixLayerV(i,j)
     &                 *MIN( dyC(i,j,bi,bj), subMeso_Lmax )
     &                 *2. _d 0/(lengthScaleF(i,j-1)+lengthScaleF(i,j))
     &                 *2. _d 0/(fcorLoc(i,j-1)+fcorLoc(i,j))
        ENDDO
       ENDDO
#ifdef GM_BOLUS_ADVEC
       DO k=2,Nr
        DO j=3-OLy,sNy+OLy-1
         DO i=2-OLx,sNx+OLx-1
          IF ( mixLayerV(i,j).GT.0. _d 0 ) THEN
            z2H = 2. _d 0*rF(k)/mixLayerV(i,j)
          ELSE
            z2H = 0. _d 0
          ENDIF
          mu_z = ( z2H + 1. _d 0 )*( z2H + 1. _d 0 )
          mu_z = ( 1. _d 0 - mu_z )*(1. _d 0 + mu_z*five_ov21 )
          mu_z = MAX( 0. _d 0, mu_z )
          GM_PsiY(i,j,k,bi,bj) = GM_PsiY(i,j,k,bi,bj)
     &                         + mu_z*PsiLoc(i,j)
         ENDDO
        ENDDO
       ENDDO
#endif /* GM_BOLUS_ADVEC */
#ifdef ALLOW_DIAGNOSTICS
       IF ( useDiagnostics ) THEN
         CALL DIAGNOSTICS_FILL( PsiLoc, 'SubMpsiY',
     &                          0, 1, 2, bi, bj, myThid )
       ENDIF
#endif
       IF ( debugLevel.GE.debLevD ) THEN
         CALL WRITE_LOCAL_RL( 'subMeso_psiY','I10',1,PsiLoc,
     &                         bi,bj,1,myIter,myThid )
       ENDIF

c     ENDIF
#endif /* ndef GM_EXCLUDE_SUBMESO */

      RETURN
      END

#include "PACKAGES_CONFIG.h"
#include "CPP_OPTIONS.h"
#ifdef ALLOW_AUTODIFF
# include "AUTODIFF_OPTIONS.h"
#endif

CBOP
C     !ROUTINE: CALC_VISCOSITY
C     !INTERFACE:
      SUBROUTINE CALC_VISCOSITY(
     I           bi,bj, iMin,iMax,jMin,jMax,
     O           kappaRU, kappaRV,
     I           myThid )

C     !DESCRIPTION: \bv
C     *==========================================================*
C     | SUBROUTINE CALC_VISCOSITY
C     | o Calculate net vertical viscosity
C     *==========================================================*
C     \ev

C     !USES:
      IMPLICIT NONE
C     == GLobal variables ==
#include "SIZE.h"
#include "EEPARAMS.h"
#include "PARAMS.h"
#include "DYNVARS.h"
#include "GRID.h"
#ifndef EXCLUDE_PCELL_MIX_CODE
# ifdef ALLOW_AUTODIFF
#  include "tamc.h"
# endif
#endif

C     !INPUT/OUTPUT PARAMETERS:
C     == Routine arguments ==
C     iMin,iMax,jMin,jMax :: Range of points for which calculation
C     bi,bj   :: current tile indices
C     kappaRU :: Total vertical viscosity for zonal flow.
C     kappaRV :: Total vertical viscosity for meridional flow.
C     myThid  :: my Thread Id number
      INTEGER iMin,iMax,jMin,jMax
      INTEGER bi,bj
      _RL kappaRU(1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nr+1)
      _RL kappaRV(1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nr+1)
      INTEGER myThid

C     !LOCAL VARIABLES:
C     == Local variables ==
C     i, j, k :: Loop counters
      INTEGER i,j,k
      INTEGER ki
#ifndef EXCLUDE_PCELL_MIX_CODE
      INTEGER km, mixSurf, mixBott
      _RL pC_kFac
      _RL tmpFac(1-OLx:sNx+OLx,1-OLy:sNy+OLy)
# ifdef ALLOW_AUTODIFF_TAMC
C     tkey :: tape key (tile dependent)
C     kkey :: tape key (level and tile dependent)
      INTEGER tkey, kkey
# endif
#endif
CEOP

      DO k = 1,Nr+1
       ki = MIN(k,Nr)

       DO j = 1-OLy, sNy+OLy
        DO i = 1-OLx, sNx+OLx
         kappaRU(i,j,k) = viscArNr(ki)
         kappaRV(i,j,k) = viscArNr(ki)
        ENDDO
       ENDDO

#ifdef ALLOW_KPP
       IF ( useKPP .AND. k.LE.Nr ) THEN
         CALL KPP_CALC_VISC(
     I        bi,bj, iMin,iMax,jMin,jMax, k,
     O        kappaRU, kappaRV,
     I        myThid)
       ENDIF
#endif

#ifdef ALLOW_PP81
       IF ( usePP81 .AND. k.LE.Nr ) THEN
         CALL PP81_CALC_VISC(
     I        bi,bj, iMin,iMax,jMin,jMax, k,
     O        kappaRU, kappaRV,
     I        myThid)
       ENDIF
#endif

#ifdef ALLOW_KL10
       IF ( useKL10 .AND. k.LE.Nr ) THEN
         CALL KL10_CALC_VISC(
     I        bi,bj, iMin,iMax,jMin,jMax, k,
     O        kappaRU, kappaRV,
     I        myThid)
       ENDIF
#endif

#ifdef ALLOW_MY82
       IF ( useMY82 .AND. k.LE.Nr ) THEN
         CALL MY82_CALC_VISC(
     I        bi,bj, iMin,iMax,jMin,jMax, k,
     O        kappaRU, kappaRV,
     I        myThid)
       ENDIF
#endif

#ifdef ALLOW_GGL90
       IF ( useGGL90 .AND. k.LE.Nr ) THEN
         CALL GGL90_CALC_VISC(
     I        bi,bj, iMin,iMax,jMin,jMax, k,
     O        kappaRU, kappaRV,
     I        myThid)
       ENDIF
#endif

       IF ( k.EQ.Nr+1 .AND.
     &     ( usePP81 .OR. useKL10 .OR. useMY82 .OR. useGGL90 )
     &    ) THEN
        DO j = 1-OLy, sNy+OLy
         DO i = 1-OLx, sNx+OLx
          kappaRU(i,j,k) = kappaRU(i,j,ki)
          kappaRV(i,j,k) = kappaRV(i,j,ki)
         ENDDO
        ENDDO
       ENDIF

C--   end of k loop
      ENDDO

#ifndef EXCLUDE_PCELL_MIX_CODE
# ifdef ALLOW_AUTODIFF_TAMC
      tkey = bi + (bj-1)*nSx + (ikey_dynamics-1)*nSx*nSy
CADJ STORE kappaRU = comlev1_bibj, key = tkey, kind = isbyte
CADJ STORE kappaRV = comlev1_bibj, key = tkey, kind = isbyte
# endif
      IF ( interViscAr_pCell ) THEN
C--   This is a hack: alter vertical viscosity (instead of changing many S/R)
C     in order to account for missing hFac in viscous term
       DO k = 2,Nr
         km = k - 1
C-    account for true distance (including hFac) in vertical gradient
         DO j = 2-OLy, sNy+OLy
          DO i = 2-OLx, sNx+OLx
           IF ( k.GT.kSurfW(i,j,bi,bj) .AND.
     &          k.LE.MIN( kLowC(i,j,bi,bj), kLowC(i-1,j,bi,bj) )
     &        ) THEN
             kappaRU(i,j,k) = kappaRU(i,j,k)
     &                *twoRL/(hFacW(i,j,km,bi,bj)+hFacW(i,j,k,bi,bj))
           ENDIF
          ENDDO
         ENDDO
         DO j = 2-OLy, sNy+OLy
          DO i = 2-OLx, sNx+OLx
           IF ( k.GT.kSurfS(i,j,bi,bj) .AND.
     &          k.LE.MIN( kLowC(i,j,bi,bj), kLowC(i,j-1,bi,bj) )
     &        ) THEN
             kappaRV(i,j,k) = kappaRV(i,j,k)
     &                *twoRL/(hFacS(i,j,km,bi,bj)+hFacS(i,j,k,bi,bj))
           ENDIF
          ENDDO
         ENDDO
       ENDDO
      ENDIF

      IF ( pCellMix_select.GT.0 ) THEN
C--   This is a hack: alter vertical viscosity (instead of changing many S/R)
C     in order to to increase mixing for too thin surface/bottom partial cell
       mixSurf = pCellMix_select/10
       mixBott = MOD(pCellMix_select,10)
       DO k = 2,Nr
# ifdef ALLOW_AUTODIFF_TAMC
         kkey = k + (tkey-1)*Nr
# endif
         km = k - 1
         pC_kFac = 1.
         IF ( pCellMix_delR.LT.drF(k) )
     &     pC_kFac = pCellMix_delR*recip_drF(k)

C-    Increase KappaRU above bottom level:
         IF ( mixBott.GE.1 ) THEN
          DO j = 2-OLy, sNy+OLy
           DO i = 2-OLx, sNx+OLx
             tmpFac(i,j) = 0. _d 0
             IF ( k.EQ.MIN( kLowC(i,j,bi,bj), kLowC(i-1,j,bi,bj) )
     &        .AND. k.GT.kSurfW(i,j,bi,bj) ) THEN
               tmpFac(i,j) = pC_kFac*_recip_hFacW(i,j,k,bi,bj)
             ENDIF
           ENDDO
          ENDDO
          IF ( mixBott.EQ.2 ) THEN
           DO j = 2-OLy, sNy+OLy
            DO i = 2-OLx, sNx+OLx
             tmpFac(i,j) = tmpFac(i,j)*tmpFac(i,j)
            ENDDO
           ENDDO
          ELSEIF ( mixBott.EQ.3 ) THEN
           DO j = 2-OLy, sNy+OLy
            DO i = 2-OLx, sNx+OLx
             tmpFac(i,j) = tmpFac(i,j)*tmpFac(i,j)*tmpFac(i,j)
            ENDDO
           ENDDO
          ELSEIF ( mixBott.EQ.4 ) THEN
           DO j = 2-OLy, sNy+OLy
            DO i = 2-OLx, sNx+OLx
             tmpFac(i,j) = tmpFac(i,j)*tmpFac(i,j)
     &                    *tmpFac(i,j)*tmpFac(i,j)
            ENDDO
           ENDDO
          ENDIF
C-    increase mixing above bottom (by ~(1/hFac)^mixBott) if too thin p-cell
          DO j = 2-OLy, sNy+OLy
           DO i = 2-OLx, sNx+OLx
             tmpFac(i,j) = MIN( tmpFac(i,j), pCellMix_maxFac )
# ifdef ALLOW_AUTODIFF_TAMC
           ENDDO
          ENDDO
CADJ STORE tmpFac         = comlev1_bibj_k, key = kkey, kind = isbyte
CADJ STORE kappaRU(:,:,k) = comlev1_bibj_k, key = kkey, kind = isbyte
          DO j = 2-OLy, sNy+OLy
           DO i = 2-OLx, sNx+OLx
# endif
             kappaRU(i,j,k) = MAX( kappaRU(i,j,k),
     &                             pCellMix_viscAr(k)*tmpFac(i,j) )
           ENDDO
          ENDDO
         ENDIF

C-    Increase KappaRV above bottom level:
         IF ( mixBott.GE.1 ) THEN
          DO j = 2-OLy, sNy+OLy
           DO i = 2-OLx, sNx+OLx
             tmpFac(i,j) = 0. _d 0
             IF ( k.EQ.MIN( kLowC(i,j,bi,bj), kLowC(i,j-1,bi,bj) )
     &        .AND. k.GT.kSurfS(i,j,bi,bj) ) THEN
               tmpFac(i,j) = pC_kFac*_recip_hFacS(i,j,k,bi,bj)
             ENDIF
           ENDDO
          ENDDO
          IF ( mixBott.EQ.2 ) THEN
           DO j = 2-OLy, sNy+OLy
            DO i = 2-OLx, sNx+OLx
             tmpFac(i,j) = tmpFac(i,j)*tmpFac(i,j)
            ENDDO
           ENDDO
          ELSEIF ( mixBott.EQ.3 ) THEN
           DO j = 2-OLy, sNy+OLy
            DO i = 2-OLx, sNx+OLx
             tmpFac(i,j) = tmpFac(i,j)*tmpFac(i,j)*tmpFac(i,j)
            ENDDO
           ENDDO
          ELSEIF ( mixBott.EQ.4 ) THEN
           DO j = 2-OLy, sNy+OLy
            DO i = 2-OLx, sNx+OLx
             tmpFac(i,j) = tmpFac(i,j)*tmpFac(i,j)
     &                    *tmpFac(i,j)*tmpFac(i,j)
            ENDDO
           ENDDO
          ENDIF
C-    increase mixing above bottom (by ~(1/hFac)^mixBott) if too thin p-cell
          DO j = 2-OLy, sNy+OLy
           DO i = 2-OLx, sNx+OLx
             tmpFac(i,j) = MIN( tmpFac(i,j), pCellMix_maxFac )
# ifdef ALLOW_AUTODIFF_TAMC
           ENDDO
          ENDDO
CADJ STORE tmpFac         = comlev1_bibj_k, key = kkey, kind = isbyte
CADJ STORE kappaRV(:,:,k) = comlev1_bibj_k, key = kkey, kind = isbyte
          DO j = 2-OLy, sNy+OLy
           DO i = 2-OLx, sNx+OLx
# endif
             kappaRV(i,j,k) = MAX( kappaRV(i,j,k),
     &                             pCellMix_viscAr(k)*tmpFac(i,j) )
           ENDDO
          ENDDO
         ENDIF

         pC_kFac = 1.
         IF ( pCellMix_delR.LT.drF(km) )
     &     pC_kFac = pCellMix_delR*recip_drF(km)

C-    Increase KappaRU below surface level:
         IF ( mixSurf.GE.1 ) THEN
          DO j = 2-OLy, sNy+OLy
           DO i = 2-OLx, sNx+OLx
             tmpFac(i,j) = 0. _d 0
             IF ( km.EQ.kSurfW(i,j,bi,bj) .AND.
     &            km.LT.MIN( kLowC(i,j,bi,bj), kLowC(i-1,j,bi,bj) )
     &          ) THEN
               tmpFac(i,j) = pC_kFac*_recip_hFacW(i,j,km,bi,bj)
             ENDIF
           ENDDO
          ENDDO
          IF ( mixSurf.EQ.2 ) THEN
           DO j = 2-OLy, sNy+OLy
            DO i = 2-OLx, sNx+OLx
             tmpFac(i,j) = tmpFac(i,j)*tmpFac(i,j)
            ENDDO
           ENDDO
          ELSEIF ( mixSurf.EQ.3 ) THEN
           DO j = 2-OLy, sNy+OLy
            DO i = 2-OLx, sNx+OLx
             tmpFac(i,j) = tmpFac(i,j)*tmpFac(i,j)*tmpFac(i,j)
            ENDDO
           ENDDO
          ELSEIF ( mixSurf.EQ.4 ) THEN
           DO j = 2-OLy, sNy+OLy
            DO i = 2-OLx, sNx+OLx
             tmpFac(i,j) = tmpFac(i,j)*tmpFac(i,j)
     &                    *tmpFac(i,j)*tmpFac(i,j)
            ENDDO
           ENDDO
          ENDIF
C-    increase mixing below surface (by ~(1/hFac)^mixSurf) if too thin p-cell
# ifdef ALLOW_AUTODIFF_TAMC
CADJ STORE tmpFac = comlev1_bibj_k, key = kkey, kind = isbyte
# endif
          DO j = 2-OLy, sNy+OLy
           DO i = 2-OLx, sNx+OLx
             tmpFac(i,j) = MIN( tmpFac(i,j), pCellMix_maxFac )
# ifdef ALLOW_AUTODIFF_TAMC
           ENDDO
          ENDDO
CADJ STORE tmpFac         = comlev1_bibj_k, key = kkey, kind = isbyte
CADJ STORE kappaRU(:,:,k) = comlev1_bibj_k, key = kkey, kind = isbyte
          DO j = 2-OLy, sNy+OLy
           DO i = 2-OLx, sNx+OLx
# endif
             kappaRU(i,j,k) = MAX( kappaRU(i,j,k),
     &                             pCellMix_viscAr(k)*tmpFac(i,j) )
           ENDDO
          ENDDO
         ENDIF

C-    Increase KappaRV below surface level:
         IF ( mixSurf.GE.1 ) THEN
          DO j = 2-OLy, sNy+OLy
           DO i = 2-OLx, sNx+OLx
             tmpFac(i,j) = 0. _d 0
             IF ( km.EQ.kSurfS(i,j,bi,bj) .AND.
     &            km.LT.MIN( kLowC(i,j,bi,bj), kLowC(i,j-1,bi,bj) )
     &          ) THEN
               tmpFac(i,j) = pC_kFac*_recip_hFacS(i,j,km,bi,bj)
             ENDIF
           ENDDO
          ENDDO
          IF ( mixSurf.EQ.2 ) THEN
           DO j = 2-OLy, sNy+OLy
            DO i = 2-OLx, sNx+OLx
             tmpFac(i,j) = tmpFac(i,j)*tmpFac(i,j)
            ENDDO
           ENDDO
          ELSEIF ( mixSurf.EQ.3 ) THEN
           DO j = 2-OLy, sNy+OLy
            DO i = 2-OLx, sNx+OLx
             tmpFac(i,j) = tmpFac(i,j)*tmpFac(i,j)*tmpFac(i,j)
            ENDDO
           ENDDO
          ELSEIF ( mixSurf.EQ.4 ) THEN
           DO j = 2-OLy, sNy+OLy
            DO i = 2-OLx, sNx+OLx
             tmpFac(i,j) = tmpFac(i,j)*tmpFac(i,j)
     &                    *tmpFac(i,j)*tmpFac(i,j)
            ENDDO
           ENDDO
          ENDIF
C-    increase mixing below surface (by ~(1/hFac)^mixSurf) if too thin p-cell
# ifdef ALLOW_AUTODIFF_TAMC
CADJ STORE tmpFac = comlev1_bibj_k, key = kkey, kind = isbyte
# endif
          DO j = 2-OLy, sNy+OLy
           DO i = 2-OLx, sNx+OLx
             tmpFac(i,j) = MIN( tmpFac(i,j), pCellMix_maxFac )
# ifdef ALLOW_AUTODIFF_TAMC
           ENDDO
          ENDDO
CADJ STORE tmpFac         = comlev1_bibj_k, key = kkey, kind = isbyte
CADJ STORE kappaRV(:,:,k) = comlev1_bibj_k, key = kkey, kind = isbyte
          DO j = 2-OLy, sNy+OLy
           DO i = 2-OLx, sNx+OLx
# endif
             kappaRV(i,j,k) = MAX( kappaRV(i,j,k),
     &                             pCellMix_viscAr(k)*tmpFac(i,j) )
           ENDDO
          ENDDO
         ENDIF

C--   end of k loop
       ENDDO
      ENDIF
#endif /* ndef EXCLUDE_PCELL_MIX_CODE */

      RETURN
      END

#include "PACKAGES_CONFIG.h"
#include "CPP_OPTIONS.h"
#ifdef ALLOW_AUTODIFF
# include "AUTODIFF_OPTIONS.h"
#endif

CBOP
C     !ROUTINE: CALC_3D_DIFFUSIVITY
C     !INTERFACE:
      SUBROUTINE CALC_3D_DIFFUSIVITY(
     I        bi, bj, iMin,iMax,jMin,jMax,
     I        trIdentity, trUseGMRedi, trUseKPP,
     O        KappaRTr,
     I        myThid )

C     !DESCRIPTION: \bv
C     *==========================================================*
C     | SUBROUTINE CALC_3D_DIFFUSIVITY
C     | o Calculate net (3D) vertical diffusivity for 1 tracer
C     *==========================================================*
C     | Combines spatially varying diffusion coefficients from
C     | KPP and/or GM and/or convective stability test.
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
#ifdef ALLOW_GENERIC_ADVDIFF
#include "GAD.h"
#endif
#ifdef ALLOW_PTRACERS
#include "PTRACERS_SIZE.h"
#include "PTRACERS_PARAMS.h"
#endif
#ifdef ALLOW_LONGSTEP
#include "LONGSTEP.h"
#endif

C     !INPUT/OUTPUT PARAMETERS:
C     == Routine arguments ==
C     bi, bj     :: tile indices
C     iMin,iMax  :: Range of points for which calculation is performed.
C     jMin,jMax  :: Range of points for which calculation is performed.
C     trIdentity :: tracer identifier
C     trUseGMRedi:: this tracer use GM-Redi
C     trUseKPP   :: this tracer use KPP
C     myThid     :: Instance number for this innvocation of CALC_3D_DIFFUSIVITY
C     KappaRTr   :: Net diffusivity for this tracer (trIdentity)
      INTEGER bi,bj,iMin,iMax,jMin,jMax
      INTEGER trIdentity
      LOGICAL trUseGMRedi, trUseKPP
      _RL KappaRTr(1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nr)
      INTEGER myThid

#ifdef ALLOW_GENERIC_ADVDIFF
C     !LOCAL VARIABLES:
C     == Local variables ==
C     i, j, k    :: Loop counters
C     iTr        :: passive tracer index
C     msgBuf     :: message buffer
      INTEGER i,j,k
      _RL KbryanLewis79
#ifdef ALLOW_BL79_LAT_VARY
      _RL KbryanLewisEQ
#endif
      CHARACTER*(MAX_LEN_MBUF) msgBuf
#ifdef ALLOW_PTRACERS
      INTEGER iTr
#endif
#ifndef EXCLUDE_PCELL_MIX_CODE
      INTEGER km, mixSurf, mixBott
      _RL pC_kFac
      _RL tmpFac(1-OLx:sNx+OLx,1-OLy:sNy+OLy)
#endif
CEOP

      IF ( .NOT. trUseKPP ) THEN
       DO k = 1,Nr
        KbryanLewis79=diffKrBL79surf+(diffKrBL79deep-diffKrBL79surf)
     &       *(atan(-(rF(k)-diffKrBL79Ho)/diffKrBL79scl)/PI+0.5 _d 0)
#ifdef ALLOW_BL79_LAT_VARY
        KbryanLewisEQ=diffKrBLEQsurf+(diffKrBLEQdeep-diffKrBLEQsurf)
     &       *(atan(-(rF(k)-diffKrBLEQHo)/diffKrBLEQscl)/PI+0.5 _d 0)
#endif
#ifdef ALLOW_LONGSTEP
        IF ( trIdentity .GE. GAD_TR1) THEN
         DO j = 1-OLy,sNy+OLy
          DO i = 1-OLx,sNx+OLx
           KappaRTr(i,j,k) =
     &         LS_IVDConvCount(i,j,k,bi,bj)*ivdc_kappa
     &         + KbryanLewis79
#ifdef ALLOW_BL79_LAT_VARY
     &         + (KbryanLewisEQ-KbryanLewis79)*BL79LatArray(i,j,bi,bj)
#endif
          ENDDO
         ENDDO
        ELSE
#else
        IF ( .TRUE. ) THEN
#endif /* ALLOW_LONGSTEP */
         DO j = 1-OLy,sNy+OLy
          DO i = 1-OLx,sNx+OLx
           KappaRTr(i,j,k) =
     &         IVDConvCount(i,j,k,bi,bj)*ivdc_kappa
     &         + KbryanLewis79
#ifdef ALLOW_BL79_LAT_VARY
     &         + (KbryanLewisEQ-KbryanLewis79)*BL79LatArray(i,j,bi,bj)
#endif
          ENDDO
         ENDDO
        ENDIF
       ENDDO
       IF ( trIdentity.EQ.GAD_TEMPERATURE ) THEN
        DO k = 1,Nr
         DO j = 1-OLy,sNy+OLy
          DO i = 1-OLx,sNx+OLx
           KappaRTr(i,j,k) = KappaRTr(i,j,k)
#ifdef ALLOW_3D_DIFFKR
     &          + diffKr(i,j,k,bi,bj)
#else
     &          + diffKrNrT(k)
#endif
          ENDDO
         ENDDO
        ENDDO
       ELSEIF ( trIdentity.EQ.GAD_SALINITY) THEN
        DO k = 1,Nr
         DO j = 1-OLy, sNy+OLy
          DO i = 1-OLx, sNx+OLx
           KappaRTr(i,j,k) = KappaRTr(i,j,k)
#ifdef ALLOW_3D_DIFFKR
     &          + diffKr(i,j,k,bi,bj)
#else
     &          + diffKrNrS(k)
#endif
          ENDDO
         ENDDO
        ENDDO
#ifdef ALLOW_PTRACERS
       ELSEIF ( trIdentity.GE.GAD_TR1) THEN

        iTr = trIdentity - GAD_TR1 + 1
        DO k = 1,Nr
         DO j = 1-OLy, sNy+OLy
          DO i = 1-OLx, sNx+OLx
           KappaRTr(i,j,k) = KappaRTr(i,j,k)
#ifdef ALLOW_3D_DIFFKR
     &          + diffKr(i,j,k,bi,bj)
#else
     &          + PTRACERS_diffKrNr(k,iTr)
#endif
          ENDDO
         ENDDO
        ENDDO
#endif /* ALLOW_PTRACERS */
       ELSE
        WRITE(msgBuf,'(A,I4)')
     &       ' CALC_3D_DIFFUSIVITY: Invalid tracer Id: ',trIdentity
        CALL PRINT_ERROR(msgBuf, myThid)
        STOP 'ABNORMAL END: S/R CALC_3D_DIFFUSIVITY'
       ENDIF
      ENDIF

C--   Add physical pacakge contributions:

#ifdef ALLOW_KPP
      IF (trUseKPP) THEN
C--   Set vertical diffusivity contribution from KPP
       IF (trIdentity.EQ.GAD_TEMPERATURE) THEN
         CALL KPP_CALC_DIFF_T(
     I        bi,bj,iMin,iMax,jMin,jMax,0,Nr,
     O        KappaRTr,
     I        myThid)
       ELSEIF (trIdentity.EQ.GAD_SALINITY) THEN
         CALL KPP_CALC_DIFF_S(
     I        bi,bj,iMin,iMax,jMin,jMax,0,Nr,
     O        KappaRTr,
     I        myThid)
#ifdef ALLOW_PTRACERS
       ELSEIF ( trIdentity.GE.GAD_TR1) THEN
         iTr = trIdentity - GAD_TR1 + 1
         CALL KPP_CALC_DIFF_Ptr(
     I        bi,bj,iMin,iMax,jMin,jMax,0,Nr,
     O        KappaRTr,
     I        iTr, myThid )
#endif /* ALLOW_PTRACERS */
       ELSE
        WRITE(msgBuf,'(A,I4)')
     &       ' CALC_3D_DIFFUSIVITY: Invalid tracer Id: ',trIdentity
        CALL PRINT_ERROR( msgBuf, myThid )
        STOP 'ABNORMAL END: S/R CALC_3D_DIFFUSIVITY'
       ENDIF
      ENDIF
#endif /* ALLOW_KPP */

#ifdef ALLOW_GMREDI
      IF (trUseGMRedi) THEN
         CALL GMREDI_CALC_DIFF(
     I        bi,bj,iMin,iMax,jMin,jMax,0,Nr,
     U        KappaRTr,
     I        trIdentity,myThid)
      ENDIF
#endif

#ifdef ALLOW_PP81
      IF (usePP81) THEN
         CALL PP81_CALC_DIFF(
     I        bi,bj,iMin,iMax,jMin,jMax,0,Nr,
     U        KappaRTr,
     I        myThid)
      ENDIF
#endif

#ifdef ALLOW_KL10
      IF (useKL10) THEN
         CALL KL10_CALC_DIFF(
     I        bi,bj,iMin,iMax,jMin,jMax,0,Nr,
     U        KappaRTr,
     I        myThid)
      ENDIF
#endif

#ifdef ALLOW_MY82
      IF (useMY82) THEN
         CALL MY82_CALC_DIFF(
     I        bi,bj,iMin,iMax,jMin,jMax,0,Nr,
     U        KappaRTr,
     I        myThid)
      ENDIF
#endif

#ifdef ALLOW_GGL90
      IF (useGGL90) THEN
         CALL GGL90_CALC_DIFF(
     I        bi,bj,iMin,iMax,jMin,jMax,0,Nr,
     O        KappaRTr,
     I        myThid)
      ENDIF
#endif

#ifdef ALLOW_SMAG_3D_DIFFUSIVITY
       IF ( smag3D_diffCoeff.GT.zeroRL ) THEN
        DO k = 2,Nr
         DO j = 1-OLy,sNy+OLy
          DO i = 1-OLx,sNx+OLx
           KappaRTr(i,j,k) = KappaRTr(i,j,k)
     &          + halfRL*( smag3D_diffK(i,j,k-1,bi,bj)
     &                   + smag3D_diffK(i,j, k, bi,bj) )
          ENDDO
         ENDDO
        ENDDO
       ENDIF
#endif /* ALLOW_SMAG_3D_DIFFUSIVITY */

#ifndef EXCLUDE_PCELL_MIX_CODE
# ifdef ALLOW_AUTODIFF_TAMC
CADJ INIT loctape_3d_diff = COMMON, Nr
# endif
      IF ( interDiffKr_pCell ) THEN
C--   This is a hack: alter vertical diffusivity (instead of changing many S/R)
C     in order to account for missing hFac in diffusion term
       DO k = 2,Nr
         km = k - 1
C-    account for true distance (including hFac) in vertical gradient
         DO j = 2-OLy, sNy+OLy
          DO i = 2-OLx, sNx+OLx
           IF ( k.GT.kSurfC(i,j,bi,bj) .AND.
     &          k.LE.kLowC(i,j,bi,bj) ) THEN
             KappaRTr(i,j,k) = KappaRTr(i,j,k)
     &                *twoRL/(hFacC(i,j,km,bi,bj)+hFacC(i,j,k,bi,bj))
           ENDIF
          ENDDO
         ENDDO
       ENDDO
      ENDIF

      IF ( pCellMix_select.GT.0 ) THEN
C--   This is a hack: alter vertical diffusivity (instead of changing many S/R)
C     in order to to increase mixing for too thin surface/bottom partial cell
       mixSurf = pCellMix_select/10
       mixBott = MOD(pCellMix_select,10)
       DO k = 2,Nr
         km = k - 1
         pC_kFac = 1.
         IF ( pCellMix_delR.LT.drF(k) )
     &     pC_kFac = pCellMix_delR*recip_drF(k)

# ifdef ALLOW_AUTODIFF
         DO j = 1-OLy, sNy+OLy
          DO i = 1-OLx, sNx+OLx
           tmpFac(i,j) = 0. _d 0
          ENDDO
         ENDDO
# endif

C-    Increase KappaRTr above bottom level:
         IF ( mixBott.GE.1 ) THEN
          DO j = 2-OLy, sNy+OLy
           DO i = 2-OLx, sNx+OLx
             tmpFac(i,j) = 0. _d 0
             IF ( k.EQ.kLowC(i,j,bi,bj) .AND.
     &            k.GT.kSurfC(i,j,bi,bj) ) THEN
               tmpFac(i,j) = pC_kFac*_recip_hFacC(i,j,k,bi,bj)
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
     &                   * tmpFac(i,j)*tmpFac(i,j)
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
CADJ STORE tmpFac          = loctape_3d_diff, key = k
CADJ STORE kappartr(:,:,k) = loctape_3d_diff, key = k
          DO j = 2-OLy, sNy+OLy
           DO i = 2-OLx, sNx+OLx
# endif
             KappaRTr(i,j,k) = MAX( KappaRTr(i,j,k),
     &                              pCellMix_diffKr(k)*tmpFac(i,j) )
           ENDDO
          ENDDO
         ENDIF

         pC_kFac = 1.
         IF ( pCellMix_delR.LT.drF(km) )
     &     pC_kFac = pCellMix_delR*recip_drF(km)

C-    Increase KappaRTr below surface level:
         IF ( mixSurf.GE.1 ) THEN
          DO j = 2-OLy, sNy+OLy
           DO i = 2-OLx, sNx+OLx
             tmpFac(i,j) = 0. _d 0
             IF ( km.EQ.kSurfC(i,j,bi,bj) .AND.
     &            km.LT.kLowC(i,j,bi,bj) ) THEN
               tmpFac(i,j) = pC_kFac*_recip_hFacC(i,j,km,bi,bj)
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
     &                   * tmpFac(i,j)*tmpFac(i,j)
            ENDDO
           ENDDO
          ENDIF
C-    increase mixing below surface (by ~(1/hFac)^mixSurf) if too thin p-cell
          DO j = 2-OLy, sNy+OLy
           DO i = 2-OLx, sNx+OLx
             tmpFac(i,j) = MIN( tmpFac(i,j), pCellMix_maxFac )
# ifdef ALLOW_AUTODIFF_TAMC
           ENDDO
          ENDDO
CADJ STORE tmpFac          = loctape_3d_diff, key = k
CADJ STORE kappartr(:,:,k) = loctape_3d_diff, key = k
          DO j = 2-OLy, sNy+OLy
           DO i = 2-OLx, sNx+OLx
# endif
             KappaRTr(i,j,k) = MAX( KappaRTr(i,j,k),
     &                              pCellMix_diffKr(k)*tmpFac(i,j) )
           ENDDO
          ENDDO
         ENDIF

C--   end of k loop
       ENDDO
      ENDIF
#endif /* ndef EXCLUDE_PCELL_MIX_CODE */

C-    Apply mask to vertical diffusivity
C jmc: do not have the impression that masking is needed
C      but could be removed later if it is the case.
c     DO j = 1-OLy, sNy+OLy
c      DO i = 1-OLx, sNx+OLx
c       KappaRTr(i,j,k) = maskUp(i,j)*KappaRTr(i,j,k)
c      ENDDO
c     ENDDO

#endif /* ALLOW_GENERIC_ADVDIFF */

      RETURN
      END

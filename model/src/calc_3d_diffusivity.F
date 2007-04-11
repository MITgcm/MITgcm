C $Header: /u/gcmpack/MITgcm/model/src/calc_3d_diffusivity.F,v 1.7 2007/04/11 00:00:47 dimitri Exp $
C $Name:  $

#include "PACKAGES_CONFIG.h"
#include "CPP_OPTIONS.h"

CBOP
C     !ROUTINE: CALC_3D_DIFFUSIVITY
C     !INTERFACE: 
      SUBROUTINE CALC_3D_DIFFUSIVITY( 
     I        bi,bj,iMin,iMax,jMin,jMax,
     I        trIdentity, trUseGMRedi, trUseKPP,
     O        KappaRTr,
     I        myThid)

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
#include "PTRACERS.h"
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
      _RL KappaRTr(1-Olx:sNx+Olx,1-Oly:sNy+Oly,Nr)
      INTEGER myThid

#ifdef ALLOW_GENERIC_ADVDIFF
C     !LOCAL VARIABLES:
C     == Local variables ==
C     i, j, k    :: Loop counters
C     iTr        :: passive tracer index
C     msgBuf     :: message buffer
      INTEGER i,j,k
      _RL KbryanLewis79, KbryanLewisHL, KbryanLewisEQ
      CHARACTER*(MAX_LEN_MBUF) msgBuf
#ifdef ALLOW_PTRACERS
      INTEGER iTr
#endif
CEOP

      IF ( trIdentity.EQ.GAD_TEMPERATURE) THEN

       DO k = 1,Nr
        KbryanLewisHL=diffKrBL79surf+(diffKrBL79deep-diffKrBL79surf)
     &   *( atan( -( rF(k)-diffKrBL79Ho )/diffKrBL79scl )/PI+0.5 _d 0)
        KbryanLewisEQ=diffKrBLEQsurf+(diffKrBLEQdeep-diffKrBLEQsurf)
     &   *( atan( -( rF(k)-diffKrBLEQHo )/diffKrBLEQscl )/PI+0.5 _d 0)
        DO j = 1-Oly,sNy+Oly
         IF ( abs(YC(i,j,bi,bj)) .GT. KbryanLewisLatTransition ) THEN
          KbryanLewis79 = KbryanLewisHL
         ELSE
          KbryanLewis79 = KbryanLewisHL-(KbryanLewisHL-KbryanLewisEQ)*
     &         (1+cos(YC(i,j,bi,bj)*pi/KbryanLewisLatTransition)) / 2
         ENDIF
         DO i = 1-Olx,sNx+Olx
          KappaRTr(i,j,k) =
     &         IVDConvCount(i,j,k,bi,bj)*ivdc_kappa
#if (defined ALLOW_3D_DIFFKR || \
     (defined (ALLOW_AUTODIFF_TAMC) && defined (ALLOW_DIFFKR_CONTROL)))
     &       + diffKr(i,j,k,bi,bj)
#else
     &       + diffKrNrT(k)
#endif
     &       + KbryanLewis79
         ENDDO
        ENDDO
       ENDDO

      ELSEIF ( trIdentity.EQ.GAD_SALINITY) THEN

       DO k = 1,Nr
        KbryanLewisHL=diffKrBL79surf+(diffKrBL79deep-diffKrBL79surf)
     &   *( atan( -( rF(k)-diffKrBL79Ho )/diffKrBL79scl )/PI+0.5 _d 0)
        KbryanLewisEQ=diffKrBLEQsurf+(diffKrBLEQdeep-diffKrBLEQsurf)
     &   *( atan( -( rF(k)-diffKrBLEQHo )/diffKrBLEQscl )/PI+0.5 _d 0)
        DO j = 1-Oly, sNy+Oly
         IF ( abs(YC(i,j,bi,bj)) .GT. KbryanLewisLatTransition ) THEN
          KbryanLewis79 = KbryanLewisHL
         ELSE
          KbryanLewis79 = KbryanLewisHL-(KbryanLewisHL-KbryanLewisEQ)*
     &         (1+cos(YC(i,j,bi,bj)*pi/KbryanLewisLatTransition)) / 2
         ENDIF
         DO i = 1-Olx, sNx+Olx
          KappaRTr(i,j,k) =
     &         IVDConvCount(i,j,k,bi,bj)*ivdc_kappa
#if (defined ALLOW_3D_DIFFKR || \
     (defined (ALLOW_AUTODIFF_TAMC) && defined (ALLOW_DIFFKR_CONTROL)))
     &       + diffKr(i,j,k,bi,bj)
#else
     &       + diffKrNrS(k)
#endif
     &       + KbryanLewis79
         ENDDO
        ENDDO
       ENDDO

#ifdef ALLOW_PTRACERS
      ELSEIF ( trIdentity.GE.GAD_TR1
     &   .AND. trIdentity.LT.GAD_TR1+PTRACERS_numInUse) THEN

       iTr = trIdentity - GAD_TR1 + 1
       DO k = 1,Nr
        KbryanLewisHL=diffKrBL79surf+(diffKrBL79deep-diffKrBL79surf)
     &   *( atan( -( rF(k)-diffKrBL79Ho )/diffKrBL79scl )/PI+0.5 _d 0)
        KbryanLewisEQ=diffKrBLEQsurf+(diffKrBLEQdeep-diffKrBLEQsurf)
     &   *( atan( -( rF(k)-diffKrBLEQHo )/diffKrBLEQscl )/PI+0.5 _d 0)
        DO j = 1-Oly, sNy+Oly
         IF ( abs(YC(i,j,bi,bj)) .GT. KbryanLewisLatTransition ) THEN
          KbryanLewis79 = KbryanLewisHL
         ELSE
          KbryanLewis79 = KbryanLewisHL-(KbryanLewisHL-KbryanLewisEQ)*
     &         (1+cos(YC(i,j,bi,bj)*pi/KbryanLewisLatTransition)) / 2
         ENDIF
         DO i = 1-Olx, sNx+Olx
          KappaRTr(i,j,k) =
     &         IVDConvCount(i,j,k,bi,bj)*ivdc_kappa
#if (defined ALLOW_3D_DIFFKR || \
     (defined (ALLOW_AUTODIFF_TAMC) && defined (ALLOW_DIFFKR_CONTROL)))
     &       + diffKr(i,j,k,bi,bj)
#else
     &       + PTRACERS_diffKrNr(k,iTr)
#endif
     &       + KbryanLewis79
         ENDDO
        ENDDO
       ENDDO
#endif /* ALLOW_PTRACERS */

      ELSE
        WRITE(msgBuf,'(A,I4)')
     &      ' CALC_3D_DIFFUSIVITY: Invalid tracer Id: ',trIdentity
        CALL PRINT_ERROR(msgBuf, myThid)
        STOP 'ABNORMAL END: S/R CALC_3D_DIFFUSIVITY'
      ENDIF

C--   Add physical pacakge contributions:

#ifdef ALLOW_GMREDI
      IF (trUseGMRedi) THEN 
         CALL GMREDI_CALC_DIFF(
     I        bi,bj,iMin,iMax,jMin,jMax,0,Nr,
     U        KappaRTr,
     I        myThid)
      ENDIF
#endif

#ifdef ALLOW_KPP
      IF (trUseKPP) THEN
       IF (trIdentity.EQ.GAD_TEMPERATURE) THEN
         CALL KPP_CALC_DIFF_T(
     I        bi,bj,iMin,iMax,jMin,jMax,0,Nr,
     U        KappaRTr,
     I        myThid)
       ELSE
         CALL KPP_CALC_DIFF_S(
     I        bi,bj,iMin,iMax,jMin,jMax,0,Nr,
     U        KappaRTr,
     I        myThid)
       ENDIF
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
      
C-    Apply mask to vertical diffusivity 
C jmc: don't have the impression that masking is needed 
C      but could be removed later if it's the case.
c     DO j = 1-Oly, sNy+Oly
c      DO i = 1-Olx, sNx+Olx
c       KappaRTr(i,j,k) = maskUp(i,j)*KappaRTr(i,j,k)
c      ENDDO
c     ENDDO

#endif /* ALLOW_GENERIC_ADVDIFF */

      RETURN
      END

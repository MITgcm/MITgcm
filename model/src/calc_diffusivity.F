C $Header: /u/gcmpack/MITgcm/model/src/Attic/calc_diffusivity.F,v 1.23 2004/09/16 09:35:11 mlosch Exp $
C $Name:  $

#include "PACKAGES_CONFIG.h"
#include "CPP_OPTIONS.h"

CBOP
C     !ROUTINE: CALC_DIFFUSIVITY
C     !INTERFACE: 
      SUBROUTINE CALC_DIFFUSIVITY( 
     I        bi,bj,iMin,iMax,jMin,jMax,K,
     I        maskUp,
     O        KappaRT,KappaRS,
     I        myThid)

C     !DESCRIPTION: \bv
C     *==========================================================*
C     | SUBROUTINE CALC_DIFFUSIVITY                               
C     | o Calculate net vertical diffusivity                               
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

C     !INPUT/OUTPUT PARAMETERS:
C     == Routine arguments ==
C     bi, bj, :: Range of points for which calculation
C     iMin,      is performed.
C     iMax, 
C     jMin, 
C     jMax
C     maskUp  :: land/water mask for Wvel points (above tracer level)
C     myThid  :: Instance number for this innvocation of CALC_DIFFUSIVITY
C     KappaRT :: Net diffusivity for temperature
C     KappaRS :: Net diffusivity for salinity
      INTEGER bi,bj,iMin,iMax,jMin,jMax,K
      _RS maskUp(1-Olx:sNx+Olx,1-Oly:sNy+Oly)
      _RL KappaRT(1-Olx:sNx+Olx,1-Oly:sNy+Oly,Nr)
      _RL KappaRS(1-Olx:sNx+Olx,1-Oly:sNy+Oly,Nr)
      INTEGER myThid

C     !LOCAL VARIABLES:
C     == Local variables ==
C     I, J :: Loop counters
      INTEGER i,j
      _RL KbryanLewis79
CEOP

      KbryanLewis79=diffKrBL79surf+(diffKrBL79deep-diffKrBL79surf)
     & *( atan( -( rF(k)-diffKrBL79Ho )/diffKrBL79scl )/PI+0.5 _d 0)

      DO j = 1-Oly, sNy+Oly
       DO i = 1-Olx, sNx+Olx
        KappaRT(i,j,k) = maskUp(i,j)*
     &      (
     &         IVDConvCount(i,j,k,bi,bj)*ivdc_kappa
#if (defined (ALLOW_AUTODIFF_TAMC) && defined (ALLOW_DIFFKR_CONTROL))
     &       + diffKr(i,j,k,bi,bj)
#else
     &       + diffKrT
#endif
     &       + KbryanLewis79
     &      )
       ENDDO
      ENDDO

      DO j = 1-Oly, sNy+Oly
       DO i = 1-Olx, sNx+Olx
        KappaRS(i,j,k) = maskUp(i,j)*
     &      (
     &         IVDConvCount(i,j,k,bi,bj)*ivdc_kappa
#if (defined (ALLOW_AUTODIFF_TAMC) && defined (ALLOW_DIFFKR_CONTROL))
     &       + diffKr(i,j,k,bi,bj)
#else
     &       + diffKrS
#endif
     &       + KbryanLewis79
     &      )
       ENDDO
      ENDDO

#ifdef ALLOW_GMREDI
      IF (useGMRedi) CALL GMREDI_CALC_DIFF(
     I        bi,bj,iMin,iMax,jMin,jMax,K,
     I        maskUp,
     O        KappaRT,KappaRS,
     I        myThid)
#endif

#ifdef ALLOW_KPP
      IF (useKPP) CALL KPP_CALC_DIFF(
     I        bi,bj,iMin+1,iMax,jMin+1,jMax,K,
     I        maskUp,
     O        KappaRT,KappaRS,
     I        myThid)
#endif

#ifdef ALLOW_PP81
      IF (usePP81) CALL PP81_CALC_DIFF(
     I        bi,bj,iMin+1,iMax,jMin+1,jMax,K,
     I        maskUp,
     O        KappaRT,KappaRS,
     I        myThid)
#endif

#ifdef ALLOW_MY82
      IF (useMY82) CALL MY82_CALC_DIFF(
     I        bi,bj,iMin+1,iMax,jMin+1,jMax,K,
     I        maskUp,
     O        KappaRT,KappaRS,
     I        myThid)
#endif
      
#ifdef ALLOW_GGL90
      IF (useGGL90) CALL GGL90_CALC_DIFF(
     I        bi,bj,iMin+1,iMax,jMin+1,jMax,K,
     I        maskUp,
     O        KappaRT,KappaRS,
     I        myThid)
#endif
      
      RETURN
      END

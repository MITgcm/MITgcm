C $Header: /u/gcmpack/MITgcm/model/src/Attic/calc_diffusivity.F,v 1.31 2007/04/14 18:32:32 dimitri Exp $
C $Name:  $

#include "PACKAGES_CONFIG.h"
#include "CPP_OPTIONS.h"

CBOP
C     !ROUTINE: CALC_DIFFUSIVITY
C     !INTERFACE: 
      SUBROUTINE CALC_DIFFUSIVITY( 
     I        bi,bj,iMin,iMax,jMin,jMax,k,
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
C     bi, bj, :: tile indices
C     iMin,   :: Range of points for which calculation is performed.
C     iMax, 
C     jMin, 
C     jMax
C     maskUp  :: land/water mask for Wvel points (above tracer level)
C     myThid  :: Instance number for this innvocation of CALC_DIFFUSIVITY
C     KappaRT :: Net diffusivity for temperature
C     KappaRS :: Net diffusivity for salinity
      INTEGER bi,bj,iMin,iMax,jMin,jMax,K
      _RS maskUp(1-Olx:sNx+Olx,1-Oly:sNy+Oly)
      _RL KappaRT(1-Olx:sNx+Olx,1-Oly:sNy+Oly)
      _RL KappaRS(1-Olx:sNx+Olx,1-Oly:sNy+Oly)
      INTEGER myThid

C     !LOCAL VARIABLES:
C     == Local variables ==
C     I, J :: Loop counters
      INTEGER i,j
      _RL KbryanLewis79, KbryanLewisEQ
CEOP

      KbryanLewis79=diffKrBL79surf+(diffKrBL79deep-diffKrBL79surf)
     & *( atan( -( rF(k)-diffKrBL79Ho )/diffKrBL79scl )/PI+0.5 _d 0)
#ifdef ALLOW_BL79_LAT_VARY
      KbryanLewisEQ=diffKrBLEQsurf+(diffKrBLEQdeep-diffKrBLEQsurf)
     & *( atan( -( rF(k)-diffKrBLEQHo )/diffKrBLEQscl )/PI+0.5 _d 0)
#endif

      DO j = 1-Oly, sNy+Oly
       DO i = 1-Olx, sNx+Olx
        KappaRT(i,j) =
     &         IVDConvCount(i,j,k,bi,bj)*ivdc_kappa
#if (defined ALLOW_3D_DIFFKR || \
     (defined (ALLOW_AUTODIFF_TAMC) && defined (ALLOW_DIFFKR_CONTROL)))
     &       + diffKr(i,j,k,bi,bj)
#else
     &       + diffKrNrT(k)
#endif
     &       + KbryanLewis79
#ifdef ALLOW_BL79_LAT_VARY
     &       + (KbryanLewisEQ-KbryanLewis79)*BL79LatArray(i,j,bi,bj)
#endif
       ENDDO
      ENDDO

      DO j = 1-Oly, sNy+Oly
       DO i = 1-Olx, sNx+Olx
        KappaRS(i,j) =
     &         IVDConvCount(i,j,k,bi,bj)*ivdc_kappa
#if (defined ALLOW_3D_DIFFKR || \
     (defined (ALLOW_AUTODIFF_TAMC) && defined (ALLOW_DIFFKR_CONTROL)))
     &       + diffKr(i,j,k,bi,bj)
#else
     &       + diffKrNrS(k)
#endif
     &       + KbryanLewis79
#ifdef ALLOW_BL79_LAT_VARY
     &       + (KbryanLewisEQ-KbryanLewis79)*BL79LatArray(i,j,bi,bj)
#endif
       ENDDO
      ENDDO

#ifdef ALLOW_GMREDI
      IF (useGMRedi) THEN 
         CALL GMREDI_CALC_DIFF(
     I        bi,bj,iMin,iMax,jMin,jMax,k,1,
     U        KappaRT,
     I        myThid)
         CALL GMREDI_CALC_DIFF(
     I        bi,bj,iMin,iMax,jMin,jMax,k,1,
     U        KappaRS,
     I        myThid)
      ENDIF
#endif

#ifdef ALLOW_KPP
      IF (useKPP) THEN
         CALL KPP_CALC_DIFF_T(
     I        bi,bj,iMin+1,iMax,jMin+1,jMax,k,1,
     U        KappaRT,
     I        myThid)
         CALL KPP_CALC_DIFF_S(
     I        bi,bj,iMin+1,iMax,jMin+1,jMax,k,1,
     U        KappaRS,
     I        myThid)
      ENDIF
#endif

#ifdef ALLOW_PP81
      IF (usePP81) THEN
         CALL PP81_CALC_DIFF(
     I        bi,bj,iMin+1,iMax,jMin+1,jMax,k,1,
     U        KappaRT,
     I        myThid)
         CALL PP81_CALC_DIFF(
     I        bi,bj,iMin+1,iMax,jMin+1,jMax,k,1,
     U        KappaRS,
     I        myThid)
      ENDIF
#endif

#ifdef ALLOW_MY82
      IF (useMY82) THEN
         CALL MY82_CALC_DIFF(
     I        bi,bj,iMin+1,iMax,jMin+1,jMax,k,1,
     U        KappaRT,
     I        myThid)
         CALL MY82_CALC_DIFF(
     I        bi,bj,iMin+1,iMax,jMin+1,jMax,k,1,
     U        KappaRS,
     I        myThid)
      ENDIF
#endif
      
#ifdef ALLOW_GGL90
      IF (useGGL90) THEN
         CALL GGL90_CALC_DIFF(
     I        bi,bj,iMin+1,iMax,jMin+1,jMax,k,1,
     O        KappaRT,
     I        myThid)
         CALL GGL90_CALC_DIFF(
     I        bi,bj,iMin+1,iMax,jMin+1,jMax,k,1,
     O        KappaRS,
     I        myThid)
      ENDIF
#endif
      
C-    Apply mask to vertical diffusivity 
C jmc: don't have the impression that masking is needed 
C      but could be removed later if it's the case.
      DO j = 1-Oly, sNy+Oly
       DO i = 1-Olx, sNx+Olx
        KappaRT(i,j) = maskUp(i,j)*KappaRT(i,j)
        KappaRS(i,j) = maskUp(i,j)*KappaRS(i,j)
       ENDDO
      ENDDO

      RETURN
      END

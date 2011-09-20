C $Header: /u/gcmpack/MITgcm/pkg/ptracers/Attic/ptracers_calc_diff.F,v 1.9 2011/09/20 22:29:07 jmc Exp $
C $Name:  $

#include "PTRACERS_OPTIONS.h"

CBOP
C     !ROUTINE: PTRACERS_CALC_DIFF
C     !INTERFACE:
      SUBROUTINE PTRACERS_CALC_DIFF(
     I        bi,bj,iMin,iMax,jMin,jMax,k,
     I        maskUp,
     O        KappaRtr,
     I        myThid)

C     !DESCRIPTION: \bv
C     *==========================================================*
C     | SUBROUTINE PTRACERS_CALC_DIFF
C     | o Calculate net vertical diffusivity for passive tracers
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
#include "GRID.h"
#include "DYNVARS.h"
#ifdef ALLOW_LONGSTEP
#include "LONGSTEP.h"
#endif
#include "PTRACERS_SIZE.h"
#include "PTRACERS_PARAMS.h"
c #include "PTRACERS_FIELDS.h"
#include "GAD.h"

C     !INPUT/OUTPUT PARAMETERS:
C     == Routine arguments ==
C     bi, bj    :: tile indices
C     iMin,iMax :: Range of points for which calculation is performed.
C     jMin,jMax :: Range of points for which calculation is performed.
C     maskUp    :: land/water mask for Wvel points (above tracer level)
C     myThid    :: Instance number for this innvocation of PTRACERS_CALC_DIFF
C     KappaRtr  :: Net diffusivity for temperature
C     KappaRS   :: Net diffusivity for salinity
      INTEGER bi,bj,iMin,iMax,jMin,jMax,k
      _RS maskUp(1-Olx:sNx+Olx,1-Oly:sNy+Oly)
      _RL KappaRtr(1-Olx:sNx+Olx,1-Oly:sNy+Oly,PTRACERS_num)
      INTEGER myThid

C     !LOCAL VARIABLES:
C     == Local variables ==
C     I, J :: Loop counters
      INTEGER i,j,iTr,trIdentity
      _RL KbryanLewis79
#ifdef ALLOW_BL79_LAT_VARY
      _RL KbryanLewisEQ
#endif
CEOP

      KbryanLewis79=diffKrBL79surf+(diffKrBL79deep-diffKrBL79surf)
     &     *(atan(-(rF(k)-diffKrBL79Ho)/diffKrBL79scl)/PI+0.5 _d 0)
#ifdef ALLOW_BL79_LAT_VARY
      KbryanLewisEQ=diffKrBLEQsurf+(diffKrBLEQdeep-diffKrBLEQsurf)
     &     *(atan(-(rF(k)-diffKrBLEQHo)/diffKrBLEQscl)/PI+0.5 _d 0)
#endif

C Loop over tracers
      DO iTr=1,PTRACERS_numInUse

       trIdentity = iTr + GAD_TR1 - 1

       IF ( .NOT. PTRACERS_useKPP(iTr) ) THEN
        DO j = 1-Oly, sNy+Oly
         DO i = 1-Olx, sNx+Olx
          KappaRtr(i,j,iTr) =
#ifdef ALLOW_LONGSTEP
     &         LS_IVDConvCount(i,j,k,bi,bj)*ivdc_kappa
#else
     &         IVDConvCount(i,j,k,bi,bj)*ivdc_kappa
#endif
#ifdef ALLOW_3D_DIFFKR
     &       + diffKr(i,j,k,bi,bj)
#else
     &       + PTRACERS_diffKrNr(k,iTr)
#endif
     &       + KbryanLewis79
#ifdef ALLOW_BL79_LAT_VARY
     &        + (KbryanLewisEQ-KbryanLewis79)*BL79LatArray(i,j,bi,bj)
#endif
         ENDDO
        ENDDO
       ENDIF

#ifdef ALLOW_KPP
        IF ( PTRACERS_useKPP(iTr) ) THEN
C--   Set vertical diffusivity contribution from KPP
         CALL KPP_CALC_DIFF_PTR(
     I        bi,bj,iMin+1,iMax,jMin+1,jMax,k,1,
     O        KappaRtr(1-Olx,1-Oly,iTr),
     I        iTr, myThid )
        ENDIF
#endif

#ifdef ALLOW_GMREDI
        IF ( PTRACERS_useGMRedi(iTr) ) THEN
         CALL GMREDI_CALC_DIFF(
     I        bi,bj,iMin,iMax,jMin,jMax,k,1,
     U        KappaRtr(1-Olx,1-Oly,iTr),
     I        trIdentity,myThid)
        ENDIF
#endif

#ifdef ALLOW_PP81
        IF (usePP81) THEN
         CALL PP81_CALC_DIFF(
     I        bi,bj,iMin+1,iMax,jMin+1,jMax,k,1,
     U        KappaRtr(1-Olx,1-Oly,iTr),
     I        myThid)
        ENDIF
#endif

#ifdef ALLOW_MY82
        IF (useMY82) THEN
         CALL MY82_CALC_DIFF(
     I        bi,bj,iMin+1,iMax,jMin+1,jMax,k,1,
     U        KappaRtr(1-Olx,1-Oly,iTr),
     I        myThid)
        ENDIF
#endif

#ifdef ALLOW_GGL90
        IF (useGGL90) THEN
         CALL GGL90_CALC_DIFF(
     I        bi,bj,iMin+1,iMax,jMin+1,jMax,k,1,
     U        KappaRtr(1-Olx,1-Oly,iTr),
     I        myThid)
        ENDIF
#endif

C-    Apply mask to vertical diffusivity
C jmc: do not have the impression that masking is needed
C      but could be removed later if it is the case.
        DO j = 1-Oly, sNy+Oly
         DO i = 1-Olx, sNx+Olx
          KappaRtr(i,j,iTr) = maskUp(i,j)*KappaRtr(i,j,iTr)
         ENDDO
        ENDDO

C end of tracer loop
      ENDDO

      RETURN
      END

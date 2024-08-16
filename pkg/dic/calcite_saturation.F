#include "DIC_OPTIONS.h"

CBOP
C !ROUTINE: CALCITE_SATURATION

C !INTERFACE: ==========================================================
      SUBROUTINE CALCITE_SATURATION( PTR_DIC, PTR_ALK, PTR_PO4,
     I           bi,bj,imin,imax,jmin,jmax,
     I           myTime, myIter, myThid )

C !DESCRIPTION:
C  Calculate carbonate fluxes:
C   - determine carbonate ion concentration through full domain
C   - calculate calcite saturation state

C !USES: ===============================================================
      IMPLICIT NONE
#include "SIZE.h"
#include "EEPARAMS.h"
#include "PARAMS.h"
#include "GRID.h"
#include "DYNVARS.h"
#include "DIC_VARS.h"

C !INPUT PARAMETERS: ===================================================
C  myTime               :: current time
C  myIter               :: current timestep
C  myThid               :: thread number
       _RL  PTR_DIC(1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nr)
       _RL  PTR_ALK(1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nr)
       _RL  PTR_PO4(1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nr)
      INTEGER imin, imax, jmin, jmax, bi, bj
      _RL myTime
      INTEGER myIter
      INTEGER myThid

C !OUTPUT PARAMETERS: ===================================================

#if ( defined DIC_BIOTIC && defined DIC_CALCITE_SAT )

C !LOCAL VARIABLES: ====================================================
C  i,j,k                  :: loop indices
       INTEGER i,j,k
       LOGICAL debugPrt
       _RL carbonate(1-OLx:sNx+OLx,1-OLy:sNy+OLy)
       _RL calcium
       _RL sitlocal
       _RL po4local
       _RL diclocal
       _RL alklocal
       _RL pCO2local(1-OLx:sNx+OLx,1-OLy:sNy+OLy)
       _RL locTemp(1-OLx:sNx+OLx,1-OLy:sNy+OLy)
       _RL locSalt(1-OLx:sNx+OLx,1-OLy:sNy+OLy)

       INTEGER doIterCO3, CO3ITER
CEOP

       DO k=1,Nr
        DO j=1-OLy,sNy+OLy
         DO i=1-OLx,sNx+OLx
            locTemp(i,j) = theta(i,j,k,bi,bj)
            locSalt(i,j) = salt (i,j,k,bi,bj)
            carbonate(i,j) = 0. _d 0
            pCO2local(i,j) = 0. _d 0
         ENDDO
        ENDDO
#ifdef CARBONCHEM_SOLVESAPHE
#ifdef ALLOW_DEBUG
        IF (debugMode) CALL DEBUG_CALL( 'DIC_COEFFS_DEEP',myThid )
#endif
C Calculate carbon coefficients
        CALL DIC_COEFFS_SURF(
     I                       locTemp, locSalt,
     I                       bi,bj,iMin,iMax,jMin,jMax,myThid)

C Now correct the coefficients for pressure dependence
        CALL DIC_COEFFS_DEEP(
     I                       locTemp, locSalt,
     I                       bi,bj,iMin,iMax,jMin,jMax,
     I                       k,myThid)
#else
#ifdef ALLOW_DEBUG
        IF (debugMode) CALL DEBUG_CALL(
     &      'CARBON_COEFFS_PRESSURE_DEP',myThid)
#endif
        CALL CARBON_COEFFS_PRESSURE_DEP(
     I                       locTemp, locSalt,
     I                       bi,bj,iMin,iMax,jMin,jMax,
     I                       k,myThid)
#endif

        debugPrt = debugMode
        DO j=jmin,jmax
         DO i=imin,imax

          IF ( maskC(i,j,k,bi,bj).EQ.oneRS ) THEN
#ifdef CARBONCHEM_SOLVESAPHE
C calcium concentration already calculated in S/R DIC_COEFFS_SURF
           calcium = cat(i,j,bi,bj)
#else
           calcium = 1.028 _d -2*salt(i,j,k,bi,bj)/35. _d 0
#endif

C SilicaDeep filled with constant value of 0.03 mol/m3 unless
C     DIC_deepSilicaFile provided.
           sitlocal = silicaDeep(i,j,k,bi,bj)
           po4local = PTR_PO4(i,j,k)
           diclocal = PTR_DIC(i,j,k)
           alklocal = PTR_ALK(i,j,k)

#ifdef CARBONCHEM_SOLVESAPHE
           IF ( selectPHsolver.GT.0 ) THEN
C Use Munhoven (2013) Solvesaphe routine to calculate pH and pCO2
            IF ( .NOT.pH_isLoaded(2) ) THEN
#ifdef ALLOW_DEBUG
               IF (debugPrt) CALL DEBUG_CALL(
     &                      'AHINI_FOR_AT',myThid)
#endif
C call AHINI_FOR_AT to get better initial guess of pH
               CALL AHINI_FOR_AT(alklocal*permil,
     I              diclocal*permil,
     I              bt(i,j,bi,bj),
     O              pH3d(i,j,k,bi,bj),
     I              i,j,k,bi,bj,myIter,myThid )
            ENDIF /* 3d pH is not loaded */
#ifdef ALLOW_DEBUG
            IF (debugPrt) CALL DEBUG_CALL('CALC_PCO2_SOLVESAPHE',myThid)
#endif
            CALL CALC_PCO2_SOLVESAPHE(
     I          locTemp(i,j), locSalt(i,j),
     I          diclocal, po4local,
     I          sitlocal, alklocal,
     U          pH3d(i,j,k,bi,bj), pCO2local(i,j), carbonate(i,j),
     I          i,j,k,bi,bj, debugPrt,myIter,myThid )

C- convert carbonate to mol kg^-1-SW for calculation of saturation state
            carbonate(i,j) = carbonate(i,j)*permil
           ELSE
C Use the original Follows et al. (2006) solver
#endif /* CARBONCHEM_SOLVESAPHE */
#ifdef ALLOW_DEBUG
            IF (debugPrt) CALL DEBUG_CALL('CALC_PCO2_APPROX',myThid)
#endif
            IF ( .NOT.pH_isLoaded(2) ) THEN
C    Since we start cold with an arbitrary pH, we must iterate pH solver
C    to ensure accurate estimate of co3 at depth
C    Because we may call this for deep ocean infrequently can afford to make
C    several iterations
               doIterCO3 = nIterCO3
            ELSE
               doIterCO3 = 1
            ENDIF

            DO CO3iter = 1, doIterCO3
              CALL CALC_PCO2_APPROX(
     I          locTemp(i,j), locSalt(i,j),
     I          diclocal, po4local,
     I          sitlocal, alklocal,
     I          ak1(i,j,bi,bj),ak2(i,j,bi,bj),
     I          ak1p(i,j,bi,bj),ak2p(i,j,bi,bj),ak3p(i,j,bi,bj),
     I          aks(i,j,bi,bj),akb(i,j,bi,bj),akw(i,j,bi,bj),
     I          aksi(i,j,bi,bj),akf(i,j,bi,bj),
     I          ak0(i,j,bi,bj), fugf(i,j,bi,bj), ff(i,j,bi,bj),
     I          bt(i,j,bi,bj),st(i,j,bi,bj),ft(i,j,bi,bj),
     U          pH3d(i,j,k,bi,bj), pCO2local(i,j), carbonate(i,j),
     I          i,j,k,bi,bj,myIter,myThid )
            ENDDO
#ifdef CARBONCHEM_SOLVESAPHE
           ENDIF
#endif /* CARBONCHEM_SOLVESAPHE */

           omegaC(i,j,k,bi,bj) = calcium * carbonate(i,j)
     &                         / Ksp_TP_Calc(i,j,bi,bj)
           debugPrt = .FALSE.

Cmick...................................................
c            if(omegaC(i,j,k,bi,bj) .eq. 0.) then
c             if(i .eq. 76 .and. j .eq. 36  .and. k .eq. 15) then
c               write(6,*)'i,j,k,KS,CO3,pHCa,T,S,hfacc,omega',
c     &                 i,j,k,
c     &                 Ksp_TP_Calc(i,j,bi,bj),
c     &                 carbonate(i,j), calcium, pH3d(i,j,k,bi,bj),
c     &                 locTemp(i,j), locSalt(i,j),
c     &                 hfacc(i,j,k,bi,bj),omegaC(i,j,k,bi,bj)
c              write(6,*)'Ksp_TP_Calc',
c     &                 Ksp_TP_Calc(i,j,bi,bj)
c               write(6,*)'dic, alk, po4 ',
c     &                 diclocal, alklocal,po4local
c               write(6,*)'k1, k2, k1p, k2p, k3p ',
c     &                 ak1(i,j,bi,bj),ak2(i,j,bi,bj),
c     &                ak1p(i,j,bi,bj),ak2p(i,j,bi,bj),ak3p(i,j,bi,bj)
c               write(6,*)'ks, kb, kw, ksi ',
c     &                aks(i,j,bi,bj),akb(i,j,bi,bj),akw(i,j,bi,bj),
c     &                aksi(i,j,bi,bj)
c               write(6,*)'akf, ff, bt, st, ft ',
c     &                akf(i,j,bi,bj),ff(i,j,bi,bj),
c     &                bt(i,j,bi,bj),st(i,j,bi,bj),ft(i,j,bi,bj)
c             end if
Cmick....................................................
          ELSE
C-        else (case maskC = 0 , dry point)
           omegaC(i,j,k,bi,bj) = 0. _d 0
          ENDIF

         ENDDO
        ENDDO

#ifdef ALLOW_DIAGNOSTICS
C Fill up pCO2 and carbonate diagnostics from local arrays
        IF ( useDiagnostics ) THEN
         CALL DIAGNOSTICS_FILL(pCO2local,'DIC3DPCO',k,1,2,bi,bj,myThid)
         CALL DIAGNOSTICS_FILL(carbonate,'DIC3DCO3',k,1,2,bi,bj,myThid)
        ENDIF
#endif /* ALLOW_DIAGNOSTICS */

C-     end k loop
       ENDDO

#ifdef ALLOW_DIAGNOSTICS
C Fill up 3d pH, omegaC and silicate diagnostics
       IF ( useDiagnostics ) THEN
        CALL DIAGNOSTICS_FILL(pH3d      ,'DIC3DPH ',0,Nr,1,bi,bj,myThid)
        CALL DIAGNOSTICS_FILL(omegaC    ,'OMEGAC  ',0,Nr,1,bi,bj,myThid)
        CALL DIAGNOSTICS_FILL(silicaDeep,'DIC3DSIT',0,Nr,1,bi,bj,myThid)
       ENDIF
#endif /* ALLOW_DIAGNOSTICS */

#endif /* DIC_BIOTIC and DIC_CALCITE_SAT */
       RETURN
       END

#include "SEAICE_OPTIONS.h"
#ifdef ALLOW_EXF
# include "EXF_OPTIONS.h"
#endif
#ifdef ALLOW_AUTODIFF
# include "AUTODIFF_OPTIONS.h"
#endif

CBOP
C     !ROUTINE: SEAICE_REG_RIDGE
C     !INTERFACE:
      SUBROUTINE SEAICE_REG_RIDGE( myTime, myIter, myThid )
C     !DESCRIPTION: \bv
C     *=================================================================*
C     | SUBROUTINE seaice_reg_ridge
C     | o this routine has two purposes:
C     |   (1) clean up after advection (undershoots etc.);
C     |       after advection, the sea ice variables may have unphysical
C     |       values, e.g. < 0 or very thin ice, that are regularized
C     |       here.
C     |   (2) driver for ice ridging;
C     |       concentration as a special case may be > 1 in convergent
C     |       motion and a ridging algorithm redistributes the ice to
C     |       limit the concentration to 1.
C     | o called after S/R seaice_advdiff
C     *=================================================================*
C     \ev

C     !USES:
      IMPLICIT NONE
C     === Global variables ===
#include "SIZE.h"
#include "EEPARAMS.h"
#include "PARAMS.h"
#include "SEAICE_SIZE.h"
#include "SEAICE_PARAMS.h"
#include "SEAICE.h"
#include "SEAICE_TRACER.h"
#ifdef ALLOW_EXF
# include "EXF_FIELDS.h"
#endif
#ifdef ALLOW_AUTODIFF_TAMC
# include "tamc.h"
#endif /* ALLOW_AUTODIFF_TAMC */

C     !INPUT/OUTPUT PARAMETERS:
C     === Routine arguments ===
C     myTime :: Simulation time
C     myIter :: Simulation timestep number
C     myThid :: Thread no. that called this routine.
      _RL myTime
      INTEGER myIter, myThid

#ifdef ALLOW_SEAICE
C     !LOCAL VARIABLES:
C     === Local variables ===
C     i,j,bi,bj :: Loop counters
      INTEGER i, j, bi, bj
C     number of surface interface layer
C     IT :: ice thickness category index (ITD and SEAICE_multDim code)
      INTEGER IT
C     reciprocal of time step
      _RL recip_deltaTtherm
C     temporary variables available for the various computations
      _RL tmpscal1, tmpscal2
#ifdef SEAICE_ITD
      _RL tmpscal1itd(1:sNx,1:sNy), tmpscal2itd(1:sNx,1:sNy)
      _RL tmpscal3itd(1:sNx,1:sNy)
C     reciprocal number of ice classes nITD
      _RL recip_nitd
#endif /* SEAICE_ITD */
#ifdef ALLOW_DIAGNOSTICS
C     Helper variables for diagnostics
      _RL DIAGarrayA    (1:sNx,1:sNy)
#endif /* ALLOW_DIAGNOSTICS */
#ifdef ALLOW_AUTODIFF_TAMC
      INTEGER act1, act2, act3, act4
      INTEGER max1, max2, max3
      INTEGER iicekey
#endif
CEOP

C
C     === Routine body ===
C
      recip_deltaTtherm = ONE / SEAICE_deltaTtherm

      DO bj=myByLo(myThid),myByHi(myThid)
       DO bi=myBxLo(myThid),myBxHi(myThid)

#ifdef ALLOW_AUTODIFF_TAMC
        act1 = bi - myBxLo(myThid)
        max1 = myBxHi(myThid) - myBxLo(myThid) + 1
        act2 = bj - myByLo(myThid)
        max2 = myByHi(myThid) - myByLo(myThid) + 1
        act3 = myThid - 1
        max3 = nTx*nTy
        act4 = ikey_dynamics - 1
        iicekey = (act1 + 1) + act2*max1
     &                       + act3*max1*max2
     &                       + act4*max1*max2*max3
#endif /* ALLOW_AUTODIFF_TAMC */

        DO J=1-OLy,sNy+OLy
         DO I=1-OLx,sNx+OLx
          d_HEFFbyNEG(I,J,bi,bj)    = 0.0 _d 0
          d_HSNWbyNEG(I,J,bi,bj)    = 0.0 _d 0
#ifdef EXF_SEAICE_FRACTION
          d_AREAbyRLX(I,J,bi,bj)    = 0.0 _d 0
          d_HEFFbyRLX(I,J,bi,bj)    = 0.0 _d 0
#endif /* EXF_SEAICE_FRACTION */
#ifdef SEAICE_VARIABLE_SALINITY
          saltFluxAdjust(I,J,bi,bj) = 0.0 _d 0
#endif /* SEAICE_VARIABLE_SALINITY */
         ENDDO
        ENDDO

C =====================================================================
C ========== PART 1: treat pathological cases (post advdiff) ==========
C =====================================================================

#if (defined ALLOW_AUTODIFF_TAMC && defined SEAICE_MODIFY_GROWTH_ADJ)
Cgf no dependency through pathological cases treatment
        IF ( SEAICEadjMODE.EQ.0 ) THEN
#endif

#ifdef EXF_SEAICE_FRACTION
CADJ STORE heff(:,:,bi,bj) = comlev1_bibj, key = iicekey,byte=isbyte
CADJ STORE area(:,:,bi,bj) = comlev1_bibj, key = iicekey,byte=isbyte
C--   (0) relax sea ice concentration towards observation
        IF ( SEAICE_tauAreaObsRelax .GT. zeroRL ) THEN
         DO J=1,sNy
          DO I=1,sNx
           IF ( exf_iceFraction(I,J,bi,bj).GT.AREA(I,J,bi,bj) ) THEN
            d_AREAbyRLX(i,j,bi,bj) =
     &       SEAICE_deltaTtherm/SEAICE_tauAreaObsRelax
     &       * (exf_iceFraction(I,J,bi,bj) - AREA(I,J,bi,bj))
           ENDIF
           IF ( exf_iceFraction(I,J,bi,bj).GT.zeroRS .AND.
     &          AREA(I,J,bi,bj).EQ.0. _d 0) THEN
C           d_HEFFbyRLX(i,j,bi,bj) = 1. _d 1 * siEps * d_AREAbyRLX(i,j,bi,bj)
            d_HEFFbyRLX(i,j,bi,bj) = 1. _d 1 * siEps
           ENDIF
#ifdef SEAICE_ITD
           AREAITD(I,J,1,bi,bj) = AREAITD(I,J,1,bi,bj)
     &                          +  d_AREAbyRLX(i,j,bi,bj)
           HEFFITD(I,J,1,bi,bj) = HEFFITD(I,J,1,bi,bj)
     &                          +  d_HEFFbyRLX(i,j,bi,bj)
#endif /* SEAICE_ITD */
           AREA(I,J,bi,bj) = AREA(I,J,bi,bj) +  d_AREAbyRLX(i,j,bi,bj)
           HEFF(I,J,bi,bj) = HEFF(I,J,bi,bj) +  d_HEFFbyRLX(i,j,bi,bj)
          ENDDO
         ENDDO
        ENDIF
#endif /* EXF_SEAICE_FRACTION */

C--   (1) treat the case of negative values:

#ifdef SEAICE_ITD
        DO IT=1,SEAICE_multDim
         DO J=1,sNy
          DO I=1,sNx
           tmpscal1=0. _d 0
           tmpscal2=0. _d 0
           tmpscal1=MAX(-HEFFITD(I,J,IT,bi,bj),0. _d 0)
           HEFFITD(I,J,IT,bi,bj)=HEFFITD(I,J,IT,bi,bj)+tmpscal1
           d_HEFFbyNEG(I,J,bi,bj)=d_HEFFbyNEG(I,J,bi,bj)+tmpscal1
           tmpscal2=MAX(-HSNOWITD(I,J,IT,bi,bj),0. _d 0)
           HSNOWITD(I,J,IT,bi,bj)=HSNOWITD(I,J,IT,bi,bj)+tmpscal2
           d_HSNWbyNEG(I,J,bi,bj)=d_HSNWbyNEG(I,J,bi,bj)+tmpscal2
           AREAITD(I,J,IT,bi,bj)=MAX(AREAITD(I,J,IT,bi,bj),0. _d 0)
C     AREA, HEFF, and HSNOW will be updated at end of PART 1
C     by calling SEAICE_ITD_SUM
          ENDDO
         ENDDO
        ENDDO
C     update mean thicknesses HEFF and HSNOW and total ice
C     concentration AREA to match single category values
        CALL SEAICE_ITD_SUM   ( bi, bj, myTime, myIter, myThid )
#else /* ndef SEAICE_ITD */
#ifdef ALLOW_AUTODIFF_TAMC
CADJ STORE heff(:,:,bi,bj)  = comlev1_bibj, key = iicekey,byte=isbyte
CADJ STORE hsnow(:,:,bi,bj) = comlev1_bibj, key = iicekey,byte=isbyte
CADJ STORE area(:,:,bi,bj)  = comlev1_bibj, key = iicekey,byte=isbyte
#endif /* ALLOW_AUTODIFF_TAMC */
        DO J=1,sNy
         DO I=1,sNx
          d_HEFFbyNEG(I,J,bi,bj)=MAX(-HEFF(I,J,bi,bj),0. _d 0)
          HEFF(I,J,bi,bj)=HEFF(I,J,bi,bj)+d_HEFFbyNEG(I,J,bi,bj)
          d_HSNWbyNEG(I,J,bi,bj)=MAX(-HSNOW(I,J,bi,bj),0. _d 0)
          HSNOW(I,J,bi,bj)=HSNOW(I,J,bi,bj)+d_HSNWbyNEG(I,J,bi,bj)
          AREA(I,J,bi,bj)=MAX(AREA(I,J,bi,bj),0. _d 0)
         ENDDO
        ENDDO
#endif /* SEAICE_ITD */

C--   (2) treat the case of very thin ice:

#ifdef SEAICE_ITD
C     Here we risk that even though HEFF may be larger than siEps (=1e-5)
C     HEFFITD can have classes with very small (< siEps) non-zero ice volume.
C     We avoid applying the correction to each class because that leads to
C     funny structures in the net heat and freshwater flux into the ocean.
C     Let us keep our fingers crossed, that the model will be benign!
        DO IT=1,SEAICE_multDim
         DO J=1,sNy
          DO I=1,sNx
           IF (HEFF(I,J,bi,bj).LE.siEps) THEN
            HEFFITD(I,J,IT,bi,bj) = 0. _d 0
            HSNOWITD(I,J,IT,bi,bj) = 0. _d 0
           ENDIF
          ENDDO
         ENDDO
        ENDDO
#endif /* SEAICE_ITD */
#ifdef ALLOW_AUTODIFF_TAMC
CADJ STORE heff(:,:,bi,bj) = comlev1_bibj, key = iicekey,byte=isbyte
C     The following statement helps TAF to understand that TICES is not
C     modified/reset everywhere so that it is correctly stored in the
C     outer checkpoint levels. This is a TAF bug and this statement can
C     probably be removed again, once this bug is fixed (but it does not
C     hurt, either).
CADJ INCOMPLETE TICES
#endif /* ALLOW_AUTODIFF_TAMC */
        DO J=1,sNy
         DO I=1,sNx
          tmpscal1=0. _d 0
          tmpscal2=0. _d 0
          IF (HEFF(I,J,bi,bj).LE.siEps) THEN
           tmpscal1=-HEFF(I,J,bi,bj)
           tmpscal2=-HSNOW(I,J,bi,bj)
           DO IT=1,SEAICE_multDim
            TICES(I,J,IT,bi,bj)=celsius2K
           ENDDO
          ENDIF
          HEFF(I,J,bi,bj)=HEFF(I,J,bi,bj)+tmpscal1
          HSNOW(I,J,bi,bj)=HSNOW(I,J,bi,bj)+tmpscal2
          d_HEFFbyNEG(I,J,bi,bj)=d_HEFFbyNEG(I,J,bi,bj)+tmpscal1
          d_HSNWbyNEG(I,J,bi,bj)=d_HSNWbyNEG(I,J,bi,bj)+tmpscal2
         ENDDO
        ENDDO

C--   (3) treat the case of area but no ice/snow:

#ifdef SEAICE_ITD
        DO IT=1,SEAICE_multDim
         DO J=1,sNy
          DO I=1,sNx
           IF ( (HEFFITD(I,J,IT,bi,bj) .EQ.0. _d 0).AND.
     &          (HSNOWITD(I,J,IT,bi,bj).EQ.0. _d 0))
     &          AREAITD(I,J,IT,bi,bj)=0. _d 0
          ENDDO
         ENDDO
        ENDDO
#else /* ndef SEAICE_ITD */
#ifdef ALLOW_AUTODIFF_TAMC
CADJ STORE heff(:,:,bi,bj)  = comlev1_bibj, key = iicekey,byte=isbyte
CADJ STORE hsnow(:,:,bi,bj) = comlev1_bibj, key = iicekey,byte=isbyte
#endif /* ALLOW_AUTODIFF_TAMC */
         DO J=1,sNy
          DO I=1,sNx
           IF ((HEFF(i,j,bi,bj).EQ.0. _d 0).AND.
     &        (HSNOW(i,j,bi,bj).EQ.0. _d 0)) AREA(I,J,bi,bj)=0. _d 0
         ENDDO
        ENDDO
#endif /* SEAICE_ITD */

C--   (4) treat the case of very small area:

#ifndef DISABLE_AREA_FLOOR
#ifdef SEAICE_ITD
        recip_nitd = 1. _d 0 / float(SEAICE_multDim)
        DO IT=1,SEAICE_multDim
         DO J=1,sNy
          DO I=1,sNx
           IF ((HEFFITD(I,J,IT,bi,bj).GT.0).OR.
     &          (HSNOWITD(I,J,IT,bi,bj).GT.0)) THEN
C     SEAICE_area_floor*SEAICE_multDim cannot be allowed to exceed 1
C     hence use SEAICE_area_floor devided by SEAICE_multDim
C     (or install a warning in e.g. seaice_readparms.F)
            AREAITD(I,J,IT,bi,bj)=
     &        MAX(AREAITD(I,J,IT,bi,bj),SEAICE_area_floor*recip_nitd)
          ENDIF
         ENDDO
        ENDDO
       ENDDO
#else /* ndef SEAICE_ITD */
#ifdef ALLOW_AUTODIFF_TAMC
CADJ STORE area(:,:,bi,bj)  = comlev1_bibj, key = iicekey,byte=isbyte
#endif /* ALLOW_AUTODIFF_TAMC */
       DO J=1,sNy
        DO I=1,sNx
         IF ((HEFF(i,j,bi,bj).GT.0).OR.(HSNOW(i,j,bi,bj).GT.0)) THEN
          AREA(I,J,bi,bj)=MAX(AREA(I,J,bi,bj),SEAICE_area_floor)
         ENDIF
        ENDDO
       ENDDO
#endif /* SEAICE_ITD */
#endif /* DISABLE_AREA_FLOOR */

C     (5) treat sea ice salinity pathological cases
#ifdef SEAICE_VARIABLE_SALINITY
#ifdef ALLOW_AUTODIFF_TAMC
CADJ STORE hsalt(:,:,bi,bj)  = comlev1_bibj, key = iicekey,byte=isbyte
CADJ STORE heff(:,:,bi,bj)  = comlev1_bibj, key = iicekey,byte=isbyte
#endif /* ALLOW_AUTODIFF_TAMC */
        DO J=1,sNy
         DO I=1,sNx
          IF ( (HSALT(I,J,bi,bj) .LT. 0.0).OR.
     &         (HEFF(I,J,bi,bj) .EQ. 0.0)  ) THEN
             saltFluxAdjust(I,J,bi,bj) = - HEFFM(I,J,bi,bj) *
     &            HSALT(I,J,bi,bj) * recip_deltaTtherm
             HSALT(I,J,bi,bj) = 0.0 _d 0
          ENDIF
         ENDDO
        ENDDO
#endif /* SEAICE_VARIABLE_SALINITY */

C =====================================================================
C ========== PART 2: ridging algorithm  ===============================
C =====================================================================

C     treat case of excessive ice cover, e.g., due to ridging:

#ifdef SEAICE_ITD

C     catch up with item (2) that involves category sums AREA and HEFF
        DO J=1,sNy
         DO I=1,sNx
          tmpscal1itd(i,j) = 0. _d 0
          tmpscal2itd(i,j) = 0. _d 0
          tmpscal3itd(i,j) = 0. _d 0
         ENDDO
        ENDDO
        DO IT=1,SEAICE_multDim
         DO J=1,sNy
          DO I=1,sNx
C     TICES was changed above (item 2), now update TICE as ice volume
C     weighted average of TICES
           tmpscal1itd(i,j)=tmpscal1itd(i,j)
     &          +        TICES(I,J,IT,bi,bj) * HEFFITD(I,J,IT,bi,bj)
           tmpscal2itd(i,j)=tmpscal2itd(i,j) + HEFFITD(I,J,IT,bi,bj)
C     also compute total of AREAITD for diagnostics and SItrArea
           tmpscal3itd(i,j)=tmpscal3itd(i,j) + AREAITD(I,J,IT,bi,bj)
          ENDDO
         ENDDO
        ENDDO
        DO J=1,sNy
         DO I=1,sNx
C     save pre-ridging ice concentration for diagnostics:
C     these lines are executed before "ridging" is applied to AREA
C     hence we execute them here before SEAICE_ITD_REDIST is called
C     although this means that AREA has not been completely regularized
#ifdef ALLOW_DIAGNOSTICS
          DIAGarrayA(I,J) = tmpscal3itd(i,j)
#endif
#ifdef ALLOW_SITRACER
          SItrAREA(I,J,bi,bj,1)=tmpscal3itd(i,j)
#endif
         ENDDO
        ENDDO
C     ridge ice according to Lipscomb et al. (2007), Bitz et al. (2001)
C     Thorndyke et al. (1975), Hibler (1980)
        CALL SEAICE_DO_RIDGING( bi, bj, myTime, myIter, myThid )
C     check that all ice thickness categories meet their limits
C     (includes Hibler-type ridging)
        CALL SEAICE_ITD_REDIST( bi, bj, myTime, myIter, myThid )
C     update mean thicknesses HEFF and HSNOW and total ice
C     concentration AREA to match single category values
        CALL SEAICE_ITD_SUM   ( bi, bj, myTime, myIter, myThid )

#else /* ifndef SEAICE_ITD */

#ifdef ALLOW_AUTODIFF_TAMC
CADJ STORE area(:,:,bi,bj)  = comlev1_bibj, key = iicekey,byte=isbyte
#endif /* ALLOW_AUTODIFF_TAMC */
        DO J=1,sNy
         DO I=1,sNx
C     save pre-ridging ice concentration for diagnostics
#ifdef ALLOW_DIAGNOSTICS
          DIAGarrayA(I,J) = AREA(I,J,bi,bj)
#endif /*  ALLOW_DIAGNOSTICS */
#ifdef ALLOW_SITRACER
          SItrAREA(I,J,bi,bj,1)=AREA(I,J,bi,bj)
#endif /*  ALLOW_SITRACER */
C     this is the simple Hibler (1979)-type ridging (capping of
C     concentrations > 1) for the non-ITD sea ice model
          AREA(I,J,bi,bj)=MIN(AREA(I,J,bi,bj),SEAICE_area_max)
         ENDDO
        ENDDO

#endif /* SEAICE_ITD */

#if (defined ALLOW_AUTODIFF_TAMC && defined SEAICE_MODIFY_GROWTH_ADJ)
C        end SEAICEadjMODE.EQ.0 statement:
        ENDIF
#endif

#ifdef ALLOW_DIAGNOSTICS
        IF ( useDiagnostics ) THEN
         CALL DIAGNOSTICS_FILL(DIAGarrayA,'SIareaPR',0,1,3,bi,bj,myThid)
        ENDIF
#endif /* ALLOW_DIAGNOSTICS */

C close bi,bj loops
       ENDDO
      ENDDO

#endif /* ALLOW_SEAICE */
      RETURN
      END

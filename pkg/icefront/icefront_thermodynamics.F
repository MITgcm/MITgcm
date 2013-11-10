C $Header: /u/gcmpack/MITgcm/pkg/icefront/icefront_thermodynamics.F,v 1.10 2013/11/10 02:58:34 yunx Exp $
C $Name:  $

#include "ICEFRONT_OPTIONS.h"

CBOP
C     !ROUTINE: ICEFRONT_THERMODYNAMICS
C     !INTERFACE:
      SUBROUTINE ICEFRONT_THERMODYNAMICS(
     I                        myTime, myIter, myThid )
C     !DESCRIPTION: \bv
C     *=============================================================*
C     | S/R  ICEFRONT_THERMODYNAMICS
C     | o shelf-ice main routine.
C     |   compute temperature and (virtual) salt flux at the
C     |   shelf-ice ocean interface
C     |
C     | stresses at the ice/water interface are computed in separate
C     | routines that are called from mom_fluxform/mom_vecinv
C     *=============================================================*

C     !USES:
      IMPLICIT NONE

C     === Global variables ===
#include "SIZE.h"
#include "EEPARAMS.h"
#include "PARAMS.h"
#include "GRID.h"
#include "DYNVARS.h"
#include "FFIELDS.h"
#include "ICEFRONT.h"

C     !INPUT/OUTPUT PARAMETERS:
C     === Routine arguments ===
C     myIter :: iteration counter for this thread
C     myTime :: time counter for this thread
C     myThid :: thread number for this instance of the routine.
      _RL  myTime
      INTEGER myIter
      INTEGER myThid
CEOP

#ifdef ALLOW_ICEFRONT
C     !LOCAL VARIABLES :
C     === Local variables ===
C     I,J,K,bi,bj      :: loop counters
C     tLoc, sLoc, pLoc :: local in-situ temperature, salinity, pressure
C     thetaICE         :: averaged temperature of glacier interior
C     theta/saltFreeze :: temperature and salinity of water at the
C                         ice-ocean interface (at the freezing point)
C     FreshWaterFlux   :: fresh water flux due to freezing or melting of ice
C                         front in kg/m^2/s (positive increases ocean salinity)
C     HeatFlux         :: ice front heat flux in W/m^2
C                         (positive decreases ocean temperature)
C     auxiliary variables and abbreviations:
C     a0, b, c0
C     eps1, eps2, eps3, eps4, eps5, eps6, eps7
C     aqe, bqe, cqe, discrim, recip_aqe
      INTEGER I,J,K
      INTEGER bi,bj
      _RL tLoc, sLoc, pLoc
      _RL thetaICE
      _RL thetaFreeze, saltFreeze
      _RS FreshWaterFlux( 1:sNx, 1:sNy )
      _RS HeatFlux      ( 1:sNx, 1:sNy )
      _RS ICEFRONTheatTransCoeff ( 1:sNx, 1:sNy )
      _RS ICEFRONTsaltTransCoeff ( 1:sNx, 1:sNy )
      _RL a0, b, c0
      _RL eps1, eps2, eps3, eps4, eps5, eps6, eps7
      _RL aqe, bqe, cqe, discrim, recip_aqe


      _RL SW_TEMP
      EXTERNAL SW_TEMP

C---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|

C     Linear dependence of freezing point on salinity.
      a0 = -0.0575   _d  0
      c0 =  0.0901   _d  0
      b  =  -7.61    _d -4


      DO bj = myByLo(myThid), myByHi(myThid)
       DO bi = myBxLo(myThid), myBxHi(myThid)
        DO K = 1, Nr
         DO J = 1, sNy
          DO I = 1, sNx

C     Calculate ICEFRONTheatTransCoeff & ICEFRONTsaltTransCoeff
           IF( ICEFRONTlength(I,J,bi,bj) .GT. 0. _d 0
     &          .AND. K .LE. K_icefront(I,J,bi,bj) ) THEN
            ICEFRONTheatTransCoeff(I,J) = 1.0 _d -02
     &                                  *abs(wVEL(I,J,K,bi,bj))
     &                                  *sqrt(1.5 _d -03)
            ICEFRONTheatTransCoeff(I,J) = max
     &                          (ICEFRONTheatTransCoeff(I,J),1. _d -04)
            ICEFRONTsaltTransCoeff(I,J) = 5.05 _d -3
     &                                  *ICEFRONTheatTransCoeff(I,J)

C     A few abbreviations.
      eps1 = rUnit2mass*HeatCapacity_Cp*ICEFRONTheatTransCoeff(I,J)
      eps2 = rUnit2mass*ICEFRONTlatentHeat*ICEFRONTsaltTransCoeff(I,J)
      eps3 = rUnit2mass*ICEFRONTheatCapacity_Cp
     &       *ICEFRONTsaltTransCoeff(I,J)
      eps5 = mass2rUnit/HeatCapacity_Cp
      aqe  = a0  *(-eps1+eps3)
      recip_aqe = 0.5 _d 0/aqe

C     Make local copies of temperature, salinity and depth (pressure).
            pLoc = ABS(rC(k))
            tLoc = theta(I,J,K,bi,bj)
            sLoc = MAX(salt(I,J,K,bi,bj), 0. _d 0)

C     Turn potential temperature into in-situ temperature.
            tLoc = SW_TEMP(sLoc,tLoc,pLoc,0.D0)

C     Get ice temperature. Assume linear ice temperature change from 
C     the surface (ICEFRONTthetaSurface) to the base(0 degree C).
            IF ( K .EQ. K_icefront(I,J,bi,bj)) THEN
             pLoc = 0.5*(ABS(R_icefront(I,J,bi,bj))+ABS(rF(K)))
            ENDIF
            thetaICE = ICEFRONTthetaSurface*
     &           (R_icefront(I,J,bi,bj)-pLoc) 
     &           / R_icefront(I,J,bi,bj)

C     A few more abbreviations.
            eps4 = b*pLoc + c0
            eps6 = eps4 - tLoc
            eps7 = eps4 - thetaIce

C     Solve quadratic equation to get salinity at icefront-ocean interface.
            bqe = - eps1*eps6 -sLoc*a0*eps3 + eps3*eps7 + eps2
            cqe = -(eps2+eps3*eps7)*sLoc
            discrim = bqe*bqe - 4. _d 0*aqe*cqe
            saltFreeze = (- bqe - SQRT(discrim))*recip_aqe
            IF ( saltFreeze .LT. 0. _d 0 )
     &           saltFreeze = (- bqe + SQRT(discrim))*recip_aqe
            thetaFreeze = a0*saltFreeze + eps4

C--   Calculate the outward (leaving the ocean) heat (W/m^2)
C     and freshwater (kg/m^2/s).
C     Sign convention: inward (negative) fresh water flux implies glacier
C     melting due to outward (positive) heat flux.
            FreshWaterFlux(I,J) = maskC(I,J,K,bi,bj) *
     &           eps1 * ( thetaFreeze - tLoc ) /
     &           (ICEFRONTlatentHeat + ICEFRONTheatCapacity_cp*
     &           (thetaFreeze - thetaIce))
            HeatFlux(I,J) = maskC(I,J,K,bi,bj) * HeatCapacity_Cp *
     &           ( -rUnit2mass*ICEFRONTheatTransCoeff(I,J) +
     &           FreshWaterFlux(I,J) ) * ( thetaFreeze - tLoc )

C     Compute tendencies.
            icefront_TendT(i,j,K,bi,bj) = - HeatFlux(I,J)* eps5
            icefront_TendS(i,j,K,bi,bj) = FreshWaterFlux(I,J) *
     &           mass2rUnit * sLoc

C     Scale by icefrontlength, which is the ratio of the horizontal length
C     of the ice front in each model grid cell divided by the grid cell area.
            IF (k .LT. k_icefront(i,j,bi,bj)) THEN  
             icefront_TendT(i,j,K,bi,bj) = icefront_TendT(i,j,K,bi,bj)
     &            * ICEFRONTlength(i,j,bi,bj)
             icefront_TendS(i,j,K,bi,bj) = icefront_TendS(i,j,K,bi,bj)
     &            * ICEFRONTlength(i,j,bi,bj)
            ELSEIF (k .EQ. k_icefront(i,j,bi,bj)) THEN
C     At the bottom of the ice shelf there is additional scaling due
C     to the partial depth of the ice front.
             icefront_TendT(i,j,K,bi,bj) = icefront_TendT(i,j,K,bi,bj)
     &            * ICEFRONTlength(i,j,bi,bj)
     &            * (ABS(R_icefront(I,J,bi,bj))-ABS(rF(K)))
     &            * recip_drF(K)
             icefront_TendS(i,j,K,bi,bj) = icefront_TendS(i,j,K,bi,bj)
     &            * ICEFRONTlength(i,j,bi,bj)
     &            * (ABS(R_icefront(I,J,bi,bj))-ABS(rF(K)))
     &            * recip_drF(K)
            ENDIF

           ELSE                 ! K .LE. K_icefront

            HeatFlux      (I,J) = 0. _d 0
            FreshWaterFlux(I,J) = 0. _d 0

           ENDIF                ! K .LE. K_icefront

          ENDDO                 ! I = 1, sNx
         ENDDO                  ! J = 1, sNy

#ifdef ALLOW_DIAGNOSTICS
         IF ( useDiagnostics ) THEN
          CALL DIAGNOSTICS_FILL_RS(FreshWaterFlux,'ICFfwFlx',
     &         k,1,3,bi,bj,myThid)
          CALL DIAGNOSTICS_FILL_RS(HeatFlux,      'ICFhtFlx',
     &         k,1,3,bi,bj,myThid)
         ENDIF
#endif /* ALLOW_DIAGNOSTICS */

        ENDDO                   ! K = 1, Nr
       ENDDO                    ! bi = myBxLo, myBxHi
      ENDDO                     ! bj = myByLo, myByHi

#endif /* ALLOW_ICEFRONT */
      RETURN
      END

C $Header: /u/gcmpack/MITgcm/pkg/shelfice/shelfice_thermodynamics.F,v 1.12 2007/10/10 14:43:36 mlosch Exp $
C $Name:  $

#include "SHELFICE_OPTIONS.h"
 
CBOP
C     !ROUTINE: SHELFICE_THERMODYNAMICS
C     !INTERFACE:
      SUBROUTINE SHELFICE_THERMODYNAMICS( 
     I                        myTime, myIter, myThid )
C     !DESCRIPTION: \bv
C     *=============================================================*
C     | S/R  SHELFICE_THERMODYNAMICS             
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
#include "SHELFICE.h"
 
C     !INPUT/OUTPUT PARAMETERS:
C     === Routine arguments ===
C     myIter :: iteration counter for this thread
C     myTime :: time counter for this thread
C     myThid :: thread number for this instance of the routine.
      _RL  myTime
      INTEGER myIter
      INTEGER myThid
CEOP

#ifdef ALLOW_SHELFICE
C     !LOCAL VARIABLES :
C     === Local variables ===
C     I,J,K,Kp1,bi,bj  :: loop counters
C     tLoc, sLoc, pLoc :: local in-situ temperature, salinity, pressure
C     theta/saltFreeze :: temperature and salinity of water at the 
C                         ice-ocean interface (at the freezing point)
C     freshWaterFlux   :: local variable for fresh water melt flux due to 
C                         melting in kg/m^2/s (negative density x melt rate) 
C     cFac             :: 1 for conservative form, 0, otherwise
C     auxiliary variables and abbreviations:
C     a0, a1, a2, b, c0
C     eps1, eps2, eps3, eps4, eps5, eps6, eps7
C     atmp, btmp, aqe, bqe, cqe, discrim, recip_aqe
C     drKp1, recip_drLoc
      INTEGER I,J,K,Kp1
      INTEGER bi,bj
      _RL tLoc, sLoc, pLoc
      _RL thetaFreeze, saltFreeze
      _RL freshWaterFlux
      _RL a0, a1, a2, b, c0
      _RL eps1, eps2, eps3, eps4, eps5, eps6, eps7
      _RL cFac
      _RL atmp, btmp
      _RL aqe, bqe, cqe, discrim, recip_aqe
      _RL drKp1, recip_drLoc

C---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|

C     linear dependence of freezing point on salinity
      a0 = -0.0575   _d  0
      a1 =  0.0      _d -0
      a2 =  0.0      _d -0
      c0 =  0.0901   _d  0
      b  =  -7.61    _d -4
#ifdef ALLOW_ISOMIP_TD
      IF ( useISOMIPTD ) THEN
C     non-linear dependence of freezing point on salinity
       a0 = -0.0575   _d  0
       a1 = 1.710523  _d -3
       a2 = -2.154996 _d -4
       b  = -7.53     _d -4
       c0 = 0. _d 0
      ENDIF
#endif ALLOW_ISOMIP_TD

      DO bj = myByLo(myThid), myByHi(myThid)
       DO bi = myBxLo(myThid), myBxHi(myThid)
        DO J = 1-Oly,sNy+Oly
         DO I = 1-Olx,sNx+Olx
          shelfIceHeatFlux      (I,J,bi,bj) = 0. _d 0
          shelfIceFreshWaterFlux(I,J,bi,bj) = 0. _d 0
          shelficeForcingT      (I,J,bi,bj) = 0. _d 0
          shelficeForcingS      (I,J,bi,bj) = 0. _d 0
         ENDDO
        ENDDO
#ifdef ALLOW_ISOMIP_TD
        IF ( useISOMIPTD ) THEN
         DO J = 1, sNy
          DO I = 1, sNx
           K    = kTopC(I,J,bi,bj)
           pLoc = ABS(R_shelfIce(I,J,bi,bj))
           IF ( K .NE. 0 .AND. pLoc .GT. 0. _d 0 ) THEN
C--   Calculate the in-situ temperature 
            tLoc = theta(I,J,K,bi,bj)
            sLoc = MAX(salt(I,J,K,bi,bj), 0. _d 0)
            IF ( SHELFICEBoundaryLayer .AND. K .LT. Nr ) THEN
C--   average over boundary layer width
             Kp1 = MIN(Nr,K+1)
C--   overlap into lower cell
             drKp1 = drF(K)*( 1. _d 0 - _hFacC(I,J,K,bi,bj) )
C--   lower cell may not be as thick as required
             drKp1 = MIN( drKp1, drF(Kp1) * _hFacC(I,J,Kp1,bi,bj) ) 
             recip_drLoc = 1. _d 0 / 
     &            ( drF(K)*_hFacC(I,J,K,bi,bj) + drKp1 )
             tloc = ( tloc * drF(K)*_hFacC(I,J,K,bi,bj)
     &            + theta(I,J,Kp1,bi,bj) *drKp1 )
     &            * recip_drLoc
             sloc = ( sloc * drF(K)*_hFacC(I,J,K,bi,bj)
     &            + MAX(salt(I,J,Kp1,bi,bj), 0. _d 0) * drKp1 )
     &            * recip_drLoc
            ENDIF
C--   Calculate freezing temperature as a function of salinity and pressure
CML           thetaFreeze=-1.9 _d 0
            thetaFreeze = sLoc * ( a0 + a1*sqrt(sLoc) + a2*sLoc )
     &           + b*pLoc + c0
C--   Calculate the upward heat and  fresh water fluxes
            shelfIceHeatFlux(I,J,bi,bj) = 
     &           SHELFICEheatTransCoeff * ( tLoc - thetaFreeze )
     &           *HeatCapacity_Cp*rUnit2mass
C     upward heat flux into the shelf-ice implies basal melting,
C     thus a downward (negative upward) fresh water flux, and vice versa
            shelfIceFreshWaterFlux(I,J,bi,bj) = 
     &           - shelfIceHeatFlux(I,J,bi,bj)
     &           *recip_rhoConst*recip_SHELFICElatentHeat
C--   compute surface tendencies
            shelficeForcingT(i,j,bi,bj) =
     &           - shelfIceHeatFlux(I,J,bi,bj)
     &           *recip_Cp*mass2rUnit
            IF (convertFW2Salt .EQ. -1.) THEN
             shelficeForcingS(i,j,bi,bj) = 
     &            shelfIceFreshWaterFlux(I,J,bi,bj)
     &            * salt(I,J,K,bi,bj) * rhoConstFresh*mass2rUnit
            ELSE
             shelficeForcingS(i,j,bi,bj) = 
     &            shelfIceFreshWaterFlux(I,J,bi,bj)
     &            * convertFW2Salt * rhoConstFresh*mass2rUnit
            ENDIF
C--   stress at the ice/water interface is computed in separate
C     routines that are called from mom_fluxform/mom_vecinv
           ELSE
            shelfIceHeatFlux      (I,J,bi,bj) = 0. _d 0
            shelfIceFreshWaterFlux(I,J,bi,bj) = 0. _d 0
            shelficeForcingT      (I,J,bi,bj) = 0. _d 0
            shelficeForcingS      (I,J,bi,bj) = 0. _d 0
           ENDIF
          ENDDO
         ENDDO
        ELSE
#else 
        IF ( .TRUE. ) THEN
#endif /* ALLOW_ISOMIP_TD */
C     use BRIOS thermodynamics, following Hellmers PhD thesis:
C     Hellmer, H., 1989, A two-dimensional model for the thermohaline 
C     circulation under an ice shelf, Reports on Polar Research, No. 60
C     (in German).

C     are we doing the conservative form?
         cFac = 0. _d 0
         IF( SHELFICEconserve ) cFac = 1. _d 0
C     a few abbreviations
         eps1 = rhoConst*HeatCapacity_Cp*SHELFICEheatTransCoeff
         eps2 = rhoConst*SHELFICElatentHeat*SHELFICEsaltTransCoeff
         eps5 = rhoConst*HeatCapacity_Cp*SHELFICEsaltTransCoeff
      
         DO J = 1, sNy
          DO I = 1, sNx
           K    = kTopC(I,J,bi,bj)
           pLoc = ABS(R_shelfIce(I,J,bi,bj))
           IF ( K .NE. 0 .AND. pLoc .GT. 0. _d 0 ) THEN
C--   Calculate the in-situ temperature 
            tLoc = theta(I,J,K,bi,bj)
            sLoc = MAX(salt(I,J,K,bi,bj), 0. _d 0)
            IF ( SHELFICEBoundaryLayer .AND. K .LT. Nr ) THEN
C--   average over boundary layer width
             Kp1 = MIN(Nr,K+1)
C--   overlap into lower cell
             drKp1 = drF(K)*( 1. _d 0 - _hFacC(I,J,K,bi,bj) )
C--   lower cell may not be as thick as required
             drKp1 = MIN( drKp1, drF(Kp1) * _hFacC(I,J,Kp1,bi,bj) ) 
             recip_drLoc = 1. _d 0 / 
     &            ( drF(K)*_hFacC(I,J,K,bi,bj) + drKp1 )
             tloc = ( tloc * drF(K)*_hFacC(I,J,K,bi,bj)
     &            + theta(I,J,Kp1,bi,bj) *drKp1 )
     &            * recip_drLoc
             sloc = ( sloc * drF(K)*_hFacC(I,J,K,bi,bj)
     &            + MAX(salt(I,J,Kp1,bi,bj), 0. _d 0) * drKp1 )
     &            * recip_drLoc
            ENDIF
C     solve quadratic equation to get salinity at shelfice-ocean interface
C     note: this part of the code is not very intuitive as it involves
C     many arbitrary abbreviations that were introduced to derive the
C     correct form of the quadratic equation for salinity. The abbreviations
C     only make sense in connection with my notes on this (M.Losch)
            eps3 = rhoShelfIce*SHELFICEheatCapacity_Cp
     &           * SHELFICEkappa/ploc
            eps4 = b*pLoc + c0
            eps6 = eps4 - tloc
            eps7 = eps4 - SHELFICEthetaSurface
            atmp = a0*( eps1 + eps3 - eps5 )
            btmp = eps1*eps6 + eps3*eps7
            aqe = a0  *(eps1+eps3) + cFac*atmp
            recip_aqe = 0. _d 0
            IF ( aqe .NE. 0. _d 0 ) recip_aqe = 0.5 _d 0/aqe
            bqe = btmp - eps2 + cFac*( btmp - eps5*eps6 - sloc*atmp )
            cqe = eps2*sloc   - cFac*( btmp - eps5*eps6 )*sloc
            discrim = bqe*bqe - 4. _d 0*aqe*cqe
#undef ALLOW_SHELFICE_DEBUG
#ifdef ALLOW_SHELFICE_DEBUG
            IF ( discrim .LT. 0. _d 0 ) THEN
             print *, 'ml-shelfice: discrim = ', discrim,aqe,bqe,cqe
             print *, 'ml-shelfice: ploc    = ', ploc
             print *, 'ml-shelfice: tloc    = ', tloc
             print *, 'ml-shelfice: sloc    = ', sloc
             print *, 'ml-shelfice: tsurface= ', 
     &            SHELFICEthetaSurface
             print *, 'ml-shelfice: eps1    = ', eps1
             print *, 'ml-shelfice: eps2    = ', eps2
             print *, 'ml-shelfice: eps3    = ', eps3
             print *, 'ml-shelfice: eps4    = ', eps4
             print *, 'ml-shelfice: eps5    = ', eps5
             print *, 'ml-shelfice: eps6    = ', eps6
             print *, 'ml-shelfice: eps7    = ', eps7
             print *, 'ml-shelfice: rhoW    = ', rhoConst
             print *, 'ml-shelfice: rhoIce  = ', rhoShelfIce
             print *, 'ml-shelfice: cFac    = ', cFac
             print *, 'ml-shelfice: Cp_W    = ', HeatCapacity_Cp
             print *, 'ml-shelfice: Cp_I    = ',
     &            SHELFICEHeatCapacity_Cp
             print *, 'ml-shelfice: gammaT  = ', 
     &            SHELFICEheatTransCoeff
             print *, 'ml-shelfice: gammaS  = ', 
     &            SHELFICEsaltTransCoeff
             print *, 'ml-shelfice: lat.heat= ', 
     &            SHELFICElatentHeat
             STOP 'ABNORMAL END in S/R SHELFICE_THERMODYNAMICS'
            ENDIF
#endif /* ALLOW_SHELFICE_DEBUG */
            saltFreeze = (- bqe - SQRT(discrim))*recip_aqe
            IF ( saltFreeze .LT. 0. _d 0 )
     &           saltFreeze = (- bqe + SQRT(discrim))*recip_aqe
C--   upward fresh water flux due to melting (in m/s of sea water)
            freshWaterFlux = SHELFICEsaltTransCoeff
     &           * ( saltFreeze - sloc )
     &           /( saltFreeze + cFac*( saltFreeze - sloc ) )
C--   Calculate the upward heat and fresh water fluxes;
C--   MITgcm sign conventions: downward (negative) fresh water flux 
C--   implies melting and due to upward (positive) heat flux
            shelfIceHeatFlux(I,J,bi,bj) = 
     &           ( eps3*( tloc - SHELFICEthetaSurface )
     &           - rhoConst*freshWaterFlux*SHELFICElatentHeat )
     &           *rUnit2mass*recip_rhoConst
            shelfIceFreshWaterFlux(I,J,bi,bj) =  
     &           freshWaterFlux * rUnit2mass*recip_rhoConst
C--   compute surface tendencies
            shelficeForcingT(i,j,bi,bj) =
     &           - shelfIceHeatFlux(I,J,bi,bj)
     &           *recip_Cp*mass2rUnit
            shelficeForcingS(i,j,bi,bj) = 
     &           shelfIceFreshWaterFlux(I,J,bi,bj)
     &           * saltFreeze * rhoConstFresh*mass2rUnit
           ELSE
            shelfIceHeatFlux      (I,J,bi,bj) = 0. _d 0
            shelfIceFreshWaterFlux(I,J,bi,bj) = 0. _d 0
            shelficeForcingT      (I,J,bi,bj) = 0. _d 0
            shelficeForcingS      (I,J,bi,bj) = 0. _d 0
           ENDIF
          ENDDO 
         ENDDO
        ENDIF
C     endif (not) useISOMIPTD
       ENDDO
      ENDDO
      
#ifdef ALLOW_DIAGNOSTICS
      IF ( useDiagnostics ) THEN
       CALL DIAGNOSTICS_FILL(shelfIceFreshWaterFlux,'SHIfwFlx',
     &      0,1,0,1,1,myThid)
       CALL DIAGNOSTICS_FILL(shelfIceHeatFlux,      'SHIhtFlx',
     &      0,1,0,1,1,myThid)
      ENDIF
#endif /* ALLOW_DIAGNOSTICS */
#endif /* ALLOW_SHELFICE */
      RETURN
      END

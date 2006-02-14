C $Header: /u/gcmpack/MITgcm/pkg/shelfice/shelfice_thermodynamics.F,v 1.4 2006/02/14 13:09:46 mlosch Exp $
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
C     !LOCAL VARIABLES:
C     === Local variables ===
      INTEGER I,J,K
      INTEGER bi,bj
      _RL tLoc, sLoc, pLoc
      _RL thetaFreeze, saltFreeze
      _RL a0, a1, a2, b, c0
      _RL eps1, eps2, eps3, eps4, aqe, bqe, cqe, discrim, recip_aqe

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
C     first a few abbreviations
      eps1 = rhoConst*HeatCapacity_Cp*SHELFICEheatTransCoeff
      eps2 = rhoConst*SHELFICElatentHeat*SHELFICEsaltTransCoeff
      
      DO bj = myByLo(myThid), myByHi(myThid)
       DO bi = myBxLo(myThid), myBxHi(myThid)
#ifdef ALLOW_ISOMIP_TD
        IF ( useISOMIPTD ) THEN
         DO J = 1, sNy
          DO I = 1, sNx
           K    = kTopC(I,J,bi,bj)
           pLoc = ABS(R_shelfIce(I,J,bi,bj))
           IF ( K .NE. 0 .AND. pLoc .GT. 0. _d 0 ) THEN
C--   Calculate the in-situ temperature 
            tLoc = theta(I,J,K,bi,bj)
            sLoc = max(salt(I,J,K,bi,bj), 0. _d 0)
C--   Calculate freezing temperature as a function of salinity and pressure
CML           thetaFreeze=-1.9 _d 0
            thetaFreeze = sLoc * ( a0 + a1*sqrt(sLoc) + a2*sLoc )
     &           + b*pLoc + c0
C--   Calculate the upward heat and  fresh water fluxes
            shelfIceHeatFlux(I,J,bi,bj) = 
     &           SHELFICEheatTransCoeff * ( tLoc - thetaFreeze )
     &           *HeatCapacity_Cp*recip_horiVertRatio*rhoConst
C     upward heat flux into the shelf-ice implies basal melting,
C     thus a downward (negative upward) fresh water flux, and vice versa
            shelfIceFreshWaterFlux(I,J,bi,bj) = 
     &           - shelfIceHeatFlux(I,J,bi,bj)
     &           *recip_rhoConst*recip_SHELFICElatentHeat
C--   compute surface tendencies
            shelficeForcingT(i,j,bi,bj) =
     &           - shelfIceHeatFlux(I,J,bi,bj)
     &           *recip_Cp*horiVertRatio*recip_rhoConst
            IF (convertFW2Salt .EQ. -1.) THEN
             shelficeForcingS(i,j,bi,bj) = 
     &            shelfIceFreshWaterFlux(I,J,bi,bj)
     &            * salt(I,J,K,bi,bj) * convertEmP2rUnit
            ELSE
             shelficeForcingS(i,j,bi,bj) = 
     &            shelfIceFreshWaterFlux(I,J,bi,bj)
     &            * convertFW2Salt * convertEmP2rUnit
            ENDIF
C--   stress at the ice/water interface is computed in separate
C     routines that are called from mom_fluxform/mom_vecinv
           ELSE
            shelficeForcingT(i,j,bi,bj) = 0. _d 0
            shelficeForcingS(i,j,bi,bj) = 0. _d 0
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
         DO J = 1, sNy
          DO I = 1, sNx
           K    = kTopC(I,J,bi,bj)
           pLoc = ABS(R_shelfIce(I,J,bi,bj))
           IF ( K .NE. 0 .AND. pLoc .GT. 0. _d 0 ) THEN
C--   Calculate the in-situ temperature 
            tLoc = theta(I,J,K,bi,bj)
            sLoc = max(salt(I,J,K,bi,bj), 0. _d 0)
C     solve quadratic equation to get salinity at shelfice-ocean interface
            eps3 = rhoShelfIce*SHELFICEheatCapacity_Cp
     &           * SHELFICEkappa/ploc
            eps4 = b*pLoc + c0
            aqe = a0  *(eps1+eps3)
            recip_aqe = 0. _d 0
            IF ( aqe .NE. 0 ) recip_aqe = 0.5/aqe
            bqe = eps4*(eps1+eps3) - eps2
     &           - eps1*tloc - eps3*SHELFICEthetaSurface
            cqe = eps2*sloc
            discrim = bqe*bqe - 4. _d 0*aqe*cqe
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
             print *, 'ml-shelfice: rhoW    = ', rhoConst
             print *, 'ml-shelfice: rhoIce  = ', rhoShelfIce
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
C--   Calculate freezing temperature as a function of salinity and pressure
            thetaFreeze = saltFreeze * a0 + eps4
CML   Inconsistent but more accurate:
CML            thetaFreeze = sLoc * ( a0 + a1*sqrt(sLoc) + a2*sLoc )
CML     &           + b*pLoc + c0
C--   Calculate the upward heat and fresh water fluxes
            shelfIceHeatFlux(I,J,bi,bj) = 
     &           ( eps1 * ( tLoc - thetaFreeze ) )*recip_horiVertRatio
            shelfIceFreshWaterFlux(I,J,bi,bj) = 
     &           rhoConst/rhoShelfIce * SHELFICEsaltTransCoeff
     &           * ( saltFreeze - sloc )/saltFreeze
     &           * recip_horiVertRatio
C--   compute surface tendencies
            shelficeForcingT(i,j,bi,bj) =
     &           - shelfIceHeatFlux(I,J,bi,bj)
     &           *recip_Cp*horiVertRatio*recip_rhoConst
            shelficeForcingS(i,j,bi,bj) = 
     &           shelfIceFreshWaterFlux(I,J,bi,bj)
     &           * saltFreeze * convertEmP2rUnit
           ENDIF
          ENDDO 
         ENDDO
        ENDIF
C     endif (not) useISOMIPTD
       ENDDO
      ENDDO
      
#endif /* ALLOW_SHELFICE */
      RETURN
      END

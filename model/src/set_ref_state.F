#include "CPP_OPTIONS.h"

CBOP
C     !ROUTINE: SET_REF_STATE
C     !INTERFACE:
      SUBROUTINE SET_REF_STATE(
     I                          myThid )
C     !DESCRIPTION: \bv
C     *==========================================================*
C     | SUBROUTINE SET_REF_STATE
C     | o Set reference potential at level center and
C     |   level interface, using tRef,sRef profiles.
C     | note: use same discretisation as in calc_phi_hyd
C     | o Set also reference stratification here (for implicit
C     |   Internal Gravity Waves) and units conversion factor
C     |   for vertical velocity (for Non-Hydrostatic in p)
C     |   since both use also the same reference density.
C     *==========================================================*
C     \ev

C     !USES:
      IMPLICIT NONE
C     == Global variables ==
#include "SIZE.h"
#include "EEPARAMS.h"
#include "PARAMS.h"
#include "GRID.h"
#include "EOS.h"

C     !INPUT/OUTPUT PARAMETERS:
C     myThid   :: my Thread Id number
      INTEGER myThid

C     !LOCAL VARIABLES:
C     msgBuf   :: Informational/error message buffer
C     pRefIntF :: reference pressure at level interface
      CHARACTER*(MAX_LEN_MBUF) msgBuf
      INTEGER k, ks, stdUnit
      _RL rHalf(2*Nr+1), pRefIntF(Nr+1)
      _RL tLoc(Nr)
      _RL pLoc, rhoUp, rhoDw, rhoLoc
      _RL ddPI, conv_theta2T, thetaLoc
C--
      _RL     maxResid
      INTEGER maxIterNb, belowCritNb
CEOP

      _BEGIN_MASTER( myThid )

C--   Initialise:
      DO k=1,2*Nr
        phiRef(k) = 0.
      ENDDO
      stdUnit = standardMessageUnit

      DO k=1,Nr
        rUnit2z(k) = 1. _d 0
        z2rUnit(k) = 1. _d 0
        rhoRef(k)  = rhoConst
        dBdrRef(k) = 0. _d 0
        pRef4EOS(k)  = 0. _d 0
        rHalf(2*k)   = rC(k)
      ENDDO

      DO k=1,Nr+1
        pRefIntF(k)  = 0. _d 0
        rHalf(2*k-1) = rF(k)
        rVel2wUnit(k) = 1. _d 0
        wUnit2rVel(k) = 1. _d 0
      ENDDO

C-    Initialise density factor for anelastic formulation:
      DO k=1,Nr
        rhoFacC(k) = 1. _d 0
        recip_rhoFacC(k) = 1. _d 0
      ENDDO
      DO k=1,Nr+1
        rhoFacF(k) = 1. _d 0
        recip_rhoFacF(k) = 1. _d 0
      ENDDO

C---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|
C--   Oceanic: define reference pressure/geo-potential vertical scale:
      IF ( buoyancyRelation.EQ.'OCEANIC' ) THEN
        phiRef(1)   = top_Pres*recip_rhoConst
        pRefIntF(1) = top_Pres
        IF ( gravityFile.EQ.' ' ) THEN
         DO k=1,Nr
          phiRef(2*k)   = phiRef(1)
     &                  + (rC(k) - rF(1))*gravity*gravitySign
          phiRef(2*k+1) = phiRef(1)
     &                  + (rF(k+1)-rF(1))*gravity*gravitySign
c         pRef4EOS(k)   = rhoConst*phiRef(2*k)
c         pRefIntF(k+1) = rhoConst*phiRef(2*k+1)
C note: just to get the same previous machine truncation:
          pRef4EOS(k)   = pRefIntF(1)
     &                  + rhoConst*(rC(k) - rF(1))*gravity*gravitySign
          pRefIntF(k+1) = pRefIntF(1)
     &                  + rhoConst*(rF(k+1)-rF(1))*gravity*gravitySign
         ENDDO
        ELSEIF ( integr_GeoPot.EQ.1 ) THEN
         DO k=1,Nr
          phiRef(2*k)   = phiRef(2*k-1)
     &                           + halfRL*drF(k)*gravity*gravFacC(k)
          phiRef(2*k+1) = phiRef(2*k-1) + drF(k)*gravity*gravFacC(k)
          pRef4EOS(k)   = rhoConst*phiRef(2*k)
          pRefIntF(k+1) = rhoConst*phiRef(2*k+1)
         ENDDO
        ELSE
         phiRef(2)   = phiRef(1) + drC(1)*gravity*gravFacF(1)
         DO k=2,Nr
          phiRef(2*k-1) = phiRef(2*k-2)
     &                           + halfRL*drC(k)*gravity*gravFacF(k)
          phiRef(2*k)   = phiRef(2*k-2) + drC(k)*gravity*gravFacF(k)
         ENDDO
         k = Nr
         phiRef(2*k+1) = phiRef(2*k) + drC(k+1)*gravity*gravFacF(k+1)
         DO k=1,Nr
          pRef4EOS(k)   = rhoConst*phiRef(2*k)
          pRefIntF(k+1) = rhoConst*phiRef(2*k+1)
         ENDDO
        ENDIF
      ELSEIF ( buoyancyRelation.EQ.'OCEANICP' ) THEN
        phiRef(2*Nr+1) = seaLev_Z*gravity
        DO k=1,Nr
          phiRef(2*k)   = phiRef(2*Nr+1)
     &                  - recip_rhoConst*( rC(k) - rF(Nr+1) )
          phiRef(2*k-1) = phiRef(2*Nr+1)
     &                  - recip_rhoConst*( rF(k) - rF(Nr+1) )
          pRef4EOS(k)   = rC(k)
          pRefIntF(k)   = rF(k)
        ENDDO
        pRefIntF(Nr+1) = rF(Nr+1)
c     ELSEIF (buoyancyRelation .EQ. 'ATMOSPHERIC') THEN
C     phiRef is computed later (see below)
      ENDIF

C---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|
      IF ( eosType.EQ.'POLY3' ) THEN
       IF ( implicitIntGravWave ) THEN
         WRITE(msgBuf,'(2A)') 'SET_REF_STATE:',
     &    ' need to compute reference density for Impl.IGW'
         CALL PRINT_ERROR( msgBuf , myThid )
         WRITE(msgBuf,'(2A)') 'SET_REF_STATE:',
     &    ' but FIND_RHO_SCALAR(EOS="POLY3") not (yet) implemented'
         CALL PRINT_ERROR( msgBuf , myThid )
         STOP 'ABNORMAL END: S/R SET_REF_STATE'
       ELSEIF ( nonHydrostatic .AND.
     &          buoyancyRelation.EQ.'OCEANICP' ) THEN
         WRITE(msgBuf,'(2A)') 'SET_REF_STATE:',
     &    ' need to compute reference density for Non-Hyd'
         CALL PRINT_ERROR( msgBuf , myThid )
         WRITE(msgBuf,'(2A)') 'SET_REF_STATE:',
     &    ' but FIND_RHO_SCALAR(EOS="POLY3") not (yet) implemented'
         CALL PRINT_ERROR( msgBuf , myThid )
         STOP 'ABNORMAL END: S/R SET_REF_STATE'
       ELSE
         WRITE(msgBuf,'(2A)') 'SET_REF_STATE:',
     &    ' Unable to compute reference stratification'
         CALL PRINT_MESSAGE( msgBuf, errorMessageUnit,
     &                       SQUEEZE_RIGHT , myThid )
         WRITE(msgBuf,'(2A)') 'SET_REF_STATE:',
     &    '  with EOS="POLY3" ; set dBdrRef(1:Nr) to zeros'
         CALL PRINT_MESSAGE( msgBuf, errorMessageUnit,
     &                       SQUEEZE_RIGHT , myThid)
       ENDIF
C---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|
      ELSEIF ( buoyancyRelation.EQ.'OCEANIC' ) THEN

C--   Compute reference density profile
        DO k=1,Nr
          pLoc = pRef4EOS(k)
          CALL FIND_RHO_SCALAR(
     I                          tRef(k), sRef(k), pLoc,
     O                          rhoRef(k), myThid )
        ENDDO

        IF ( selectP_inEOS_Zc.GE.1 ) THEN
C--   Find reference pressure in hydrostatic balance with updated ref density
          maxResid = rhoConst* 1. _d -14
          belowCritNb = 5
          maxIterNb = 10*Nr
          CALL FIND_HYD_PRESS_1D(
     O                            pRef4EOS, pRefIntF,
     U                            rhoRef,
     I                            tRef, sRef, maxResid,
     I                            belowCritNb, maxIterNb, myThid )
        ENDIF

C--   Compute reference stratification: N^2 = -(g/rho_c) * d.rho/dz @ const. p
        dBdrRef(1) = 0. _d 0
        DO k=2,Nr
          pLoc = pRefIntF(k)
          CALL FIND_RHO_SCALAR(
     I                          tRef(k-1), sRef(k-1), pLoc,
     O                          rhoUp, myThid )
          CALL FIND_RHO_SCALAR(
     I                          tRef(k), sRef(k), pLoc,
     O                          rhoDw, myThid )
          dBdrRef(k) = (rhoDw - rhoUp)*recip_drC(k)
     &               *recip_rhoConst*gravity*gravFacF(k)
          IF ( eosType.EQ.'LINEAR' ) THEN
C- get more precise values (differences from above are due to machine round-off)
            dBdrRef(k) = ( sBeta *(sRef(k)-sRef(k-1))
     &                    -tAlpha*(tRef(k)-tRef(k-1))
     &                   )*recip_drC(k)
     &                 *rhoNil*recip_rhoConst*gravity*gravFacF(k)
          ENDIF
        ENDDO

C---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|
      ELSEIF ( buoyancyRelation.EQ.'OCEANICP' ) THEN

C--   Compute reference density profile (from tRef,sRef):
        DO k=1,Nr
          pLoc = pRef4EOS(k)
          CALL FIND_RHO_SCALAR(
     I                          tRef(k), sRef(k), pLoc,
     O                          rhoRef(k), myThid )
C-    set units convertion factor @ level center:
C       z2rUnit = gravity*rhoRef : dr [Pa] = dz [m] * z2rUnit
C       rUnit2z = 1/z2rUnit      : dz [m] = dr [Pa] * rUnit2z
          z2rUnit(k) = gravity*rhoRef(k)
          rUnit2z(k) = 1. _d 0 / z2rUnit(k)
        ENDDO

C--   Compute reference stratification: -d.alpha/dp @ constant p
        dBdrRef(1) = 0. _d 0
        DO k=1,Nr+1
          pLoc = pRefIntF(k)
          IF ( k.GE.2 )  CALL FIND_RHO_SCALAR(
     I                             tRef(k-1), sRef(k-1), pLoc,
     O                             rhoDw, myThid )
          IF ( k.LE.Nr ) CALL FIND_RHO_SCALAR(
     I                             tRef(k), sRef(k), pLoc,
     O                             rhoUp, myThid )
          IF ( k.GE.2 .AND. k.LE.Nr ) THEN
            dBdrRef(k) = (rhoDw - rhoUp)*recip_drC(k)
     &                 / (rhoDw*rhoUp)
            rhoLoc = ( rhoDw + rhoUp )*0.5 _d 0
          ELSEIF ( k.EQ.1 ) THEN
            rhoLoc = rhoUp
          ELSE
            rhoLoc = rhoDw
          ENDIF
C--   Units convertion factor for vertical velocity:
C       wUnit2rVel = gravity*rhoRef : rVel  [Pa/s] = wSpeed [m/s] * wUnit2rVel
C       rVel2wUnit = 1/rVel2wUnit   : wSpeed [m/s] = rVel  [Pa/s] * rVel2wUnit
C     note: wUnit2rVel & rVel2wUnit replace horiVertRatio & recip_horiVertRatio
          wUnit2rVel(k) = gravity*rhoLoc
          rVel2wUnit(k) = 1. _d 0 / wUnit2rVel(k)
        ENDDO

C---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|
      ELSEIF ( buoyancyRelation.EQ.'ATMOSPHERIC' ) THEN

C--   Compute reference stratification: -d.alpha/dp @ constant p
        dBdrRef(1) = 0. _d 0
        DO k=2,Nr
          conv_theta2T = (rF(k)/atm_Po)**atm_kappa
c         dBdrRef(k) = (tRef(k) - tRef(k-1))*recip_drC(k)
c    &               * conv_theta2T*atm_Rd/rF(k)
          ddPI=atm_Cp*( ((rC(k-1)/atm_Po)**atm_kappa)
     &                 -((rC( k )/atm_Po)**atm_kappa) )
          dBdrRef(k) = (tRef(k) - tRef(k-1))*recip_drC(k)
     &               * ddPI*recip_drC(k)
        ENDDO

C--   Set units convertion factor @ level center:
C       z2rUnit = gravity/alpha : dr [Pa] = dz [m] * z2rUnit
C       rUnit2z = alpha/gravity : dz [m] = dr [Pa] * rUnit2z
        DO k=1,Nr
          pRef4EOS(k)  = rC(k)
          thetaLoc = tRef(k)
          IF ( thetaLoc.GT.0. _d 0 .AND. rC(k).GT.0. _d 0 ) THEN
            conv_theta2T = (rC(k)/atm_Po)**atm_kappa
            z2rUnit(k) = gravity
     &                 * rC(k)/(atm_Rd*conv_theta2T*thetaLoc)
            rUnit2z(k) = 1. _d 0 / z2rUnit(k)
          ENDIF
        ENDDO

C--   Units convertion factor for vertical velocity:
C       wUnit2rVel = gravity/alpha : rVel  [Pa/s] = wSpeed [m/s] * wUnit2rVel
C       rVel2wUnit = alpha/gravity : wSpeed [m/s] = rVel  [Pa/s] * rVel2wUnit
C       with alpha = 1/rhoRef = (R.T/p) (ideal gas)
C     note: wUnit2rVel & rVel2wUnit replace horiVertRatio & recip_horiVertRatio
        DO k=1,Nr+1
          pRefIntF(k)  = rF(k)
          IF ( k.EQ.1 ) THEN
            thetaLoc = tRef(k)
          ELSEIF ( k.GT.Nr ) THEN
            thetaLoc = tRef(k-1)
          ELSE
            thetaLoc = (tRef(k) + tRef(k-1))*0.5 _d 0
          ENDIF
          IF ( thetaLoc.GT.0. _d 0 .AND. rF(k).GT.0. _d 0 ) THEN
            conv_theta2T  = (rF(k)/atm_Po)**atm_kappa
            wUnit2rVel(k) = gravity
     &                    * rF(k)/(atm_Rd*conv_theta2T*thetaLoc)
            rVel2wUnit(k) = 1. _d 0 / wUnit2rVel(k)
          ENDIF
        ENDDO

C-    Compute Reference Geopotential at Half levels :
C      Tracer level k: phiRef(2k)  ;  Interface_W level k: phiRef(2k-1)

       phiRef(1) = seaLev_Z*gravity
       IF ( select_rStar.GE.1 .OR. selectSigmaCoord.GE.1 ) THEN
C-      isothermal (theta=const) reference state
        DO k=1,Nr
         tLoc(k) = thetaConst
        ENDDO
       ELSE
C-      horizontally uniform (tRef) reference state
        DO k=1,Nr
         tLoc(k) = tRef(k)
        ENDDO
       ENDIF

       IF (integr_GeoPot.EQ.1) THEN
C-    Finite Volume Form, linear by half level :
        DO k=1,2*Nr
          ks = (k+1)/2
          ddPI=atm_Cp*( ((rHalf( k )/atm_Po)**atm_kappa)
     &                 -((rHalf(k+1)/atm_Po)**atm_kappa) )
          phiRef(k+1) = phiRef(k)+ddPI*tLoc(ks)
        ENDDO
C------
       ELSE
C-    Finite Difference Form, linear between Tracer level :
C      works with integr_GeoPot = 0, 2 or 3
        k = 1
          ddPI=atm_Cp*( ((rF(k)/atm_Po)**atm_kappa)
     &                 -((rC(k)/atm_Po)**atm_kappa) )
          phiRef(2*k)   = phiRef(1) + ddPI*tLoc(k)
        DO k=1,Nr-1
          ddPI=atm_Cp*( ((rC( k )/atm_Po)**atm_kappa)
     &                 -((rC(k+1)/atm_Po)**atm_kappa) )
          phiRef(2*k+1) = phiRef(2*k) + ddPI*0.5*tLoc(k)
          phiRef(2*k+2) = phiRef(2*k)
     &                  + ddPI*0.5*(tLoc(k)+tLoc(k+1))
        ENDDO
        k = Nr
          ddPI=atm_Cp*( ((rC( k )/atm_Po)**atm_kappa)
     &                 -((rF(k+1)/atm_Po)**atm_kappa) )
          phiRef(2*k+1) = phiRef(2*k) + ddPI*tLoc(k)
C------
       ENDIF

C---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|
      ELSE
        STOP 'SET_REF_STATE: Bad value of buoyancyRelation !'
C--   endif buoyancyRelation
      ENDIF

C---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|

      IF ( usingZCoords .AND. rhoRefFile .NE. ' ' ) THEN
C--   anelastic formulation : set density factor from reference density profile
C       surface-interface rho-factor has to be 1:
        rhoFacF(1)   = 1. _d 0
C       rhoFac(k) = density ratio between layer k and top interface
        DO k=1,Nr
          rhoFacC(k) = rho1Ref(k)/rhoConst
c         rhoFacC(k) = rho1Ref(k)*recip_rhoConst
        ENDDO
        DO k=2,Nr
C       since rkSign=-1, recip_drC(k) = 1./(rC(k-1)-rC(k))
          rhoFacF(k) = ( rhoFacC(k-1)*(rF(k)-rC(k))
     &                 + rhoFacC(k)*(rC(k-1)-rF(k)) )*recip_drC(k)
        ENDDO
C       extrapolate down to the bottom:
        rhoFacF(Nr+1) = ( rhoFacC(Nr)*(rF(Nr+1)-rF(Nr))
     &                  + rhoFacF(Nr)*(rC(Nr)-rF(Nr+1))
     &                  ) / (rC(Nr)-rF(Nr))
C-      set reciprocal rho-factor:
        DO k=1,Nr
          recip_rhoFacC(k) = 1. _d 0/rhoFacC(k)
        ENDDO
        DO k=1,Nr+1
          recip_rhoFacF(k) = 1. _d 0/rhoFacF(k)
        ENDDO
      ENDIF

C---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|

C--   Write to check :
      IF ( gravityFile .NE. ' ' ) THEN
C-    write gravity vertical profile factor to binary file:
        CALL WRITE_GLVEC_RL( 'GravFacC',' ',gravFacC, Nr, -1, myThid )
        CALL WRITE_GLVEC_RL( 'GravFacF',' ',gravFacF,Nr+1,-1, myThid )
      ENDIF
      IF ( selectP_inEOS_Zc.GE.1 ) THEN
C-    write (to binary file) Hyd-Pressure used in EOS:
        CALL WRITE_GLVEC_RL( 'PRef4EOS',' ',pRef4EOS, Nr, -1, myThid )
c       CALL WRITE_GLVEC_RL( 'PRefIntF',' ',pRefIntF,Nr+1,-1, myThid )
      ENDIF
      IF ( buoyancyRelation.EQ.'ATMOSPHERIC' ) THEN
       WRITE(msgBuf,'(A)') ' '
       CALL PRINT_MESSAGE( msgBuf, stdUnit, SQUEEZE_RIGHT, myThid )
       WRITE(msgBuf,'(A)')
     &  'SET_REF_STATE: PhiRef/g [m] at level Center (integer)'
       CALL PRINT_MESSAGE( msgBuf, stdUnit, SQUEEZE_RIGHT, myThid )
       WRITE(msgBuf,'(A)')
     &  '                     and at level Interface (half-int.) :'
       CALL PRINT_MESSAGE( msgBuf, stdUnit, SQUEEZE_RIGHT, myThid )
       DO k=1,2*Nr+1
        WRITE(msgBuf,'(A,F5.1,A,F15.1,A,F13.3)')
     &    ' K=',k*0.5,'  ;  r=',rHalf(k),'  ;  phiRef/g=',
     &    phiRef(k)*recip_gravity
        CALL PRINT_MESSAGE(msgBuf, stdUnit, SQUEEZE_RIGHT, myThid )
       ENDDO
      ELSE
C-    Write reference density to binary file :
        CALL WRITE_GLVEC_RL( 'RhoRef', ' ', rhoRef, Nr, -1, myThid )
      ENDIF
      IF ( usingZCoords .AND. rhoRefFile .NE. ' ' ) THEN
C-    Write Anelastic density factor to binary file :
        CALL WRITE_GLVEC_RL( 'RhoFacC',' ',rhoFacC, Nr ,  -1, myThid )
        CALL WRITE_GLVEC_RL( 'RhoFacF',' ',rhoFacF, Nr+1, -1, myThid )
      ENDIF

C---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|

      _END_MASTER( myThid )
      _BARRIER

      RETURN
      END

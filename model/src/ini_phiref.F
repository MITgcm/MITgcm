C $Header: /u/gcmpack/MITgcm/model/src/Attic/ini_phiref.F,v 1.4 2006/10/17 18:52:34 jmc Exp $
C $Name:  $

#include "CPP_OPTIONS.h"

CBOP
C     !ROUTINE: INI_PHIREF
C     !INTERFACE:
      SUBROUTINE INI_PHIREF(
     I                       myThid )
C     !DESCRIPTION: \bv
C     *==========================================================*
C     | SUBROUTINE INI_PHIREF
C     | o Set reference potential at level center and
C     |   level interface, using tRef,sRef profiles.
C     | note: use same discretisation as in calc_phi_hyd
C     | o Set also reference stratification here (for implicit
C     |   Internal Gravity Waves) since it uses also the
C     |   same reference density.
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
C     == Routine arguments ==
      INTEGER myThid

C     !LOCAL VARIABLES:
C     == Local variables ==
C     msgBuf :: Informational/error meesage buffer
      CHARACTER*(MAX_LEN_MBUF) msgBuf
      INTEGER k, ks, stdUnit
      _RL rHalf(2*Nr+1)
      _RL rhoRef(Nr)
      _RL pLoc, rhoUp, rhoDw
      _RL ddPI, conv_theta2T
CEOP

      _BEGIN_MASTER( myThid )

      DO k=1,2*Nr
        phiRef(k) = 0.
      ENDDO
      stdUnit = standardMessageUnit

      DO k=1,Nr
        rhoRef(k) = 0.
        rHalf(2*k-1) = rF(k)
        rHalf(2*k)   = rC(k)
      ENDDO
      rHalf(2*Nr+1) = rF(Nr+1)

C---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|
      IF ( eosType.EQ.'POLY3' ) THEN
       IF ( implicitIntGravWave ) THEN
         WRITE(msgBuf,'(A)')
     &    'INI_PHIREF: need to compute reference density for Impl.IGW'
         CALL PRINT_ERROR( msgBuf , myThid)
         WRITE(msgBuf,'(2A)')
     &    'INI_PHIREF: but FIND_RHO_SCALAR(EOS="POLY3")',
     &    ' not (yet) implemented'
         CALL PRINT_ERROR( msgBuf , myThid)
         STOP 'ABNORMAL END: S/R INI_PHIREF'
       ELSE
         WRITE(msgBuf,'(A)')
     &    'INI_PHIREF: Unable to compute reference stratification'
         CALL PRINT_MESSAGE( msgBuf, errorMessageUnit,
     &                       SQUEEZE_RIGHT , myThid)
         WRITE(msgBuf,'(A)')
     &    'INI_PHIREF:  with EOS="POLY3" ; set dBdrRef(1:Nr) to zeros'
         CALL PRINT_MESSAGE( msgBuf, errorMessageUnit,
     &                       SQUEEZE_RIGHT , myThid)
       ENDIF
C---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|
      ELSEIF (buoyancyRelation .EQ. 'OCEANIC') THEN

C--   Compute reference density profile and reference stratification
        DO k=1,Nr
          pLoc = -rhoConst*rC(k)*gravity
          CALL FIND_RHO_SCALAR(
     I                          tRef(k), sRef(k), pLoc,
     O                          rhoRef(k), myThid )
          rhoRef(k) = rhoRef(k) + rhoConst
        ENDDO

C--   Compute reference stratification: N^2 = -(g/rho_c) * d.rho/dz @ const. p
        dBdrRef(1) = 0. _d 0
        DO k=2,Nr
          pLoc = -rhoConst*rF(k)*gravity
          CALL FIND_RHO_SCALAR(
     I                          tRef(k-1), sRef(k-1), pLoc,
     O                          rhoUp, myThid )
          CALL FIND_RHO_SCALAR(
     I                          tRef(k), sRef(k), pLoc,
     O                          rhoDw, myThid )
          dBdrRef(k) = (rhoDw - rhoUp)*recip_drC(k)
     &               *recip_rhoConst*gravity
          IF (eosType .EQ. 'LINEAR') THEN
C- get more precise values (differences from above are due to machine round-off)
            dBdrRef(k) = ( sBeta *(sRef(k)-sRef(k-1))
     &                    -tAlpha*(tRef(k)-tRef(k-1))
     &                   )*recip_drC(k)
     &                 *rhoNil*recip_rhoConst*gravity
          ENDIF
        ENDDO

C---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|
      ELSEIF (buoyancyRelation .EQ. 'OCEANICP') THEN

C--   Compute reference density profile
        DO k=1,Nr
          pLoc = rC(k)
          CALL FIND_RHO_SCALAR(
     I                          tRef(k), sRef(k), pLoc,
     O                          rhoRef(k), myThid )
          rhoRef(k) = rhoRef(k) + rhoConst
        ENDDO

C--   Compute reference stratification: -d.alpha/dp @ constant p
        dBdrRef(1) = 0. _d 0
        DO k=2,Nr
          pLoc = rF(k)
          CALL FIND_RHO_SCALAR(
     I                          tRef(k-1), sRef(k-1), pLoc,
     O                          rhoDw, myThid )
          CALL FIND_RHO_SCALAR(
     I                          tRef(k), sRef(k), pLoc,
     O                          rhoUp, myThid )
          dBdrRef(k) = (rhoDw - rhoUp)*recip_drC(k)
     &               / (rhoDw+rhoConst)
     &               / (rhoUp+rhoConst)
        ENDDO

C---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|
      ELSEIF (buoyancyRelation .EQ. 'ATMOSPHERIC') THEN

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

C-    Compute Reference Geopotential at Half levels :
C      Tracer level: phiRef(2k)  ;  Interface_W level: phiRef(2k+1)

       phiRef(1) = 0. _d 0

       IF (integr_GeoPot.EQ.1) THEN
C-    Finite Volume Form, linear by half level :
        DO k=1,2*Nr
          ks = (k+1)/2
          ddPI=atm_Cp*( ((rHalf( k )/atm_Po)**atm_kappa)
     &                 -((rHalf(k+1)/atm_Po)**atm_kappa) )
          phiRef(k+1) = phiRef(k)+ddPI*tRef(ks)
        ENDDO
C------
       ELSE
C-    Finite Difference Form, linear between Tracer level :
C      works with integr_GeoPot = 0, 2 or 3
        k = 1
          ddPI=atm_Cp*( ((rF(k)/atm_Po)**atm_kappa)
     &                 -((rC(k)/atm_Po)**atm_kappa) )
          phiRef(2*k)   = phiRef(1) + ddPI*tRef(k)
        DO k=1,Nr-1
          ddPI=atm_Cp*( ((rC( k )/atm_Po)**atm_kappa)
     &                 -((rC(k+1)/atm_Po)**atm_kappa) )
          phiRef(2*k+1) = phiRef(2*k) + ddPI*0.5*tRef(k)
          phiRef(2*k+2) = phiRef(2*k)
     &                  + ddPI*0.5*(tRef(k)+tRef(k+1))
        ENDDO
        k = Nr
          ddPI=atm_Cp*( ((rC( k )/atm_Po)**atm_kappa)
     &                 -((rF(k+1)/atm_Po)**atm_kappa) )
          phiRef(2*k+1) = phiRef(2*k) + ddPI*tRef(k)
C------
       ENDIF

C---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|
      ELSE
        STOP 'INI_PHIREF: Bad value of buoyancyRelation !'
C--   endif buoyancyRelation
      ENDIF

C---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|

C--   Write to check :
      IF (buoyancyRelation .EQ. 'ATMOSPHERIC') THEN
       WRITE(msgBuf,'(A)') ' '
       CALL PRINT_MESSAGE( msgBuf, stdUnit, SQUEEZE_RIGHT, myThid )
       WRITE(msgBuf,'(A)')
     &  'INI_PHIREF: PhiRef/g [m] at level Center (integer)'
       CALL PRINT_MESSAGE( msgBuf, stdUnit, SQUEEZE_RIGHT, myThid )
       WRITE(msgBuf,'(A)')
     &  '                     and at level Interface (half-int.) :'
       CALL PRINT_MESSAGE( msgBuf, stdUnit, SQUEEZE_RIGHT, myThid )
       DO k=1,2*Nr+1
        WRITE(msgBuf,'(A,F5.1,A,F10.1,A,F12.3)')
     &    ' K=',k*0.5,'  ;  r=',rHalf(k),'  ;  phiRef/g=',
     &    phiRef(k)*recip_gravity
        CALL PRINT_MESSAGE(msgBuf, stdUnit, SQUEEZE_RIGHT, myThid )
       ENDDO
      ENDIF

C---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|

      _END_MASTER( myThid )
      _BARRIER

      RETURN
      END

C $Header: /u/gcmpack/MITgcm/model/src/find_alpha.F,v 1.11 2002/11/01 22:00:33 mlosch Exp $
C $Name:  $

#include "CPP_OPTIONS.h"
#define USE_FACTORIZED_POLY

CBOP
C     !ROUTINE: FIND_ALPHA
C     !INTERFACE:
      SUBROUTINE FIND_ALPHA (
     I     bi, bj, iMin, iMax, jMin, jMax,  k, kRef, 
     O     alphaloc )

C     !DESCRIPTION: \bv
C     *==========================================================*
C     | o SUBROUTINE FIND_ALPHA                                   
C     |   Calculates [drho(S,T,z) / dT] of a horizontal slice     
C     *==========================================================*
C     |                                                           
C     | k - is the Theta/Salt level                               
C     | kRef - determines pressure reference level                
C     |        (not used in 'LINEAR' mode)                        
C     |                                                           
C     | alphaloc - drho / dT (kg/m^3/C)                           
C     |                                                           
C     *==========================================================*
C     \ev

C     !USES:
      IMPLICIT NONE
c Common
#include "SIZE.h"
#include "DYNVARS.h"
#include "EEPARAMS.h"
#include "PARAMS.h"
#include "EOS.h"
#include "GRID.h"

C     !INPUT/OUTPUT PARAMETERS:
c Arguments
      integer bi,bj,iMin,iMax,jMin,jMax
      integer k			! Level of Theta/Salt slice
      integer kRef		! Pressure reference level
      _RL alphaloc(1-Olx:sNx+Olx,1-Oly:sNy+Oly)

C     !LOCAL VARIABLES:
c Local
      integer i,j
      _RL refTemp,refSalt,tP,sP
      _RL t1, t2, t3, s1, s3o2, p1, p2, sp5, p1t1
      _RL drhoP0dtheta, drhoP0dthetaFresh, drhoP0dthetaSalt
      _RL dKdtheta, dKdthetaFresh, dKdthetaSalt, dKdthetaPres
      _RL rhoP0(1-Olx:sNx+Olx,1-Oly:sNy+Oly)
      _RL bulkMod(1-Olx:sNx+Olx,1-Oly:sNy+Oly)
      _RL dnum_dtheta, dden_dtheta
      _RL rhoDen(1-Olx:sNx+Olx,1-Oly:sNy+Oly)
      _RL rhoLoc(1-Olx:sNx+Olx,1-Oly:sNy+Oly)
      integer myThid
CEOP

Cml      stop 'myThid is not properly defined in this subroutine'

#ifdef CHECK_SALINITY_FOR_NEGATIVE_VALUES
      CALL LOOK_FOR_NEG_SALINITY( bi, bj, iMin, iMax, jMin, jMax,  k,
     &     sFld, myThid )
#endif

      if (equationOfState.eq.'LINEAR') then

         do j=jMin,jMax
            do i=iMin,iMax
               alphaloc(i,j) = -rhonil * tAlpha
            enddo
         enddo
         
      elseif (equationOfState.eq.'POLY3') then

         refTemp=eosRefT(kRef)
         refSalt=eosRefS(kRef)

         do j=jMin,jMax
            do i=iMin,iMax
               tP=theta(i,j,k,bi,bj)-refTemp
               sP=salt(i,j,k,bi,bj)-refSalt
#ifdef USE_FACTORIZED_POLY
               alphaloc(i,j) =
     &     ( eosC(6,kRef)
     &         *tP*3.
     &        +(eosC(7,kRef)*sP + eosC(3,kRef))*2.
     &       )*tP
     &      +(eosC(8,kRef)*sP + eosC(4,kRef) )*sP + eosC(1,kRef)
     &     
#else
               alphaloc(i,j) =
     &              eosC(1,kRef)                +
     &              eosC(3,kRef)*tP*2.          +
     &              eosC(4,kRef)         *sP    +
     &              eosC(6,kRef)*tP*tP*3.       +
     &              eosC(7,kRef)*tP*2.   *sP    +
     &              eosC(8,kRef)         *sP*sP
#endif
            enddo
         enddo
         
      elseif ( equationOfState(1:5).eq.'JMD95'
     &        .or. equationOfState.eq.'UNESCO' ) then
C     nonlinear equation of state in pressure coordinates

         CALL FIND_RHOP0(
     I        bi, bj, iMin, iMax, jMin, jMax, k,
     I        theta, salt,
     O        rhoP0,
     I        myThid )
         
         CALL FIND_BULKMOD(
     I        bi, bj, iMin, iMax, jMin, jMax,  k, kRef,
     I        theta, salt,
     O        bulkMod,
     I        myThid )

         do j=jMin,jMax
            do i=iMin,iMax

C     abbreviations
               t1 = theta(i,j,k,bi,bj) 
               t2 = t1*t1
               t3 = t2*t1
               
               s1  = salt(i,j,k,bi,bj)
               s3o2 = sqrt(s1*s1*s1)
               
               p1  = pressure(i,j,kRef,bi,bj)*SItoBar
               p2 = p1*p1

C     d(rho)/d(theta)
C     of fresh water at p = 0
               drhoP0dthetaFresh = 
     &                eosJMDCFw(2)
     &           + 2.*eosJMDCFw(3)*t1
     &           + 3.*eosJMDCFw(4)*t2
     &           + 4.*eosJMDCFw(5)*t3
     &           + 5.*eosJMDCFw(6)*t3*t1
C     of salt water at p = 0
               drhoP0dthetaSalt = 
     &        s1*(
     &                eosJMDCSw(2)
     &           + 2.*eosJMDCSw(3)*t1
     &           + 3.*eosJMDCSw(4)*t2
     &           + 4.*eosJMDCSw(5)*t3
     &           )
     &       + s3o2*(
     &           +    eosJMDCSw(7)
     &           + 2.*eosJMDCSw(8)*t1
     &           )
C     d(bulk modulus)/d(theta)
C     of fresh water at p = 0
               dKdthetaFresh = 
     &                eosJMDCKFw(2)
     &           + 2.*eosJMDCKFw(3)*t1
     &           + 3.*eosJMDCKFw(4)*t2
     &           + 4.*eosJMDCKFw(5)*t3
C     of sea water at p = 0
               dKdthetaSalt =
     &        s1*(    eosJMDCKSw(2)
     &           + 2.*eosJMDCKSw(3)*t1
     &           + 3.*eosJMDCKSw(4)*t2
     &           )
     &    + s3o2*(    eosJMDCKSw(6)
     &           + 2.*eosJMDCKSw(7)*t1
     &           )
C     of sea water at p
               dKdthetaPres =
     &        p1*(    eosJMDCKP(2)
     &           + 2.*eosJMDCKP(3)*t1
     &           + 3.*eosJMDCKP(4)*t2
     &           )
     &   + p1*s1*(    eosJMDCKP(6)
     &           + 2.*eosJMDCKP(7)*t1
     &           )
     &      + p2*(    eosJMDCKP(10)
     &           + 2.*eosJMDCKP(11)*t1
     &           )
     &   + p2*s1*(    eosJMDCKP(13)
     &           + 2.*eosJMDCKP(14)*t1
     &           )

               drhoP0dtheta  = drhoP0dthetaFresh 
     &                       + drhoP0dthetaSalt
               dKdtheta      = dKdthetaFresh 
     &                       + dKdthetaSalt 
     &                       + dKdthetaPres
               alphaloc(i,j) = 
     &              ( bulkmod(i,j)**2*drhoP0dtheta
     &              - bulkmod(i,j)*p1*drhoP0dtheta
     &              - rhoP0(i,j)*p1*dKdtheta )
     &              /( bulkmod(i,j) - p1 )**2
               
              
            end do
         end do
      elseif ( equationOfState.eq.'MDJWF' ) then

         CALL FIND_RHO( bi, bj, iMin, iMax, jMin, jMax,  k, kRef,
     &        theta, salt, rhoLoc, myThid )
         CALL FIND_RHODEN( bi, bj, iMin, iMax, jMin, jMax, k, kRef,
     &      theta, salt, rhoDen, myThid )

         do j=jMin,jMax
            do i=iMin,iMax
               t1  = theta(i,j,k,bi,bj)
               t2  = t1*t1
               s1  = salt(i,j,k,bi,bj)
               sp5 = sqrt(s1) 

               p1   = pressure(i,j,kRef,bi,bj)*SItodBar
               p1t1 = p1*t1
            
               dnum_dtheta = eosMDJWFnum(1)
     &              + t1*(2.*eosMDJWFnum(2) + 3.*eosMDJWFnum(3)*t1) 
     &              + eosMDJWFnum(5)*s1                                 
     &              + p1t1*(2.*eosMDJWFnum(8) + 2.*eosMDJWFnum(11)*p1)    
            
               dden_dtheta = eosMDJWFden(1) 
     &              + t1*(2.*eosMDJWFden(2) 
     &              +     t1*(3.*eosMDJWFden(3) 
     &              +         4.*eosMDJWFden(4)*t1 ) )
     &              + s1*(eosMDJWFden(6) 
     &              +     t1*(3.*eosMDJWFden(7)*t1 
     &              +         2.*eosMDJWFden(9)*sp5 ) )
     &              + p1*p1*(3.*eosMDJWFden(11)*t2 + eosMDJWFden(12)*p1)
            
               alphaLoc(i,j)    = rhoDen(i,j)*(dnum_dtheta 
     &              - rhoLoc(i,j)*dden_dtheta)
            
         end do
      end do

      else
         write(*,*) 'FIND_ALPHA: equationOfState = ',equationOfState
         stop 'FIND_ALPHA: "equationOfState" has illegal value'
      endif

      return
      end

      subroutine FIND_BETA (
     I     bi, bj, iMin, iMax, jMin, jMax,  k, kRef,
     O     betaloc )
C     /==========================================================\
C     | o SUBROUTINE FIND_BETA                                   |
C     |   Calculates [drho(S,T,z) / dS] of a horizontal slice    |
C     |==========================================================|
C     |                                                          |
C     | k - is the Theta/Salt level                              |
C     | kRef - determines pressure reference level               |
C     |        (not used in 'LINEAR' mode)                       |
C     |                                                          |
C     | betaloc - drho / dS (kg/m^3/PSU)                         |
C     |                                                          |
C     \==========================================================/
      implicit none

c Common
#include "SIZE.h"
#include "DYNVARS.h"
#include "EEPARAMS.h"
#include "PARAMS.h"
#include "EOS.h"
#include "GRID.h"

c Arguments
      integer bi,bj,iMin,iMax,jMin,jMax
      integer k			! Level of Theta/Salt slice
      integer kRef		! Pressure reference level
      _RL betaloc(1-Olx:sNx+Olx,1-Oly:sNy+Oly)

c Local
      integer i,j
      _RL refTemp,refSalt,tP,sP
      _RL t1, t2, t3, s1, s3o2, p1, sp5, p1t1
      _RL drhoP0dS
      _RL dKdS, dKdSSalt, dKdSPres
      _RL rhoP0(1-Olx:sNx+Olx,1-Oly:sNy+Oly)
      _RL bulkMod(1-Olx:sNx+Olx,1-Oly:sNy+Oly)
      _RL dnum_dsalt, dden_dsalt
      _RL rhoDen(1-Olx:sNx+Olx,1-Oly:sNy+Oly)
      _RL rhoLoc(1-Olx:sNx+Olx,1-Oly:sNy+Oly)
      integer myThid
CEOP

Cml      stop 'myThid is not properly defined in this subroutine'

#ifdef CHECK_SALINITY_FOR_NEGATIVE_VALUES
      CALL LOOK_FOR_NEG_SALINITY( bi, bj, iMin, iMax, jMin, jMax,  k,
     &     sFld, myThid )
#endif

      if (equationOfState.eq.'LINEAR') then

         do j=jMin,jMax
            do i=iMin,iMax
               betaloc(i,j) = rhonil * sBeta
            enddo
         enddo
         
      elseif (equationOfState.eq.'POLY3') then

         refTemp=eosRefT(kRef)
         refSalt=eosRefS(kRef)

         do j=jMin,jMax
            do i=iMin,iMax
               tP=theta(i,j,k,bi,bj)-refTemp
               sP=salt(i,j,k,bi,bj)-refSalt
#ifdef USE_FACTORIZED_POLY
               betaloc(i,j) =
     &    ( eosC(9,kRef)*sP*3. + eosC(5,kRef)*2. )*sP + eosC(2,kRef)
     &   + ( eosC(7,kRef)*tP
     &      +eosC(8,kRef)*sP*2. + eosC(4,kRef)
     &     )*tP
#else
               betaloc(i,j) =
     &              eosC(2,kRef)                +
     &              eosC(4,kRef)*tP             +
     &              eosC(5,kRef)         *sP*2. +
     &              eosC(7,kRef)*tP*tP          +
     &              eosC(8,kRef)*tP      *sP*2. +
     &              eosC(9,kRef)         *sP*sP*3.
#endif
            enddo
         enddo

      elseif ( equationOfState(1:5).eq.'JMD95'
     &        .or. equationOfState.eq.'UNESCO' ) then
C     nonlinear equation of state in pressure coordinates

         CALL FIND_RHOP0(
     I        bi, bj, iMin, iMax, jMin, jMax, k,
     I        theta, salt,
     O        rhoP0,
     I        myThid )
         
         CALL FIND_BULKMOD(
     I        bi, bj, iMin, iMax, jMin, jMax,  k, kRef,
     I        theta, salt,
     O        bulkMod,
     I        myThid )

         do j=jMin,jMax
            do i=iMin,iMax

C     abbreviations
               t1 = theta(i,j,k,bi,bj) 
               t2 = t1*t1
               t3 = t2*t1
               
               s1  = salt(i,j,k,bi,bj)
               s3o2 = 1.5*sqrt(s1)

               p1  = pressure(i,j,kRef,bi,bj)*SItoBar

C     d(rho)/d(S)
C     of fresh water at p = 0
               drhoP0dS = 0. _d 0
C     of salt water at p = 0
               drhoP0dS = drhoP0dS
     &              + eosJMDCSw(1)
     &              + eosJMDCSw(2)*t1
     &              + eosJMDCSw(3)*t2
     &              + eosJMDCSw(4)*t3
     &              + eosJMDCSw(5)*t3*t1
     &       + s3o2*(
     &                eosJMDCSw(6)
     &              + eosJMDCSw(7)*t1
     &              + eosJMDCSw(8)*t2
     &              )
     &              + 2*eosJMDCSw(9)*s1
C     d(bulk modulus)/d(S)
C     of fresh water at p = 0
               dKdS = 0. _d 0
C     of sea water at p = 0
               dKdSSalt =
     &                eosJMDCKSw(1)
     &              + eosJMDCKSw(2)*t1
     &              + eosJMDCKSw(3)*t2
     &              + eosJMDCKSw(4)*t3
     &       + s3o2*( eosJMDCKSw(5)
     &              + eosJMDCKSw(6)*t1
     &              + eosJMDCKSw(7)*t2
     &              )

C     of sea water at p
               dKdSPres = 
     &           p1*( eosJMDCKP(5)
     &              + eosJMDCKP(6)*t1
     &              + eosJMDCKP(7)*t2
     &              )
     &        + s3o2*p1*eosJMDCKP(8)
     &      + p1*p1*( eosJMDCKP(12)
     &              + eosJMDCKP(13)*t1
     &              + eosJMDCKP(14)*t2
     &              )

               dKdS = dKdSSalt + dKdSPres

               betaloc(i,j) = 
     &              ( bulkmod(i,j)**2*drhoP0dS
     &              - bulkmod(i,j)*p1*drhoP0dS
     &              - rhoP0(i,j)*p1*dKdS )
     &              /( bulkmod(i,j) - p1 )**2
               
              
            end do
         end do
      elseif ( equationOfState.eq.'MDJWF' ) then

         CALL FIND_RHO( bi, bj, iMin, iMax, jMin, jMax,  k, kRef,
     &        theta, salt, rhoLoc, myThid )
         CALL FIND_RHODEN( bi, bj, iMin, iMax, jMin, jMax, k, kRef,
     &      theta, salt, rhoDen, myThid )
         
         do j=jMin,jMax
            do i=iMin,iMax
               t1  = theta(i,j,k,bi,bj)
               t2  = t1*t1
               s1  = salt(i,j,k,bi,bj)
               sp5 = sqrt(s1) 
         
               p1   = pressure(i,j,k,bi,bj)*SItodBar
               p1t1 = p1*t1
               
               dnum_dsalt = eosMDJWFnum(4) 
     &              + eosMDJWFnum(5)*t1
     &              + 2.*eosMDJWFnum(6)*s1 + eosMDJWFnum(9)*p1
               dden_dsalt = eosMDJWFden(5) 
     &              + t1*( eosMDJWFden(6) + eosMDJWFden(7)*t2 ) 
     &              + 1.5*sp5*(eosMDJWFden(8) + eosMDJWFden(9)*t2)

               betaLoc(i,j) = rhoDen(i,j)*( dnum_dsalt 
     &              - rhoLoc(i,j)*dden_dsalt )

            end do
         end do

      else
         write(*,*) 'FIND_BETA: equationOfState = ',equationOfState
         stop 'FIND_BETA: "equationOfState" has illegal value'
      endif

      return
      end

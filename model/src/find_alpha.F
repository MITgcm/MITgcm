C $Header: /u/gcmpack/MITgcm/model/src/find_alpha.F,v 1.7 2002/08/15 18:48:47 adcroft Exp $
C $Name:  $

#include "CPP_OPTIONS.h"
#define USE_FACTORIZED_POLY

CBOP
C     !ROUTINE: FIND_ALPHA
C     !INTERFACE:
      SUBROUTINE FIND_ALPHA (
     I     bi, bj, iMin, iMax, jMin, jMax,  k, kRef, eqn,
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
C     | eqn - determines the eqn. of state: 'LINEAR' or 'POLY3'   
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
      character*(*) eqn
      _RL alphaloc(1-Olx:sNx+Olx,1-Oly:sNy+Oly)

C     !LOCAL VARIABLES:
c Local
      integer i,j
      _RL refTemp,refSalt,tP,sP
      _RL t, t2, t3, s, s3o2, p, p2
      _RL drhoP0dtheta, drhoP0dthetaFresh, drhoP0dthetaSalt
      _RL dKdtheta, dKdthetaFresh, dKdthetaSalt, dKdthetaPres
      _RL rhoP0(1-Olx:sNx+Olx,1-Oly:sNy+Oly)
      _RL bulkMod(1-Olx:sNx+Olx,1-Oly:sNy+Oly)
      integer myThid
CEOP

Cml      stop 'myThid is not properly defined in this subroutine'

      if (eqn.eq.'LINEAR') then

         do j=jMin,jMax
            do i=iMin,iMax
               alphaloc(i,j) = -rhonil * tAlpha
            enddo
         enddo
         
      elseif (eqn.eq.'POLY3') then

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
         
      elseif ( eqn(1:5).eq.'JMD95' ) then
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
               t  = theta(i,j,k,bi,bj) 
               t2 = t*t
               t3 = t2*t
               
               if ( salt(i,j,k,bi,bj) .lt. 0. _d 0 ) then
C     issue a warning
                  write(*,'(a,i3,a,i3,a,i3,a,e13.5)') 
     &             ' FIND_BETA: WARNING, s(',i,',',j,',',k,') = ', s
                  s = 0. _d 0
               else
                  s  = salt(i,j,k,bi,bj)
               end if
               s3o2 = sqrt(s*s*s)
               
               p  = pressure(i,j,kRef,bi,bj)*SItoBar
               p2 = p*p

C     d(rho)/d(theta)
C     of fresh water at p = 0
               drhoP0dthetaFresh = 
     &                eosJMDCFw(2)
     &           + 2.*eosJMDCFw(3)*t
     &           + 3.*eosJMDCFw(4)*t2
     &           + 4.*eosJMDCFw(5)*t3
     &           + 5.*eosJMDCFw(6)*t3*t
C     of salt water at p = 0
               drhoP0dthetaSalt = 
     &         s*(
     &                eosJMDCSw(2)
     &           + 2.*eosJMDCSw(3)*t
     &           + 3.*eosJMDCSw(4)*t2
     &           + 4.*eosJMDCSw(5)*t3
     &           )
     &       + s3o2*(
     &           +    eosJMDCSw(7)
     &           + 2.*eosJMDCSw(8)*t
     &           )
C     d(bulk modulus)/d(theta)
C     of fresh water at p = 0
               dKdthetaFresh = 
     &                eosJMDCKFw(2)
     &           + 2.*eosJMDCKFw(3)*t
     &           + 3.*eosJMDCKFw(4)*t2
     &           + 4.*eosJMDCKFw(5)*t3
C     of sea water at p = 0
               dKdthetaSalt =
     &         s*(    eosJMDCKSw(2)
     &           + 2.*eosJMDCKSw(3)*t
     &           + 3.*eosJMDCKSw(4)*t2
     &           )
     &    + s3o2*(    eosJMDCKSw(6)
     &           + 2.*eosJMDCKSw(7)*t
     &           )
C     of sea water at p
               dKdthetaPres =
     &         p*(    eosJMDCKP(2)
     &           + 2.*eosJMDCKP(3)*t
     &           + 3.*eosJMDCKP(4)*t2
     &           )
     &     + p*s*(    eosJMDCKP(6)
     &           + 2.*eosJMDCKP(7)*t
     &           )
     &      + p2*(    eosJMDCKP(10)
     &           + 2.*eosJMDCKP(11)*t
     &           )
     &    + p2*s*(    eosJMDCKP(13)
     &           + 2.*eosJMDCKP(14)*t
     &           )

               drhoP0dtheta  = drhoP0dthetaFresh 
     &                       + drhoP0dthetaSalt
               dKdtheta      = dKdthetaFresh 
     &                       + dKdthetaSalt 
     &                       + dKdthetaPres
               alphaloc(i,j) = 
     &              ( bulkmod(i,j)**2*drhoP0dtheta
     &              - bulkmod(i,j)*p*drhoP0dtheta
     &              - rhoP0(i,j)*p*dKdtheta )
     &              /( bulkmod(i,j) - p )**2
               
              
            end do
         end do
      else
         write(*,*) 'FIND_ALPHA: eqn = ',eqn
         stop 'FIND_ALPHA: argument "eqn" has illegal value'
      endif

      return
      end

      subroutine FIND_BETA (
     I     bi, bj, iMin, iMax, jMin, jMax,  k, kRef, eqn,
     O     betaloc )
C     /==========================================================\
C     | o SUBROUTINE FIND_BETA                                   |
C     |   Calculates [drho(S,T,z) / dS] of a horizontal slice    |
C     |==========================================================|
C     |                                                          |
C     | k - is the Theta/Salt level                              |
C     | kRef - determines pressure reference level               |
C     |        (not used in 'LINEAR' mode)                       |
C     | eqn - determines the eqn. of state: 'LINEAR' or 'POLY3'  |
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
      character*(*) eqn
      _RL betaloc(1-Olx:sNx+Olx,1-Oly:sNy+Oly)

c Local
      integer i,j
      _RL refTemp,refSalt,tP,sP
      _RL t, t2, t3, s, s3o2, p
      _RL drhoP0dS
      _RL dKdS, dKdSSalt, dKdSPres
      _RL rhoP0(1-Olx:sNx+Olx,1-Oly:sNy+Oly)
      _RL bulkMod(1-Olx:sNx+Olx,1-Oly:sNy+Oly)
      integer myThid
CEOP

Cml      stop 'myThid is not properly defined in this subroutine'

      if (eqn.eq.'LINEAR') then

         do j=jMin,jMax
            do i=iMin,iMax
               betaloc(i,j) = rhonil * sBeta
            enddo
         enddo
         
      elseif (eqn.eq.'POLY3') then

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

      elseif ( eqn(1:5).eq.'JMD95' ) then
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
               t  = theta(i,j,k,bi,bj) 
               t2 = t*t
               t3 = t2*t
               
               if ( salt(i,j,k,bi,bj) .lt. 0. _d 0 ) then
C     issue a warning
                  write(*,'(a,i3,a,i3,a,i3,a,e13.5)') 
     &             ' FIND_BETA: WARNING, s(',i,',',j,',',k,') = ', s
                  s = 0. _d 0
               else
                  s  = salt(i,j,k,bi,bj)
               end if
               s3o2 = 1.5*sqrt(s)

               p  = pressure(i,j,kRef,bi,bj)*SItoBar

C     d(rho)/d(S)
C     of fresh water at p = 0
               drhoP0dS = 0. _d 0
C     of salt water at p = 0
               drhoP0dS = drhoP0dS
     &              + eosJMDCSw(1)
     &              + eosJMDCSw(2)*t
     &              + eosJMDCSw(3)*t2
     &              + eosJMDCSw(4)*t3
     &              + eosJMDCSw(5)*t3*t
     &       + s3o2*(
     &                eosJMDCSw(6)
     &              + eosJMDCSw(7)*t
     &              + eosJMDCSw(8)*t2
     &              )
     &              + 2*eosJMDCSw(9)*s
C     d(bulk modulus)/d(S)
C     of fresh water at p = 0
               dKdS = 0. _d 0
C     of sea water at p = 0
               dKdSSalt =
     &                eosJMDCKSw(1)
     &              + eosJMDCKSw(2)*t
     &              + eosJMDCKSw(3)*t2
     &              + eosJMDCKSw(4)*t3
     &       + s3o2*( eosJMDCKSw(5)
     &              + eosJMDCKSw(6)*t
     &              + eosJMDCKSw(7)*t2
     &              )

C     of sea water at p
               dKdSPres = 
     &            p*( eosJMDCKP(5)
     &              + eosJMDCKP(6)*t
     &              + eosJMDCKP(7)*t2
     &              )
     &        + s3o2*p*eosJMDCKP(8)
     &        + p*p*( eosJMDCKP(12)
     &              + eosJMDCKP(13)*t
     &              + eosJMDCKP(14)*t2
     &              )

               dKdS = dKdSSalt + dKdSPres

               betaloc(i,j) = 
     &              ( bulkmod(i,j)**2*drhoP0dS
     &              - bulkmod(i,j)*p*drhoP0dS
     &              - rhoP0(i,j)*p*dKdS )
     &              /( bulkmod(i,j) - p )**2
               
              
            end do
         end do
      else
         write(*,*) 'FIND_BETA: eqn = ',eqn
         stop 'FIND_BETA: argument "eqn" has illegal value'
      endif

      return
      end

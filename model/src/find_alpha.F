C $Header: /u/gcmpack/MITgcm/model/src/find_alpha.F,v 1.5 2001/09/26 18:09:14 cnh Exp $
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
CEOP

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

c Arguments
      integer bi,bj,iMin,iMax,jMin,jMax
      integer k			! Level of Theta/Salt slice
      integer kRef		! Pressure reference level
      character*(*) eqn
      _RL betaloc(1-Olx:sNx+Olx,1-Oly:sNy+Oly)

c Local
      integer i,j
      _RL refTemp,refSalt,tP,sP

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

      else
         write(*,*) 'FIND_BETA: eqn = ',eqn
         stop 'FIND_BETA: argument "eqn" has illegal value'
      endif

      return
      end

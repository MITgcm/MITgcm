C $Header: /u/gcmpack/MITgcm/model/src/find_alpha.F,v 1.2 2000/11/13 16:32:58 heimbach Exp $

#include "CPP_OPTIONS.h"
#define USE_FACTORIZED_POLY

      subroutine FIND_ALPHA (
     I     bi, bj, iMin, iMax, jMin, jMax,  k, kRef, eqn,
     O     alphaloc )
C     /==========================================================\
C     | o SUBROUTINE FIND_ALPHA                                  |
C     |   Calculates [drho(S,T,z) / dT] of a horizontal slice    |
C     |==========================================================|
C     |                                                          |
C     | k - is the Theta/Salt level                              |
C     | kRef - determines pressure reference level               |
C     |        (not used in 'LINEAR' mode)                       |
C     | eqn - determines the eqn. of state: 'LINEAR' or 'POLY3'  |
C     |                                                          |
C     | alphaloc - drho / dT (kg/m^3/C)                          |
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
      _RL alphaloc(1-Olx:sNx+Olx,1-Oly:sNy+Oly)

c Local
      integer i,j
      _RL refTemp,refSalt,tP,sP

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
cph(  write statement confuses TAMC
cph         write(0,*) 'FIND_ALPHA: eqn = ',eqn
cph)
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
cph(  write statement confuses TAMC
cph         write(0,*) 'FIND_BETA: eqn = ',eqn
cph)
         stop 'FIND_BETA: argument "eqn" has illegal value'
      endif

      return
      end

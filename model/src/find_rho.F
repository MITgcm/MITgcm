C $Header: /u/gcmpack/MITgcm/model/src/find_rho.F,v 1.4 1998/04/29 21:30:18 adcroft Exp $

#include "CPP_EEOPTIONS.h"

! ==============================================================================
      subroutine FIND_RHO(
     I      bi, bj, iMin, iMax, jMin, jMax,  k, kRef, eqn,
     O      rholoc,
     I      myThid )
C     /==========================================================\
C     | o SUBROUTINE FIND_RHO                                    |
C     |   Calculates [rho(S,T,z)-Rhonil] of a slice              |
C     |==========================================================|
C     |                                                          |
C     | k - is the Theta/Salt level                              |
C     | kRef - determines pressure reference level               |
C     |        (not used in 'LINEAR' mode)                       |
C     | eqn - determines the eqn. of state: 'LINEAR' or 'POLY3'  |
C     |                                                          |
C     \==========================================================/
      implicit none
! Common
#include "SIZE.h"
#include "DYNVARS.h"
#include "PARAMS.h"
! Arguments
      integer bi,bj,iMin,iMax,jMin,jMax
      integer k			! Level of Theta/Salt slice
      integer kRef		! Pressure reference level
      character*(*) eqn
      _RL rholoc(1-Olx:sNx+Olx,1-Oly:sNy+Oly)
      integer myThid
! Local
      integer i,j
      _RL refTemp,refSalt,sigRef,tP,sP,deltaSig
! ------------------------------------------------------------------------------

      if (eqn.eq.'LINEAR') then

C ***NOTE***
C In the linear EOS, to make the static stability calculation meaningful
C we alway calculate the perturbation with respect to the surface level.
C **********
       refTemp=tRef(1)
       refSalt=sRef(1)

       do j=jMin,jMax
        do i=iMin,iMax
         rholoc(i,j)=rhonil*(
     &     sBeta*( salt(i,j,k,bi,bj)-refSalt)
     &   -tAlpha*(theta(i,j,k,bi,bj)-refTemp) )
        enddo
       enddo

      elseif (eqn.eq.'POLY3') then

       refTemp=eosRefT(kRef)
       refSalt=eosRefS(kRef)
       sigRef=eosSig0(kRef)

       do j=jMin,jMax
        do i=iMin,iMax
         tP=theta(i,j,k,bi,bj)-refTemp
         sP=salt(i,j,k,bi,bj)-refSalt
         deltaSig=
     &     eosC(kRef,1)*tP
     &    +eosC(kRef,2)         *sP
     &    +eosC(kRef,3)*tP*tP
     &    +eosC(kRef,4)*tP      *sP
     &    +eosC(kRef,5)         *sP*sP
     &    +eosC(kRef,6)*tP*tP*tP
     &    +eosC(kRef,7)*tP*tP   *sP
     &    +eosC(kRef,8)*tP      *sP*sP
     &    +eosC(kRef,9)         *sP*sP*sP
         rholoc(i,j)=1000.*(1.+sigRef+deltaSig)-Rhonil
        enddo
       enddo

      else
       write(0,*) 'FIND_RHO: eqn = ',eqn
       stop 'FIND_RHO: argument "eqn" has illegal value'
      endif

! ------------------------------------------------------------------------------
      return
      end
! ==============================================================================

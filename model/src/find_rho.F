C $Header: /u/gcmpack/MITgcm/model/src/find_rho.F,v 1.12 2000/09/11 23:01:36 heimbach Exp $

#include "CPP_OPTIONS.h"
#define USE_FACTORIZED_POLY

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
C     == Global variables ==
#include "SIZE.h"
#include "DYNVARS.h"
#include "EEPARAMS.h"
#include "PARAMS.h"

C     == Routine arguments ==
      integer bi,bj,iMin,iMax,jMin,jMax
      integer k			! Level of Theta/Salt slice
      integer kRef		! Pressure reference level
      character*(*) eqn
      _RL rholoc(1-Olx:sNx+Olx,1-Oly:sNy+Oly)
      integer myThid

C     == Local variables ==
      integer i,j
      _RL refTemp,refSalt,sigRef,tP,sP,deltaSig
      character*(max_len_mbuf) msgbuf

#ifdef ALLOW_AUTODIFF_TAMC
      DO j=1-OLy,sNy+OLy
       DO i=1-OLx,sNx+OLx
        rholoc(i,j) = 0.0
       ENDDO
      ENDDO
#endif

      if (eqn.eq.'LINEAR') then

C ***NOTE***
C In the linear EOS, to make the static stability calculation meaningful
C we alway calculate the perturbation with respect to the surface level.
C **********
       refTemp=tRef(kRef)
       refSalt=sRef(kRef)

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
       sigRef=eosSig0(kRef) + (1000.-Rhonil)

       do j=jMin,jMax
        do i=iMin,iMax
         tP=theta(i,j,k,bi,bj)-refTemp
         sP=salt(i,j,k,bi,bj)-refSalt
#ifdef USE_FACTORIZED_POLY
         deltaSig=
     &    (( eosC(9,kRef)*sP + eosC(5,kRef) )*sP + eosC(2,kRef) )*sP
     &   + ( ( eosC(6,kRef)
     &         *tP
     &        +eosC(7,kRef)*sP + eosC(3,kRef)
     &       )*tP
     &      +(eosC(8,kRef)*sP + eosC(4,kRef) )*sP + eosC(1,kRef)
     &     )*tP
#else
         deltaSig=
     &     eosC(1,kRef)*tP
     &    +eosC(2,kRef)         *sP
     &    +eosC(3,kRef)*tP*tP
     &    +eosC(4,kRef)*tP      *sP
     &    +eosC(5,kRef)         *sP*sP
     &    +eosC(6,kRef)*tP*tP*tP
     &    +eosC(7,kRef)*tP*tP   *sP
     &    +eosC(8,kRef)*tP      *sP*sP
     &    +eosC(9,kRef)         *sP*sP*sP
#endif
         rholoc(i,j)=sigRef+deltaSig
        enddo
       enddo

      else
       write(msgbuf,'(a,a)') ' FIND_RHO: eqn = ',eqn
       call print_error( msgbuf, mythid )
       stop 'ABNORMAL END: S/R FIND_RHO'
      endif

      return
      end

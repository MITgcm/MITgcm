C $Header: /u/gcmpack/MITgcm/model/src/find_rho.F,v 1.15 2001/09/26 18:09:15 cnh Exp $
C $Name:  $

#include "CPP_OPTIONS.h"
#define USE_FACTORIZED_POLY

CBOP
C     !ROUTINE: FIND_RHO
C     !INTERFACE:
      subroutine FIND_RHO(
     I      bi, bj, iMin, iMax, jMin, jMax,  k, kRef, eqn,
     I      tFld, sFld,
     O      rholoc,
     I      myThid )

C     !DESCRIPTION: \bv
C     *==========================================================*
C     | o SUBROUTINE FIND_RHO                                     
C     |   Calculates [rho(S,T,z)-Rhonil] of a slice               
C     *==========================================================*
C     |                                                           
C     | k - is the Theta/Salt level                               
C     | kRef - determines pressure reference level                
C     |        (not used in 'LINEAR' mode)                        
C     | eqn - determines the eqn. of state: 'LINEAR' or 'POLY3'   
C     |                                                           
C     *==========================================================*
C     \ev

C     !USES:
      implicit none
C     == Global variables ==
#include "SIZE.h"
#include "EEPARAMS.h"
#include "PARAMS.h"

C     !INPUT/OUTPUT PARAMETERS:
C     == Routine arguments ==
      integer bi,bj,iMin,iMax,jMin,jMax
      integer k			! Level of Theta/Salt slice
      integer kRef		! Pressure reference level
      character*(*) eqn
      _RL tFld(1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nr,nSx,nSy)
      _RL sFld(1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nr,nSx,nSy)
      _RL rholoc(1-Olx:sNx+Olx,1-Oly:sNy+Oly)
      integer myThid

C     !LOCAL VARIABLES:
C     == Local variables ==
      integer i,j
      _RL refTemp,refSalt,sigRef,tP,sP,deltaSig
      character*(max_len_mbuf) msgbuf
CEOP

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
     &     sBeta*(sFld(i,j,k,bi,bj)-refSalt)
     &   -tAlpha*(tFld(i,j,k,bi,bj)-refTemp) )
        enddo
       enddo
       
      elseif (eqn.eq.'POLY3') then

       refTemp=eosRefT(kRef)
       refSalt=eosRefS(kRef)
       sigRef=eosSig0(kRef) + (1000.-Rhonil)

       do j=jMin,jMax
        do i=iMin,iMax
         tP=tFld(i,j,k,bi,bj)-refTemp
         sP=sFld(i,j,k,bi,bj)-refSalt
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
       write(msgbuf,'(3a)') ' FIND_RHO: eqn = "',eqn,'"'
       call print_error( msgbuf, mythid )
       stop 'ABNORMAL END: S/R FIND_RHO'
      endif

      return
      end

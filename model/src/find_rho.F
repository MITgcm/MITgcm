C $Header: /u/gcmpack/MITgcm/model/src/find_rho.F,v 1.16 2002/08/07 16:55:52 mlosch Exp $
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
C     | eqn - determines the eqn. of state: 
C     |       'LINEAR', 'POLY3', 'UNESCO', 'JMD95Z', 'JMD95P'
C     |                                                           
C     *==========================================================*
C     \ev

C     !USES:
      implicit none
C     == Global variables ==
#include "SIZE.h"
#include "EEPARAMS.h"
#include "PARAMS.h"
#include "EOS.h"
#include "GRID.h"

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
      _RL rhoP0(1-Olx:sNx+Olx,1-Oly:sNy+Oly)
      _RL bulkMod(1-Olx:sNx+Olx,1-Oly:sNy+Oly)
      character*(max_len_mbuf) msgbuf
CEOP

#ifdef ALLOW_AUTODIFF_TAMC
      DO j=1-OLy,sNy+OLy
       DO i=1-OLx,sNx+OLx
        rholoc(i,j)  = 0. _d 0
        rhoP0(i,j)   = 0. _d 0
        bulkMod(i,j) = 0. _d 0
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

      elseif ( eqn(1:5).eq.'JMD95' .or. eqn.eq.'UNESCO' ) then
C     nonlinear equation of state in pressure coordinates

         CALL FIND_RHOP0(
     I        bi, bj, iMin, iMax, jMin, jMax, k,
     I        tFld, sFld,
     O        rhoP0,
     I        myThid )
         
         CALL FIND_BULKMOD(
     I        bi, bj, iMin, iMax, jMin, jMax,  k,
     I        tFld, sFld,
     O        bulkMod,
     I        myThid )
         
         do j=jMin,jMax
            do i=iMin,iMax

C     density of sea water at pressure p
               rholoc(i,j) = rhoP0(i,j)
     &              /(1. _d 0 - 
     &              pressure(i,j,k,bi,bj)*SItoBar/bulkMod(i,j))
     &              - rhonil

            end do
         end do

      else
       write(msgbuf,'(3a)') ' FIND_RHO: eqn = "',eqn,'"'
       call print_error( msgbuf, mythid )
       stop 'ABNORMAL END: S/R FIND_RHO'
      endif

      return
      end

CBOP
C     !ROUTINE: FIND_RHOP0
C     !INTERFACE:
      subroutine FIND_RHOP0(
     I      bi, bj, iMin, iMax, jMin, jMax, k,
     I      tFld, sFld,
     O      rhoP0,
     I      myThid )

C     !DESCRIPTION: \bv
C     *==========================================================*
C     | o SUBROUTINE FIND_RHOP0
C     |   Calculates rho(S,T,0) of a slice               
C     *==========================================================*
C     |                                                           
C     | k - is the surface level                               
C     |                                                           
C     *==========================================================*
C     \ev

C     !USES:
      implicit none
C     == Global variables ==
#include "SIZE.h"
#include "EEPARAMS.h"
#include "PARAMS.h"
#include "EOS.h"

C     !INPUT/OUTPUT PARAMETERS:
C     == Routine arguments ==
      integer bi,bj,iMin,iMax,jMin,jMax
      integer k			! Level of surface slice
      _RL tFld(1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nr,nSx,nSy)
      _RL sFld(1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nr,nSx,nSy)
      _RL rhoP0(1-Olx:sNx+Olx,1-Oly:sNy+Oly)
      _RL rfresh, rsalt
      integer myThid

C     !LOCAL VARIABLES:
C     == Local variables ==
      integer i,j
      _RL t, t2, t3, t4, s, s3o2
CEOP

      do j=jMin,jMax
         do i=iMin,iMax
C     abbreviations
            t  = tFld(i,j,k,bi,bj) 
            t2 = t*t
            t3 = t2*t
            t4 = t3*t

            if ( sFld(i,j,k,bi,bj) .lt. 0. _d 0 ) then
C     issue a warning
               write(*,'(a,i3,a,i3,a,i3,a,e13.5)') 
     &           ' FIND_RHOP0:   WARNING, s(',i,',',j,',',k,') = ', s
               s = 0. _d 0
            else
               s  = sFld(i,j,k,bi,bj)
            end if
            s3o2 = s*sqrt(s)
            
C     density of freshwater at the surface
            rfresh = 
     &             eosJMDCFw(1) 
     &           + eosJMDCFw(2)*t
     &           + eosJMDCFw(3)*t2
     &           + eosJMDCFw(4)*t3
     &           + eosJMDCFw(5)*t4
     &           + eosJMDCFw(6)*t4*t
C     density of sea water at the surface
            rsalt = 
     &         s*(
     &             eosJMDCSw(1)
     &           + eosJMDCSw(2)*t
     &           + eosJMDCSw(3)*t2
     &           + eosJMDCSw(4)*t3
     &           + eosJMDCSw(5)*t4
     &           )
     &       + s3o2*(
     &             eosJMDCSw(6)
     &           + eosJMDCSw(7)*t
     &           + eosJMDCSw(8)*t2
     &           )
     &           + eosJMDCSw(9)*s*s

            rhoP0(i,j) = rfresh + rsalt

         end do
      end do

      return
      end

C     !ROUTINE: FIND_BULKMOD
C     !INTERFACE:
      subroutine FIND_BULKMOD(
     I      bi, bj, iMin, iMax, jMin, jMax,  k,
     I      tFld, sFld,
     O      bulkMod,
     I      myThid )

C     !DESCRIPTION: \bv
C     *==========================================================*
C     | o SUBROUTINE FIND_BULKMOD                                     
C     |   Calculates the secant bulk modulus K(S,T,p) of a slice
C     *==========================================================*
C     |                                                           
C     | k - is the surface level                               
C     |                                                           
C     *==========================================================*
C     \ev

C     !USES:
      implicit none
C     == Global variables ==
#include "SIZE.h"
#include "EEPARAMS.h"
#include "PARAMS.h"
#include "EOS.h"

C     !INPUT/OUTPUT PARAMETERS:
C     == Routine arguments ==
      integer bi,bj,iMin,iMax,jMin,jMax
      integer k			! Level of surface slice
      _RL tFld(1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nr,nSx,nSy)
      _RL sFld(1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nr,nSx,nSy)
      _RL bulkMod(1-Olx:sNx+Olx,1-Oly:sNy+Oly)
      _RL bMfresh, bMsalt, bMpres
      integer myThid

C     !LOCAL VARIABLES:
C     == Local variables ==
      integer i,j
      _RL t, t2, t3, t4, s, s3o2, p, p2
CEOP

      do j=jMin,jMax
         do i=iMin,iMax
C     abbreviations
            t  = tFld(i,j,k,bi,bj) 
            t2 = t*t
            t3 = t2*t
            t4 = t3*t

            if ( sFld(i,j,k,bi,bj) .lt. 0. _d 0 ) then
C     issue a warning
               write(*,'(a,i3,a,i3,a,i3,a,e13.5)') 
     &           ' FIND_BULKMOD: WARNING, s(',i,',',j,',',k,') = ', s
               s = 0. _d 0
            else
               s  = sFld(i,j,k,bi,bj)
            end if
            s3o2 = s*sqrt(s)
C
            p = pressure(i,j,k,bi,bj)*SItoBar
            p2 = p*p
C     secant bulk modulus of fresh water at the surface
            bMfresh = 
     &             eosJMDCKFw(1)
     &           + eosJMDCKFw(2)*t
     &           + eosJMDCKFw(3)*t2
     &           + eosJMDCKFw(4)*t3
     &           + eosJMDCKFw(5)*t4
C     secant bulk modulus of sea water at the surface
            bMsalt =
     &         s*( eosJMDCKSw(1)
     &           + eosJMDCKSw(2)*t
     &           + eosJMDCKSw(3)*t2
     &           + eosJMDCKSw(4)*t3
     &           )
     &    + s3o2*( eosJMDCKSw(5)
     &           + eosJMDCKSw(6)*t
     &           + eosJMDCKSw(7)*t2
     &           )
C     secant bulk modulus of sea water at pressure p
            bMpres = 
     &         p*( eosJMDCKP(1)
     &           + eosJMDCKP(2)*t
     &           + eosJMDCKP(3)*t2
     &           + eosJMDCKP(4)*t3
     &           )
     &     + p*s*( eosJMDCKP(5)
     &           + eosJMDCKP(6)*t
     &           + eosJMDCKP(7)*t2
     &           )
     &      + p*s3o2*eosJMDCKP(8)
     &      + p2*( eosJMDCKP(9)
     &           + eosJMDCKP(10)*t
     &           + eosJMDCKP(11)*t2
     &           )
     &    + p2*s*( eosJMDCKP(12)
     &           + eosJMDCKP(13)*t
     &           + eosJMDCKP(14)*t2
     &           )

            bulkMod(i,j) = bMfresh + bMsalt + bMpres

         end do
      end do

      return
      end


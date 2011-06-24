C $Header: /u/gcmpack/MITgcm/pkg/fizhi/step_fizhi_corr.F,v 1.19 2011/06/24 01:20:49 jmc Exp $
C $Name:  $

#include "FIZHI_OPTIONS.h"
       subroutine step_fizhi_corr (myTime, myIter, myThid, dt)
c----------------------------------------------------------------------
c  Subroutine step_fizhi_corr - 'Wrapper' routine to advance
c        the physics state and make the new value.
c        At this point, increment with the "correction term"
c        which includes the dynamics tendency and the integral
c        constraint to enforce agreement with the dynamics state
c        Also: Set up "bi, bj loop" and some timers and clocks here.
c
c Call:phys2dyn (4) (interpolate physics state to dynamics grid
c                           for use in the correction terms)
c      AtoC         (convert physics state on dynamics grid to C-Grid)
c      CtoA         (convert correction term on dynamics grid to A-Grid)
c      dyn2phys (4) (interpolate A-Grid correction term to physics grid)
c      step_physics (advance physics state by correction term)
c-----------------------------------------------------------------------
       implicit none
#include "SIZE.h"
#include "GRID.h"
#include "fizhi_SIZE.h"
#include "fizhi_land_SIZE.h"
#include "DYNVARS.h"
#include "fizhi_coms.h"
#include "gridalt_mapping.h"
#include "EEPARAMS.h"
#include "SURFACE.h"
#ifdef ALLOW_DIAGNOSTICS
#include "fizhi_SHP.h"
#endif

       integer myIter, myThid
       _RL myTime
#ifdef ALLOW_DIAGNOSTICS
      logical  diagnostics_is_on
      external diagnostics_is_on
#endif

c pe on dynamics and physics grid refers to bottom edge
       _RL pephy(1-OLx:sNx+Olx,1-Oly:sNy+Oly,Nrphys+1,nSx,nSy)
       _RL pedyn(1-Olx:sNx+Olx,1-Oly:sNy+Oly,Nr+1,nSx,nSy)
       _RL windphy(1-Olx:sNx+Olx,1-Oly:sNy+Oly,Nrphys,nSx,nSy)
       _RL udyntemp(1-Olx:sNx+Olx,1-Oly:sNy+Oly,Nr,nSx,nSy)
       _RL vdyntemp(1-Olx:sNx+Olx,1-Oly:sNy+Oly,Nr,nSx,nSy)
       _RL thdyntemp(1-Olx:sNx+Olx,1-Oly:sNy+Oly,Nr,nSx,nSy)
       _RL sdyntemp(1-Olx:sNx+Olx,1-Oly:sNy+Oly,Nr,nSx,nSy)
       _RL uphytemp(1-Olx:sNx+Olx,1-Oly:sNy+Oly,Nrphys,nSx,nSy)
       _RL vphytemp(1-Olx:sNx+Olx,1-Oly:sNy+Oly,Nrphys,nSx,nSy)
       _RL thphytemp(1-Olx:sNx+Olx,1-Oly:sNy+Oly,Nrphys,nSx,nSy)
       _RL sphytemp(1-Olx:sNx+Olx,1-Oly:sNy+Oly,Nrphys,nSx,nSy)
       _RL tempphy(1-Olx:sNx+Olx,1-Oly:sNy+Oly,Nrphys,nSx,nSy)

       integer i, j, L, Lbotij, bi, bj
       integer im1, im2, jm1, jm2, idim1, idim2, jdim1, jdim2
       _RL dt

       _RL tempij(sNx,sNy)
       _RL dtfake
       _RL dtinv

       im1 = 1-OLx
       im2 = sNx+OLx
       jm1 = 1-OLy
       jm2 = sNy+OLy
       idim1 = 1
       idim2 = sNx
       jdim1 = 1
       jdim2 = sNy
       dtfake = 1. _d 0
       dtinv = 1. _d 0 / dt

       do bj = myByLo(myThid), myByHi(myThid)
       do bi = myBxLo(myThid), myBxHi(myThid)

C Construct Pressures on physics and dynamics grids
        do j = 1,sNy
        do i = 1,sNx
         do L = 1,Nr
          pedyn(i,j,L,bi,bj) = 0.
         enddo
        enddo
        enddo
        do j = 1,sNy
        do i = 1,sNx
         Lbotij = ksurfC(i,j,bi,bj)
         if(Lbotij.lt.nr+1)
     .    pedyn(i,j,Lbotij,bi,bj) = Ro_surf(i,j,bi,bj) + etaH(i,j,bi,bj)
        enddo
        enddo
        do j = 1,sNy
        do i = 1,sNx
         Lbotij = ksurfC(i,j,bi,bj)
         do L = Lbotij+1,Nr+1
          pedyn(i,j,L,bi,bj) = pedyn(i,j,L-1,bi,bj) -
     .               drF(L-1)* rStarExpC(i,j,bi,bj)*hfacC(i,j,L-1,bi,bj)
         enddo
c Do not use a zero field as the top edge pressure for interpolation
         if(pedyn(i,j,Nr+1,bi,bj).lt.1.e-5)
     .                               pedyn(i,j,Nr+1,bi,bj) = 1.e-5
        enddo
        enddo

        do j = 1,sNy
        do i = 1,sNx
         pephy(i,j,1,bi,bj)=Ro_surf(i,j,bi,bj) + etaH(i,j,bi,bj)
         do L = 2,Nrphys+1
          pephy(i,j,L,bi,bj)=pephy(i,j,L-1,bi,bj)-dpphys(i,j,L-1,bi,bj)
         enddo
c Do not use a zero field as the top edge pressure for interpolation
         if(pephy(i,j,Nrphys+1,bi,bj).lt.1.e-5)
     .                               pephy(i,j,Nrphys+1,bi,bj) = 1.e-5
        enddo
        enddo
c
c Create a wind magnitude field on the physics grid -
c   (Load the wind speed bottom up for use by dyn2phys)
        do L = 1,Nrphys
        do j = 1,sNy
        do i = 1,sNx
         windphy(i,j,L,bi,bj) =
     .     sqrt(uphy(i,j,Nrphys+1-L,bi,bj)*uphy(i,j,Nrphys+1-L,bi,bj)
     .        + vphy(i,j,Nrphys+1-L,bi,bj)*vphy(i,j,Nrphys+1-L,bi,bj))
        enddo
        enddo
        enddo
       enddo
       enddo

       CALL TIMER_START('PHYS2DYN          [STEP_FIZHI_CORR]',mythid)
       do bj = myByLo(myThid), myByHi(myThid)
       do bi = myBxLo(myThid), myBxHi(myThid)

c Compute correction term (new dyn state-phys state to dyn) on physics grid:
c    First: interp physics state to dynamics grid
C Note: physics field levels are numbered top down - need bottom up
        do L = 1,Nrphys
        do j = 1,sNy
        do i = 1,sNx
         tempphy(i,j,Nrphys+1-L,bi,bj) = uphy(i,j,L,bi,bj)
        enddo
        enddo
        enddo
        call phys2dyn(tempphy,pephy,im1,im2,jm1,jm2,Nrphys,Nsx,Nsy,
     .        1,sNx,1,sNy,bi,bj,pedyn,ksurfC,Nr,nlperdyn,udyntemp)
        do L = 1,Nrphys
        do j = 1,sNy
        do i = 1,sNx
         tempphy(i,j,Nrphys+1-L,bi,bj) = vphy(i,j,L,bi,bj)
        enddo
        enddo
        enddo
        call phys2dyn(tempphy,pephy,im1,im2,jm1,jm2,Nrphys,Nsx,Nsy,
     .        1,sNx,1,sNy,bi,bj,pedyn,ksurfC,Nr,nlperdyn,vdyntemp)
        do L = 1,Nrphys
        do j = 1,sNy
        do i = 1,sNx
         tempphy(i,j,Nrphys+1-L,bi,bj) = thphy(i,j,L,bi,bj)
        enddo
        enddo
        enddo
        call phys2dyn(tempphy,pephy,im1,im2,jm1,jm2,Nrphys,Nsx,Nsy,
     .        1,sNx,1,sNy,bi,bj,pedyn,ksurfC,Nr,nlperdyn,thdyntemp)
        do L = 1,Nrphys
        do j = 1,sNy
        do i = 1,sNx
         tempphy(i,j,Nrphys+1-L,bi,bj) = sphy(i,j,L,bi,bj)
        enddo
        enddo
        enddo
        call phys2dyn(tempphy,pephy,im1,im2,jm1,jm2,Nrphys,Nsx,Nsy,
     .        1,sNx,1,sNy,bi,bj,pedyn,ksurfC,Nr,nlperdyn,sdyntemp)

       enddo
       enddo
       CALL TIMER_STOP('PHYS2DYN          [STEP_FIZHI_CORR]',mythid)

c    Second: Convert physics state on dynamics grid to C-Grid

       CALL TIMER_START('ATOC              [STEP_FIZHI_CORR]',mythid)
       call AtoC(myThid,udyntemp,vdyntemp,maskC,im1,im2,jm1,jm2,Nr,
     .                     Nsx,Nsy,1,sNx,1,sNy,udyntemp,vdyntemp)
       CALL TIMER_STOP('ATOC              [STEP_FIZHI_CORR]',mythid)

c    Third: Subtract Phys state on dyn. grid from new dynamics state
       do bj = myByLo(myThid), myByHi(myThid)
       do bi = myBxLo(myThid), myBxHi(myThid)

        do L = 1,Nr
        do j = jdim1,jdim2
        do i = idim1,idim2
        udyntemp(i,j,L,bi,bj)=uvel(i,j,L,bi,bj)-udyntemp(i,j,L,bi,bj)
        vdyntemp(i,j,L,bi,bj)=vvel(i,j,L,bi,bj)-vdyntemp(i,j,L,bi,bj)
        thdyntemp(i,j,L,bi,bj)=theta(i,j,L,bi,bj)-thdyntemp(i,j,L,bi,bj)
        sdyntemp(i,j,L,bi,bj)=salt(i,j,L,bi,bj)-sdyntemp(i,j,L,bi,bj)
        enddo
        enddo
        enddo

       enddo
       enddo

c    Fourth: Convert correction terms to A-Grid
       CALL TIMER_START('CTOA              [STEP_FIZHI_CORR]',mythid)
        call CtoA(myThid,udyntemp,vdyntemp,maskW,maskS,im1,im2,jm1,jm2,
     .     Nr,Nsx,Nsy,1,sNx,1,sNy,udyntemp,vdyntemp)
       CALL TIMER_STOP('CTOA              [STEP_FIZHI_CORR]',mythid)

c    Fifth: Interpolate correction terms to physics grid
       CALL TIMER_START('DYN2PHYS          [STEP_FIZHI_CORR]',mythid)
       do bj = myByLo(myThid), myByHi(myThid)
       do bi = myBxLo(myThid), myBxHi(myThid)

        call dyn2phys(udyntemp,pedyn,im1,im2,jm1,jm2,Nr,Nsx,Nsy,1,sNx,
     .      1,sNy,bi,bj,windphy,pephy,ksurfC,Nrphys,nlperdyn,1,tempphy)
C Note: correction term is now bottom up - needed in top down arrays
        do L = 1,Nrphys
        do j = 1,sNy
        do i = 1,sNx
         uphytemp(i,j,Nrphys+1-L,bi,bj) = tempphy(i,j,L,bi,bj)
        enddo
        enddo
        enddo
        call dyn2phys(vdyntemp,pedyn,im1,im2,jm1,jm2,Nr,Nsx,Nsy,1,sNx,
     .      1,sNy,bi,bj,windphy,pephy,ksurfC,Nrphys,nlperdyn,1,tempphy)
        do L = 1,Nrphys
        do j = 1,sNy
        do i = 1,sNx
         vphytemp(i,j,Nrphys+1-L,bi,bj) = tempphy(i,j,L,bi,bj)
        enddo
        enddo
        enddo
        call dyn2phys(thdyntemp,pedyn,im1,im2,jm1,jm2,Nr,Nsx,Nsy,1,sNx,
     .     1,sNy,bi,bj,windphy,pephy,ksurfC,Nrphys,nlperdyn,0,tempphy)
        do L = 1,Nrphys
        do j = 1,sNy
        do i = 1,sNx
         thphytemp(i,j,Nrphys+1-L,bi,bj) = tempphy(i,j,L,bi,bj)
        enddo
        enddo
        enddo
        call dyn2phys(sdyntemp,pedyn,im1,im2,jm1,jm2,Nr,Nsx,Nsy,1,sNx,
     .      1,sNy,bi,bj,windphy,pephy,ksurfC,Nrphys,nlperdyn,0,tempphy)
        do L = 1,Nrphys
        do j = 1,sNy
        do i = 1,sNx
         sphytemp(i,j,Nrphys+1-L,bi,bj) = tempphy(i,j,L,bi,bj)
        enddo
        enddo
        enddo
       enddo
       enddo
       CALL TIMER_STOP('DYN2PHYS          [STEP_FIZHI_CORR]',mythid)

c    Last: Increment physics state by the correction term
       do bj = myByLo(myThid), myByHi(myThid)
       do bi = myBxLo(myThid), myBxHi(myThid)
        call step_physics(uphy,vphy,thphy,sphy,dtfake,im1,im2,jm1,jm2,
     .   Nrphys,Nsx,Nsy,1,sNx,1,sNy,bi,bj,
     .                            uphytemp,vphytemp,thphytemp,sphytemp)

        call qcheck (im1,im2,jm1,jm2,Nrphys,Nsx,Nsy,idim1,idim2,
     .         jdim1,jdim2,bi,bj,dpphys,sphy)

       enddo
       enddo

#ifdef ALLOW_DIAGNOSTICS
      call diagnostics_fill(uphy,'UAVE    ',0,Nrphys,0,1,1,myThid)
      call diagnostics_fill(vphy,'VAVE    ',0,Nrphys,0,1,1,myThid)
      call diagnostics_fill(thphy,'TAVE    ',0,Nrphys,0,1,1,myThid)
      call diagnostics_fill(sphy,'QAVE    ',0,Nrphys,0,1,1,myThid)
#endif

#ifdef ALLOW_DIAGNOSTICS
      do bj = myByLo(myThid), myByHi(myThid)
      do bi = myBxLo(myThid), myBxHi(myThid)
      do L=1,Nrphys

c Total Tendency on Fizhi Grid for U (m/sec/day)
c -----------------------------------------------
       if(diagnostics_is_on('TENDUFIZ',myThid) ) then
        do j=jm1,jm2
        do i=im1,im2
         tempij(i,j) = (uphy (i,j,L,bi,bj)-ubef(i,j,L,bi,bj) )
     .                     * 86400. _d 0 * dtinv
        enddo
        enddo
        call diagnostics_fill(tempij,'TENDUFIZ',L,1,3,bi,bj,myThid)
       endif

c Total Tendency on Fizhi Grid for V (m/sec/day)
c -----------------------------------------------
       if(diagnostics_is_on('TENDVFIZ',myThid) ) then
        do j=jm1,jm2
        do i=im1,im2
         tempij(i,j) = (vphy (i,j,L,bi,bj)-vbef(i,j,L,bi,bj) )
     .                     * 86400. _d 0 * dtinv
        enddo
        enddo
        call diagnostics_fill(tempij,'TENDVFIZ',L,1,3,bi,bj,myThid)
       endif

c Total Tendency on Fizhi Grid for U (m/sec/day)
c -----------------------------------------------
       if(diagnostics_is_on('TENDTFIZ',myThid) ) then
        do j=jm1,jm2
        do i=im1,im2
         tempij(i,j) = (thphy (i,j,L,bi,bj)-thbef(i,j,L,bi,bj) )
     .                     * 86400. _d 0 * dtinv
        enddo
        enddo
        call diagnostics_fill(tempij,'TENDTFIZ',L,1,3,bi,bj,myThid)
       endif

c Total Tendency on Fizhi Grid for U (m/sec/day)
c -----------------------------------------------
       if(diagnostics_is_on('TENDQFIZ',myThid) ) then
        do j=jm1,jm2
        do i=im1,im2
         tempij(i,j) = (sphy (i,j,L,bi,bj)-sbef(i,j,L,bi,bj) )
     .                     * 86400. _d 0 * dtinv
        enddo
        enddo
        call diagnostics_fill(tempij,'TENDQFIZ',L,1,3,bi,bj,myThid)
       endif

      enddo
      enddo
      enddo

c Gridalt Correction Term Tendency for U and V (m/sec/day)
c --------------------------------------------------------
      if(diagnostics_is_on('CORRDU  ',myThid) .or.
     .    diagnostics_is_on('CORRDV  ',myThid)   ) then

C gridalt correction term - first step is to compute adv+filters tendency
C                           on dynamics grid (total - physics tend)
      do bj = myByLo(myThid), myByHi(myThid)
      do bi = myBxLo(myThid), myBxHi(myThid)
       do L=1,Nr
       do j=jm1,jm2
       do i=im1,im2
        udyntemp(i,j,L,bi,bj) =
     .    (uvel(i,j,L,bi,bj)-udynbef(i,j,L,bi,bj))*dtinv -
     .                                        guphy(i,j,L,bi,bj)
        vdyntemp(i,j,L,bi,bj) =
     .    (vvel(i,j,L,bi,bj)-vdynbef(i,j,L,bi,bj))*dtinv -
     .                                        gvphy(i,j,L,bi,bj)
       enddo
       enddo
       enddo
C Next step - interpolate to fizhi grid
C  first put the u and v tendencies on an a-grid
       CALL TIMER_START('CTOA              [STEP_FIZHI_CORR]',mythid)
       call CtoA(myThid,udyntemp,vdyntemp,maskW,maskS,im1,im2,jm1,jm2,
     .     Nr,Nsx,Nsy,1,sNx,1,sNy,udyntemp,vdyntemp)
       CALL TIMER_STOP('CTOA              [STEP_FIZHI_CORR]',mythid)
C  then do vertical interpolation
       do L = 1,Nrphys
       do j = 1,sNy
       do i = 1,sNx
        windphy(i,j,L,bi,bj) =
     .     sqrt(uphy(i,j,Nrphys+1-L,bi,bj)*uphy(i,j,Nrphys+1-L,bi,bj)
     .        + vphy(i,j,Nrphys+1-L,bi,bj)*vphy(i,j,Nrphys+1-L,bi,bj))
       enddo
       enddo
       enddo
       CALL TIMER_START('DYN2PHYS          [STEP_FIZHI_CORR]',mythid)
        call dyn2phys(udyntemp,pedyn,im1,im2,jm1,jm2,Nr,Nsx,Nsy,1,sNx,
     .      1,sNy,bi,bj,windphy,pephy,ksurfC,Nrphys,nlperdyn,1,tempphy)
       CALL TIMER_STOP('DYN2PHYS          [STEP_FIZHI_CORR]',mythid)
C Note: adv+filters term is now bottom up - needed in top down arrays
       do L = 1,Nrphys
       do j = 1,sNy
       do i = 1,sNx
         uphytemp(i,j,Nrphys+1-L,bi,bj) = tempphy(i,j,L,bi,bj)
       enddo
       enddo
       enddo
       call dyn2phys(vdyntemp,pedyn,im1,im2,jm1,jm2,Nr,Nsx,Nsy,1,sNx,
     .      1,sNy,bi,bj,windphy,pephy,ksurfC,Nrphys,nlperdyn,1,tempphy)
       do L = 1,Nrphys
       do j = 1,sNy
       do i = 1,sNx
         vphytemp(i,j,Nrphys+1-L,bi,bj) = tempphy(i,j,L,bi,bj)
       enddo
       enddo
       enddo
C Last Step - subtract adv+filters and physics tend from total tend on physics grid
       do L = 1,Nrphys
       do j = 1,sNy
       do i = 1,sNx
        uphytemp(i,j,L,bi,bj)=
     .    ( (uphy(i,j,L,bi,bj)-ubef(i,j,L,bi,bj))*dtinv
     .     - duphy(i,j,L,bi,bj) - uphytemp(i,j,L,bi,bj) ) * 86400. _d 0
        vphytemp(i,j,L,bi,bj)=
     .    ( (vphy(i,j,L,bi,bj)-vbef(i,j,L,bi,bj))*dtinv
     .     - dvphy(i,j,L,bi,bj) - vphytemp(i,j,L,bi,bj) ) * 86400. _d 0
       enddo
       enddo
       enddo
      enddo
      enddo

      if(diagnostics_is_on('CORRDU  ',myThid)) then
       call diagnostics_fill(uphytemp,'CORRDU  ',0,Nrphys,0,1,1,myThid)
      endif
      if(diagnostics_is_on('CORRDV  ',myThid)) then
       call diagnostics_fill(vphytemp,'CORRDV  ',0,Nrphys,0,1,1,myThid)
      endif

      endif

c Gridalt Correction Term Tendency for TH (deg K/day)
c --------------------------------------------------------
      if(diagnostics_is_on('CORRDT  ',myThid))  then

C gridalt correction term - first step is to compute adv+filters tendency
C                           on dynamics grid (total - physics tend)
      do bj = myByLo(myThid), myByHi(myThid)
      do bi = myBxLo(myThid), myBxHi(myThid)
       do L=1,Nr
       do j=jm1,jm2
       do i=im1,im2
        thdyntemp(i,j,L,bi,bj) =
     .    (theta(i,j,L,bi,bj)-thdynbef(i,j,L,bi,bj))*dtinv -
     .                                        gthphy(i,j,L,bi,bj)
       enddo
       enddo
       enddo
C Next step - interpolate to fizhi grid
       CALL TIMER_START('DYN2PHYS          [STEP_FIZHI_CORR]',mythid)
       call dyn2phys(thdyntemp,pedyn,im1,im2,jm1,jm2,Nr,Nsx,Nsy,1,sNx,
     .     1,sNy,bi,bj,windphy,pephy,ksurfC,Nrphys,nlperdyn,0,tempphy)
       CALL TIMER_STOP('DYN2PHYS          [STEP_FIZHI_CORR]',mythid)
C Note: adv+filters term is now bottom up - needed in top down arrays
       do L = 1,Nrphys
       do j = 1,sNy
       do i = 1,sNx
         thphytemp(i,j,Nrphys+1-L,bi,bj) = tempphy(i,j,L,bi,bj)
       enddo
       enddo
       enddo
C Last Step - subtract adv+filters and physics tend from total tend on physics grid
       do L = 1,Nrphys
       do j = 1,sNy
       do i = 1,sNx
        thphytemp(i,j,L,bi,bj)=
     .    ( (thphy(i,j,L,bi,bj)-thbef(i,j,L,bi,bj))*dtinv
     .     - dthphy(i,j,L,bi,bj) - thphytemp(i,j,L,bi,bj)
     .    ) * 86400. _d 0
       enddo
       enddo
       enddo
      enddo
      enddo

      call diagnostics_fill(thphytemp,'CORRDT  ',0,Nrphys,0,1,1,myThid)
      endif

c Gridalt Correction Term Tendency for Q (kg/kg/day)
c --------------------------------------------------------
      if(diagnostics_is_on('CORRDQ  ',myThid))  then

C gridalt correction term - first step is to compute adv+filters tendency
C                           on dynamics grid (total - physics tend)
      do bj = myByLo(myThid), myByHi(myThid)
      do bi = myBxLo(myThid), myBxHi(myThid)
       do L=1,Nr
       do j=jm1,jm2
       do i=im1,im2
        sdyntemp(i,j,L,bi,bj) =
     .    (salt(i,j,L,bi,bj)-sdynbef(i,j,L,bi,bj))*dtinv -
     .                                        gsphy(i,j,L,bi,bj)
       enddo
       enddo
       enddo
C Next step - interpolate to fizhi grid
       CALL TIMER_START('DYN2PHYS          [STEP_FIZHI_CORR]',mythid)
       call dyn2phys(sdyntemp,pedyn,im1,im2,jm1,jm2,Nr,Nsx,Nsy,1,sNx,
     .      1,sNy,bi,bj,windphy,pephy,ksurfC,Nrphys,nlperdyn,0,tempphy)
       CALL TIMER_STOP('DYN2PHYS          [STEP_FIZHI_CORR]',mythid)
C Note: adv+filters term is now bottom up - needed in top down arrays
       do L = 1,Nrphys
       do j = 1,sNy
       do i = 1,sNx
         sphytemp(i,j,Nrphys+1-L,bi,bj) = tempphy(i,j,L,bi,bj)
       enddo
       enddo
       enddo
C Last Step - subtract adv+filters and physics tend from total tend on physics grid
       do L = 1,Nrphys
       do j = 1,sNy
       do i = 1,sNx
        sphytemp(i,j,L,bi,bj)=
     .    ( (sphy(i,j,L,bi,bj)-sbef(i,j,L,bi,bj))*dtinv
     .     - dsphy(i,j,L,bi,bj) - sphytemp(i,j,L,bi,bj) ) * 86400. _d 0
       enddo
       enddo
       enddo
      enddo
      enddo

      call diagnostics_fill(sphytemp,'CORRDQ  ',0,Nrphys,0,1,1,myThid)
      endif
#endif

      return
      end

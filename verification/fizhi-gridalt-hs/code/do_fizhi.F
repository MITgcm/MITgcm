       subroutine do_fizhi(uphy,vphy,thphy,sphy,pephy,
     .                              ctmt,xxmt,yymt,zetamt,xlmt,khmt,tke,
     .                xC,yC,
     .im1,im2,jm1,jm2,Nrphys,Nsx,Nsy,idim1,idim2,jdim1,jdim2,bi,bj,nchp,
     .                                         duphy,dvphy,dthphy,dsphy)
c-----------------------------------------------------------------------
c Dummy routine to calculate physics increments - here set them to 
c                                  the Held-Suarez forcing terms
c-----------------------------------------------------------------------
       implicit none
#include "CPP_OPTIONS.h"

       integer im1,im2,jm1,jm2,idim1,idim2,jdim1,jdim2
       integer Nrphys,Nsx,Nsy,bi,bj,nchp
       _RL uphy(im1:im2,jm1:jm2,Nrphys,Nsx,Nsy)
       _RL vphy(im1:im2,jm1:jm2,Nrphys,Nsx,Nsy)
       _RL thphy(im1:im2,jm1:jm2,Nrphys,Nsx,Nsy)
       _RL sphy(im1:im2,jm1:jm2,Nrphys,Nsx,Nsy)
       _RL pephy(im1:im2,jm1:jm2,Nrphys+1,Nsx,Nsy)
       _RL ctmt(nchp,Nsx,Nsy),xxmt(nchp,Nsx,Nsy),yymt(nchp,Nsx,Nsy)
       _RL zetamt(nchp,Nsx,Nsy)
       _RL xlmt(nchp,Nrphys,Nsx,Nsy),khmt(nchp,Nrphys,Nsx,Nsy)
       _RL tke(nchp,Nrphys,Nsx,Nsy)
       _RL duphy(im1:im2,jm1:jm2,Nrphys,Nsx,Nsy)
       _RL dvphy(im1:im2,jm1:jm2,Nrphys,Nsx,Nsy)
       _RL dthphy(im1:im2,jm1:jm2,Nrphys,Nsx,Nsy)
       _RL dsphy(im1:im2,jm1:jm2,Nrphys,Nsx,Nsy)
       _RS xC(im1:im2,jm1:jm2,Nsx,Nsy)
       _RS yC(im1:im2,jm1:jm2,Nsx,Nsy)
c
       integer i,j,L
       _RL kF,sigma_b,ks,ka,deg2rad,pi,atm_po,atm_kappa,termp,kv,kT
       _RL term1,term2,thetalim,thetaeq,recip_p0g
       _RL getcon

       kF=1. _d 0/86400. _d 0
       sigma_b = 0.7 _d 0
       ka=1. _d 0/(40. _d 0*86400. _d 0)
       ks=1. _d 0/(4. _d 0 *86400. _d 0)
       pi = getcon('PI')
       atm_kappa = getcon('KAPPA')
       atm_po = getcon('ATMPOPA')
       deg2rad = getcon('DEG2RAD')

       do L = 1,Nrphys
        do j = jdim1,jdim2
        do i = idim1,idim2
         recip_P0g= 1. _d 0 / pephy(i,j,1,bi,bj)
c U  and V terms:
         termP=0.5 _d 0*((pephy(i,j,L,bi,bj)+pephy(i,j,L+1,bi,bj))
     &                   *recip_P0g )
         kV=kF*MAX( 0. _d 0, (termP-sigma_b)/(1. _d 0-sigma_b) )
         duphy(i,j,L,bi,bj)= -kV*uphy(i,j,L,bi,bj)
         dvphy(i,j,L,bi,bj)= -kV*vphy(i,j,L,bi,bj)

c T terms
C--   Forcing term(s)
         term1=60. _d 0*(sin(yC(I,J,bi,bj)*deg2rad)**2)
         termP=0.5 _d 0*( pephy(i,j,L,bi,bj) + pephy(i,j,L+1,bi,bj) )
         term2=10. _d 0*log(termP/atm_po)
     &            *(cos(yC(I,J,bi,bj)*deg2rad)**2)
         thetaLim = 200. _d 0/ ((termP/atm_po)**atm_kappa)
         thetaEq=315. _d 0-term1-term2
         thetaEq=MAX(thetaLim,thetaEq)
         kT=ka+(ks-ka)
     &     *MAX(0. _d 0,
     &       (termP*recip_P0g-sigma_b)/(1. _d 0-sigma_b) )
     &     *COS((yC(I,J,bi,bj)*deg2rad))**4
         if(termP*recip_P0g.gt.0.04)then
          dthphy(i,j,L,bi,bj)=- kT*( thphy(I,J,L,bi,bj)-thetaEq )
         else
          dthphy(i,j,L,bi,bj)=0.
         endif

c S terms (hs runs dry - no moisture)
C--   Forcing term(s)
         dsphy(i,j,L,bi,bj)=0.

        enddo
        enddo
       enddo

       return
       end

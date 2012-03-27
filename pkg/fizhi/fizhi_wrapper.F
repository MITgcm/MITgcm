C $Header: /u/gcmpack/MITgcm/pkg/fizhi/fizhi_wrapper.F,v 1.36 2012/03/27 15:48:27 jmc Exp $
C $Name:  $

#include "FIZHI_OPTIONS.h"
       SUBROUTINE FIZHI_WRAPPER (myTime, myIter, myThid)
c-----------------------------------------------------------------------
c  Subroutine fizhi_wrapper - 'Wrapper' routine to interface
c        with physics driver.
c        1) Set up "bi, bj loop"  and some timers and clocks.
c        2) Call do_fizhi - driver for physics which computes tendencies
c        3) Interpolate tendencies to dynamics grid in vertical
c        4) Convert u,v tendencies to C-Grid
c
c Calls: do_fizhi (get u,v,t,s tend, step tke, etc and tc, etc. forward)
c        phys2dyn (4 calls - all physics tendencies)
c        AtoC (u and v tendencies)
c-----------------------------------------------------------------------
       IMPLICIT NONE
#include "SIZE.h"
#include "GRID.h"
#include "EEPARAMS.h"
#include "PARAMS.h"
#include "SURFACE.h"
#include "DYNVARS.h"
#include "fizhi_land_SIZE.h"
#include "fizhi_SIZE.h"
#include "fizhi_coms.h"
#include "gridalt_mapping.h"
#include "fizhi_land_coms.h"
#include "fizhi_earth_coms.h"
#include "fizhi_ocean_coms.h"
#include "fizhi_chemistry_coms.h"
#ifdef ALLOW_DIAGNOSTICS
#include "fizhi_SHP.h"
#endif

       INTEGER myIter, myThid
       _RL myTime
       LOGICAL  diagnostics_is_on
       EXTERNAL diagnostics_is_on

c pe on dynamics and physics grid refers to bottom edge
       _RL pephy4fiz(1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nrphys+1,nSx,nSy)
       _RL pephy(1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nrphys+1,nSx,nSy)
       _RL pedyn(1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nr+1,nSx,nSy)
       _RL tempphy(1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nrphys,nSx,nSy)
       _RL fracland(sNx,sNy,nSx,nSy)
       _RL tempLdiag(sNx,sNy,Nrphys+1)
       _RL tempLdiag2(sNx,sNy,Nrphys)
       _RL tempdiag(sNx,sNy)
       _RL slp(sNx,sNy)

       INTEGER i, j, L, Lbotij, bi, bj
       INTEGER im1, im2, jm1, jm2, idim1, idim2, jdim1, jdim2
       _RL grav, getcon

       grav = getcon('GRAVITY')
       idim1 = 1-OLx
       idim2 = sNx+OLx
       jdim1 = 1-OLy
       jdim2 = sNy+OLy
       im1 = 1
       im2 = sNx
       jm1 = 1
       jm2 = sNy

#ifdef ALLOW_DIAGNOSTICS
      if ( useDiagnostics ) then
       if(diagnostics_is_on('TENDUFIZ',myThid) .or.
     &       diagnostics_is_on('CORRDU  ',myThid) ) then
        do bj = myByLo(myThid), myByHi(myThid)
        do bi = myBxLo(myThid), myBxHi(myThid)
        do L = 1,Nrphys
        do j = 1,sNy
        do i = 1,sNx
         ubef(i,j,L,bi,bj) = uphy(i,j,L,bi,bj)
        enddo
        enddo
        enddo
        do L = 1,Nr
        do j = 1,sNy
        do i = 1,sNx+1
         udynbef(i,j,L,bi,bj) = uvel(i,j,L,bi,bj)
        enddo
        enddo
        enddo
        enddo
        enddo
       endif
       if(diagnostics_is_on('TENDVFIZ',myThid) .or.
     &       diagnostics_is_on('CORRDV  ',myThid) ) then
        do bj = myByLo(myThid), myByHi(myThid)
        do bi = myBxLo(myThid), myBxHi(myThid)
        do L = 1,Nrphys
        do j = 1,sNy
        do i = 1,sNx
         vbef(i,j,L,bi,bj) = vphy(i,j,L,bi,bj)
        enddo
        enddo
        enddo
        do L = 1,Nr
        do j = 1,sNy+1
        do i = 1,sNx
         vdynbef(i,j,L,bi,bj) = vvel(i,j,L,bi,bj)
        enddo
        enddo
        enddo
        enddo
        enddo
       endif
       if(diagnostics_is_on('TENDTFIZ',myThid) .or.
     &       diagnostics_is_on('CORRDT  ',myThid) ) then
        do bj = myByLo(myThid), myByHi(myThid)
        do bi = myBxLo(myThid), myBxHi(myThid)
        do L = 1,Nrphys
        do j = 1,sNy
        do i = 1,sNx
         thbef(i,j,L,bi,bj) = thphy(i,j,L,bi,bj)
        enddo
        enddo
        enddo
        do L = 1,Nr
        do j = 1,sNy
        do i = 1,sNx
         thdynbef(i,j,L,bi,bj) = theta(i,j,L,bi,bj)
        enddo
        enddo
        enddo
        enddo
        enddo
       endif
       if(diagnostics_is_on('TENDQFIZ',myThid) .or.
     &       diagnostics_is_on('CORRDQ  ',myThid) ) then
        do bj = myByLo(myThid), myByHi(myThid)
        do bi = myBxLo(myThid), myBxHi(myThid)
        do L = 1,Nrphys
        do j = 1,sNy
        do i = 1,sNx
         sbef(i,j,L,bi,bj) = sphy(i,j,L,bi,bj)
        enddo
        enddo
        enddo
        do L = 1,Nr
        do j = 1,sNy
        do i = 1,sNx
         sdynbef(i,j,L,bi,bj) = salt(i,j,L,bi,bj)
        enddo
        enddo
        enddo
        enddo
        enddo
       endif
      endif
#endif

       do bj = myByLo(myThid), myByHi(myThid)
       do bi = myBxLo(myThid), myBxHi(myThid)

c Construct the physics grid pressures
C  Note: Need one array to send to fizhi (top-down) and another
C        For the interpolations between physics and dynamics (bottom-up)
        do j = 1,sNy
        do i = 1,sNx
         pephy(i,j,1,bi,bj)=(Ro_surf(i,j,bi,bj) + etaH(i,j,bi,bj))
         do L = 2,Nrphys+1
          pephy(i,j,L,bi,bj)=pephy(i,j,L-1,bi,bj)-dpphys(i,j,L-1,bi,bj)
         enddo
c Do not use a zero field as the top edge pressure for interpolation
         do L = 1,Nrphys+1
          pephy4fiz(i,j,Nrphys+2-L,bi,bj)=pephy(i,j,L,bi,bj)
         enddo
         if(pephy(i,j,Nrphys+1,bi,bj).lt.1.e-5)
     &                               pephy(i,j,Nrphys+1,bi,bj) = 1.e-5
        enddo
        enddo
C Build pressures on dynamics grid
        do j = 1,sNy
        do i = 1,sNx
         do L = 1,Nr
          pedyn(i,j,L,bi,bj) = 0.
         enddo
        enddo
        enddo
        do j = 1,sNy
        do i = 1,sNx
         Lbotij = kSurfC(i,j,bi,bj)
         if(Lbotij.ne.0.)
     & pedyn(i,j,Lbotij,bi,bj) = (Ro_surf(i,j,bi,bj) + etaH(i,j,bi,bj))
        enddo
        enddo
        do j = 1,sNy
        do i = 1,sNx
         Lbotij = kSurfC(i,j,bi,bj)
         do L = Lbotij+1,Nr+1
          pedyn(i,j,L,bi,bj) = pedyn(i,j,L-1,bi,bj) -
     &           drF(L-1)* rStarExpC(i,j,bi,bj)*hfacC(i,j,L-1,bi,bj)
         enddo
c Do not use a zero field as the top edge pressure for interpolation
         if(pedyn(i,j,Nr+1,bi,bj).lt.1.e-5)
     &                               pedyn(i,j,Nr+1,bi,bj) = 1.e-5
        enddo
        enddo
       enddo
       enddo

#ifdef ALLOW_DIAGNOSTICS
      if ( useDiagnostics ) then
       if(diagnostics_is_on('FIZPRES ',myThid) ) then
        do bj = myByLo(myThid), myByHi(myThid)
        do bi = myBxLo(myThid), myBxHi(myThid)
        do j = 1,sNy
        do i = 1,sNx
        do L = 1,Nrphys
         tempphy(i,j,L,bi,bj) = pephy4fiz(i,j,L,bi,bj)
        enddo
        enddo
        enddo
        enddo
        enddo
        call diagnostics_fill(tempphy,'FIZPRES ',0,
     &                                     Nrphys,0,1,1,myThid)
       endif
      endif
#endif

       CALL TIMER_START ('DO_FIZHI          [FIZHI_WRAPPER]',myThid)
       do bj = myByLo(myThid), myByHi(myThid)
       do bi = myBxLo(myThid), myBxHi(myThid)
        call get_landfrac(im2,jm2,nSx,nSy,bi,bj,maxtyp,
     &        surftype,tilefrac,fracland(1,1,bi,bj))

#ifdef ALLOW_DIAGNOSTICS
      if ( useDiagnostics ) then
       if(diagnostics_is_on('SLP     ',myThid) ) then
        L = Nrphys+1
        do j = 1,sNy
        do i = 1,sNx
         tempdiag(i,j) = topoZ(i,j,bi,bj)*grav
         tempLdiag(i,j,L) = pephy4fiz(i,j,L,bi,bj)/100.
        enddo
        enddo
        do L = 1,Nrphys
        do j = 1,sNy
        do i = 1,sNx
         tempLdiag(i,j,L) = pephy4fiz(i,j,L,bi,bj)/100.
         tempLdiag2(i,j,L) = thphy(i,j,L,bi,bj) *
     &        (1.+0.609*sphy(i,j,L,bi,bj))

        enddo
        enddo
        enddo
        call slprs( tempdiag, tempLdiag, tempLdiag2,
     &              fracland(1,1,bi,bj),sNx,sNy,Nrphys,slp)
        call diagnostics_fill( slp,'SLP     ',1,1,3,bi,bj,myThid )
       endif
      endif
#endif
c
c Compute physics increments
        turbStart(bi,bj) = turbStart(bi,bj) .AND. ( myIter.EQ.nIter0 )

        call do_fizhi(myIter,myThid,
     &  idim1,idim2,jdim1,jdim2,Nrphys,nSx,nSy,im1,im2,jm1,jm2,bi,bj,
     &  turbStart(bi,bj), nchp,nchptot,nchpland,
     &  uphy,vphy,thphy,sphy,pephy4fiz,xC,yC,topoZ,
     &  ctmt,xxmt,yymt,zetamt,xlmt,khmt,tke,
     &  tgz,sst,sice,phis_var,landtype,fracland,emiss,albnirdr,albnirdf,
     &  albvisdr,albvisdf,ityp,chfr,alai,agrn,igrd,chlt,chlon,
     &  tcanopy,tdeep,ecanopy,swetshal,swetroot,swetdeep,snodep,capac,
     &  o3,qstr,co2,cfc11,cfc12,cfc22,n2o,methane,
     &  iras,nlwcld,cldtot_lw,cldras_lw,cldlsp_lw,nlwlz,lwlz,
     &  nswcld,cldtot_sw,cldras_sw,cldlsp_sw,nswlz,swlz,
     &  imstturbsw,imstturblw,qliqavesw,qliqavelw,fccavesw,fccavelw,
     &  raincon,rainlsp,snowfall,
     &  duphy,dvphy,dthphy,dsphy)
       enddo
       enddo

       CALL TIMER_STOP ('DO_FIZHI          [FIZHI_WRAPPER]',myThid)

       CALL TIMER_START ('PHYS2DYN          [FIZHI_WRAPPER]',myThid)
       do bj = myByLo(myThid), myByHi(myThid)
       do bi = myBxLo(myThid), myBxHi(myThid)
c Interpolate (A-Grid) physics increments to dynamics grid
C   First flip the physics arrays (which are top-down)
C   into bottom-up arrays for interpolation to dynamics grid
        do j = 1,sNy
        do i = 1,sNx
         do L = 1,Nrphys
          tempphy(i,j,Nrphys+1-L,bi,bj)=duphy(i,j,L,bi,bj)
         enddo
        enddo
        enddo
        call phys2dyn(tempphy,pephy,idim1,idim2,jdim1,jdim2,Nrphys,
     & nSx,nSy,im1,im2,jm1,jm2,bi,bj,pedyn,kSurfC,Nr,nlperdyn,guphy)
        do j = 1,sNy
        do i = 1,sNx
         do L = 1,Nrphys
          tempphy(i,j,Nrphys+1-L,bi,bj)=dvphy(i,j,L,bi,bj)
         enddo
        enddo
        enddo
        call phys2dyn(tempphy,pephy,idim1,idim2,jdim1,jdim2,Nrphys,
     & nSx,nSy,im1,im2,jm1,jm2,bi,bj,pedyn,kSurfC,Nr,nlperdyn,gvphy)
        do j = 1,sNy
        do i = 1,sNx
         do L = 1,Nrphys
          tempphy(i,j,Nrphys+1-L,bi,bj)=dthphy(i,j,L,bi,bj)
         enddo
        enddo
        enddo
        call phys2dyn(tempphy,pephy,idim1,idim2,jdim1,jdim2,Nrphys,
     & nSx,nSy,im1,im2,jm1,jm2,bi,bj,pedyn,kSurfC,Nr,nlperdyn,gthphy)
        do j = 1,sNy
        do i = 1,sNx
         do L = 1,Nrphys
          tempphy(i,j,Nrphys+1-L,bi,bj)=dsphy(i,j,L,bi,bj)
         enddo
        enddo
        enddo
        call phys2dyn(tempphy,pephy,idim1,idim2,jdim1,jdim2,Nrphys,
     & nSx,nSy,im1,im2,jm1,jm2,bi,bj,pedyn,kSurfC,Nr,nlperdyn,gsphy)

       enddo
       enddo

       CALL TIMER_STOP ('PHYS2DYN          [FIZHI_WRAPPER]',myThid)

c Convert guphy and gvphy from A-grid to C-grid for use by dynamics
       CALL TIMER_START ('ATOC              [FIZHI_WRAPPER]',myThid)
       call AtoC(myThid,guphy,gvphy,maskC,idim1,idim2,jdim1,jdim2,Nr,
     &                      nSx,nSy,im1,im2,jm1,jm2,guphy,gvphy)
       CALL TIMER_STOP ('ATOC              [FIZHI_WRAPPER]',myThid)

       CALL TIMER_START ('EXCHANGES         [FIZHI_WRAPPER]',myThid)
c Call the c-grid exchange routine to fill in the halo regions (du,dv)
       call exch_uv_xyz_RL(guphy,gvphy,.TRUE.,myThid)
c Call the a-grid exchange routine to fill in the halo regions (dth,ds)
       _EXCH_XYZ_RL(gthphy,myThid)
       _EXCH_XYZ_RL(gsphy,myThid)
       CALL TIMER_STOP ('EXCHANGES         [FIZHI_WRAPPER]',myThid)

      return
      end

C $Header: /u/gcmpack/MITgcm/pkg/fizhi/fizhi_init_vars.F,v 1.23 2012/03/27 15:48:27 jmc Exp $
C $Name:  $

#include "FIZHI_OPTIONS.h"
       SUBROUTINE FIZHI_INIT_VARS (myThid)
c-----------------------------------------------------------------------
c  Routine to initialise the fizhi state.
c
c  Input: myThid       - Process number calling this routine
c
c  Notes:
c   1) For a Cold Start -
c      This routine takes the initial condition on the dynamics grid
c      and interpolates to the physics grid to initialize the state
c      variables that are on both grids. It initializes the variables
c      of the turbulence scheme to 0., and the land state from a model
c      climatology.
c   2) For a Restart, read the fizhi pickup file
c   3) The velocity component physics fields are on an A-Grid
c
c Calls: dyn2phys (x4)
c-----------------------------------------------------------------------
       IMPLICIT NONE
#include "SIZE.h"
#include "fizhi_SIZE.h"
#include "fizhi_land_SIZE.h"
#include "GRID.h"
#include "DYNVARS.h"
#include "gridalt_mapping.h"
#include "fizhi_coms.h"
#include "fizhi_land_coms.h"
#include "fizhi_earth_coms.h"
#include "EEPARAMS.h"
#include "SURFACE.h"
#include "PARAMS.h"
#include "chronos.h"
#ifdef ALLOW_EXCH2
#include "W2_EXCH2_SIZE.h"
#include "W2_EXCH2_TOPOLOGY.h"
#endif /* ALLOW_EXCH2 */

       INTEGER myThid

       INTEGER xySize
#if defined(ALLOW_EXCH2)
       PARAMETER ( xySize = W2_ioBufferSize )
#else
       PARAMETER ( xySize = Nx*Ny )
#endif
       Real*8 globalArr( xySize*8 )

c pe on dynamics and physics grid refers to bottom edge
       _RL pephy(1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nrphys+1,nSx,nSy)
       _RL pedyn(1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nr+1,nSx,nSy)
       _RL windphy(1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nrphys,nSx,nSy)
       _RL udyntemp(1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nr,nSx,nSy)
       _RL vdyntemp(1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nr,nSx,nSy)
       _RL tempphy(1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nrphys,nSx,nSy)

       INTEGER i, j, L, bi, bj, Lbotij
       INTEGER im1, im2, jm1, jm2, idim1, idim2, jdim1, jdim2
       INTEGER xsize, ysize
       LOGICAL alarm
       EXTERNAL alarm

#if defined(ALLOW_EXCH2)
       xsize = exch2_global_Nx
       ysize = exch2_global_Ny
#else
       xsize = Nx
       ysize = Ny
#endif
       im1 = 1-OLx
       im2 = sNx+OLx
       jm1 = 1-OLy
       jm2 = sNy+OLy
       idim1 = 1
       idim2 = sNx
       jdim1 = 1
       jdim2 = sNy

c   First Check to see if we can start a fizhi experiment at current time
c    All Fizhi alarms must be on for the first time step of a segment

      if( .not.alarm('moist') .or. .not.alarm('turb')   .or.
     &    .not.alarm('radsw') .or. .not.alarm('radlw') ) then
       write(15,*) ' Cant Start Fizhi experiment at ',nymd,' ',nhms
       stop
      endif

C Deal Here with Variables that are on a Fizhi Pickup or need Initialization

      IF ( startTime.EQ.baseTime .AND. nIter0.EQ.0 ) THEN
      print *,' In fizhi_init_vars: Beginning of New Experiment '

       do bj = myByLo(myThid), myByHi(myThid)
       do bi = myBxLo(myThid), myBxHi(myThid)

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
     &    pedyn(i,j,Lbotij,bi,bj) = Ro_surf(i,j,bi,bj) + etaH(i,j,bi,bj)
        enddo
        enddo
        do j = 1,sNy
        do i = 1,sNx
         Lbotij = kSurfC(i,j,bi,bj)
         do L = Lbotij+1,Nr+1
          pedyn(i,j,L,bi,bj) = pedyn(i,j,L-1,bi,bj) -
     &                        drF(L-1)*hfacC(i,j,L-1,bi,bj)
         enddo
c Do not use a zero field as the top edge pressure for interpolation
         if(pedyn(i,j,Nr+1,bi,bj).lt.1.e-5)
     &                               pedyn(i,j,Nr+1,bi,bj) = 1.e-5
        enddo
        enddo
C Build pressures on physics grid
        do j = 1,sNy
        do i = 1,sNx
         pephy(i,j,1,bi,bj)=Ro_surf(i,j,bi,bj) + etaH(i,j,bi,bj)
         do L = 2,Nrphys+1
          pephy(i,j,L,bi,bj)=pephy(i,j,L-1,bi,bj)-dpphys0(i,j,L-1,bi,bj)
         enddo
c Do not use a zero field as the top edge pressure for interpolation
         if(pephy(i,j,Nrphys+1,bi,bj).lt.1.e-5)
     &                               pephy(i,j,Nrphys+1,bi,bj) = 1.e-5
        enddo
        enddo
c
c Create an initial wind magnitude field on the physics grid -
c   Use a log wind law with z0=1cm, u*=1 cm/sec,
c   do units and get u = .025*ln(dP*10), with dP in pa.
        do L = 1,Nrphys
        do j = 1,sNy
        do i = 1,sNx
         windphy(i,j,L,bi,bj) = 0.025 *
     &             log((pephy(i,j,1,bi,bj)-pephy(i,j,L+1,bi,bj))*10.)
        enddo
        enddo
        enddo

       enddo
       enddo

c Create initial fields on phys. grid - Move Dynamics u and v to A-Grid
       call CtoA(myThid,uvel,vvel,maskW,maskS,im1,im2,jm1,jm2,Nr,
     &                     nSx,nSy,1,sNx,1,sNy,udyntemp,vdyntemp)

       do bj = myByLo(myThid), myByHi(myThid)
       do bi = myBxLo(myThid), myBxHi(myThid)

c Create initial fields on phys. grid - interpolate from dyn. grid
        call dyn2phys(udyntemp,pedyn,im1,im2,jm1,jm2,Nr,nSx,nSy,
     & 1,sNx,1,sNy,bi,bj,windphy,pephy,kSurfC,Nrphys,nlperdyn,1,tempphy)
c   Note: Interpolation gives bottom-up arrays (level 1 is bottom),
c         Physics works top-down. so -> need to flip arrays
        do L = 1,Nrphys
        do j = 1,sNy
        do i = 1,sNx
         uphy(i,j,Nrphys+1-L,bi,bj) = tempphy(i,j,L,bi,bj)
        enddo
        enddo
        enddo
        call dyn2phys(vdyntemp,pedyn,im1,im2,jm1,jm2,Nr,nSx,nSy,
     & 1,sNx,1,sNy,bi,bj,windphy,pephy,kSurfC,Nrphys,nlperdyn,1,tempphy)
        do L = 1,Nrphys
        do j = 1,sNy
        do i = 1,sNx
         vphy(i,j,Nrphys+1-L,bi,bj) = tempphy(i,j,L,bi,bj)
        enddo
        enddo
        enddo
        call dyn2phys(theta,pedyn,im1,im2,jm1,jm2,Nr,nSx,nSy,
     & 1,sNx,1,sNy,bi,bj,windphy,pephy,kSurfC,Nrphys,nlperdyn,2,tempphy)
        do L = 1,Nrphys
        do j = 1,sNy
        do i = 1,sNx
         thphy(i,j,Nrphys+1-L,bi,bj) = tempphy(i,j,L,bi,bj)
        enddo
        enddo
        enddo
        call dyn2phys(salt,pedyn,im1,im2,jm1,jm2,Nr,nSx,nSy,
     & 1,sNx,1,sNy,bi,bj,windphy,pephy,kSurfC,Nrphys,nlperdyn,0,tempphy)
        do L = 1,Nrphys
        do j = 1,sNy
        do i = 1,sNx
         sphy(i,j,Nrphys+1-L,bi,bj) = tempphy(i,j,L,bi,bj)
        enddo
        enddo
        enddo

c Zero out fizhi tendency arrays on the fizhi grid
        do L = 1,Nrphys
        do j = 1,sNy
        do i = 1,sNx
         duphy(i,j,L,bi,bj) = 0.
         dvphy(i,j,L,bi,bj) = 0.
         dthphy(i,j,L,bi,bj) = 0.
         dsphy(i,j,L,bi,bj) = 0.
        enddo
        enddo
        enddo

c Zero out fizhi tendency arrays on the dynamics grid
        do L = 1,Nr
        do j = jm1,jm2
        do i = im1,im2
         guphy(i,j,L,bi,bj) = 0.
         gvphy(i,j,L,bi,bj) = 0.
         gthphy(i,j,L,bi,bj) = 0.
         gsphy(i,j,L,bi,bj) = 0.
        enddo
        enddo
        enddo

c Initialize vegetation tile tke, xlmt, khmt, xxmt, yymt, ctmt, zetamt,
        if( (nhms.eq.nhms0) .and. (nymd.eq.nymd0) ) then
         print *,' Cold Start: Zero out Turb second moments '
         do i = 1,nchp
          ctmt(i,bi,bj) = 0.
          xxmt(i,bi,bj) = 0.
          yymt(i,bi,bj) = 0.
          zetamt(i,bi,bj) = 0.
         enddo
         do L = 1,Nrphys
         do i = 1,nchp
          tke(i,L,bi,bj) = 0.
          xlmt(i,L,bi,bj) = 0.
          khmt(i,L,bi,bj) = 0.
         enddo
         enddo
        else
         print *,' Need initial Values for TKE - dont have them! '
         stop
        endif
        turbStart(bi,bj) = .TRUE.

c Now initialize vegetation tile land state too - tcanopy, etc...
        call fizhi_init_vegsurftiles( globalArr, xsize, ysize,
     &                                nymd,nhms, 'D', myThid )

c Now initialize fizhi arrays that will be on a pickup
        print *,' Initialize fizhi arrays that will be on pickup '
        imstturblw(bi,bj) = 0
        imstturbsw(bi,bj) = 0
        iras(bi,bj) = 0
        nlwcld(bi,bj) = 0
        nlwlz(bi,bj) = 0
        nswcld(bi,bj) = 0
        nswlz(bi,bj) = 0
        do L = 1,Nrphys
        do j = 1,sNy
        do i = 1,sNx
         swlz(i,j,L,bi,bj) = 0.
         lwlz(i,j,L,bi,bj) = 0.
         qliqavesw(i,j,L,bi,bj) = 0.
         qliqavelw(i,j,L,bi,bj) = 0.
         fccavesw(i,j,L,bi,bj) = 0.
         fccavelw(i,j,L,bi,bj) = 0.
         cldtot_sw(i,j,L,bi,bj) = 0.
         cldras_sw(i,j,L,bi,bj) = 0.
         cldlsp_sw(i,j,L,bi,bj) = 0.
         cldtot_lw(i,j,L,bi,bj) = 0.
         cldras_lw(i,j,L,bi,bj) = 0.
         cldlsp_lw(i,j,L,bi,bj) = 0.
        enddo
        enddo
        enddo
        do j = 1,sNy
        do i = 1,sNx
         rainlsp(i,j,bi,bj) = 0.
         raincon(i,j,bi,bj) = 0.
         snowfall(i,j,bi,bj) = 0.
        enddo
        enddo

       enddo
       enddo

      ELSE
      print *,' In fizhi_init_vars: Read from restart '

C--   Read fizhi package state variables from pickup file

       call fizhi_read_pickup( nIter0, myThid )
       CALL FIZHI_READ_VEGTILES( nIter0, 'D', myThid )
       do bj = myByLo(myThid), myByHi(myThid)
       do bi = myBxLo(myThid), myBxHi(myThid)
         turbStart(bi,bj) = .FALSE.
       enddo
       enddo

      ENDIF

      RETURN
      END

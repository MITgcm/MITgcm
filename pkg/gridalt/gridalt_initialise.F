C $Header: /u/gcmpack/MITgcm/pkg/gridalt/gridalt_initialise.F,v 1.7 2012/07/07 00:08:09 jmc Exp $
C $Name:  $

#include "GRIDALT_OPTIONS.h"

       subroutine gridalt_initialise (myThid)
c-----------------------------------------------------------------------
c  Routine to initialise the gridalt package.
c
c  Input: myThid       - Process number calling this routine
c
c  Notes:
c   When used with fizhi, this routine is the interface
c                                       to make_phys_grid
c  Calls: make_phys_grid (define the physics grid and mappings)
c         when diagnostics are used, call gridalt_diagnostics_init
c-----------------------------------------------------------------------
       implicit none
#include "SIZE.h"
#include "EEPARAMS.h"
#include "PARAMS.h"
#include "GRID.h"
#include "fizhi_SIZE.h"
#include "gridalt_mapping.h"
#include "SURFACE.h"

       integer myThid

       integer nlph
       integer bi, bj
       integer im1, im2, jm1, jm2, idim1, idim2, jdim1, jdim2

       im1 = 1-OLx
       im2 = sNx+OLx
       jm1 = 1-OLy
       jm2 = sNy+OLy
       idim1 = 1
       idim2 = sNx
       jdim1 = 1
       jdim2 = sNy

#ifdef ALLOW_FIZHI
       if(usefizhi) then
        do bj = myByLo(myThid), myByHi(myThid)
        do bi = myBxLo(myThid), myBxHi(myThid)

         call make_phys_grid(drF,hfacC,im1,im2,jm1,jm2,Nr,Nsx,Nsy,
     .       1,sNx,1,sNy,bi,bj,Nrphys,ksurfC,dpphys0,nlph,nlperdyn)

         if( NrPhys.ne.nlph ) then
          print *,' ERROR CONDITION - Model has been brought down '
          print *,' Physics dimension in fizhi_size is ',Nrphys,
     .                     ' New grid has ',nlph,' levels '
          stop
         endif

        enddo
        enddo
       endif

#ifdef ALLOW_DIAGNOSTICS
      if ( useDiagnostics ) then
        call gridalt_diagnostics_init( myThid )
      endif
#endif

#endif

       return
       end

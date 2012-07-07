C $Header: /u/gcmpack/MITgcm/pkg/gridalt/gridalt_update.F,v 1.11 2012/07/07 00:08:09 jmc Exp $
C $Name:  $

#include "GRIDALT_OPTIONS.h"

       subroutine gridalt_update(myThid)
c***********************************************************************
c subroutine gridalt_update
c
c Purpose: Update the pressure thicknesses of the layers of the
c          alternative vertical grid (used now for atmospheric physics).
c
c Calculate: dpphys    - new delta r (p*) edge-to-edge of physics grid
c                        using dpphys0 (initial value) and rstarfacC
c
c***********************************************************************
       implicit none

#include "SIZE.h"
#include "EEPARAMS.h"
#include "PARAMS.h"
#include "GRID.h"
#include "fizhi_SIZE.h"
#include "gridalt_mapping.h"
#include "SURFACE.h"

       integer myThid

       integer im1,im2,jm1,jm2
       integer i1,i2,j1,j2,bi,bj
       integer i,j,L
       logical  diagnostics_is_on
       external diagnostics_is_on
       _RL temparr(1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nrphys,nSx,nSy)

       im1 = 1-OLx
       im2 = sNx+OLx
       jm1 = 1-OLy
       jm2 = sNy+OLy
       i1 = 1
       i2 = sNx
       j1 = 1
       j2 = sNy

#ifdef ALLOW_FIZHI
#ifdef NONLIN_FRSURF

       if(usefizhi .and. (select_rStar.GT.0) )then
        do bj = myByLo(myThid), myByHi(myThid)
        do bi = myBxLo(myThid), myBxHi(myThid)
c
         do L = 1,Nrphys
          do j = j1,j2
          do i = i1,i2
           dpphys(i,j,L,bi,bj) = dpphys0(i,j,L,bi,bj) *
     .                     rstarfacC(i,j,bi,bj)
          enddo
          enddo
         enddo
c
        enddo
        enddo

       endif

#endif

#ifdef ALLOW_DIAGNOSTICS
       if(useDiagnostics) then
       IF ( DIAGNOSTICS_IS_ON('DPPHYS  ',myThid) ) THEN
        do bj = myByLo(myThid), myByHi(myThid)
        do bi = myBxLo(myThid), myBxHi(myThid)
         do L = 1,Nrphys
          do j = j1,j2
          do i = i1,i2
           temparr(i,j,L,bi,bj) = dpphys(i,j,L,bi,bj) * 1. _d -2
          enddo
          enddo
         enddo
        enddo
        enddo
        CALL DIAGNOSTICS_FILL(temparr,'DPPHYS  ',-1,Nrphys,0,1,1,myThid)
       ENDIF
       endif
#endif

#endif
       return
       end

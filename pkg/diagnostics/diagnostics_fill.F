C $Header: /u/gcmpack/MITgcm/pkg/diagnostics/diagnostics_fill.F,v 1.1 2004/12/13 21:55:48 jmc Exp $
C $Name:  $

#include "DIAG_OPTIONS.h"

      subroutine diagnostics_fill (arrayin, chardiag, 
     .                       levflg, nlevs, bibjflg, bi, bj, myThid) 
C***********************************************************************
C  Purpose
C  -------
C   Wrapper routine to increment the diagnostics array with a field
C
C  Arguments Description
C  ----------------------
C     arrayin .... Field to increment diagnostics array
C     chardiag ... Character expression for diag to fill
C     levflg ..... Integer flag for vertical levels:
C                  0 indicates multiple levels incremented in qdiag
C                  non-0 (any integer) - WHICH single level to increment.
C                  negative integer - the input data array is single-leveled
C                  positive integer - the input data array is multi-leveled
C     nlevs ...... indicates Number of levels to be filled (1 if levflg <> 0)
C                  positive: fill in "nlevs" levels in the same order as
C                            the input array 
C                  negative: fill in -nlevs levels in reverse order.
C     bibjflg .... Integer flag to indicate instructions for bi bj loop
C                  0 indicates that the bi-bj loop must be done here
C                  1 indicates that the bi-bj loop is done OUTSIDE
C                  2 indicates that the bi-bj loop is done OUTSIDE
C                     AND that we have been sent a local array (with overlap regions)
C                  3 indicates that the bi-bj loop is done OUTSIDE
C                     AND that we have been sent a local array
C                     AND that the array has no overlap region (interior only)
C     bi ......... X-direction process(or) number - used for bibjflg=1-3
C     bj ......... Y-direction process(or) number - used for bibjflg=1-3
C     myThid     ::  my thread Id number
C***********************************************************************
C                  NOTE: User beware! If a local (1 tile only) array
C                        is sent here, bibjflg MUST NOT be set to 0
C                        or there will be out of bounds problems!
C***********************************************************************
       implicit none
#include "EEPARAMS.h"
#include "SIZE.h"
#include "DIAGNOSTICS_SIZE.h"
#include "DIAGNOSTICS.h"

      integer myThid,levflg,nlevs,bibjflg,bi,bj
      character *8 chardiag
      _RL arrayin(*)

c Local variables
c ===============
      integer i, j, m, n 
      integer ndiagnum, bihere, bjhere, levhere, ipointer
      _RL array(1-OLx:sNx+OLx,1-OLy:sNy+OLy)
      _RL arrayloc(sNx,sNy)
      integer irun,jrun,krun,birun,bjrun
      integer level

C Run through list of active diagnostics to make sure
C we are trying to fill a valid diagnostic

      ndiagnum = 0
      ipointer = 0
      DO n=1,nlists
       DO m=1,nActive(n)
        IF ( chardiag.EQ.flds(m,n) ) THEN
         ndiagnum = jdiag(m,n)
         ipointer = idiag(ndiagnum)
        ENDIF
       ENDDO
      ENDDO

C If-sequence to see if we are a valid and an active diagnostic

      IF ( ndiagnum.ne.0 .and. ipointer.ne.0 ) THEN

C Increment the counter for the diagnostic (if we are at bi=bj=1)
      _BEGIN_MASTER(myThid)
      if ((bi.eq.1).and.(bj.eq.1).and.(abs(levflg).le.1) ) 
     .                     ndiag(ndiagnum) = ndiag(ndiagnum) + 1
      _END_MASTER(myThid)

C   Check to see if we need to do a bi-bj loop here

C---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|
      if(bibjflg.eq.0) then
       irun = sNx+2*OLx
       jrun = sNy+2*OLy
       krun = abs(nlevs)
       birun = nSx
       bjrun = nSy

       do bjhere=myByLo(myThid), myByHi(myThid)
       do bihere=myBxLo(myThid), myBxHi(myThid)

       if(levflg.eq.0)then
        do levhere = 1,krun
         level = levhere
         IF (nlevs.LT.0) level=1-nlevs-levhere
         call diagnostics_fillit(arrayin,irun,jrun,krun,levhere,
     .                           birun,bjrun,bihere,bjhere,array,myThid)
         do j = 1,sNy
         do i = 1,sNx
          qdiag(i,j,ipointer+level-1,bihere,bjhere) = 
     .  qdiag(i,j,ipointer+level-1,bihere,bjhere) + 
     .                       array(i,j)
         enddo
         enddo
        enddo
       elseif(levflg.gt.0)then
        call diagnostics_fillit(arrayin,irun,jrun,krun,levflg,
     .                           birun,bjrun,bihere,bjhere,array,myThid)
        do j = 1,sNy
        do i = 1,sNx
         qdiag(i,j,ipointer+levflg-1,bihere,bjhere) = 
     .  qdiag(i,j,ipointer+levflg-1,bihere,bjhere) + 
     .                       array(i,j)
        enddo
        enddo
       else
        level = -1 * levflg
        call diagnostics_fillit(arrayin,irun,jrun,1,1,birun,bjrun,
     .                                       bihere,bjhere,array,myThid)
        do j = 1,sNy
        do i = 1,sNx
         qdiag(i,j,ipointer+level-1,bihere,bjhere) = 
     .  qdiag(i,j,ipointer+level-1,bihere,bjhere) + 
     .                       array(i,j)
        enddo
        enddo
       endif

       enddo
       enddo

C---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|
      elseif(bibjflg.eq.1) then
       irun = sNx+2*OLx
       jrun = sNy+2*OLy
       krun = abs(nlevs)
       birun = nSx
       bjrun = nSy

       if(levflg.eq.0)then
        do levhere = 1,krun
         level = levhere
         IF (nlevs.LT.0) level=1-nlevs-levhere
         call diagnostics_fillit(arrayin,irun,jrun,krun,levhere,
     .                           birun,bjrun,bi,bj,array,myThid)
         do j = 1,sNy
         do i = 1,sNx
          qdiag(i,j,ipointer+level-1,bi,bj) =
     .  qdiag(i,j,ipointer+level-1,bi,bj) +
     .                       array(i,j)
         enddo
         enddo
        enddo
       elseif(levflg.gt.0)then
        call diagnostics_fillit(arrayin,irun,jrun,krun,levflg,
     .                           birun,bjrun,bi,bj,array,myThid)
        do j = 1,sNy
        do i = 1,sNx
         qdiag(i,j,ipointer+levflg-1,bi,bj) =
     .  qdiag(i,j,ipointer+levflg-1,bi,bj) + array(i,j)
        enddo
        enddo
       else
        level = -1 * levflg
        call diagnostics_fillit(arrayin,irun,jrun,1,1,birun,bjrun,
     .                                       bi,bj,array,myThid)
        do j = 1,sNy
        do i = 1,sNx
         qdiag(i,j,ipointer+level-1,bi,bj) =
     .  qdiag(i,j,ipointer+level-1,bi,bj) + array(i,j)
        enddo
        enddo
       endif

C---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|
      elseif(bibjflg.eq.3) then
       irun = sNx
       jrun = sNy
       krun = abs(nlevs)
       birun = 1
       bjrun = 1

       if(levflg.eq.0)then
        do levhere = 1,krun
         level = levhere
         IF (nlevs.LT.0) level=1-nlevs-levhere
         call diagnostics_fillit(arrayin,irun,jrun,krun,levhere,
     .                           birun,bjrun,1,1,arrayloc,myThid)
         do j = 1,sNy
         do i = 1,sNx
          qdiag(i,j,ipointer+level-1,bi,bj) =
     .  qdiag(i,j,ipointer+level-1,bi,bj) + arrayloc(i,j)
         enddo
         enddo
        enddo
       elseif(levflg.gt.0)then
        call diagnostics_fillit(arrayin,irun,jrun,krun,levflg,
     .                           birun,bjrun,1,1,arrayloc,myThid)
        do j = 1,sNy
        do i = 1,sNx
         qdiag(i,j,ipointer+levflg-1,bi,bj) =
     .  qdiag(i,j,ipointer+levflg-1,bi,bj) + arrayloc(i,j)
        enddo
        enddo
       else
        level = -1 * levflg
        call diagnostics_fillit(arrayin,irun,jrun,1,1,birun,bjrun,
     .                                       1,1,arrayloc,myThid)
        do j = 1,sNy
        do i = 1,sNx
         qdiag(i,j,ipointer+level-1,bi,bj) =
     .  qdiag(i,j,ipointer+level-1,bi,bj) + arrayloc(i,j)
        enddo
        enddo
       endif

C---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|
      elseif(bibjflg.eq.2) then
       irun = sNx+2*OLx
       jrun = sNy+2*OLy
       krun = abs(nlevs)
       birun = 1
       bjrun = 1

       if(levflg.eq.0)then
        do levhere = 1,krun
         level = levhere
         IF (nlevs.LT.0) level=1-nlevs-levhere
         call diagnostics_fillit(arrayin,irun,jrun,krun,levhere,
     .                              birun,bjrun,1,1,array,myThid)
         do j = 1,sNy
         do i = 1,sNx
          qdiag(i,j,ipointer+level-1,bi,bj) =
     .  qdiag(i,j,ipointer+level-1,bi,bj) + array(i,j)
         enddo
         enddo
        enddo
       elseif(levflg.gt.0)then
        call diagnostics_fillit(arrayin,irun,jrun,krun,levflg,
     .                              birun,bjrun,1,1,array,myThid)
        do j = 1,sNy
        do i = 1,sNx
         qdiag(i,j,ipointer+levflg-1,bi,bj) =
     .  qdiag(i,j,ipointer+levflg-1,bi,bj) + array(i,j)
        enddo
        enddo
       else
        level = -1 * levflg
        call diagnostics_fillit(arrayin,irun,jrun,1,1,birun,bjrun,
     .                                          1,1,array,myThid)
        do j = 1,sNy
        do i = 1,sNx
         qdiag(i,j,ipointer+level-1,bi,bj) =
     .  qdiag(i,j,ipointer+level-1,bi,bj) + array(i,j)
        enddo
        enddo
       endif

C---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|
      endif

      ELSE

C     if (myThid.eq.1) write(6,1000) chardiag

      ENDIF

 1000 format(' ',' Warning: Trying to write to diagnostic ',a8,
     .        ' But it is not a valid (or active) name ')
      return
      end

C---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|

      subroutine diagnostics_fillit(
     I                  arrayin,irun,jrun,krun,klevf,birun,bjrun,bi,bj,
     O                  arrayout,
     I                  myThid)

      implicit none
#include "EEPARAMS.h"

      integer irun, jrun, krun, klevf, birun, bjrun, bi, bj
      _RL arrayin(irun,jrun,krun,birun,bjrun)
      _RL arrayout(irun,jrun)
      integer myThid

      integer i, j

      do j = 1,jrun
       do i = 1,irun
        arrayout(i,j) = arrayin(i,j,klevf,bi,bj)
       enddo
      enddo

      return
      end

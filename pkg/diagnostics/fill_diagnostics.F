      subroutine fill_diagnostics (myThid, chardiag, levflg, nlevs, 
     .          bibjflg, bi, bj, arrayin) 
C***********************************************************************
C  Purpose
C  -------
C   Wrapper routine to increment the diagnostics array with a field
C
C  Arguments Description
C  ----------------------
C     myThid ..... Current Process(or)
C     chardiag ... Character expression for diag to fill
C     levflg ..... Integer flag for vertical levels:
C                  0 indicates multiple levels incremented in qdiag
C                  non-0 (any integer) - WHICH single level to increment
C     nlevs ...... Number of levels to be incremented (1 if levflg > 0)
C     bibjflg .... Integer flag to indicate instructions for bi bj loop
C                  0 indicates that the bi-bj loop must be done here
C                  1 indicates that the bi-bj loop is done OUTSIDE
C                  2 indicates that the bi-bj loop is done OUTSIDE
C                      AND that we have been sent a local array
C     bi ......... X-direction process(or) number - used for bibjflg=1,2
C     bj ......... Y-direction process(or) number - used for bibjflg=1,2
C     arrayin .... Field to increment diagnostics array
C                  NOTE: User beware! If a local (1 tile only) array
C                        is sent here, bibjflg MUST NOT be set to 0
C                        or there will be out of bounds problems!
C***********************************************************************
       implicit none
#include "EEPARAMS.h"
#include "CPP_OPTIONS.h"
#include "SIZE.h"
#include "diagnostics_SIZE.h"
#include "diagnostics.h"

      integer myThid,levflg,nlevs,bibjflg,bi,bj
      character *8 chardiag
      _RL arrayin(*)

c Local variables
c ===============
      integer i, j, n, ndiagnum, bihere, bjhere, levhere, ipointer
      _RL array(1-OLx:sNx+Olx,1-Oly:sNy+Oly,nlevs,Nsx,Nsy)
      _RL arrayloc(sNx,sNy,nlevs)
      integer irun,jrun,krun,birun,bjrun

C Run through list of active diagnostics to make sure
C we are trying to fill a valid diagnostic

      ndiagnum = 0
      ipointer = 0
      do n = 1,ndiagt
       if(chardiag.eq.cdiag(n)) then
        ndiagnum = n
        ipointer = idiag(n)
       endif
      enddo

C If-sequence to see if we are a valid and an active diagnostic

      IF ( ndiagnum.ne.0 .and. ipointer.ne.0 ) THEN

C Increment the counter for the diagnostic
      ndiag(ndiagnum) = ndiag(ndiagnum) + 1

C   Check to see if we need to do a bi-bj loop here

      if(bibjflg.eq.0) then
       irun = sNx+2*Olx
       jrun = sNy+2*Oly
       krun = nlevs
       birun = myBxHi(myThid)-myBxLo(myThid)+1
       bjrun = myByHi(myThid)-myByLo(myThid)+1

       do bjhere=myByLo(myThid), myByHi(myThid)
       do bihere=myBxLo(myThid), myBxHi(myThid)

       if(levflg.eq.0)then
        do levhere = 1,nlevs
         call fillit(arrayin,irun,jrun,krun,levhere,birun,bjrun,
     .                                              bihere,bjhere,array)
         do j = 1,sNy
         do i = 1,sNx
          qdiag(i,j,ipointer+levhere-1,bihere,bjhere) = 
     .  qdiag(i,j,ipointer+levhere-1,bihere,bjhere) + 
     .                       array(i,j,levhere,bihere,bjhere)
         enddo
         enddo
        enddo
       else
        call fillit(arrayin,irun,jrun,krun,levflg,birun,bjrun,
     .                                              bihere,bjhere,array)
        do j = 1,sNy
        do i = 1,sNx
         qdiag(i,j,ipointer+levflg-1,bihere,bjhere) = 
     .  qdiag(i,j,ipointer+levflg-1,bihere,bjhere) + 
     .                       array(i,j,nlevs,bihere,bjhere)
        enddo
        enddo
       endif

       enddo
       enddo

      elseif(bibjflg.eq.1) then
       irun = sNx+2*Olx
       jrun = sNy+2*Oly
       krun = nlevs
       birun = myBxHi(myThid)-myBxLo(myThid)+1
       bjrun = myByHi(myThid)-myByLo(myThid)+1

       if(levflg.eq.0)then
        do levhere = 1,nlevs
         call fillit(arrayin,irun,jrun,krun,levhere,birun,bjrun,
     .                                              bi,bj,array)
         do j = 1,sNy
         do i = 1,sNx
          qdiag(i,j,ipointer+levhere-1,bi,bj) =
     .  qdiag(i,j,ipointer+levhere-1,bi,bj) + array(i,j,levhere,bi,bj)
         enddo
         enddo
        enddo
       else
        call fillit(arrayin,irun,jrun,krun,levflg,birun,bjrun,
     .                                              bi,bj,array)
        do j = 1,sNy
        do i = 1,sNx
         qdiag(i,j,ipointer+levflg-1,bi,bj) =
     .  qdiag(i,j,ipointer+levflg-1,bi,bj) + array(i,j,nlevs,bi,bj)
        enddo
        enddo
       endif

      elseif(bibjflg.eq.2) then
       irun = sNx
       jrun = sNy
       krun = nlevs
       birun = 1
       bjrun = 1

       if(levflg.eq.0)then
        do levhere = 1,nlevs
         call fillit(arrayin,irun,jrun,krun,levhere,birun,bjrun,
     .                                              1,1,arrayloc)
         do j = 1,sNy
         do i = 1,sNx
          qdiag(i,j,ipointer+levhere-1,bi,bj) =
     .  qdiag(i,j,ipointer+levhere-1,bi,bj) + arrayloc(i,j,levhere)
         enddo
         enddo
        enddo
       else
        call fillit(arrayin,irun,jrun,krun,levflg,birun,bjrun,
     .                                              1,1,arrayloc)
        do j = 1,sNy
        do i = 1,sNx
         qdiag(i,j,ipointer+levflg-1,bi,bj) =
     .  qdiag(i,j,ipointer+levflg-1,bi,bj) + arrayloc(i,j,nlevs)
        enddo
        enddo
       endif

      endif

      ELSE

C     if (myThid.eq.1) write(6,1000) chardiag

      ENDIF

 1000 format(' ',' Warning: Trying to write to diagnostic ',a8,
     .        ' But it is not a valid (or active) name ')
      return
      end

      subroutine fillit(arrayin,irun,jrun,krun,klev,birun,bjrun,
     .                                              bi,bj,arrayout)

      implicit none
#include "EEPARAMS.h"

      integer irun, jrun, krun, klev, birun, bjrun, bi, bj
      _RL arrayin(irun,jrun,krun,birun,bjrun)
      _RL arrayout(irun,jrun,krun,birun,bjrun)

      integer i, j

      do j = 1,jrun
       do i = 1,irun
        arrayout(i,j,klev,bi,bj) = arrayin(i,j,klev,bi,bj)
       enddo
      enddo

      return
      end

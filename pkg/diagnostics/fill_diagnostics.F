      subroutine fill_diagnostics (myThid, chardiag, levflg, nlevs, 
     .          bibjflg, bi, bj, array) 
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
C     bi ......... X-direction process(or) number - used for bibjflg=1
C     bj ......... Y-direction process(or) number - used for bibjflg=1
C     array ...... Field to increment diagnostics array
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
      _RL array(1-OLx:sNx+Olx,1-Oly:sNy+Oly,nlevs,Nsx,Nsy)

c Local variables
c ===============
      integer i, j, n, ndiagnum, bihere, bjhere, levhere, ipointer

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

       do bjhere=myByLo(myThid), myByHi(myThid)
       do bihere=myBxLo(myThid), myBxHi(myThid)

       if(levflg.eq.0)then
        do levhere = 1,nlevs
         do j = 1,sNy
         do i = 1,sNx
          qdiag(i,j,ipointer+levhere-1,bihere,bjhere) = 
     .  qdiag(i,j,ipointer+levhere-1,bihere,bjhere) + 
     .                       array(i,j,levhere,bihere,bjhere)
         enddo
         enddo
        enddo
       else
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

       if(levflg.eq.0)then
        do levhere = 1,nlevs
         do j = 1,sNy
         do i = 1,sNx
          qdiag(i,j,ipointer+levhere-1,bi,bj) =
     .  qdiag(i,j,ipointer+levhere-1,bi,bj) + array(i,j,levhere,bi,bj)
         enddo
         enddo
        enddo
       else
        do j = 1,sNy
        do i = 1,sNx
         qdiag(i,j,ipointer+levflg-1,bi,bj) =
     .  qdiag(i,j,ipointer+levflg-1,bi,bj) + array(i,j,nlevs,bi,bj)
        enddo
        enddo
       endif

      elseif(bibjflg.eq.2) then

       if(levflg.eq.0)then
        do levhere = 1,nlevs
         do j = 1,sNy
         do i = 1,sNx
          qdiag(i,j,ipointer+levhere-1,bi,bj) =
     .  qdiag(i,j,ipointer+levhere-1,bi,bj) + array(i,j,levhere,1,1)
         enddo
         enddo
        enddo
       else
        do j = 1,sNy
        do i = 1,sNx
         qdiag(i,j,ipointer+levflg-1,bi,bj) =
     .  qdiag(i,j,ipointer+levflg-1,bi,bj) + array(i,j,nlevs,1,1)
        enddo
        enddo
       endif

      endif

      ELSE

      if (myThid.eq.1) write(6,1000) chardiag

      ENDIF

 1000 format(' ',' Warning: Trying to write to diagnostic ',a8,
     .        ' But it is not a valid (or active) name ')
      return
      end

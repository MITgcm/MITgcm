C $Header: /u/gcmpack/MITgcm/pkg/flt/flt_up.F,v 1.1 2001/09/13 17:43:56 adcroft Exp $
C $Name:  $

#include "FLT_CPPOPTIONS.h"

      subroutine flt_up (
     I                    myCurrentIter, 
     I                    myCurrentTime, 
     I                    myThid
     &                   )

c     ==================================================================
c     SUBROUTINE flt_up
c     ==================================================================
c
c     o This routine moves particles vertical from the target depth to 
c       the surface and samples the model state over the full water 
c       column at horizontal float position every flt_int_prof time steps 
c       and writes output.
c
c     ==================================================================
c     SUBROUTINE flt_up
c     ==================================================================

c     == global variables ==

#include "EEPARAMS.h"
#include "SIZE.h"
#include "DYNVARS.h"
#include "PARAMS.h"
#include "GRID.h"
#include "FLT.h"
#include "SOLVE_FOR_PRESSURE.h"
c#include "UNITS.h"

c     == routine arguments ==

      INTEGER myCurrentIter, myThid
      _RL myCurrentTime
      INTEGER bi, bj

c     == local variables ==

      integer imax
      parameter (imax=(6+4*Nr))
      integer ip, k
      _RL xx, yy, xlo, xhi, ylo, yhi
      _RL uu,vv,tt,ss, pp
      _RL global2local_i
      _RL global2local_j
      integer irecord
      _RL tmp(imax)

      _RL npart_read,npart_times

      CHARACTER*(MAX_LEN_FNAM) fn
      character*(max_len_mbuf) msgbuf

C Functions
      integer ILNBLNK
C Local variables
      character*(80) dataFName
      integer iG,jG,IL
      logical exst
      logical globalFile

c     == end of interface ==

      fn = 'float_profiles'


      DO bj=myByLo(myThid),myByHi(myThid)
      DO bi=myBxLo(myThid),myBxHi(myThid)

c
c (1) read actual number floats from file (if exists)
         IL=ILNBLNK( fn )
         iG=bi+(myXGlobalLo-1)/sNx ! Kludge until unstructered tiles
         jG=bj+(myYGlobalLo-1)/sNy ! Kludge until unstructered tiles
         write(dataFname(1:80),'(2a,i3.3,a,i3.3,a)')
     &              fn(1:IL),'.',iG,'.',jG,'.data'
         inquire( file=dataFname, exist=exst )
         if (exst) then
            call mdsreadvector_flt(fn,globalFile,64,'RL',
     &                             imax,tmp,bi,bj,1,mythid)
            npart_read  = tmp(1)
            npart_times = tmp(5)
         else
            npart_read  = 0.
            npart_times = 0.
            tmp(2)      = myCurrentTime
         endif

c
c the standard routine mdswritevector can be used here
c (2) write new actual number floats and time into file
c
c total number of records in this file
         tmp(1) = DBLE(npart_tile(bi,bj))+npart_read
c first time of writing floats (do not change when written)
c        tmp(2) = tmp(2)
c current time
         tmp(3) = myCurrentTime
c timestep
         tmp(4) = flt_int_prof
c total number of timesteps
         tmp(5) = npart_times + 1.
c total number of floats
         tmp(6) = max_npart
         do ip=7,imax
            tmp(ip) = 0.
         enddo
         call mdswritevector(fn,64,.false.,'RL',imax,tmp,bi,bj,1,
     &                     myCurrentIter,mythid)

         do ip=1,npart_tile(bi,bj)

c     Move float to the surface
c
         if(
     &  (                       myCurrentTime.ge.tstart(ip,bi,bj))
     &   .and.
     &  (tend(ip,bi,bj).eq.-1..or.myCurrentTime.le.tend(ip,bi,bj))
     &   .and.
     &  (kpart(ip,bi,bj) .eq. kfloat(ip,bi,bj))
     &   .and.
     &  (iup(ip,bi,bj)   .gt. 0.)
     &      ) then

c         if(myCurrentTime   .ge. tstart(ip,bi,bj) .and.
c     &      myCurrentTime   .le.   tend(ip,bi,bj) .and.
c     &      kpart(ip,bi,bj) .eq. kfloat(ip,bi,bj) .and.
c     &      iup(ip,bi,bj)   .gt. 0.) then

            if(mod(myCurrentTime,iup(ip,bi,bj)).eq.0.)
     &      kpart(ip,bi,bj) = flt_surf

         endif

c     If float has died move to level 0
c
         if(
     &  (tend(ip,bi,bj).ne.-1..and.myCurrentTime.gt.tend(ip,bi,bj))
     &      ) then

            kpart(ip,bi,bj) = 0.

         endif

c     Convert to local indices
c
            xx=global2local_i(xpart(ip,bi,bj),bi,bj,mythid)
            yy=global2local_j(ypart(ip,bi,bj),bi,bj,mythid)

            tmp(1)  = npart(ip,bi,bj)  
            tmp(2)  = myCurrentTime
            tmp(3)  = xpart(ip,bi,bj)  
            tmp(4)  = ypart(ip,bi,bj) 
            tmp(5)  = kpart(ip,bi,bj)

         if(
     &  (                         myCurrentTime.ge.tstart(ip,bi,bj))
     &   .and.
     &  (tend(ip,bi,bj).eq.-1..or.myCurrentTime.le.tend(ip,bi,bj))
     &      ) then

c            if(tstart(ip,bi,bj) .ne. -1.              .and.
c     &         myCurrentTime    .ge. tstart(ip,bi,bj) .and.
c     &         myCurrentTime    .le.   tend(ip,bi,bj)) then

               call flt_bilinear2d(xx,yy,pp,cg2d_x,1,bi,bj)
               tmp(6)   = pp

            do k=1,Nr
               call flt_bilinear  (xx,yy,uu,k,uVel,  2,bi,bj)
               call flt_bilinear  (xx,yy,vv,k,vVel,  3,bi,bj)
               call flt_bilinear  (xx,yy,tt,k,theta, 1,bi,bj)
               call flt_bilinear  (xx,yy,ss,k,salt,  1,bi,bj)
               tmp(6+k)      = uu
               tmp(6+1*Nr+k) = vv
               tmp(6+2*Nr+k) = tt
               tmp(6+3*Nr+k) = ss
            enddo

            else
               tmp(6)   = flt_nan

            do k=1,Nr
               tmp(6+k)      = flt_nan
               tmp(6+1*Nr+k) = flt_nan
               tmp(6+2*Nr+k) = flt_nan
               tmp(6+3*Nr+k) = flt_nan
            enddo
            endif

c
c the standard routine mdswritevector can be used here
c (3) write float positions into file
            irecord=npart_read+ip+1
            call mdswritevector(fn,64,.false.,'RL',imax,tmp,bi,bj,
     &                        irecord,myCurrentIter,mythid)

         enddo

      ENDDO
      ENDDO

      return
      end


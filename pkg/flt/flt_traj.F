C $Header: /u/gcmpack/MITgcm/pkg/flt/flt_traj.F,v 1.1 2001/09/13 17:43:56 adcroft Exp $
C $Name:  $

#include "FLT_CPPOPTIONS.h"


      subroutine flt_traj (
     I                     myCurrentIter, 
     I                     myCurrentTime, 
     I                     myThid
     &                     )

c     ==================================================================
c     SUBROUTINE flt_traj
c     ==================================================================
c
c     o This routine samples the model state at float position every 
c       flt_int_traj time steps and writes output.
c
c     ==================================================================
c     SUBROUTINE flt_traj
c     ==================================================================

c     == global variables ==

#include "EEPARAMS.h"
#include "SIZE.h"
#include "DYNVARS.h"
#include "PARAMS.h"
#include "SOLVE_FOR_PRESSURE.h"
#include "GRID.h"
#include "FLT.h"
c#include "UNITS.h"

c     == routine arguments ==

      INTEGER myCurrentIter, myThid
      _RL myCurrentTime
      INTEGER bi, bj, imax
      parameter (imax=10)

c     == local variables ==

      integer ip, kp, iip
      _RL xx, yy, xlo, xhi, ylo, yhi
      _RL uu,vv,ww,tt,ss, pp
      _RL global2local_i
      _RL global2local_j
c
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

      fn = 'float_trajectories'


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
c (2) write new actual number floats and time axis into file
c
c total number of records in this file
         tmp(1) = DBLE(npart_tile(bi,bj))+npart_read
c first time of writing floats (do not change when written)
c        tmp(2) = tmp(2)
c current time
         tmp(3) = myCurrentTime
c timestep
         tmp(4) = flt_int_traj
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

            xx=global2local_i(xpart(ip,bi,bj),bi,bj,mythid)
            yy=global2local_j(ypart(ip,bi,bj),bi,bj,mythid)
            kp=INT(kpart(ip,bi,bj))
            tmp(1)  = npart(ip,bi,bj)
            tmp(2)  = myCurrentTime
            tmp(3)  = xpart(ip,bi,bj)  
            tmp(4)  = ypart(ip,bi,bj) 
            tmp(5)  = kpart(ip,bi,bj)

         if(
     &  (                             myCurrentTime.ge.tstart(ip,bi,bj))
     &   .and.
     &  (  tend(ip,bi,bj).eq.-1. .or. myCurrentTime.le.  tend(ip,bi,bj))
     &      ) then

c            if(tstart(ip,bi,bj) .ne. -1.              .and.
c     &         myCurrentTime    .ge. tstart(ip,bi,bj) .and.
c     &         myCurrentTime    .le.   tend(ip,bi,bj)) then

               call flt_bilinear  (xx,yy,uu,kp,uVel,  2,bi,bj)
               call flt_bilinear  (xx,yy,vv,kp,vVel,  3,bi,bj)
               call flt_bilinear2d(xx,yy,pp,   cg2d_x,1,bi,bj)
               call flt_bilinear  (xx,yy,tt,kp,theta, 1,bi,bj)
               call flt_bilinear  (xx,yy,ss,kp,salt,  1,bi,bj)

               tmp(6)   = uu
               tmp(7)   = vv
               tmp(8)   = tt
               tmp(9)   = ss
               tmp(10)  = pp
             else
               tmp(6)   = flt_nan
               tmp(7)   = flt_nan
               tmp(8)   = flt_nan
               tmp(9)   = flt_nan
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

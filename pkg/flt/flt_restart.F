C $Header: /u/gcmpack/MITgcm/pkg/flt/Attic/flt_restart.F,v 1.3 2008/12/29 03:40:07 jmc Exp $
C $Name:  $

#include "FLT_OPTIONS.h"


      subroutine flt_restart (
     I                         myTime,
     I                         myIter,
     I                         myThid )

c     ==================================================================
c     SUBROUTINE flt_restart
c     ==================================================================
c
c     o This routine writes the actual float positions to a local files
c       that can used as restarts
c
c     ==================================================================
c     SUBROUTINE flt_restart
c     ==================================================================

c     == global variables ==

#include "EEPARAMS.h"
#include "SIZE.h"
#include "PARAMS.h"
#include "FLT.h"

c     == routine arguments ==
      _RL myTime
      INTEGER myIter, myThid

c     == local variables ==
      INTEGER bi, bj, imax
      parameter(imax=9)
      integer ip
      _RL tmp(imax)
      _RL npart_dist

c     == end of interface ==

      npart_dist = 0.

      DO bj=myByLo(myThid),myByHi(myThid)
      DO bi=myBxLo(myThid),myBxHi(myThid)

c
c the standard routine mdswritevector can be used here
c (1) write actual number floats and time into file
c
            tmp(1) = npart_tile(bi,bj)
            tmp(2) = myIter
            tmp(3) = myTime
            tmp(4) = 0.
            tmp(5) = 0.
            tmp(6) = max_npart
            tmp(7) = 0.
            tmp(8) = 0.
            tmp(9) = 0.
            call mdswritevector( flt_file,64,.false.,'RL',imax,tmp,
     *                           bi,bj,1,myIter,myThid )

         do ip=1,npart_tile(bi,bj)

            tmp(1) =   npart(ip,bi,bj)
            tmp(2) =  tstart(ip,bi,bj)
            tmp(3) =   xpart(ip,bi,bj)
            tmp(4) =   ypart(ip,bi,bj)
            tmp(5) =   kpart(ip,bi,bj)
            tmp(6) =  kfloat(ip,bi,bj)
            tmp(7) =     iup(ip,bi,bj)
            tmp(8) =    itop(ip,bi,bj)
            tmp(9) =    tend(ip,bi,bj)

c
c the standard routine mdswritevector can be used here
c (2) write float positions into file

            call mdswritevector( flt_file,64,.false.,'RL',imax,tmp,
     &                           bi,bj,ip+1,myIter,myThid )

         enddo

         npart_dist = npart_dist + DBLE(npart_tile(bi,bj))

       ENDDO
       ENDDO

       _GLOBAL_SUM_R8( npart_dist, myThid )
       _BEGIN_MASTER( myThid )
        write(errorMessageUnit,*) npart_dist,' floats written'
       _END_MASTER( myThid )

      return
      end

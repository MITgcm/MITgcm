C $Header: /u/gcmpack/MITgcm/pkg/flt/flt_up.F,v 1.3 2009/01/04 00:58:23 jmc Exp $
C $Name:  $

#include "FLT_OPTIONS.h"

      SUBROUTINE FLT_UP (
     I                    myTime, myIter, myThid )

C     ==================================================================
C     SUBROUTINE flt_up
C     ==================================================================
C
C     o This routine moves particles vertical from the target depth to
C       the surface and samples the model state over the full water
C       column at horizontal float position every flt_int_prof time steps
C       and writes output.
C
C     ==================================================================
C     SUBROUTINE flt_up
C     ==================================================================

C     == global variables ==

#include "EEPARAMS.h"
#include "SIZE.h"
#include "DYNVARS.h"
#include "PARAMS.h"
#include "GRID.h"
#include "FLT.h"
#include "SOLVE_FOR_PRESSURE.h"
c#include "UNITS.h"

C     == routine arguments ==

      _RL myTime
      INTEGER myIter, myThid

C     == local variables ==
      INTEGER bi, bj

      INTEGER imax
      PARAMETER (imax=(6+4*Nr))
      INTEGER ip, k
      _RL xx, yy
      _RL uu,vv,tt,ss, pp
      _RL global2local_i
      _RL global2local_j
      INTEGER irecord
      _RL tmp(imax)

      _RL npart_read,npart_times

      CHARACTER*(MAX_LEN_FNAM) fn

C Functions
      INTEGER ILNBLNK
C Local variables
      CHARACTER*(80) dataFName
      INTEGER iG,jG,IL
      LOGICAL exst
      LOGICAL globalFile

C     == end of interface ==

      fn = 'float_profiles'

      DO bj=myByLo(myThid),myByHi(myThid)
      DO bi=myBxLo(myThid),myBxHi(myThid)

C (1) read actual number floats from file (if exists)
         IL=ILNBLNK( fn )
         iG=bi+(myXGlobalLo-1)/sNx ! Kludge until unstructered tiles
         jG=bj+(myYGlobalLo-1)/sNy ! Kludge until unstructered tiles
         WRITE(dataFname(1:80),'(2a,i3.3,a,i3.3,a)')
     &              fn(1:IL),'.',iG,'.',jG,'.data'
         INQUIRE( file=dataFname, exist=exst )
         IF (exst) THEN
            CALL FLT_MDSREADVECTOR(fn,globalFile,64,'RL',
     &                             imax,tmp,bi,bj,1,mythid)
            npart_read  = tmp(1)
            npart_times = tmp(5)
         ELSE
            npart_read  = 0.
            npart_times = 0.
            tmp(2)      = myTime
         ENDIF

C the standard routine mdswritevector can be used here
C (2) write new actual number floats and time into file
C
C total number of records in this file
         tmp(1) = DBLE(npart_tile(bi,bj))+npart_read
C first time of writing floats (do not change when written)
c        tmp(2) = tmp(2)
C current time
         tmp(3) = myTime
C timestep
         tmp(4) = flt_int_prof
C total number of timesteps
         tmp(5) = npart_times + 1.
C total number of floats
         tmp(6) = max_npart
         DO ip=7,imax
            tmp(ip) = 0.
         ENDDO
         CALL MDSWRITEVECTOR(fn,64,.false.,'RL',imax,tmp,bi,bj,1,
     &                     myIter,mythid)

         DO ip=1,npart_tile(bi,bj)

C     Move float to the surface

         IF (
     &  (                       myTime.GE.tstart(ip,bi,bj))
     &   .AND.
     &  (tend(ip,bi,bj).EQ.-1..OR.myTime.LE.tend(ip,bi,bj))
     &   .AND.
     &  (kpart(ip,bi,bj) .EQ. kfloat(ip,bi,bj))
     &   .AND.
     &  (iup(ip,bi,bj)   .GT. 0.)
     &      ) THEN

c         IF (myTime   .GE. tstart(ip,bi,bj) .AND.
c     &      myTime   .LE.   tend(ip,bi,bj) .AND.
c     &      kpart(ip,bi,bj) .EQ. kfloat(ip,bi,bj) .AND.
c     &      iup(ip,bi,bj)   .GT. 0.) THEN

            IF (mod(myTime,iup(ip,bi,bj)).EQ.0.)
     &      kpart(ip,bi,bj) = flt_surf

         ENDIF

C     If float has died move to level 0

         IF (
     &  (tend(ip,bi,bj).NE.-1..AND.myTime.GT.tend(ip,bi,bj))
     &      ) THEN

            kpart(ip,bi,bj) = 0.

         ENDIF

C     Convert to local indices

            xx=global2local_i(xpart(ip,bi,bj),bi,bj,mythid)
            yy=global2local_j(ypart(ip,bi,bj),bi,bj,mythid)

            tmp(1)  = npart(ip,bi,bj)
            tmp(2)  = myTime
            tmp(3)  = xpart(ip,bi,bj)
            tmp(4)  = ypart(ip,bi,bj)
            tmp(5)  = kpart(ip,bi,bj)

         IF (
     &  (                         myTime.GE.tstart(ip,bi,bj))
     &   .AND.
     &  (tend(ip,bi,bj).EQ.-1..OR.myTime.LE.tend(ip,bi,bj))
     &      ) THEN

c            IF (tstart(ip,bi,bj) .NE. -1.              .AND.
c     &         myTime    .GE. tstart(ip,bi,bj) .AND.
c     &         myTime    .LE.   tend(ip,bi,bj)) THEN

               CALL flt_bilinear2d(xx,yy,pp,cg2d_x,1,bi,bj)
               tmp(6)   = pp

            DO k=1,Nr
               CALL flt_bilinear  (xx,yy,uu,k,uVel,  2,bi,bj)
               CALL flt_bilinear  (xx,yy,vv,k,vVel,  3,bi,bj)
               CALL flt_bilinear  (xx,yy,tt,k,theta, 1,bi,bj)
               CALL flt_bilinear  (xx,yy,ss,k,salt,  1,bi,bj)
               tmp(6+k)      = uu
               tmp(6+1*Nr+k) = vv
               tmp(6+2*Nr+k) = tt
               tmp(6+3*Nr+k) = ss
            ENDDO

            ELSE
               tmp(6)   = flt_nan

            DO k=1,Nr
               tmp(6+k)      = flt_nan
               tmp(6+1*Nr+k) = flt_nan
               tmp(6+2*Nr+k) = flt_nan
               tmp(6+3*Nr+k) = flt_nan
            ENDDO
            ENDIF

C the standard routine mdswritevector can be used here
C (3) write float positions into file
            irecord=npart_read+ip+1
            CALL mdswritevector(fn,64,.false.,'RL',imax,tmp,bi,bj,
     &                        irecord,myIter,mythid)

         ENDDO

      ENDDO
      ENDDO

      RETURN
      END


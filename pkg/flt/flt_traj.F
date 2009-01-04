C $Header: /u/gcmpack/MITgcm/pkg/flt/flt_traj.F,v 1.3 2009/01/04 00:58:23 jmc Exp $
C $Name:  $

#include "FLT_OPTIONS.h"


      SUBROUTINE FLT_TRAJ (
     I                      myTime, myIter, myThid )

C     ==================================================================
C     SUBROUTINE FLT_TRAJ
C     ==================================================================
C
C     o This routine samples the model state at float position every
C       flt_int_traj time steps and writes output.
C
C     ==================================================================
C     SUBROUTINE FLT_TRAJ
C     ==================================================================

C     == global variables ==

#include "SIZE.h"
#include "EEPARAMS.h"
#include "PARAMS.h"
#include "GRID.h"
#include "DYNVARS.h"
#include "SOLVE_FOR_PRESSURE.h"
#include "FLT.h"
c#include "UNITS.h"

C     == routine arguments ==

      _RL myTime
      INTEGER myIter, myThid

C     == local variables ==
      INTEGER bi, bj, imax
      PARAMETER (imax=10)

      INTEGER ip, kp, ii
      _RL xx, yy
      _RL uu,vv,tt,ss, pp
      _RL global2local_i
      _RL global2local_j

      INTEGER irecord
      _RL tmp(imax)
      _RL npart_read,npart_times
      CHARACTER*(MAX_LEN_FNAM) fn
      CHARACTER*(MAX_LEN_MBUF) msgBuf

C Functions
      INTEGER ILNBLNK
C Local variables
      CHARACTER*(80) dataFName
      INTEGER iG,jG,IL
      LOGICAL exst
      LOGICAL globalFile

C     == end of interface ==

      fn = 'float_trajectories'


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
     &                             imax,tmp,bi,bj,1,myThid)
            npart_read  = tmp(1)
            npart_times = tmp(5)
         ELSE
            npart_read  = 0.
            npart_times = 0.
            tmp(2)      = myTime
         ENDIF

C the standard routine mdswritevector can be used here
C (2) WRITE new actual number floats and time axis into file
C
C total number of records in this file
         tmp(1) = DBLE(npart_tile(bi,bj))+npart_read
C first time of writing floats (do not change when written)
c        tmp(2) = tmp(2)
C current time
         tmp(3) = myTime
C timestep
         tmp(4) = flt_int_traj
C total number of timesteps
         tmp(5) = npart_times + 1.
C total number of floats
         tmp(6) = max_npart
         DO ip=7,imax
            tmp(ip) = 0.
         ENDDO

         CALL MDSWRITEVECTOR(fn,64,.false.,'RL',imax,tmp,bi,bj,1,
     &                       myIter,myThid)

         DO ip=1,npart_tile(bi,bj)

            xx=global2local_i(xpart(ip,bi,bj),bi,bj,myThid)
            yy=global2local_j(ypart(ip,bi,bj),bi,bj,myThid)
            kp =  INT(kpart(ip,bi,bj))
            tmp(1)  = npart(ip,bi,bj)
            tmp(2)  = myTime
            tmp(3)  = xpart(ip,bi,bj)
            tmp(4)  = ypart(ip,bi,bj)
            tmp(5)  = kpart(ip,bi,bj)

            IF (
     &           ( myTime.GE.tstart(ip,bi,bj))
     &       .AND.
     &           ( tend(ip,bi,bj).EQ.-1. .OR. myTime.LE.tend(ip,bi,bj))
     &         ) THEN

c           if (tstart(ip,bi,bj) .NE. -1.              .AND.
c     &         myTime    .GE. tstart(ip,bi,bj) .AND.
c     &         myTime    .LE.   tend(ip,bi,bj)) THEN

              IF ( kp.LT.1 .OR. kp.GT.Nr ) THEN
                WRITE(msgBuf,'(2A,I8)') '** WARNING ** FLT_TRAJ: ',
     &            ' illegal value for kp=',kp
                CALL PRINT_MESSAGE( msgBuf, errorMessageUnit,
     &                              SQUEEZE_RIGHT, myThid )
                WRITE(msgBuf,'(A,1P5E20.13)')
     &            ' FLT_TRAJ: ', (tmp(ii),ii=1,5)
                CALL PRINT_MESSAGE( msgBuf, errorMessageUnit,
     &                              SQUEEZE_RIGHT, myThid )
c               CALL PRINT_ERROR( msgBuf, myThid )
c               STOP 'ABNORMAL END: S/R FLT_TRAJ'
              ENDIF
C-- jmc: not sure if this is right but added to avoid Pb in FLT_BILINEAR:
              kp = MIN( MAX(kp,1), Nr)

              CALL FLT_BILINEAR  (xx,yy,uu,kp,uVel,  2,bi,bj)
              CALL FLT_BILINEAR  (xx,yy,vv,kp,vVel,  3,bi,bj)
              CALL FLT_BILINEAR2D(xx,yy,pp,   cg2d_x,1,bi,bj)
              CALL FLT_BILINEAR  (xx,yy,tt,kp,theta, 1,bi,bj)
              CALL FLT_BILINEAR  (xx,yy,ss,kp,salt,  1,bi,bj)

              tmp(6)   = uu
              tmp(7)   = vv
              tmp(8)   = tt
              tmp(9)   = ss
              tmp(10)  = pp
            ELSE
              tmp(6)   = flt_nan
              tmp(7)   = flt_nan
              tmp(8)   = flt_nan
              tmp(9)   = flt_nan
              tmp(10)  = flt_nan
            ENDIF

C the standard routine mdswritevector can be used here
C (3) WRITE float positions into file

            irecord=npart_read+ip+1
            CALL MDSWRITEVECTOR(fn,64,.false.,'RL',imax,tmp,bi,bj,
     &                          irecord,myIter,myThid)

         ENDDO

       ENDDO
       ENDDO

      RETURN
      END

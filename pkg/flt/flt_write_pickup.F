C $Header: /u/gcmpack/MITgcm/pkg/flt/flt_write_pickup.F,v 1.1 2009/01/04 00:57:00 jmc Exp $
C $Name:  $

#include "FLT_OPTIONS.h"


      SUBROUTINE FLT_WRITE_PICKUP(
     I                     suff, myTime, myIter, myThid )

C     ==================================================================
C     SUBROUTINE FLT_WRITE_PICKUP
C     ==================================================================
C
C     o This routine writes the actual float positions to a local files
C       that can be used as restarts
C
C     ==================================================================
C     SUBROUTINE FLT_WRITE_PICKUP
C     ==================================================================

C     == global variables ==

#include "EEPARAMS.h"
#include "SIZE.h"
#include "PARAMS.h"
#include "FLT.h"

C     == routine arguments ==
C     suff    :: suffix for pickup file (eg. ckptA or 0000000010)
C     myTime  :: current time
C     myIter  :: time-step number
C     myThid  :: my Thread Id number
      CHARACTER*(*) suff
      _RL myTime
      INTEGER myIter, myThid

C     == Functions ==
      INTEGER  ILNBLNK
      EXTERNAL ILNBLNK

C     == local variables ==
      CHARACTER*(MAX_LEN_FNAM) fn
      CHARACTER*(MAX_LEN_MBUF) msgBuf
      INTEGER bi, bj, imax, iLen
      PARAMETER(imax=9)
      INTEGER ip
      _RL tmp(imax)
      _RL npart_dist

C     == end of interface ==

      iLen = ILNBLNK(suff)
      WRITE(fn,'(A,A)') 'pickup_flt.', suff(1:iLen)
      npart_dist = 0.

      DO bj=myByLo(myThid),myByHi(myThid)
      DO bi=myBxLo(myThid),myBxHi(myThid)

C the standard routine mdswritevector can be used here
C (1) write actual number floats and time into file

            tmp(1) = npart_tile(bi,bj)
            tmp(2) = myIter
            tmp(3) = myTime
            tmp(4) = 0.
            tmp(5) = 0.
            tmp(6) = max_npart
            tmp(7) = 0.
            tmp(8) = 0.
            tmp(9) = 0.
            CALL MDSWRITEVECTOR( fn, precFloat64, .FALSE., 'RL',
     &                           imax,tmp, bi,bj, 1, myIter,myThid )

         DO ip=1,npart_tile(bi,bj)

            tmp(1) =   npart(ip,bi,bj)
            tmp(2) =  tstart(ip,bi,bj)
            tmp(3) =   xpart(ip,bi,bj)
            tmp(4) =   ypart(ip,bi,bj)
            tmp(5) =   kpart(ip,bi,bj)
            tmp(6) =  kfloat(ip,bi,bj)
            tmp(7) =     iup(ip,bi,bj)
            tmp(8) =    itop(ip,bi,bj)
            tmp(9) =    tend(ip,bi,bj)

C the standard routine mdswritevector can be used here
C (2) write float positions into file

            CALL MDSWRITEVECTOR( fn, precFloat64, .FALSE., 'RL',
     &                           imax,tmp, bi,bj,ip+1, myIter,myThid )

         ENDDO

         npart_dist = npart_dist + DBLE(npart_tile(bi,bj))

       ENDDO
       ENDDO

       _GLOBAL_SUM_R8( npart_dist, myThid )
       _BEGIN_MASTER( myThid )
        WRITE(msgBuf,*) 'FLT_WRITE_PICKUP:', 
     &                   npart_dist, 'floats written'
        CALL PRINT_MESSAGE( msgBuf, standardMessageUnit,
     &                      SQUEEZE_RIGHT, myThid )
       _END_MASTER( myThid )

      RETURN
      END

C $Header: /u/gcmpack/MITgcm/pkg/flt/flt_init_varia.F,v 1.7 2010/12/22 21:25:18 jahn Exp $
C $Name:  $

#include "FLT_OPTIONS.h"

      SUBROUTINE FLT_INIT_VARIA ( myThid  )

C     ==================================================================
C     SUBROUTINE FLT_INIT_VARIA
C     ==================================================================
C     o This routine initializes the start/restart positions.
C       It does the following:
C     o First it checks for local files. These are supposed to be restarts
C       from a previous integration. The floats can therefore be read in
C       without any further check (because they exist on the specific tile).
C     o If no local files are available the routine assumes that this
C       is an initialization. In that case it reads a global file
C       (that has the same format as local files) and sorts those floats
C       that exist on the specific tile into the local array.
C     o At the end the float positions are written to the trajectory file
C     ==================================================================

C     !USES:
      IMPLICIT NONE

#include "SIZE.h"
#include "EEPARAMS.h"
#include "PARAMS.h"
#include "FLT_SIZE.h"
#include "FLT.h"

C     == routine arguments ==
C     myThid - thread number for this instance of the routine.
      INTEGER myThid

C     == Functions ==
      INTEGER  ILNBLNK
      EXTERNAL ILNBLNK
      _RL      FLT_MAP_R2K
      EXTERNAL FLT_MAP_R2K

C     == local variables ==
      INTEGER bi, bj
      INTEGER ip, iL
      INTEGER imax
      PARAMETER(imax=9)
      _RL tmp(imax)
      _RL ix, jy, kz
      _RL iLo, iHi, jLo, jHi
      LOGICAL globalFile
      CHARACTER*(MAX_LEN_FNAM) fn
      CHARACTER*(MAX_LEN_MBUF) msgBuf

C     number of active record in the file (might be lower than the
C     total number of records because the tile could have contained
C     more floats at an earlier restart
      INTEGER npart_read
      _RL     npart_dist

C     == end of interface ==

C-    all threads initialise local var:
      npart_read = 0
      npart_dist = 0.

C read floats initial condition from file
      _BEGIN_MASTER(myThid)
      IF ( nIter0.EQ.0 ) THEN
        fn = flt_file
      ELSE
        WRITE(fn,'(A,I10.10)') 'pickup_flt.', nIter0
      ENDIF
      iL = ILNBLNK(fn)
      WRITE(msgBuf,'(2A)')
     &   'FLT_INIT_VARIA: reading Floats from: ', fn(1:iL)
      CALL PRINT_MESSAGE( msgBuf, standardMessageUnit,
     &                    SQUEEZE_RIGHT, myThid )

      DO bj = 1,nSy
       DO bi = 1,nSx

C     tile boundary on index map:
         iLo = 0.5 _d 0
         iHi = 0.5 _d 0 + DFLOAT(sNx)
         jLo = 0.5 _d 0
         jHi = 0.5 _d 0 + DFLOAT(sNy)

C (1) read actual number floats from file
         CALL FLT_MDSREADVECTOR( fn, globalFile, precFloat64, 'RL',
     &                            imax, tmp, bi, bj, 1, myThid )
         WRITE(msgBuf,'(A,2I4,A,1P2E15.8)')
     &    ' bi,bj=', bi, bj, ' , npart,max_npart=', tmp(1), tmp(6)
         CALL PRINT_MESSAGE( msgBuf, standardMessageUnit,
     &                       SQUEEZE_RIGHT, myThid )
         npart_read = NINT(tmp(1))
         max_npart  = tmp(6)

         IF (globalFile) THEN
           npart_tile(bi,bj) = 0
         ELSE
           npart_tile(bi,bj) = npart_read
           npart_read = MIN( npart_read, max_npart_tile )
         ENDIF

         DO ip=1,npart_read

           CALL FLT_MDSREADVECTOR( fn, globalFile, precFloat64, 'RL',
     &                             imax, tmp, bi, bj, ip+1, myThid )

           IF ( nIter0.EQ.0 ) THEN
C--   For initial condition only, convert coordinates to index map:
             CALL FLT_MAP_XY2IJLOCAL( ix, jy,
     I                    tmp(3), tmp(4),bi,bj,myThid )
             kz = FLT_MAP_R2K( tmp(5), bi, bj, myThid )
           ELSE
             ix = tmp(3)
             jy = tmp(4)
             kz = tmp(5)
           ENDIF

           IF ( globalFile .AND. nIter0.EQ.0 ) THEN
C     Check if floats are existing on tile. If not, jump to next float

            IF ( ix.GE.iLo .AND. ix.LT.iHi .AND.
     &           jy.GE.jLo .AND. jy.LT.jHi ) THEN
             npart_tile(bi,bj) = npart_tile(bi,bj) + 1
             IF ( npart_tile(bi,bj).LE.max_npart_tile ) THEN

              npart( npart_tile(bi,bj),bi,bj) = tmp(1)
              tstart(npart_tile(bi,bj),bi,bj) = tmp(2)
              ipart( npart_tile(bi,bj),bi,bj) = ix
              jpart( npart_tile(bi,bj),bi,bj) = jy
              kpart( npart_tile(bi,bj),bi,bj) = kz
              kfloat(npart_tile(bi,bj),bi,bj) = tmp(6)
              iup(   npart_tile(bi,bj),bi,bj) = tmp(7)
              itop(  npart_tile(bi,bj),bi,bj) = tmp(8)
              tend(  npart_tile(bi,bj),bi,bj) = tmp(9)

             ENDIF
            ENDIF

           ELSEIF ( globalFile ) THEN
            WRITE(msgBuf,'(2A)') 'FLT_INIT_VARIA:',
     &                           ' global pickup not supported'
            CALL PRINT_ERROR( msgBuf , myThid)
            STOP 'ABNORMAL END: S/R FLT_INIT_VARIA'

           ELSE
C     not a global file: assume that all particles from this tiled-file
C     belong to this current tile (=> do not no check)

              npart(ip,bi,bj)  = tmp(1)
              tstart(ip,bi,bj) = tmp(2)
              ipart(ip,bi,bj)  = tmp(3)
              jpart(ip,bi,bj)  = tmp(4)
              kpart(ip,bi,bj)  = tmp(5)
              kfloat(ip,bi,bj) = tmp(6)
              iup(  ip,bi,bj)  = tmp(7)
              itop( ip,bi,bj)  = tmp(8)
              tend( ip,bi,bj)  = tmp(9)

           ENDIF

         ENDDO

         npart_dist = npart_dist + DBLE(npart_tile(bi,bj))
         IF ( npart_tile(bi,bj).GT.max_npart_tile ) THEN
           WRITE(msgBuf,'(2A,2I4,2(A,I8))') 'FLT_INIT_VARIA:',
     &       ' bi,bj=', bi, bj,
     &       ' npart_tile=', npart_tile(bi,bj),
     &       ' > max_npart_tile=', max_npart_tile
           CALL PRINT_ERROR( msgBuf , myThid)
           STOP 'ABNORMAL END: S/R FLT_INIT_VARIA'
         ENDIF

       ENDDO
      ENDDO
      _END_MASTER( myThid )
      _BARRIER

      _GLOBAL_SUM_RL( npart_dist, myThid )

      _BEGIN_MASTER( myThid )
        WRITE(msgBuf,'(A,2(A,I9))') 'FLT_INIT_VARIA:',
     &          ' max npart=', NINT(max_npart),
     &   ' , sum npart_tile=', NINT(npart_dist)
        CALL PRINT_MESSAGE( msgBuf, standardMessageUnit,
     &                      SQUEEZE_RIGHT, myThid )
        WRITE(msgBuf,'(A)') ' '
        CALL PRINT_MESSAGE( msgBuf, standardMessageUnit,
     &                      SQUEEZE_RIGHT, myThid )
      _END_MASTER( myThid )

      RETURN
      END

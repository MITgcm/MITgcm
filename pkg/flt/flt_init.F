C $Header: /u/gcmpack/MITgcm/pkg/flt/Attic/flt_init.F,v 1.4 2009/01/04 00:58:23 jmc Exp $
C $Name:  $

#include "FLT_OPTIONS.h"

      SUBROUTINE FLT_INIT ( myThid  )

C     ==================================================================
C     SUBROUTINE flt_init
C     ==================================================================
C
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
C
C     ==================================================================
C     SUBROUTINE flt_init
C     ==================================================================

#include "EEPARAMS.h"
#include "SIZE.h"
#include "FLT.h"
#include "GRID.h"
#include "PARAMS.h"

C     == routine arguments ==

C     myThid - thread number for this instance of the routine.
      INTEGER myThid

C-    Functions:
      INTEGER  IFNBLNK
      EXTERNAL IFNBLNK
      INTEGER  ILNBLNK
      EXTERNAL ILNBLNK

C     == local variables ==
      INTEGER bi, bj
      INTEGER ip, iG, jG, iL
      INTEGER imax
      PARAMETER(imax=9)
      _RL tmp(imax)
      _RL xlo, xhi, ylo, yhi

      LOGICAL globalFile
      CHARACTER*(MAX_LEN_FNAM) fn
      CHARACTER*(MAX_LEN_MBUF) msgBuf
      INTEGER iUnit

C     number of active record in the file (might be lower than the
C     total number of records because the tile could have contained
C     more floats at an earlier restart
      INTEGER npart_read
      _RL     npart_dist

      NAMELIST /flt_nml/ flt_int_traj, flt_int_prof, flt_noise
     &                  ,flt_file

C     == end of interface ==

C-    all threads initialise local var:
      npart_read = 0
      npart_dist = 0.

      _BEGIN_MASTER(myThid)

C     Set default values.
      flt_int_traj =  3600.
      flt_int_prof = 43200.
      flt_noise    = 0.0
      flt_file     = 'float_pos'

      WRITE(msgBuf,'(A)') ' FLT_INIT: opening data.flt'
      CALL PRINT_MESSAGE( msgBuf, standardMessageUnit,
     &                    SQUEEZE_RIGHT, myThid )
      CALL OPEN_COPY_DATA_FILE(
     I                          'data.flt', 'FLT_INIT',
     O                          iUnit,
     I                          myThid )

C     Read parameters from open data file
      READ(UNIT=iUnit,NML=FLT_NML)
      WRITE(msgBuf,'(A)') ' FLT_INIT: finished reading data.flt'
      CALL PRINT_MESSAGE( msgBuf, standardMessageUnit,
     &                    SQUEEZE_RIGHT, myThid )
C     Close the open data file
      CLOSE(iUnit)

C DO some checks
      IF ( useFLT .AND. useOBCS ) THEN
       WRITE(msgBuf,'(A,A)')
     &  'S/R FLT_INIT: Integrating floats is currently not possible',
     &  'in combination with open boundaries.'
       CALL PRINT_ERROR( msgBuf , myThid)
       STOP 'ABNORMAL END: S/R FLT_INIT'
      ENDIF

C read floats initial condition from file
      IF ( nIter0.EQ.0 ) THEN
        fn = flt_file
      ELSE
        WRITE(fn,'(A,I10.10)') 'pickup_flt.', nIter0
      ENDIF
      iL = ILNBLNK(fn)
      WRITE(msgBuf,'(2A)') 'FLT_INIT: reading Floats from: ',fn(1:iL)
      CALL PRINT_MESSAGE( msgBuf, standardMessageUnit,
     &                    SQUEEZE_RIGHT, myThid )

      DO bj = 1,nSy
       DO bi = 1,nSx

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

           IF (globalFile) THEN

C     check IF floats are existing on tile. If not, set to zero
C     use southern/western side for axis information

C     note: The possible area for a float has to extended to the
C           space "between" two T points, i.e. xc(sNx) of one tile
C           and xc(1) of the neighboring tile. This cannot be solved
C           by simply using xc(sNx+1) or xc(0) because periodicity
C           could imply wrong values

            iG = myXGlobalLo + (bi-1)*sNx
            jG = myYGlobalLo + (bj-1)*sNy

            xlo = xc(1,  1,  bi,bj) - delX(iG)*0.5
            xhi = xc(sNx,1,bi,bj)   + delX(iG+sNx-1)*0.5
            ylo = yc(1,  1,  bi,bj) - delY(jG)*0.5
            yhi = yc(1,sNy,bi,bj)   + delY(jG+sNy-1)*0.5

            IF (tmp(3) .GE. xlo .AND. tmp(3) .LE. xhi .AND.
     &          tmp(4) .GE. ylo .AND. tmp(4) .LE. yhi) THEN

             npart_tile(bi,bj) = npart_tile(bi,bj) + 1
             IF ( npart_tile(bi,bj).LE.max_npart_tile ) THEN

              npart(npart_tile(bi,bj),bi,bj)  = tmp(1)
              tstart(npart_tile(bi,bj),bi,bj) = tmp(2)
              xpart(npart_tile(bi,bj),bi,bj)  = tmp(3)
              ypart(npart_tile(bi,bj),bi,bj)  = tmp(4)
              kpart(npart_tile(bi,bj),bi,bj)  = tmp(5)
              kfloat(npart_tile(bi,bj),bi,bj) = tmp(6)
              iup(  npart_tile(bi,bj),bi,bj)  = tmp(7)
              itop( npart_tile(bi,bj),bi,bj)  = tmp(8)
              tend( npart_tile(bi,bj),bi,bj)  = tmp(9)
             ENDIF
            ENDIF

           ELSE
C     not a global file: assume that all particles from this tiled-file
C     belong to this current tile (=> do not no check)

              npart(ip,bi,bj)  = tmp(1)
              tstart(ip,bi,bj) = tmp(2)
              xpart(ip,bi,bj)  = tmp(3)
              ypart(ip,bi,bj)  = tmp(4)
              kpart(ip,bi,bj)  = tmp(5)
              kfloat(ip,bi,bj) = tmp(6)
              iup(  ip,bi,bj)  = tmp(7)
              itop( ip,bi,bj)  = tmp(8)
              tend( ip,bi,bj)  = tmp(9)

           ENDIF

         ENDDO
         npart_dist = npart_dist + DBLE(npart_tile(bi,bj))
         IF ( npart_tile(bi,bj).GT.max_npart_tile ) THEN
           WRITE(msgBuf,'(2A,2I4,2(A,I8))') 'FLT_INIT:',
     &       ' bi,bj=', bi, bj,
     &       ' npart_tile=', npart_tile(bi,bj),
     &       ' > max_npart_tile=', max_npart_tile
           CALL PRINT_ERROR( msgBuf , myThid)
           STOP 'ABNORMAL END: S/R FLT_INIT'
         ENDIF

       ENDDO
      ENDDO
      _END_MASTER( myThid )
      _BARRIER

      _GLOBAL_SUM_R8( npart_dist, myThid )

      _BEGIN_MASTER( myThid )
        WRITE(msgBuf,'(A,2(A,I9))') 'FLT_INIT:',
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

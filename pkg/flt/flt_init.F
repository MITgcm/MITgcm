C $Header: /u/gcmpack/MITgcm/pkg/flt/Attic/flt_init.F,v 1.1 2001/09/13 17:43:55 adcroft Exp $
C $Name:  $

#include "FLT_CPPOPTIONS.h"

      subroutine flt_init ( myCurrentIter,myCurrentTime, myThid  )

c     ==================================================================
c     SUBROUTINE flt_init
c     ==================================================================
c
c     o This routine initializes the start/restart positions.
c       It does the following:
c     o First it checks for local files. These are supposed to be restarts
c       from a previous integration. The floats can therefore be read in
c       without any further check (because they exist on the specific tile).
c     o If no local files are available the routine assumes that this
c       is an initialization. In that case it reads a global file
c       (that has the same format as local files) and sorts those floats
c       that exist on the specific tile into the local array.
c     o At the end the float positions are written to the trajectory file
c
c     ==================================================================
c     SUBROUTINE flt_init
c     ==================================================================

#include "EEPARAMS.h"
#include "SIZE.h"
#include "FLT.h"    
#include "GRID.h" 
#include "PARAMS.h"

c     == routine arguments ==

c     mythid - thread number for this instance of the routine.

      INTEGER myCurrentIter, myThid
      INTEGER ip, iG, jG
      _RL myCurrentTime

c     == local variables ==

      INTEGER imax
      parameter(imax=9)
      _RL tmp(imax)
      integer jtlo,jthi,itlo,ithi
      INTEGER bi, bj, xx, yy
      _RL xlo, xhi, ylo, yhi

      logical globalFile

c     number of active record in the file (might be lower than the 
c     total number of records because the tile could have contained
c     more floats at an earlier restart
      _RL npart_read, npart_dist

      character*(max_len_mbuf) msgbuf
      INTEGER K, I, J, IL, iUnit
      INTEGER errIO
      INTEGER  IFNBLNK
      EXTERNAL IFNBLNK
      INTEGER  ILNBLNK
      EXTERNAL ILNBLNK
      CHARACTER*(MAX_LEN_PREC) record

      namelist /flt_nml/ flt_int_traj, flt_int_prof, flt_noise
     &                  ,flt_file

c     == end of interface ==

      _BEGIN_MASTER(mythid)

c     Set default values.
      flt_int_traj =  3600.
      flt_int_prof = 43200.
      flt_noise    = 0.0
      flt_file     = 'float_pos'

c      call nml_filter( 'data.flt', scrunit1, myThid )
c      if (scrunit1 .eq. 0) then
c         stop 'flt_init: reading namelist failed'
c      end if
c      read(  scrunit1, nml = flt_nml )
c      close( scrunit1 )

C--   Open the parameter file
      OPEN(UNIT=scrUnit1,STATUS='SCRATCH')
      OPEN(UNIT=scrUnit2,STATUS='SCRATCH')
      OPEN(UNIT=modelDataUnit,FILE='data.flt',STATUS='OLD',
     &     IOSTAT=errIO)
      IF ( errIO .LT. 0 ) THEN
       WRITE(msgBuf,'(A)')
     &  'S/R FLT_INIT'
       CALL PRINT_ERROR( msgBuf , 1)
       WRITE(msgBuf,'(A)')
     &  'Unable to open flt parameter'
       CALL PRINT_ERROR( msgBuf , 1)
       WRITE(msgBuf,'(A)')
     &  'file "data.flt"'
       CALL PRINT_ERROR( msgBuf , 1)
       CALL MODELDATA_EXAMPLE( myThid )
       STOP 'ABNORMAL END: S/R FLT_INIT'
      ENDIF     

      DO WHILE ( .TRUE. )
       READ(modelDataUnit,FMT='(A)',END=1001) RECORD
       IL = MAX(ILNBLNK(RECORD),1)
       IF ( RECORD(1:1) .NE. commentCharacter )
     &     WRITE(UNIT=scrUnit1,FMT='(A)') RECORD(:IL)
        WRITE(UNIT=scrUnit2,FMT='(A)') RECORD(:IL)
      ENDDO
 1001 CONTINUE
      CLOSE(modelDataUnit)

C--   Report contents of model parameter file
      WRITE(msgBuf,'(A)') 
     &'// ======================================================='
      CALL PRINT_MESSAGE( msgBuf, standardMessageUnit, 
     &                    SQUEEZE_RIGHT , 1)
      WRITE(msgBuf,'(A)') '// Float parameter file "data.flt"'
      CALL PRINT_MESSAGE( msgBuf, standardMessageUnit, 
     &                    SQUEEZE_RIGHT , 1)
      WRITE(msgBuf,'(A)') 
     &'// ======================================================='
      CALL PRINT_MESSAGE( msgBuf, standardMessageUnit,
     &  SQUEEZE_RIGHT , 1)
      iUnit = scrUnit2
      REWIND(iUnit)
      DO WHILE ( .TRUE. )
       READ(UNIT=iUnit,FMT='(A)',END=2001) RECORD
       IL = MAX(ILNBLNK(RECORD),1)
       WRITE(msgBuf,'(A,A)') '>',RECORD(:IL)
       CALL PRINT_MESSAGE( msgBuf, standardMessageUnit, 
     &                    SQUEEZE_RIGHT , 1)
      ENDDO
 2001 CONTINUE
      CLOSE(iUnit)
      WRITE(msgBuf,'(A)') ' '
      CALL PRINT_MESSAGE( msgBuf, standardMessageUnit,
     &  SQUEEZE_RIGHT , 1)

      _END_MASTER( mythid )

C--   Read settings from model parameter file "data.flt".
      iUnit = scrUnit1
      REWIND(iUnit)

      READ(UNIT=iUnit,NML=FLT_NML) !,IOSTAT=errIO)
      IF ( errIO .LT. 0 ) THEN
       WRITE(msgBuf,'(A)')
     &  'S/R FLT_INIT'
       CALL PRINT_ERROR( msgBuf , 1)
       WRITE(msgBuf,'(A)')
     &  'Error reading float parameter file '
       CALL PRINT_ERROR( msgBuf , 1)
       WRITE(msgBuf,'(A)')
     &  'parameter file "data.flt"'
       CALL PRINT_ERROR( msgBuf , 1)
       WRITE(msgBuf,'(A)')
     &  'Problem in namelist FLT_NML'
       CALL PRINT_ERROR( msgBuf , 1)
       CALL MODELDATA_EXAMPLE( myThid )
       STOP 'ABNORMAL END: S/R FLT_INIT'
      ENDIF

c do some checks

      IF ( useFLT .AND. useOBCS ) THEN
       WRITE(msgBuf,'(A,A)')
     &  'S/R FLT_INIT: Integrating floats is currently not possible',
     &  'in combination with open boundaries.'
       CALL PRINT_ERROR( msgBuf , myThid)
       STOP 'ABNORMAL END: S/R FLT_INIT'
      ENDIF

      _BARRIER

c     This might be faster, since the assignment is only done once.
      jtlo = mybylo(mythid)
      jthi = mybyhi(mythid)
      itlo = mybxlo(mythid)
      ithi = mybxhi(mythid)


      do bj = jtlo,jthi
        do bi = itlo,ithi
c
c (1) read actual number floats from file
          call mdsreadvector_flt(flt_file,globalFile,64,'RL',
     &                           imax,tmp,bi,bj,1,mythid)
          npart_read = tmp(1)
          max_npart  = tmp(6)

          if (globalFile) then
             npart_tile(bi,bj) = 0
          else
             npart_tile(bi,bj) = INT(npart_read)
          endif

          do ip=1,INT(npart_read)

          call mdsreadvector_flt(flt_file,globalFile,64,'RL',
     &                           imax,tmp,bi,bj,ip+1,mythid)

          if (globalFile) then
c
c     check if floats are existing on tile. If not, set to zero
c     use southern/western side for axis information
c

c     note: The possible area for a float has to extended to the 
c           space "between" two T points, i.e. xc(sNx) of one tile
c           and xc(1) of the neighboring tile. This cannot be solved
c           by simply using xc(sNx+1) or xc(0) because periodicity
c           could imply wrong values
c
            iG = myXGlobalLo + (bi-1)*sNx
            jG = myYGlobalLo + (bj-1)*sNy

            xlo = xc(1,  1,  bi,bj) - delX(iG)*0.5
            xhi = xc(sNx,1,bi,bj)   + delX(iG+sNx-1)*0.5
            ylo = yc(1,  1,  bi,bj) - delY(jG)*0.5
            yhi = yc(1,sNy,bi,bj)   + delY(jG+sNy-1)*0.5
 
            if (tmp(3) .ge. xlo .and. tmp(3) .le. xhi .and.
     &          tmp(4) .ge. ylo .and. tmp(4) .le. yhi) then

               npart_tile(bi,bj) = npart_tile(bi,bj) + 1
               if (npart_tile(bi,bj) .gt. max_npart_tile) 
     &         stop ' max_npart_tile too low. stop in flt_init'

              npart(npart_tile(bi,bj),bi,bj)  = tmp(1)
             tstart(npart_tile(bi,bj),bi,bj)  = tmp(2)
              xpart(npart_tile(bi,bj),bi,bj)  = tmp(3)
              ypart(npart_tile(bi,bj),bi,bj)  = tmp(4)
              kpart(npart_tile(bi,bj),bi,bj)  = tmp(5)
             kfloat(npart_tile(bi,bj),bi,bj)  = tmp(6)
                iup(npart_tile(bi,bj),bi,bj)  = tmp(7)
               itop(npart_tile(bi,bj),bi,bj)  = tmp(8)
               tend(npart_tile(bi,bj),bi,bj)  = tmp(9)
              endif

c            else

c              npart(ip,bi,bj)  = tmp(1)
c             tstart(ip,bi,bj)  = tmp(2)
c              xpart(ip,bi,bj)  = tmp(3)
c              ypart(ip,bi,bj)  = tmp(4)
c              kpart(ip,bi,bj)  = tmp(5)
c             kfloat(ip,bi,bj)  = tmp(6)
c                iup(ip,bi,bj)  = tmp(7)
c               itop(ip,bi,bj)  = tmp(8)
c               tend(ip,bi,bj)  = tmp(9)

            endif
            

          enddo

         _BARRIER
         _BEGIN_MASTER( myThid )
         npart_dist = DBLE(npart_tile(bi,bj))
         _GLOBAL_SUM_R8( npart_dist, myThid )
         if (.not. globalFile) _GLOBAL_SUM_R8(npart_read,myThid)
         if (myProcId .eq. 0) then
            write(errormessageunit,*) '    max _npart: ',max_npart 
            write(errormessageunit,*) 'sum npart_read: ',npart_read
            write(errormessageunit,*) 'sum npart_tile: ',npart_dist
         endif
         _END_MASTER( myThid )
         _BARRIER

        enddo
      enddo     

      return
      end


#include "ECCO_OPTIONS.h"

      subroutine cost_averagesgeneric(
     &     localbarfile,
     &     localbar, localfld, xx_localbar_mean_dummy,
     &     first, last, startofloc, endofloc, inloc,
     &     sum1loc, locrec, nnz, myThid )

c     ==================================================================
c     SUBROUTINE cost_averagesgeneric
c     ==================================================================
c
c     o Compute time averages of cost variables
c
c     ==================================================================
c     SUBROUTINE cost_averagesgeneric
c     ==================================================================

      implicit none

c     == global variables ==

#include "EEPARAMS.h"
#include "SIZE.h"
#include "PARAMS.h"

#ifdef ALLOW_ECCO
# include "ECCO_SIZE.h"
# include "ECCO.h"
#endif

c     == routine arguments ==

      integer myThid
      integer nnz
      integer locrec
      integer sum1loc

      _RL localbar(1-OLx:sNx+OLx,1-OLy:sNy+OLy,nnz,nSx,nSy)
      _RL localfld(1-OLx:sNx+OLx,1-OLy:sNy+OLy,nnz,nSx,nSy)
      _RL xx_localbar_mean_dummy

      logical first
      logical last
      logical startofloc
      logical endofloc
      logical inloc

      character*(MAX_LEN_FNAM) localbarfile

c     == local variables ==

      integer bi,bj
      integer i,j,k
      integer itlo,ithi
      integer jtlo,jthi
      integer jmin,jmax
      integer imin,imax

      integer il

      character*(128) fname
#ifdef ALLOW_ECCO_DEBUG
      character*(max_len_mbuf) msgbuf
#endif

c     == external functions ==

      integer  ilnblnk
      external ilnblnk

c     == end of interface ==

#ifdef ALLOW_ECCO_DEBUG
       write(msgbuf,'(a)') '>> entering'
        call print_message( msgbuf, standardmessageunit,
     &                    SQUEEZE_RIGHT , myThid)

       il=ilnblnk( localbarfile )
       write(msgbuf,'(a,a)')
     & 'cost_averagesgeneric, file : ',localbarfile(1:il)
       call print_message( msgbuf, standardmessageunit,
     &                    SQUEEZE_RIGHT , myThid)

       write(msgbuf,'(a,5L5)')
     & 'cost_averagesgeneric, logicals : ',
     &  first, last, startofloc, endofloc, inloc
       call print_message( msgbuf, standardmessageunit,
     &                    SQUEEZE_RIGHT , myThid)

       write(msgbuf,'(a,3i5)')
     & 'cost_averagesgeneric, integers : ',
     &  sum1loc, locrec, nnz
       call print_message( msgbuf, standardmessageunit,
     &                    SQUEEZE_RIGHT , myThid)

       write(msgbuf,'(a)') '<< leaving'
        call print_message( msgbuf, standardmessageunit,
     &                    SQUEEZE_RIGHT , myThid)
#endif

      jtlo = myByLo(myThid)
      jthi = myByHi(myThid)
      itlo = myBxLo(myThid)
      ithi = myBxHi(myThid)
      jmin = 1
      jmax = sNy
      imin = 1
      imax = sNx

      if (startofloc .and. endofloc) then
c--     Save snapshot at every time step
        do bj = jtlo,jthi
          do bi = itlo,ithi
            do k = 1,nnz
              do j = jmin,jmax
                do i = imin,imax
                  localbar(i,j,k,bi,bj) = localfld(i,j,k,bi,bj)
                enddo
              enddo
            enddo
          enddo
        enddo
c--     Save ...bar on file.
        il=ilnblnk( localbarfile )
        write(fname,'(2a,i10.10)')
     &       localbarfile(1:il), '.', eccoiter
#ifdef ALLOW_AUTODIFF
        if ( nnz .EQ. 1 ) then
           call active_write_xy( fname, localbar, locrec, eccoiter,
     &          myThid, xx_localbar_mean_dummy )
        else
           call active_write_xyz( fname, localbar, locrec, eccoiter,
     &          myThid, xx_localbar_mean_dummy )
        endif
#else
        if ( nnz .EQ. 1 ) then
           CALL WRITE_REC_XY_RL( fname, localbar, locrec, 1, myThid )
        else
           CALL WRITE_REC_XYZ_RL( fname, localbar, locrec, 1, myThid )
        endif
#endif

      elseif (first .or. startofloc) then
c--     Assign the first value to the array holding the average.
        do bj = jtlo,jthi
          do bi = itlo,ithi
            do k = 1,nnz
              do j = jmin,jmax
                do i =  imin,imax
                  localbar(i,j,k,bi,bj) = localfld(i,j,k,bi,bj)
                enddo
              enddo
            enddo
          enddo
        enddo
      else if (last .or. endofloc) then
c--     Add the last value and devide by the number of accumulated records.
        do bj = jtlo,jthi
          do bi = itlo,ithi
            do k = 1,nnz
              do j = jmin,jmax
                do i = imin,imax
                  localbar(i,j,k,bi,bj) =
     &                  (localbar(i,j,k,bi,bj)
     &                  +localfld(i,j,k,bi,bj))/
     &                  float(sum1loc)
                enddo
              enddo
            enddo
          enddo
        enddo
c--     Save ...bar on file.
        il=ilnblnk( localbarfile )
        write(fname,'(2a,i10.10)')
     &       localbarfile(1:il), '.', eccoiter
#ifdef ALLOW_AUTODIFF
        if ( nnz .EQ. 1 ) then
           call active_write_xy( fname, localbar, locrec, eccoiter,
     &          myThid, xx_localbar_mean_dummy )
        else
           call active_write_xyz( fname, localbar, locrec, eccoiter,
     &          myThid, xx_localbar_mean_dummy )
        endif
#else
        if ( nnz .EQ. 1 ) then
           CALL WRITE_REC_XY_RL( fname, localbar, locrec, 1, myThid )
        else
           CALL WRITE_REC_XYZ_RL( fname, localbar, locrec, 1, myThid )
        endif
#endif
      else if (       (       inloc        ) .and.
     &          .not. (first .or. startofloc) .and.
     &          .not. (last  .or. endofloc  )       ) then
c--     Accumulate the array holding the average.
        do bj = jtlo,jthi
          do bi = itlo,ithi
            do j = jmin,jmax
              do k = 1,nnz
                do i = imin,imax
                  localbar(i,j,k,bi,bj) =
     &                  localbar(i,j,k,bi,bj) + localfld(i,j,k,bi,bj)
                enddo
              enddo
            enddo
          enddo
        enddo
      else
        stop   'in cost_averagesgeneric'
      endif

      return
      end

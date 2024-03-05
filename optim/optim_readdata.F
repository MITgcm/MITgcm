C     ECCO_CPPOPTIONS used to affect maxcvars and defined ALLOW_OBCS?_CONTROL
C#include "ECCO_CPPOPTIONS.h"
C     now:
C     CTRL_OPTIONS affects maxcvars and may define ALLOW_OBCS?_CONTROL
#include "CTRL_OPTIONS.h"

      subroutine optim_readdata(
     I                      nn,
     I                      dfile,
     I                      lheaderonly,
     O                      ff,
     O                      vv
     &                    )

c     ==================================================================
c     SUBROUTINE optim_readdata
c     ==================================================================
c
c     o Read the data written by the MITgcmUV state estimation setup and
c       join them to one vector that is subsequently used by the minimi-
c       zation algorithm "lsopt". Depending on the specified file name
c       either the control vector or the gradient vector can be read.
c
c       *dfile* should be the radix of the file: ecco_ctrl or ecco_cost
c
c     started: Christian Eckert eckert@mit.edu 12-Apr-2000
c
c     changed:  Patrick Heimbach heimbach@mit.edu 19-Jun-2000
c               - finished, revised and debugged
c
c     ==================================================================
c     SUBROUTINE optim_readdata
c     ==================================================================

      IMPLICIT NONE

c     == global variables ==

#include "EEPARAMS.h"
#include "SIZE.h"
#include "CTRL_SIZE.h"
#ifdef ALLOW_OBCS_CONTROL
# include "CTRL_OBCS.h"
#endif
#include "CTRL.h"
#include "optim.h"

c     == routine arguments ==

      integer nn
      _RL     ff

#ifdef DYNAMIC
      _RL     vv(nn)
#else
      integer nmax
      parameter( nmax = MAX_INDEPEND )
      _RL   vv(nmax)
#endif

      character*(9) dfile
      logical lheaderonly

c     == local variables ==
#ifdef READ_OLD_CTRL_PACK_FILE
      INTEGER maxLocal
      PARAMETER( maxLocal = old_maxcvars )
      INTEGER file_varIndex(maxLocal)
      INTEGER file_varRecs(maxLocal)
      INTEGER file_varNxMax(maxLocal)
      INTEGER file_varNyMax(maxLocal)
      INTEGER file_varNrMax(maxLocal)
      CHARACTER*( 1) file_varGrid(maxLocal)
      CHARACTER*( 5) file_varType(maxLocal)
#endif

CM      integer bi,bj
      integer biG,bjG
      integer i,j
      integer ii,k
      integer ivar
      integer icvrec
      integer icvcomp
      integer icvoffset
      integer nopt
      integer funit

      integer cbuffindex
      real*4 cbuff( sNx*nSx*nPx*sNy*nSy*nPy )

      character*(128) fname
      character*(17)  prefix
      parameter ( prefix =  " OPTIM_READDATA: " )

      integer         ncvarindex_loc
      integer         filei
      integer         filej
      integer         filek
      integer         fileiG
      integer         filejG
      integer         filensx
      integer         filensy
      integer         filenopt
      _RL             fileff

cgg(
      _RL     gg
      integer igg
      integer iobcs
cgg)

c     == end of interface ==

c--   The reference i/o unit.
      funit = 20

c--   Next optimization cycle.
      nopt = optimcycle

      if      ( dfile .eq. ctrlname ) then
       print*
       print*,' OPTIM_READDATA: Reading control vector'
       print*,'                 for optimization cycle: ',nopt
      else if ( dfile .eq. costname ) then
       print*
       print*,' OPTIM_READDATA: Reading cost function and'//
     &                        ' gradient of cost function'
       print*,'                 for optimization cycle: ',nopt
      else
       print*
       print*,' OPTIM_READDATA: subroutine called by a false *dfile*'
       print*,'                 argument. *dfile* = ',dfile
       print*
       stop   '  ...  stopped in OPTIM_READDATA.'
      endif
      if ( lheaderonly ) then
       print*,'                 ... header only'
      endif
      print*, ' '

c--   Read the data.

      bjG = 1 + (myYGlobalLo - 1)/sNy
      biG = 1 + (myXGlobalLo - 1)/sNx

c--   Generate file name and open the file.
      write(fname(1:128),'(4a,i4.4)')
     &     dfile,'_',yctrlid(1:10),'.opt', nopt
      open( funit, file   = fname,
     &     status = 'old',
     &     form   = 'unformatted',
     &     access = 'sequential'   )
      print*, prefix, 'opened file ', fname

c--   Read the header.
      read( funit ) nvartype
      read( funit ) nvarlength
      read( funit ) yctrlid
      read( funit ) filenopt
      read( funit ) fileff
C     filei/jG are dummy values and not used
      read( funit ) fileiG
      read( funit ) filejG
      read( funit ) filensx
      read( funit ) filensy

      read( funit ) (nWetcGlobal(k), k=1,Nr)
      read( funit ) (nWetsGlobal(k), k=1,Nr)
      read( funit ) (nWetwGlobal(k), k=1,Nr)
c#ifdef ALLOW_CTRL_WETV
c     read( funit ) (nWetvGlobal(k), k=1,Nr)
c#endif
#ifdef ALLOW_SHIFWFLX_CONTROL
      read(funit) (nWetiGlobal(k), k=1,Nr)
c     read(funit) nWetiGlobal(1)
#endif

cgg(    Add OBCS Mask information into the header section for optimization.
CML#if (defined (ALLOW_CTRL) && defined (ALLOW_OBCS))
#ifdef ALLOW_OBCSN_CONTROL
      read( funit ) ((nWetobcsnGlo(k,iobcs), k=1,Nr),iobcs= 1,nobcs)
#endif
#ifdef ALLOW_OBCSS_CONTROL
      read( funit ) ((nWetobcssGlo(k,iobcs), k=1,Nr),iobcs= 1,nobcs)
#endif
#ifdef ALLOW_OBCSW_CONTROL
      read( funit ) ((nWetobcswGlo(k,iobcs), k=1,Nr),iobcs= 1,nobcs)
#endif
#ifdef ALLOW_OBCSE_CONTROL
      read( funit ) ((nWetobcseGlo(k,iobcs), k=1,Nr),iobcs= 1,nobcs)
#endif
CML#endif /* ALLOW_CTRL and ALLOW_OBCS */
cgg)
#ifdef READ_OLD_CTRL_PACK_FILE
      DO ivar = 1,maxLocal
        file_varType(ivar)  = '     '
      ENDDO
      read( funit ) (file_varIndex(ivar), ivar=1,maxLocal)
      read( funit ) (file_varRecs(ivar),  ivar=1,maxLocal)
      read( funit ) (file_varNxMax(ivar), ivar=1,maxLocal)
      read( funit ) (file_varNyMax(ivar), ivar=1,maxLocal)
      read( funit ) (file_varNrMax(ivar), ivar=1,maxLocal)
      read( funit ) (file_varGrid(ivar),  ivar=1,maxLocal)
      read( funit )
      CALL CTRL_CONVERT_HEADER(
     I              maxLocal, nvartype, 6,
     U              file_varIndex, file_varRecs,
     U              file_varNxMax, file_varNyMax, file_varNrMax,
     U              file_varGrid, file_varType,
     I              1 )
      DO ivar = 1,nvartype
        ncvarindex(ivar) = file_varIndex(ivar)
        ncvarrecs(ivar)  = file_varRecs(ivar)
        ncvarxmax(ivar)  = file_varNxMax(ivar)
        ncvarymax(ivar)  = file_varNyMax(ivar)
        ncvarnrmax(ivar) = file_varNrMax(ivar)
        ncvargrd(ivar)   = file_varGrid(ivar)
        ncvartype(ivar)  = file_varType(ivar)
      ENDDO
#else /* READ_OLD_CTRL_PACK_FILE */
      read( funit ) (ncvarindex(ivar), ivar=1,nvartype)
      read( funit ) (ncvarrecs(ivar),  ivar=1,nvartype)
      read( funit ) (ncvarxmax(ivar),  ivar=1,nvartype)
      read( funit ) (ncvarymax(ivar),  ivar=1,nvartype)
      read( funit ) (ncvarnrmax(ivar), ivar=1,nvartype)
      read( funit ) (ncvargrd(ivar),   ivar=1,nvartype)
      read( funit ) (ncvartype(ivar),  ivar=1,nvartype)
#endif /* READ_OLD_CTRL_PACK_FILE */

      print *, prefix, 'nvartype ', nvartype
      print *, prefix, 'nvarlength ', nvarlength
      print *, prefix, 'yctrlid ', yctrlid
      print *, prefix, 'filenopt ', filenopt
      print *, prefix, 'fileff ', fileff
      print *, prefix, 'fileiG ', fileiG
      print *, prefix, 'filejG ', filejG
      print *, prefix, 'filensx ', filensx
      print *, prefix, 'filensy ', filensy

      if (lheaderonly) then
       print *, prefix, 'nWetcGlobal ', (nWetcGlobal(k), k=1,Nr)
       print *, prefix, 'nWetsGlobal ', (nWetsGlobal(k), k=1,Nr)
       print *, prefix, 'nWetwGlobal ', (nWetwGlobal(k), k=1,Nr)
c      print *, prefix, 'nWetvGlobal ', (nWetvGlobal(k), k=1,Nr)
#ifdef ALLOW_SHIFWFLX_CONTROL
       print *, prefix, 'nWetiGlobal ', (nWetiGlobal(k), k=1,Nr)
#endif
#ifdef ALLOW_OBCSN_CONTROL
       do iobcs=1,nobcs
        print *, prefix, 'nWetobcsnGlo (iobcs=', iobcs,')',
     &       (nWetobcsnGlo(k,iobcs), k=1,Nr)
       enddo
#endif
#ifdef ALLOW_OBCSS_CONTROL
       do iobcs=1,nobcs
        print *, prefix, 'nWetobcssGlo (iobcs=', iobcs,')',
     &         (nWetobcssGlo(k,iobcs), k=1,Nr)
       enddo
#endif
#ifdef ALLOW_OBCSW_CONTROL
       do iobcs=1,nobcs
        print *, prefix, 'nWetobcswGlo (iobcs=', iobcs,')',
     &       (nWetobcswGlo(k,iobcs), k=1,Nr)
       enddo
#endif
#ifdef ALLOW_OBCSE_CONTROL
       do iobcs=1,nobcs
        print *, prefix, 'nWetobcseGlo (iobcs=', iobcs,')',
     &       (nWetobcseGlo(k,iobcs), k=1,Nr)
       enddo
#endif
       print *, prefix, 'ncvarindex ', (ncvarindex(i), i=1,maxcvars)
       print *, prefix, 'ncvarrecs  ', (ncvarrecs(i),  i=1,maxcvars)
       print *, prefix, 'ncvarxmax  ', (ncvarxmax(i),  i=1,maxcvars)
       print *, prefix, 'ncvarymax  ', (ncvarymax(i),  i=1,maxcvars)
       print *, prefix, 'ncvarnrmax ', (ncvarnrmax(i), i=1,maxcvars)
       print *, prefix, 'ncvargrd   ', (ncvargrd(i), ',',i=1,maxcvars)
       print *, prefix, 'ncvartype  ', (ncvartype(i),',',i=1,maxcvars)
      end if
c--   Check the header information for consistency.

cph      if ( filenopt .ne. nopt ) then
cph       print*
cph       print*,' READ_HEADER: Input data belong to the wrong'
cph       print*,'              optimization cycle.'
cph       print*,'              optimization cycle = ',nopt
cph       print*,'              input optim  cycle = ',filenopt
cph       print*
cph       stop   ' ... stopped in READ_HEADER.'
cph      endif

      if ( (fileiG .ne. biG) .or. (filejG .ne. bjG) ) then
       print*
       print*, prefix, 'Tile indices of loop and data do not match.'
       print*,'                 loop x/y component = ',
     &      biG,bjG
       print*,'                 data x/y component = ',
     &      fileiG,filejG
       print*
       stop   ' ... stopped in OPTIM_READDATA.'
      endif

      if ( (filensx .ne. nSx) .or. (filensy .ne. nSy) ) then
       print*
       print*, prefix, ' Numbers of tiles do not match.'
       print*,'                 parameter x/y no. of tiles = ',
     &      nSx,nSy
       print*,'                 data      x/y no. of tiles = ',
     &      filensx,filensy
       print*
       stop   ' ... stopped in OPTIM_READDATA.'
      endif

ce    Add some more checks. ...

      if (.NOT. lheaderonly) then
c--   Read the data.
       icvoffset = 0
c      do ivar = 1,maxcvars
       do ivar = 1,nvartype
        if ( ncvarindex(ivar) .ne. -1 ) then
         do icvrec = 1,ncvarrecs(ivar)
cph          do bj = 1,nsy
cph           do bi = 1,nsx
            read( funit ) ncvarindex_loc
            read( funit ) filej
            read( funit ) filei
            if ( ncvarindex_loc.NE.ncvarindex(ivar) .AND. icvrec.EQ.1 )
     &       print*, prefix, ' ivar=', ivar, ' , ncvarindex(com,loc)=',
     &               ncvarindex(ivar), ncvarindex_loc
            do k = 1,ncvarnrmax(ivar)
             cbuffindex = 0
             if (ncvargrd(ivar) .eq. 'c') then
              cbuffindex = nWetcGlobal(k)
             else if (ncvargrd(ivar) .eq. 's') then
              cbuffindex = nWetsGlobal(k)
             else if (ncvargrd(ivar) .eq. 'w') then
              cbuffindex = nWetwGlobal(k)
c            else if (ncvargrd(ivar) .eq. 'v') then
c             cbuffindex = nWetvGlobal(k)
#ifdef ALLOW_SHIFWFLX_CONTROL
             else if (ncvargrd(ivar) .eq. 'i') then
              cbuffindex = nWetiGlobal(k)
#endif
#ifdef ALLOW_OBCS_CONTROL
cgg(   O.B. points have the grid mask "m".
             else if (ncvargrd(ivar) .eq. 'm') then
cgg    From "icvrec", calculate what iobcs must be.
              gg   = (icvrec-1)/nobcs
              igg  = int(gg)
              iobcs= icvrec - igg*nobcs
# ifdef ALLOW_OBCSN_CONTROL
              if (ncvarindex(ivar) .eq. 1)
     &             cbuffindex = nWetobcsnGlo(k,iobcs)
# endif
# ifdef ALLOW_OBCSS_CONTROL
              if (ncvarindex(ivar) .eq. 2)
     &             cbuffindex = nWetobcssGlo(k,iobcs)
# endif
# ifdef ALLOW_OBCSE_CONTROL
              if (ncvarindex(ivar) .eq. 3)
     &             cbuffindex = nWetobcseGlo(k,iobcs)
# endif
# ifdef ALLOW_OBCSW_CONTROL
              if (ncvarindex(ivar) .eq. 4)
     &             cbuffindex = nWetobcswGlo(k,iobcs)
# endif
cgg)
#endif /* ALLOW_OBCS_CONTROL */
             endif
             if ( icvoffset + cbuffindex .gt. nvarlength ) then
              print*
              print *, ' ERROR:'
              print *, ' There are at least ', icvoffset+cbuffindex,
     &             ' records in '//fname(1:28)//'.'
              print *, ' This is more than expected from nvarlength =',
     &             nvarlength, '.'
              print *, ' Something is wrong in the computation of '//
     &             'the wet points or'
              print *, ' in computing the number of records in '//
     &             'some variable(s).'
              print *, '  ...  stopped in OPTIM_READDATA.'
              stop     '  ...  stopped in OPTIM_READDATA.'
             endif
             if (cbuffindex .gt. 0) then
              read( funit ) cbuffindex
              read( funit ) filek
              read( funit ) (cbuff(ii), ii=1,cbuffindex)
              do icvcomp = 1,cbuffindex
               vv(icvoffset+icvcomp) = cbuff(icvcomp)
c     If you want to optimize with respect to just O.B. T and S
c     uncomment the next two lines.
c              if (iobcs .eq. 3) vv(icvoffset+icvcomp)=0.
c              if (iobcs .eq. 4) vv(icvoffset+icvcomp)=0.
              enddo
              icvoffset = icvoffset + cbuffindex
             endif
            enddo
cph           enddo
cph          enddo
         enddo
        endif
       enddo

      else

c--   Assign the number of control variables.
       nn = nvarlength

      endif

      close( funit )

c--   Assign the cost function value in case we read the cost file.

      if      ( dfile .eq. ctrlname ) then
       ff = 0. d 0
      else if ( dfile .eq. costname ) then
       ff = fileff
      endif
c--   Always return the cost function value if lheaderonly
      if ( lheaderonly) ff = fileff

      print *, prefix, 'end of optim_readdata'
      print *, ' '

      return
      end

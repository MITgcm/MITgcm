
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

      implicit none

c     == global variables ==

#include "EEPARAMS.h"
#include "SIZE.h"
cgg   Include ECCO_CPPOPTIONS because the ecco_ctrl,cost files 
cgg   have headers with options for OBCS masks.
#include "ECCO_CPPOPTIONS.h"

#include "ctrl.h"
#include "optim.h"
#include "minimization.h"

c     == routine arguments ==

      integer nn
      _RL     ff

#if defined (DYNAMIC)
      _RL     vv(nn)
#elif defined (USE_POINTER) || (MAX_INDEPEND == 0)
      _RL            vv
      pointer (pvv,vv(1))
#else
      integer nmax
      parameter( nmax = MAX_INDEPEND )
      _RL   vv(nmax)
#endif

      character*(9) dfile
      logical lheaderonly

c     == local variables ==

      integer bi,bj
      integer biG,bjG
      integer i,j
      integer ii,k
      integer icvar
      integer icvrec
      integer icvcomp
      integer icvoffset
      integer nopt
      integer funit

      integer cbuffindex
      real*4 cbuff( sNx*nSx*nPx*sNy*nSy*nPy )

      character*(128) fname

c      integer         filei
c      integer         filej
c      integer         filek
c      integer         fileiG
c      integer         filejG
c      integer         filensx
c      integer         filensy
      integer         filenopt
      _RL             fileff

cgg(
      _RL     gg
      integer igg
      integer iobcs
cgg)

c     == end of interface ==

      print *, 'pathei-lsopt in optim_readdata'

c--   The reference i/o unit.
      funit = 20

c--   Next optimization cycle.
      nopt = optimcycle

      if      ( dfile .eq. ctrlname ) then
        print*
        print*,' OPTIM_READDATA: Reading control vector'
        print*,'            for optimization cycle: ',nopt
        print*
      else if ( dfile .eq. costname ) then
        print*
        print*,' OPTIM_READDATA: Reading cost function and'
        print*,'            gradient of cost function'
        print*,'            for optimization cycle: ',nopt
        print*
      else
        print*
        print*,' OPTIM_READDATA: subroutine called by a false *dfile*'
        print*,'            argument. *dfile* = ',dfile
        print*
        stop   '  ...  stopped in OPTIM_READDATA.'
      endif

c--   Read the data.

      bjG = 1 + (myygloballo - 1)/sny
      biG = 1 + (myxgloballo - 1)/snx

c--   Generate file name and open the file.
      write(fname(1:128),'(4a,i4.4)')
     &     dfile,'_',yctrlid(1:10),'.opt', nopt
      open( funit, file   = fname,
     &     status = 'old',
     &     form   = 'unformatted',
     &     access = 'sequential'   )
      print*, 'opened file ', fname

c--   Read the header.
      read( funit ) nvartype
      read( funit ) nvarlength
      read( funit ) yctrlid
      read( funit ) filenopt
      read( funit ) fileff
      read( funit ) fileiG
      read( funit ) filejG
      read( funit ) filensx
      read( funit ) filensy

      read( funit ) (nWetcGlobal(k), k=1,nr)
      read( funit ) (nWetsGlobal(k), k=1,nr)
      read( funit ) (nWetwGlobal(k), k=1,nr)
#ifdef ALLOW_CTRL_WETV
      read( funit ) (nWetvGlobal(k), k=1,nr)
#endif
#ifdef ALLOW_SHIFWFLX_CONTROL
      read(funit) (nWetiGlobal(k), k=1,nr)
c     read(funit) nWetiGlobal(1)
#endif

cgg(    Add OBCS Mask information into the header section for optimization.
#ifdef ALLOW_OBCSN_CONTROL
      read( funit ) ((nWetobcsnGlo(k,iobcs), k=1,nr),iobcs= 1,nobcs)
#endif
#ifdef ALLOW_OBCSS_CONTROL
      read( funit ) ((nWetobcssGlo(k,iobcs), k=1,nr),iobcs= 1,nobcs)
#endif
#ifdef ALLOW_OBCSW_CONTROL
      read( funit ) ((nWetobcswGlo(k,iobcs), k=1,nr),iobcs= 1,nobcs)
#endif
#ifdef ALLOW_OBCSE_CONTROL
      read( funit ) ((nWetobcseGlo(k,iobcs), k=1,nr),iobcs= 1,nobcs)
#endif
cgg)
      read( funit ) (ncvarindex(i), i=1,maxcvars)
      read( funit ) (ncvarrecs(i),  i=1,maxcvars)
      read( funit ) (ncvarxmax(i),  i=1,maxcvars)
      read( funit ) (ncvarymax(i),  i=1,maxcvars)
      read( funit ) (ncvarnrmax(i), i=1,maxcvars)
      read( funit ) (ncvargrd(i),   i=1,maxcvars)
      read( funit )

cph(
cph      if (lheaderonly) then
         print *, 'pathei: nvartype ', nvartype
         print *, 'pathei: nvarlength ', nvarlength
         print *, 'pathei: yctrlid ', yctrlid
         print *, 'pathei: filenopt ', filenopt
         print *, 'pathei: fileff ', fileff
         print *, 'pathei: fileiG ', fileiG
         print *, 'pathei: filejG ', filejG
         print *, 'pathei: filensx ', filensx
         print *, 'pathei: filensy ', filensy
         
         print *, 'pathei: nWetcGlobal ', 
     &        (nWetcGlobal(k),  k=1,nr)
         print *, 'pathei: nWetsGlobal ', 
     &        (nWetsGlobal(k),  k=1,nr)
         print *, 'pathei: nWetwGlobal ', 
     &        (nWetwGlobal(k),  k=1,nr)
         print *, 'pathei: nWetvGlobal ',
     &        (nWetvGlobal(k),  k=1,nr)
#ifdef ALLOW_SHIFWFLX_CONTROL
         print *, 'pathei: nWetiGlobal ',
     &        (nWetiGlobal(k), k=1,nr)
#endif
#ifdef ALLOW_OBCSN_CONTROL
         do iobcs=1,nobcs
          print *, 'pathei: nWetobcsnGlo (iobcs=', iobcs,')',
     &         (nWetobcsnGlo(k,iobcs), k=1,nr)
         enddo
#endif
#ifdef ALLOW_OBCSS_CONTROL
         do iobcs=1,nobcs
          print *, 'pathei: nWetobcssGlo (iobcs=', iobcs,')',
     &         (nWetobcssGlo(k,iobcs), k=1,nr)
         enddo
#endif
#ifdef ALLOW_OBCSW_CONTROL
         do iobcs=1,nobcs
          print *, 'pathei: nWetobcswGlo (iobcs=', iobcs,')',
     &         (nWetobcswGlo(k,iobcs), k=1,nr)
         enddo
#endif
#ifdef ALLOW_OBCSE_CONTROL
         do iobcs=1,nobcs
          print *, 'pathei: nWetobcseGlo (iobcs=', iobcs,')',
     &         (nWetobcseGlo(k,iobcs), k=1,nr)
         enddo
#endif
         print *, 'pathei: ncvarindex ', 
     &        (ncvarindex(i), i=1,maxcvars)
         print *, 'pathei: ncvarrecs ', 
     &        (ncvarrecs(i),  i=1,maxcvars)
         print *, 'pathei: ncvarxmax ', 
     &        (ncvarxmax(i),  i=1,maxcvars)
         print *, 'pathei: ncvarymax ', 
     &        (ncvarymax(i),  i=1,maxcvars)
         print *, 'pathei: ncvarnrmax ', 
     &        (ncvarnrmax(i), i=1,maxcvars)
         print *, 'pathei: ncvargrd ', 
     &        (ncvargrd(i),   i=1,maxcvars)
cph      end if
cph)
c--   Check the header information for consistency.

cph      if ( filenopt .ne. nopt ) then
cph         print*
cph         print*,' READ_HEADER: Input data belong to the wrong'
cph         print*,'              optimization cycle.'
cph         print*,'              optimization cycle = ',nopt
cph         print*,'              input optim  cycle = ',filenopt
cph         print*
cph         stop   ' ... stopped in READ_HEADER.'
cph      endif
      
      if ( (fileiG .ne. biG) .or. (filejG .ne. bjG) ) then
         print*
         print*,' READ_HEADER: Tile indices of loop and data '
         print*,'              do not match.'
         print*,'              loop x/y component = ',
     &        biG,bjG
         print*,'              data x/y component = ',
     &        fileiG,filejG
         print*
         stop   ' ... stopped in READ_HEADER.'
      endif
      
      if ( (filensx .ne. nsx) .or. (filensy .ne. nsy) ) then
         print*
         print*,' READ_HEADER: Numbers of tiles do not match.'
         print*,'              parameter x/y no. of tiles = ',
     &        bi,bj
         print*,'              data      x/y no. of tiles = ',
     &        filensx,filensy
         print*
         stop   ' ... stopped in READ_HEADER.'
      endif

ce    Add some more checks. ...

      if (.NOT. lheaderonly) then
c--   Read the data.
       icvoffset = 0
       do icvar = 1,maxcvars
        if ( ncvarindex(icvar) .ne. -1 ) then
         do icvrec = 1,ncvarrecs(icvar)
          do bj = 1,nsy
           do bi = 1,nsx
            read( funit ) ncvarindex(icvar)
            read( funit ) filej
            read( funit ) filei
            do k = 1,ncvarnrmax(icvar)
             cbuffindex = 0
             if (ncvargrd(icvar) .eq. 'c') then
              cbuffindex = nWetcGlobal(k)
             else if (ncvargrd(icvar) .eq. 's') then
              cbuffindex = nWetsGlobal(k)
             else if (ncvargrd(icvar) .eq. 'w') then
              cbuffindex = nWetwGlobal(k)
             else if (ncvargrd(icvar) .eq. 'v') then
              cbuffindex = nWetvGlobal(k)
#ifdef ALLOW_SHIFWFLX_CONTROL
             else if (ncvargrd(icvar) .eq. 'i') then
              cbuffindex = nWetiGlobal(k)
#endif
cgg(   O.B. points have the grid mask "m".
             else if (ncvargrd(icvar) .eq. 'm') then
cgg    From "icvrec", calculate what iobcs must be.
              gg   = (icvrec-1)/nobcs
              igg  = int(gg)
              iobcs= icvrec - igg*nobcs
#ifdef ALLOW_OBCSN_CONTROL
              if (icvar .eq. 11) then                    
               cbuffindex = nWetobcsnGlo(k,iobcs)
              endif
#endif
#ifdef ALLOW_OBCSS_CONTROL
              if (icvar .eq. 12) then
               cbuffindex = nWetobcssGlo(k,iobcs)
              endif
#endif 
#ifdef ALLOW_OBCSW_CONTROL
              if (icvar .eq. 13) then
               cbuffindex = nWetobcswGlo(k,iobcs)
              endif
#endif
#ifdef ALLOW_OBCSE_CONTROL
              if (icvar .eq. 14) then
               cbuffindex = nWetobcseGlo(k,iobcs)
              endif
#endif
cgg)
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
           enddo
          enddo
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

      return
      end

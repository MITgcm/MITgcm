
      subroutine optim_writedata(
     I                       nn,
     I                       dfile,
     I                       lheaderonly,
     I                       ff,
     I                       vv
     &                     ) 

c     ==================================================================
c     SUBROUTINE optim_writedata
c     ==================================================================
c
c     o Writes the latest update of the control vector to file(s). These
c       files can then be used by the MITgcmUV state estimation setup
c       for the next forward/adjoint simluation.
c
c     started: Christian Eckert eckert@mit.edu 12-Apr-2000
c
c     changed:  Patrick Heimbach heimbach@mit.edu 19-Jun-2000
c               - finished, revised and debugged
c
c     ==================================================================
c     SUBROUTINE optim_writedata
c     ==================================================================

      implicit none

c     == global variables ==

#include "EEPARAMS.h"
#include "SIZE.h"
cgg   Include ECCO_CPPOPTIONS because the ecco_ctrl,cost files have headers with
cgg   options for OBCS masks. 
#include "ECCO_CPPOPTIONS.h"

#include "ctrl.h"
#include "optim.h"
#include "minimization.h"

c     == routine arguments ==

      integer nn
      _RL     ff
      _RL     vv(nn)

      character*(9) dfile
      logical lheaderonly

c     == local variables ==

      integer i,j,k
      integer ii
      integer bi,bj
      integer biG,bjG
      integer nopt
      integer icvcomp
      integer icvoffset
      integer icvrec
      integer icvar
      integer funit
      integer cbuffindex

      real*4 cbuff( sNx*nSx*nPx*sNy*nSy*nPy )

      character*(128) fname
cgg(
      _RL     gg
      integer igg
      integer iobcs
cgg)

c     == end of interface ==

c--   I/O unit to use.
      funit = 20

c--   Next optimization cycle.
      nopt = optimcycle + 1

      if ( dfile .eq. ctrlname ) then
        print*
        print*,' OPTIM_WRITEDATA: Writing new control vector to file(s)'
        print*,'             for optimization cycle: ',nopt
        print*
      else
        print*
        print*,' OPTIM_WRITEDATA: subroutine called by a false *dfile*'
        print*,'             argument. *dfile* = ',dfile
        print*
        stop   '  ...  stopped in OPTIM_WRITEDATA.'
      endif

      bjG = 1 + (myygloballo - 1)/sny
      biG = 1 + (myxgloballo - 1)/snx

c--         Generate file name and open the file.
      write(fname(1:128),'(4a,i4.4)')
     &     dfile,'_',yctrlid(1:10),'.opt', nopt
      open( funit, file   = fname,
     &     status = 'new',
     &     form   = 'unformatted',
     &     access = 'sequential'   )

cph(
         print *, 'pathei: nvartype ', nvartype
         print *, 'pathei: nvarlength ', nvarlength
         print *, 'pathei: yctrlid ', yctrlid
         print *, 'pathei: nopt ', nopt
         print *, 'pathei: ff ', ff
         print *, 'pathei: iG ', biG
         print *, 'pathei: jG ', bjG
         print *, 'pathei: nsx ', nsx
         print *, 'pathei: nsy ', nsy
         
         print *, 'pathei: nWetcGlobal ', 
     &        (nWetcGlobal(k), k=1,nr)
         print *, 'pathei: nWetsGlobal ', 
     &        (nWetsGlobal(k), k=1,nr)
         print *, 'pathei: nWetwGlobal ', 
     &        (nWetwGlobal(k), k=1,nr)
         print *, 'pathei: nWetvGlobal ', 
     &        (nWetvGlobal(k), k=1,nr)
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
cph)

c--   Write the header.
      write( funit ) nvartype
      write( funit ) nvarlength
      write( funit ) yctrlid
      write( funit ) optimcycle
      write( funit ) ff
      write( funit ) big
      write( funit ) bjg
      write( funit ) nsx
      write( funit ) nsy
      write( funit ) (nWetcGlobal(k), k=1,nr)
      write( funit ) (nWetsGlobal(k), k=1,nr)
      write( funit ) (nWetwGlobal(k), k=1,nr)
#ifdef ALLOW_CTRL_WETV
      write( funit ) (nWetvGlobal(k), k=1,nr)
#endif
#ifdef ALLOW_SHIFWFLX_CONTROL
      write(funit) (nWetiGlobal(k),   k=1,nr)
c     write(funit) nWetiGlobal(1)
#endif

cgg(    Add OBCS Mask information into the header section for optimization.
#ifdef ALLOW_OBCSN_CONTROL
          write(funit) ((nWetobcsnGlo(k,iobcs), k=1,nr),iobcs= 1,nobcs)
#endif
#ifdef ALLOW_OBCSS_CONTROL
          write(funit) ((nWetobcssGlo(k,iobcs), k=1,nr),iobcs= 1,nobcs)
#endif
#ifdef ALLOW_OBCSW_CONTROL
          write(funit) ((nWetobcswGlo(k,iobcs), k=1,nr),iobcs= 1,nobcs)
#endif
#ifdef ALLOW_OBCSE_CONTROL
          write(funit) ((nWetobcseGlo(k,iobcs), k=1,nr),iobcs= 1,nobcs)
#endif
cgg)

      write( funit ) (ncvarindex(i), i=1,maxcvars)
      write( funit ) (ncvarrecs(i),  i=1,maxcvars)
      write( funit ) (ncvarxmax(i),  i=1,maxcvars)
      write( funit ) (ncvarymax(i),  i=1,maxcvars)
      write( funit ) (ncvarnrmax(i), i=1,maxcvars)
      write( funit ) (ncvargrd(i),   i=1,maxcvars)
      write( funit )

c--         Write the data.
      icvoffset = 0
      do icvar = 1,maxcvars
         if ( ncvarindex(icvar) .ne. -1 ) then
            do icvrec = 1,ncvarrecs(icvar)
               do bj = 1,nsy
                  do bi = 1,nsx
                     write( funit ) ncvarindex(icvar)
                     write( funit ) bj
                     write( funit ) bi
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
                        endif
cgg)
                        if (cbuffindex .gt. 0) then
                           do icvcomp = 1,cbuffindex
                              cbuff(icvcomp) = vv(icvoffset + icvcomp)
c     If you want to optimize with respect to just O.B. T and S
c     uncomment the next two lines.
c                              if (iobcs .eq. 3) cbuff(icvcomp)=0.
c                              if (iobcs .eq. 4) cbuff(icvcomp)=0.
                           enddo
                           write( funit ) cbuffindex
                           write( funit ) k
                           write( funit ) (cbuff(ii), ii=1,cbuffindex)
                           icvoffset = icvoffset + cbuffindex
                        endif
                     enddo
                  enddo
               enddo
            enddo
         endif
      enddo

      close( funit )
cph(
      print *,'in owd: icvoffset', icvoffset
cph)

      return
      end





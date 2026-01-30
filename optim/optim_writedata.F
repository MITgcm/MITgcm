C     ECCO_CPPOPTIONS used to affect maxcvars and defined ALLOW_OBCS?_CONTROL
C#include "ECCO_CPPOPTIONS.h"
C     now:
C     CTRL_OPTIONS affects maxcvars and may define ALLOW_OBCS?_CONTROL
#include "CTRL_OPTIONS.h"

      subroutine optim_writedata(
     I                       nn,
     I                       dfile,
     I                       printlists,
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
      _RL     vv(nn)

      character*(9) dfile
      logical printlists

c     == local variables ==

      integer i,j,k
      integer ii
CML      integer bi,bj
      integer biG,bjG
      integer nopt
      integer icvcomp
      integer icvoffset
      integer icvrec
      integer ivar
      integer funit
      integer cbuffindex

      real*4 cbuff( sNx*nSx*nPx*sNy*nSy*nPy )

      character*(128) fname
      character*(18)  prefix
      parameter ( prefix =  " OPTIM_WRITEDATA: " )
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
       print*,'                  for optimization cycle: ',nopt
       print*
      else
       print*
       print*,' OPTIM_WRITEDATA: subroutine called by a false *dfile*'
       print*,'                  argument. *dfile* = ',dfile
       print*
       stop   '  ...  stopped in OPTIM_WRITEDATA.'
      endif

      bjG = 1 + (myYGlobalLo - 1)/sNy
      biG = 1 + (myXGlobalLo - 1)/sNx

c--         Generate file name and open the file.
      write(fname(1:128),'(4a,i4.4)')
     &     dfile,'_',yctrlid(1:10),'.opt', nopt
      open( funit, file   = fname,
     &     status = 'new',
     &     form   = 'unformatted',
     &     access = 'sequential'   )

      print *, prefix, 'nvartype   ', nvartype
      print *, prefix, 'nvarlength ', nvarlength
      print *, prefix, 'yctrlid    ', yctrlid
      print *, prefix, 'nopt       ', nopt
      print *, prefix, 'ff         ', ff
      print *, prefix, 'iG         ', biG
      print *, prefix, 'jG         ', bjG
      print *, prefix, 'nSx        ', nSx
      print *, prefix, 'nSy        ', nSy

      if ( printlists ) then
       print *, prefix, 'nWetcGlobal ', (nWetcGlobal(k), k=1,Nr)
       print *, prefix, 'nWetsGlobal ', (nWetsGlobal(k), k=1,Nr)
       print *, prefix, 'nWetwGlobal ', (nWetwGlobal(k), k=1,Nr)
c      print *, prefix, 'nWetvGlobal ', (nWetvGlobal(k), k=1,Nr)
       print *, prefix, 'ncvarindex ',  (ncvarindex(i), i=1,maxcvars)
       print *, prefix, 'ncvarrecs ',   (ncvarrecs(i),  i=1,maxcvars)
       print *, prefix, 'ncvarxmax ',   (ncvarxmax(i),  i=1,maxcvars)
       print *, prefix, 'ncvarymax ',   (ncvarymax(i),  i=1,maxcvars)
       print *, prefix, 'ncvarnrmax ',  (ncvarnrmax(i), i=1,maxcvars)
       print *, prefix, 'ncvargrd ',    (ncvargrd(i),   i=1,maxcvars)
      endif

c--   Write the header.
      write( funit ) nvartype
      write( funit ) nvarlength
      write( funit ) yctrlid
      write( funit ) optimcycle
      write( funit ) ff
      write( funit ) big
      write( funit ) bjg
      write( funit ) nSx
      write( funit ) nSy
      write( funit ) (nWetcGlobal(k), k=1,Nr)
      write( funit ) (nWetsGlobal(k), k=1,Nr)
      write( funit ) (nWetwGlobal(k), k=1,Nr)
c#ifdef ALLOW_CTRL_WETV
c     write( funit ) (nWetvGlobal(k), k=1,Nr)
c#endif
#ifdef ALLOW_SHELFICE
      write(funit) (nWetiGlobal(k),   k=1,Nr)
c     write(funit) nWetiGlobal(1)
#endif

c     Add OBCS Mask information into the header section for optimization.
#ifdef ALLOW_OBCSN_CONTROL
      write(funit) ((nWetobcsnGlo(k,iobcs), k=1,Nr),iobcs= 1,nobcs)
#endif
#ifdef ALLOW_OBCSS_CONTROL
      write(funit) ((nWetobcssGlo(k,iobcs), k=1,Nr),iobcs= 1,nobcs)
#endif
#ifdef ALLOW_OBCSW_CONTROL
      write(funit) ((nWetobcswGlo(k,iobcs), k=1,Nr),iobcs= 1,nobcs)
#endif
#ifdef ALLOW_OBCSE_CONTROL
      write(funit) ((nWetobcseGlo(k,iobcs), k=1,Nr),iobcs= 1,nobcs)
#endif

      write( funit ) (ncvarindex(ivar), ivar=1,nvartype)
      write( funit ) (ncvarrecs(ivar),  ivar=1,nvartype)
      write( funit ) (ncvarxmax(ivar),  ivar=1,nvartype)
      write( funit ) (ncvarymax(ivar),  ivar=1,nvartype)
      write( funit ) (ncvarnrmax(ivar), ivar=1,nvartype)
      write( funit ) (ncvargrd(ivar),   ivar=1,nvartype)
      write( funit ) (ncvartype(ivar),  ivar=1,nvartype)

c--         Write the data.
      icvoffset = 0
c     do ivar = 1,maxcvars
      do ivar = 1,nvartype
       if ( ncvarindex(ivar) .ne. -1 ) then
        do icvrec = 1,ncvarrecs(ivar)
cph         do bj = 1,nsy
cph          do bi = 1,nsx
         write( funit ) ncvarindex(ivar)
CML         write( funit ) bi
CML         write( funit ) bj
         write( funit ) nSx
         write( funit ) nSy
         do k = 1,ncvarnrmax(ivar)
          cbuffindex = 0
          if (ncvargrd(ivar) .eq. 'c') then
           cbuffindex = nWetcGlobal(k)
          else if (ncvargrd(ivar) .eq. 's') then
           cbuffindex = nWetsGlobal(k)
          else if (ncvargrd(ivar) .eq. 'w') then
           cbuffindex = nWetwGlobal(k)
c         else if (ncvargrd(ivar) .eq. 'v') then
c          cbuffindex = nWetvGlobal(k)
#ifdef ALLOW_SHELFICE
          else if (ncvargrd(ivar) .eq. 'i') then
           cbuffindex = nWetiGlobal(k)
#endif
#ifdef ALLOW_OBCS_CONTROL
c     O.B. points have the grid mask "m".
          else if (ncvargrd(ivar) .eq. 'm') then
c     From "icvrec", calculate what iobcs must be.
           gg   = (icvrec-1)/nobcs
           igg  = int(gg)
           iobcs= icvrec - igg*nobcs
# ifdef ALLOW_OBCSN_CONTROL
           if (ncvarindex(ivar).eq.1) cbuffindex=nWetobcsnGlo(k,iobcs)
# endif
# ifdef ALLOW_OBCSS_CONTROL
           if (ncvarindex(ivar).eq.2) cbuffindex=nWetobcssGlo(k,iobcs)
# endif
# ifdef ALLOW_OBCSE_CONTROL
           if (ncvarindex(ivar).eq.3) cbuffindex=nWetobcseGlo(k,iobcs)
# endif
# ifdef ALLOW_OBCSW_CONTROL
           if (ncvarindex(ivar).eq.4) cbuffindex=nWetobcswGlo(k,iobcs)
# endif
#endif /* ALLOW_OBCS_CONTROL */
          endif
          if (cbuffindex .gt. 0) then
           do icvcomp = 1,cbuffindex
            cbuff(icvcomp) = vv(icvoffset + icvcomp)
c     If you want to optimize with respect to just O.B. T and S
c     uncomment the next two lines.
c           if (iobcs .eq. 3) cbuff(icvcomp)=0.
c           if (iobcs .eq. 4) cbuff(icvcomp)=0.
           enddo
           write( funit ) cbuffindex
           write( funit ) k
           write( funit ) (cbuff(ii), ii=1,cbuffindex)
           icvoffset = icvoffset + cbuffindex
          endif
         enddo
cph                  enddo
cph               enddo
        enddo
       endif
      enddo

      close( funit )

      print *, prefix, 'end of optim_writedata, icvoffset ', icvoffset
      print *, ' '

      return
      end

      subroutine diag_readparms (myThid)
C***********************************************************************
C  Purpose
C  -------
C     Read Diagnostics Namelists to specify output sequence
C
C  Arguments  Description
C  ----------------------
C     myThid ... Process(or) number
C***********************************************************************
      implicit none
      integer myThid

#include "diagnostics_SIZE.h"
#include "diagnostics.h"
#include "EEPARAMS.h"

c Declare Local Variables
c -----------------------
      character*(MAX_LEN_MBUF) msgBuf
      integer ku
      integer l,n,m,ndim
      _RL undef
      parameter ( ndim = 100 )       ! Max Number of Levels,Fields,Lists
      integer nlevel                 ! Total   Number of Output Levels
      integer nfield                 ! Total   Number of Output Fields
      _RL getcon
      integer frequency(ndim)        ! Frequency of Output (hhmmss)
      _RL levels (ndim,ndim)         ! List Output Levels
      character*8 fields(ndim,ndim)  ! List Output Fields
      character*80 filename(ndim)    ! List Output Filename
      character*8 blank

      namelist / diagnostics_list / frequency,levels,fields,filename

C **********************************************************************
C ****           Initialize and Read Diagnostics Namelist           ****
C **********************************************************************
      _BEGIN_MASTER(myThid)

      undef = getcon('UNDEF')
      blank  = '        '

      do n = 1,ndim
      frequency(n) = 0
      do m = 1,ndim
      levels (n,m) = undef
      fields (n,m) = blank
      enddo
      enddo

      WRITE(msgBuf,'(A)') ' DIAG_READPARMS: opening data.diagnostics'
      CALL PRINT_MESSAGE( msgBuf, standardMessageUnit,SQUEEZE_RIGHT,1)
                                                                                        
      CALL OPEN_COPY_DATA_FILE('data.diagnostics', 'DIAG_READPARMS',
     .                          ku,myThid )
      read  (ku,NML=diagnostics_list)
      close (ku)

C **********************************************************************
C ****         Fill Diagnostics Common Block with Namelist Info     ****
C **********************************************************************

      nlists = 0
      do n = 1,numlists
        if( freq(n).ne.0 ) then
           nlists = nlists + 1
           freq(n)   = frequency(n)
           fnames(n)    = filename (n)
           nlevels(n) = 0
           nfields(n) = 0
          do m=1,ndim
          if( levels(m,n).ne.undef ) nlevels(n) = nlevels(n) + 1
          if( fields(m,n).ne.blank ) nfields(n) = nfields(n) + 1
          enddo
          do m=1,nlevels(n)
          levs(m,n) = levels(m,n)
          enddo
          do m=1,nfields(n)
          flds(m,n) = fields(m,n)
          enddo
  
        endif
      enddo

C **********************************************************************
C ****               Echo History List Data Structure               ****
C **********************************************************************

      do n=1,nlists
         print *, 'Creating Output Stream: ',fnames(n)
         print *, 'Frequency: ',freq(n)
         print *, 'Levels:    ',(levs(n,l),l=1,nlevels(n))
         print *, 'Fields:    ',(flds(n,l),l=1,nfields(n))
         print *
      enddo

      _END_MASTER(myThid)

      return
      end

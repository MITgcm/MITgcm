      subroutine diag_readparms (myThid,ku,nlist)
C***********************************************************************
C  Purpose
C  -------
C     Read Diagnostics Namelists to specify output sequence
C
C  Arguments  Description
C  ----------------------
C     ku ....... Namelist Unit Number
C     list ..... History  List Data Structure
C     nlist .... Number of Distinct Lists
C
C***********************************************************************
      implicit none
      integer myThid,ku,nlist

c Declare Local Variables
c -----------------------
      integer l,n,m,ndim
      real    undef
      parameter ( ndim = 100 )   ! Maximum Number of Levels,Fields,Lists
      character*120     command
      integer           nlevel   ! Total   Number of Output Levels
      integer           nfield   ! Total   Number of Output Fields
      real              getcon
      integer           frequency   (ndim)          ! Frequency of Output (hhmmss)
      real              levels (ndim,ndim)          ! List Output Levels
      character*8       fields (ndim,ndim)          ! List Output Fields
      character*80      filename    (ndim)          ! List Output Filename
      character*8       blank

      namelist / diagnostics_list / frequency,levels,fields,filename

C **********************************************************************
C ****           Initialize and Read Diagnostics Namelist           ****
C **********************************************************************
      undef = getcon('UNDEF')
      blank  = '        '

      do n = 1,ndim
      frequency(n) = 0
      do m = 1,ndim
      levels (n,m) = undef
      fields (n,m) = blank
      enddo
      enddo

      rewind ku
      read  (ku,diagnostics_list)

C **********************************************************************
C ****         Fill Diagnostics Common Block with Namelist Info     ****
C **********************************************************************

      nlist = 0
      do n = 1,ndim
        if( frequency(n).ne.0 ) then
           list(n)%frequency   = frequency(n)
           list(n)%filename    = filename (n)

                                 nlevel = 0
                                 nfield = 0
          do m=1,ndim
          if( levels(m,n).ne.undef ) nlevel = nlevel + 1
          if( fields(m,n).ne.blank ) nfield = nfield + 1
          enddo
                          list(n)%nlevel = nlevel
                          list(n)%nfield = nfield

          do m=1,nlevel
          list(n)%levels(m) = levels(m,n)
          enddo

          do m=1,nfield
          list(n)%fields(m) = fields(m,n)
          enddo
  
        endif
      enddo

C **********************************************************************
C ****               Echo History List Data Structure               ****
C **********************************************************************

      if( myid.eq.0 ) then
      do n=1,nlist
         print *, 'Creating Output Stream: ',list(n)%filename
         print *, 'Frequency: ',list(n)%frequency
         print *, 'Levels:    ',(list(n)%levels(l),l=1,list(n)%nlevel)
         print *, 'Fields:    ',(list(n)%fields(l),l=1,list(n)%nfield)
         print *
      enddo
      endif

      return
      end

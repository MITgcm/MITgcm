C $Header: /u/gcmpack/MITgcm/eesupp/src/Attic/nml_filter.F,v 1.2 2001/02/04 14:38:44 cnh Exp $
C $Name:  $

#include "CPP_OPTIONS.h"

#define FTN_NML_F90

      subroutine nml_filter(
     I                       fname
     O                     , outunit
     I                     , myThid
     &                    )

c     ==================================================================
c     SUBROUTINE nml_filter
c     ==================================================================
c
c     o 
c
c     started: Ralf.Giering@FastOpt.de 15-Mai-2000
c
c              - remove comments from namelist file
c              - usage
c
c          call nml_filter( 'datafile', iunit, myThid )
c          read  ( unit = iunit, nml = the_namelist )
c          close ( iunit )
c
c     ==================================================================
c     SUBROUTINE nml_filter
c     ==================================================================

      implicit none

c     == global variables ==

#include "EEPARAMS.h"

c     == routine arguments ==
      character*(*) fname
      integer outunit
      integer myThid

c     == local variables ==
      integer errio
      integer il
      integer inunit

      character*(MAX_LEN_MBUF) msgBuf
      character*(MAX_LEN_PREC) record

c     == external ==

      integer  ilnblnk
      external ilnblnk

c     == end of interface ==

c--  open the data file

      call mdsfindunit(  inunit, mythid )

      open( unit   = inunit
     &    , file   = fname
     &    , status = 'old'
     &    , iostat = errio
     &    )

c--   open the filtered data file
      call mdsfindunit( outunit, mythid )
      open( unit=outunit, status='scratch' )

      if ( errio .lt. 0 ) then
         write(msgBuf,'(A)')  'S/R nml_filter'
         call PRINT_ERROR( msgBuf , 1)
         write(msgBuf,'(A)')  'Unable to open execution environment'
         call PRINT_ERROR( msgBuf , 1)
         write(msgBuf,'(3a)')  'namelist file "', fname, '"'
         call PRINT_ERROR( msgBuf , 1)
         close(outunit)
         outunit = 0
         stop ' stopped in nml_filter'
      endif

      do while ( .true. )
         read(inunit, fmt='(a)', iostat=errio) record
         if  ( errio .ne. 0 ) then
            goto 1001
         end if
         il = max(ilnblnk(record),1)
         if      ( record(1:1) .eq. commentcharacter ) then
         else if ( record(1:1) .eq. '/' ) then
#ifdef FTN_NML_F90
            write(outunit, fmt='(a)') record(:il)
#else
            write(outunit, fmt='(a)') ' &'
#endif
         else if ( record(1:2) .eq. ' /' ) then
#ifdef FTN_NML_F90
            write(outunit, fmt='(a)') record(:il)
#else
            write(outunit, fmt='(a)') ' &'
#endif
         else
            write(outunit, fmt='(a)') record(:il)
         end if
      enddo
 1001 continue
      close( inunit )

      rewind( outunit )

      end


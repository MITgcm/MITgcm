C $Header: /u/gcmpack/MITgcm/eesupp/src/Attic/nml_filter.F,v 1.8 2008/04/16 20:48:06 heimbach Exp $
C $Name:  $

#include "CPP_OPTIONS.h"

#define FTN_NML_F90

CBOP

C     !ROUTINE: NML_FILTER

C     !INTERFACE:
      SUBROUTINE NML_FILTER(
     I                       fName
     O                     , outUnit
     I                     , myThid
     &                    )
      IMPLICIT NONE

C     !DESCRIPTION:
C     *=================================================================*
C     | SUBROUTINE NML\_FILTER
C     | o Remove comments from namelist.
C     *=================================================================*
C     |
C     | Started: Ralf.Giering@FastOpt.de 15-Mai-2000
C     |
C     |        - remove comments from namelist file
c     |        - usage
C     |
C     |    CALL NML\_FILTER( 'datafile', iUnit, myThid )
C     |    READ  ( UNIT = iunit, NML = the\_namelist )
C     |    CLOSE ( iUnit )
C     |
C     *=================================================================*

C     !USES:
C     == Global variables ==
#include "EEPARAMS.h"
      INTEGER  ILNBLNK
      EXTERNAL ILNBLNK

C     !INPUT/OUTPUT PARAMETERS:
C     == Routine arguments ==
      CHARACTER*(*) fName
      INTEGER outUnit
      INTEGER myThid

C     !LOCAL VARIABLES:
C     == Local variables ==
      INTEGER errIo
      INTEGER il
      INTEGER inUnit
      CHARACTER*(MAX_LEN_MBUF) msgBuf
      CHARACTER*(MAX_LEN_PREC) record
CEOP

C--   Open the data file

      CALL mdsFindUnit(  inunit, mythid )

      open( unit   = inunit
     &    , file   = fname
     &    , status = 'old'
     &    , iostat = errio
     &    )

c--   open the filtered data file
      call mdsfindunit( outunit, mythid )
#if defined (TARGET_BGL) || defined (TARGET_CRAYXT)
      open( unit=outunit, file='scratch3', status='unknown' )
#else
      open( unit=outunit, status='scratch' )
#endif

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
      else
         write(msgBuf,'(3a)') 'Processing namelist file ', fname, ' ...'
         call PRINT_MESSAGE(msgBuf,standardMessageUnit,
     &                      SQUEEZE_RIGHT,myThid)
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
            CALL NML_SET_TERMINATOR( RECORD )
            write(outunit, fmt='(a)') record(:il)
         end if
      enddo
 1001 continue
      close( inunit )

      rewind( outunit )

      end


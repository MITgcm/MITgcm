C $Header:

#include "SBO_OPTIONS.h"

#undef  SAFE_IO

#ifdef SAFE_IO
#define _NEW_STATUS 'new'
#else
#define _NEW_STATUS 'unknown'
#endif

#ifdef ALLOW_AUTODIFF_TAMC
#define _OLD_STATUS 'unknown'
#else
#define _OLD_STATUS 'old'
#endif

      SUBROUTINE SBO_WRITEVECTOR(
     I   fName,
     I   narr,
     I   arr,
     I   irecord,
     I   myIter,
     I   myThid )
C     /==========================================================\
C     | SUBROUTINE SBO_WRITEVECTOR                               |
C     | o Routine to write a vector to a direct access file.     |
C     |==========================================================|
C     | This is a rewrite of MDSWRITEVECTOR that outputs a       |
C     | single Real*8 vector from the master process and thread. |
C     \==========================================================/
      IMPLICIT NONE

C     === Global variables ===
#include "SIZE.h"
#include "EEPARAMS.h"
#include "PARAMS.h"

C     == Routine arguments ==
C
C fName		string	base name for file to written
C narr		integer	size of vector dimension
C arr		Real array to write, arr(narr)
C irecord	integer	record number to read
C myIter	integer	time step number
C myThid	integer thread identifier

C Routine arguments
      character*(*) fName
      integer narr
      Real*8  arr(narr)
      integer irecord
      integer myIter
      integer myThid

#ifdef ALLOW_SBO

C Functions
      integer ILNBLNK
      integer MDS_RECLEN

C Local variables
C filePrec	integer	number of bits per word in file (64)
      integer filePrec / 64 /
      character*(80) dataFName,metaFName
      integer dUnit,IL
      logical fileIsOpen
      integer dimList(3,3),ndims
      integer length_of_rec
      character*(max_len_mbuf) msgbuf

C Only do I/O if I am the master process
      IF( myProcId .EQ. 0 ) THEN

C Only do I/O if I am the master thread
         _BEGIN_MASTER( myThid )

C Record number must be >= 1
         if (irecord .LT. 1) then
            write(msgbuf,'(a,i9.8)')
     &           ' SBO_WRITEVECTOR: argument irecord = ',irecord
            call print_message( msgbuf, standardmessageunit,
     &           SQUEEZE_RIGHT , mythid)
            write(msgbuf,'(a)')
     &           ' SBO_WRITEVECTOR: invalid value for irecord'
            call print_error( msgbuf, mythid )
            stop 'ABNORMAL END: S/R SBO_WRITEVECTOR'
         endif

C Assume nothing
         fileIsOpen = .FALSE.
         IL=ILNBLNK( fName )

C Assign a free unit number as the I/O channel for this routine
         call MDSFINDUNIT( dUnit, mythid )

C Open file
         write(dataFname(1:80),'(2a)') fName(1:IL),'.data'
         if (irecord .EQ. 1) then
            length_of_rec = MDS_RECLEN( filePrec, narr, mythid )
            open( dUnit, file=dataFName, status=_NEW_STATUS,
     &           access='direct', recl=length_of_rec )
            fileIsOpen=.TRUE.
         else
            length_of_rec = MDS_RECLEN( filePrec, narr, mythid )
            open( dUnit, file=dataFName, status=_OLD_STATUS,
     &           access='direct', recl=length_of_rec )
            fileIsOpen=.TRUE.
         endif

C Write record
         if (fileIsOpen) then
            write(msgbuf,'(a,i9.8,2x,i9.8)')
     &           ' SBO_WRITEVECTOR: irec = ',irecord,narr
            call print_message( msgbuf, standardmessageunit,
     &           SQUEEZE_RIGHT , mythid)
            write(dUnit,rec=irecord) arr
         else
            write(msgbuf,'(a)')
     &           ' SBO_WRITEVECTOR: I should never get to this point'
            call print_error( msgbuf, mythid )
            stop 'ABNORMAL END: S/R SBO_WRITEVECTOR'
         endif

C Close file
         if (fileIsOpen) then
            close( dUnit )
            fileIsOpen = .FALSE.
         endif

C Create meta-file
         write(metaFName(1:80),'(2a)') fName(1:IL),'.meta'
         dimList(1,1) = narr
         dimList(2,1) = 1
         dimList(3,1) = narr
         dimList(1,2) = 1
         dimList(2,2) = 1
         dimList(3,2) = 1
         dimList(1,3) = 1
         dimList(2,3) = 1
         dimList(3,3) = 1
         ndims=1
         call MDSWRITEMETA( metaFName, dataFName,
     &        filePrec, ndims, dimList, irecord, myIter, mythid )

         _END_MASTER( myThid )

      ENDIF

#endif ALLOW_SBO

      return
      end

C $Header: /u/gcmpack/MITgcm/pkg/mdsio/Attic/mdsio_writemeta.F,v 1.1 2001/03/06 15:28:54 adcroft Exp $
C $Name:  $

#include "MDSIO_OPTIONS.h"

      subroutine MDSWRITEMETA(
     I    mFileName,
     I    dFileName,
     I    filePrec,
     I    ndims,
     I    dimList,
     I    nrecords,
     I    myIter,
     I    mythid )

C IN:
C     mFileName	string  - complete name of meta-file
C     dFileName	string  - complete name of data-file
C     ndims	integer - number of dimensions
C     dimList	integer - array of dimensions, etc.
C     nrecords	integer - record number
C     myIter	integer - time-step number
C     mythid    integer - thread id
C OUT:
C
C Created: 03/20/99 adcroft@mit.edu

      implicit none
C Arguments
      character*(*) mFileName
      character*(*) dFileName
      integer filePrec
      integer ndims
      integer dimList(3,ndims)
      integer nrecords
      integer myIter
      integer mythid

C Global variables / common blocks
#include "SIZE.h"
#include "EEPARAMS.h"
#include "PARAMS.h"

C Functions

C Local
      integer i,ii,mUnit
      logical ex
      character*(max_len_mbuf) msgbuf
C     ------------------------------------------------------------------

C We should *read* the met-file if it exists to check
C that the information we are writing is consistent
C with the current contents
      inquire( file=mFileName, exist=ex )
C However, it is bloody difficult to parse files
C in fortran so someone else can do this.
C For now, we will assume everything is ok
C and that the last record is written to the
C last consecutive record in the file.

C Assign a free unit number as the I/O channel for this subroutine
      call MDSFINDUNIT( mUnit, mythid )

C Open meta-file
      open( mUnit, file=mFileName, status='unknown',
     &    form='formatted' )

C Write the number of dimensions
      write(mUnit,'(x,a,i3,a)') 'nDims = [ ',ndims,' ];'

C For each dimension, write the following:
C   1  global size  (ie. the size of the global dimension of all files)
C   2  global start (ie. the global position of the start of this file)
C   3  global end   (ie. the global position of the end   of this file)

      write(mUnit,'(x,a)') 'dimList = ['
      do ii=1,ndims
       if (ii.lt.ndims) then
        write(mUnit,'(10x,3(i5,","))') (dimList(i,ii),i=1,3)
       else
        write(mUnit,'(10x,i5,",",i5,",",i5)') (dimList(i,ii),i=1,3)
       endif
      enddo
      write(mUnit,'(10x,a)') '];'

C Record the precision of the file
      if (filePrec .EQ. precFloat32) then
       write(mUnit,'(x,a)') "format = [ 'float32' ];"
      elseif (filePrec .EQ. precFloat64) then
       write(mUnit,'(x,a)') "format = [ 'float64' ];"
      else
       write(msgbuf,'(a)')
     &   ' MDSWRITEMETA: invalid filePrec'
       call print_error( msgbuf, mythid )
       stop 'ABNORMAL END: S/R MDSWRITEMETA'
      endif

C Record the current record number
C This is a proxy for the actual number of records in the file.
C If we could read the file then we could do this properly.
      write(mUnit,'(x,a,i5,a)') 'nrecords = [ ',nrecords,' ];'

C Record the file-name for the binary data
Cveto ii=ILNBLNK( dFileName )
Cveto write(mUnit,'(x,3a)') 'binarydatafile = [ ',dFileName(1:ii),' ];'

C Write the integer time (integer iteration number) for later record
C keeping. If the timestep number is less than 0 then we assume
C that the information is superfluous and do not write it.
      if (myIter .ge. 0)
     & write(mUnit,'(x,a,i8,a)') 'timeStepNumber = [ ',myIter,' ];'

C Close meta-file
      close(mUnit)
      
C     ------------------------------------------------------------------
      return
      end

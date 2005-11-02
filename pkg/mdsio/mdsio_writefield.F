C $Header: /u/gcmpack/MITgcm/pkg/mdsio/Attic/mdsio_writefield.F,v 1.9 2005/11/02 14:39:46 jmc Exp $
C $Name:  $

#include "MDSIO_OPTIONS.h"

      SUBROUTINE MDSWRITEFIELD(
     I   fName,
     I   filePrec,
     I   globalFile,
     I   arrType,
     I   nNz,
     I   arr,
     I   irecord,
     I   myIter,
     I   myThid )
C
C Arguments:
C
C fName		string	base name for file to written
C filePrec	integer	number of bits per word in file (32 or 64)
C globalFile	logical	selects between writing a global or tiled file
C arrType	char(2)	declaration of "arr": either "RS" or "RL"
C nNz		integer	size of third dimension: normally either 1 or Nr
C arr		RS/RL	array to write, arr(:,:,nNz,:,:)
C irecord	integer	record number to read
C myIter	integer	time step number
C myThid	integer thread identifier
C
C Routine now calls mdswritefield_new, just a way to add 2 extra arguments
C to the argument list. The 1rst new argument is to make the difference between
C the vertical dimension (3rd dimension) of an array and the number of levels
C the output routine should process. This routine assumes they are the same.
C The 2nd new argument (useCurrentDir=.FALSE.) allows to write files to
C the "mdsioLocalDir" (if it is set) directory.

      implicit none
C Global variables / common blocks
#include "SIZE.h"
c #include "EEPARAMS.h"

C Routine arguments
      character*(*) fName
      integer filePrec
      logical globalFile
      character*(2) arrType
      integer nNz
      _RL     arr(1-oLx:sNx+oLx,1-oLy:sNy+oLy,nNz,nSx,nSy)
      integer irecord
      integer myIter
      integer myThid
C     ------------------------------------------------------------------
      CALL MDSWRITEFIELD_NEW(
     I                        fName,filePrec,globalFile,.FALSE.,
     I                        arrType,nNz,nNz,arr,irecord,
     I                        myIter,myThid )
C     ------------------------------------------------------------------
      return
      end

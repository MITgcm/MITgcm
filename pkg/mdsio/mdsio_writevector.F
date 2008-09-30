C $Header: /u/gcmpack/MITgcm/pkg/mdsio/Attic/mdsio_writevector.F,v 1.9 2008/09/30 22:39:25 heimbach Exp $
C $Name:  $

#include "MDSIO_OPTIONS.h"

      SUBROUTINE MDSWRITEVECTOR(
     I   fName,
     I   filePrec,
     I   globalfile,
     I   arrType,
     I   narr,
     I   arr,
     I   bi,
     I   bj,
     I   irecord,
     I   myIter,
     I   myThid )
C Arguments:
C
C fName		string	base name for file to written
C filePrec	integer	number of bits per word in file (32 or 64)
C globalFile	logical	selects between writing a global or tiled file
C arrType	char(2)	declaration of "arr": either "RS" or "RL"
C narr		integer	size of third dimension: normally either 1 or Nr
C arr		RS/RL	array to write, arr(narr)
ce bi           integer x tile index
ce bj           integer y tile index
C irecord	integer	record number to read
C myIter	integer	time step number
C myThid	integer thread identifier
C
C Created: 03/26/99 eckert@mit.edu
C Modified: 03/29/99 adcroft@mit.edu + eckert@mit.edu
C           Fixed to work work with _RS and _RL declarations
C Modified: 07/27/99 eckert@mit.edu
C           Customized  for state estimation (--> active_file_control.F)
C Changed: 05/31/00 heimbach@mit.edu
C          open(dUnit, ..., status='old', ... -> status='unknown'

      implicit none
C Global variables / common blocks
#include "SIZE.h"
#include "EEPARAMS.h"
#include "PARAMS.h"
#include "EESUPPORT.h"

C Routine arguments
      character*(*) fName
      integer filePrec
      logical globalfile
      character*(2) arrType
      integer narr
      _RL arr(narr)
      integer irecord
      integer myIter
      integer myThid
ce
      integer bi,bj
ce

C Functions
      integer ILNBLNK
      integer MDS_RECLEN
C Local variables
      character*(MAX_LEN_FNAM) dataFName,metaFName,pfName
      integer iG,jG,irec,dUnit,IL,pIL
      logical fileIsOpen
      integer dimList(3,3),ndims
      integer length_of_rec
      character*(max_len_mbuf) msgbuf

cph(
cph Deal with useSingleCpuIO
cph Not implemented here for EXCH2
      INTEGER nNz
      INTEGER vec_size
#ifdef ALLOW_USE_MPI
      logical lprint
      INTEGER K,L
c     INTEGER iG_IO,jG_IO,npe
      Real*4 xy_buffer_r4(narr*nPx*nPy)
      Real*8 xy_buffer_r8(narr*nPx*nPy)
      Real*8 global(narr*nPx*nPy)
      _RL    local(narr)
#endif
cph)

C     ------------------------------------------------------------------

      vec_size = narr*nPx*nPy
      nNz = 1

C Only do I/O if I am the master thread
      _BEGIN_MASTER( myThid )

C Record number must be >= 1
      if (irecord .LT. 1) then
       write(msgbuf,'(a,i9.8)')
     &   ' MDSWRITEVECTOR: argument irecord = ',irecord
       call print_message( msgbuf, standardmessageunit,
     &                     SQUEEZE_RIGHT , mythid)
       write(msgbuf,'(a)')
     &   ' MDSWRITEVECTOR: invalid value for irecord'
       call print_error( msgbuf, mythid )
       stop 'ABNORMAL END: S/R MDSWRITEVECTOR'
      endif

C Assume nothing
      fileIsOpen = .FALSE.
      IL  = ILNBLNK( fName )
      pIL = ILNBLNK( mdsioLocalDir )

C Assign special directory
      if ( mdsioLocalDir .NE. ' ' ) then
       write(pFname,'(2a)') 
     &  mdsioLocalDir(1:pIL), fName(1:IL)
      else
       pFname= fName
      endif
      pIL=ILNBLNK( pfName )

C Assign a free unit number as the I/O channel for this routine
      call MDSFINDUNIT( dUnit, mythid )

#ifdef ALLOW_USE_MPI
      _END_MASTER( myThid )
C If option globalFile is desired but does not work or if
C globalFile is too slow, then try using single-CPU I/O.
      if (useSingleCpuIO) then

C Master thread of process 0, only, opens a global file
       _BEGIN_MASTER( myThid )
        IF( mpiMyId .EQ. 0 ) THEN
         write(dataFname,'(2a)') fName(1:IL),'.data'
         length_of_rec=MDS_RECLEN(filePrec,vec_size,mythid)
         if (irecord .EQ. 1) then
          open( dUnit, file=dataFName, status=_NEW_STATUS,
     &        access='direct', recl=length_of_rec )
         else
          open( dUnit, file=dataFName, status=_OLD_STATUS,
     &        access='direct', recl=length_of_rec )
         endif
        ENDIF
       _END_MASTER( myThid )

C Gather array and write it to file, one vertical level at a time
       DO k=1,1
        DO L=1,narr
            local(L) = arr(L)
        ENDDO
cph(
cph        if ( irecord .EQ. 1 .AND. fName(1:IL) .EQ.
cph     &       'tapelev2_7_the_main_loop_theta.it0000' ) then
cph           lprint = .TRUE.
cph        else
           lprint = .FALSE.
cph        endif
cph)
        CALL GATHER_VECTOR( lprint, narr, global, local, myThid )
        _BEGIN_MASTER( myThid )
         IF( mpiMyId .EQ. 0 ) THEN
          irec=irecord
          if (filePrec .eq. precFloat32) then
cph#if defined(ALLOW_EXCH2) && !defined(MISSING_TILE_IO)
c
cph#else /* defined(ALLOW_EXCH2) && !defined(MISSING_TILE_IO) */
           DO L=1,narr*nPx*nPy
            xy_buffer_r4(L) = global(L)
           ENDDO
cph#endif /* defined(ALLOW_EXCH2) && !defined(MISSING_TILE_IO) */
#ifdef _BYTESWAPIO
           call MDS_BYTESWAPR4( vec_size, xy_buffer_r4 )
#endif
           write(dUnit,rec=irec) xy_buffer_r4
          elseif (filePrec .eq. precFloat64) then
cph#if defined(ALLOW_EXCH2) && !defined(MISSING_TILE_IO)
c
cph#else /* defined(ALLOW_EXCH2) && !defined(MISSING_TILE_IO) */
           DO L=1,narr*nPx*nPy
            xy_buffer_r8(L) = global(L)
           ENDDO
cph#endif /* defined(ALLOW_EXCH2) && !defined(MISSING_TILE_IO) */
#ifdef _BYTESWAPIO
           call MDS_BYTESWAPR8( vec_size, xy_buffer_r8 )
#endif
           write(dUnit,rec=irec) xy_buffer_r8
          else
           write(msgbuf,'(a)')
     &       ' MDSWRITEFIELD: illegal value for filePrec'
           call print_error( msgbuf, mythid )
           stop 'ABNORMAL END: S/R MDSWRITEFIELD'
          endif
         ENDIF
        _END_MASTER( myThid )
       ENDDO

C Close data-file and create meta-file
       _BEGIN_MASTER( myThid )
        IF( mpiMyId .EQ. 0 ) THEN
         close( dUnit )
         write(metaFName,'(2a)') fName(1:IL),'.meta'
         dimList(1,1)=vec_size
         dimList(2,1)=1
         dimList(3,1)=vec_size
         dimList(1,2)=vec_size
         dimList(2,2)=1
         dimList(3,2)=vec_size
         dimList(1,3)=1
         dimList(2,3)=1
         dimList(3,3)=1
         ndims=1
cph         if (nNz .EQ. 1) ndims=2
         call MDSWRITEMETA( metaFName, dataFName,
     &     filePrec, ndims, dimList, irecord, myIter, mythid )
        ENDIF
       _END_MASTER( myThid )
C To be safe, make other processes wait for I/O completion
       _BARRIER

      elseif ( .NOT. useSingleCpuIO ) then
      _BEGIN_MASTER( myThid )
#endif /* ALLOW_USE_MPI */

C If we are writing to a global file then we open it here
      if (globalFile) then
       write(dataFname,'(2a)') fName(1:IL),'.data'
       if (irecord .EQ. 1) then
        length_of_rec = MDS_RECLEN( filePrec, narr, mythid )
        open( dUnit, file=dataFName, status=_NEW_STATUS,
     &      access='direct', recl=length_of_rec )
        fileIsOpen=.TRUE.
       else
        length_of_rec = MDS_RECLEN( filePrec, narr, mythid )
        open( dUnit, file=dataFName, status=_OLD_STATUS,
     &      access='direct', recl=length_of_rec )
        fileIsOpen=.TRUE.
       endif
      endif

C Loop over all tiles
ce      do bj=1,nSy
ce       do bi=1,nSx
C If we are writing to a tiled MDS file then we open each one here
        if (.NOT. globalFile) then
         iG=bi+(myXGlobalLo-1)/sNx ! Kludge until unstructered tiles
         jG=bj+(myYGlobalLo-1)/sNy ! Kludge until unstructered tiles
         write(dataFname,'(2a,i3.3,a,i3.3,a)')
     &              pfName(1:pIL),'.',iG,'.',jG,'.data'
         if (irecord .EQ. 1) then
          length_of_rec = MDS_RECLEN( filePrec, narr, mythid )
          open( dUnit, file=dataFName, status=_NEW_STATUS,
     &       access='direct', recl=length_of_rec )
          fileIsOpen=.TRUE.
         else
          length_of_rec = MDS_RECLEN( filePrec, narr, mythid )
          open( dUnit, file=dataFName, status=_OLD_STATUS,
     &       access='direct', recl=length_of_rec )
          fileIsOpen=.TRUE.
         endif
        endif
        if (fileIsOpen) then
          if (globalFile) then
            iG   = myXGlobalLo-1+(bi-1)*sNx
            jG   = myYGlobalLo-1+(bj-1)*sNy
            irec = 1 + int(iG/sNx) + (jG/sNy)*nSx*nPx +
     &             (irecord-1)*nSx*nPx*nSy*nPy
          else
            iG   = 0
            jG   = 0
            irec = irecord
          endif
          if (filePrec .eq. precFloat32) then
           call MDS_WRITE_RS_VEC( dUnit, irec, narr, arr, myThid )
          elseif (filePrec .eq. precFloat64) then
           call MDS_WRITE_RL_VEC( dUnit, irec, narr, arr, myThid )
          else
           write(msgbuf,'(a)')
     &       ' MDSWRITEVECTOR: illegal value for filePrec'
           call print_error( msgbuf, mythid )
           stop 'ABNORMAL END: S/R MDSWRITEVECTOR'
          endif
        else
         write(msgbuf,'(a)')
     &     ' MDSWRITEVECTOR: I should never get to this point'
         call print_error( msgbuf, mythid )
         stop 'ABNORMAL END: S/R MDSWRITEVECTOR'
        endif
C If we were writing to a tiled MDS file then we close it here
        if (fileIsOpen .AND. (.NOT. globalFile)) then
          close( dUnit )
          fileIsOpen = .FALSE.
        endif
C Create meta-file for each tile file
        if (.NOT. globalFile) then
         iG=bi+(myXGlobalLo-1)/sNx ! Kludge until unstructered tiles
         jG=bj+(myYGlobalLo-1)/sNy ! Kludge until unstructered tiles
         write(metaFname,'(2a,i3.3,a,i3.3,a)')
     &              pfName(1:pIL),'.',iG,'.',jG,'.meta'
         dimList(1,1) = nPx*nSx*narr
         dimList(2,1) = ((myXGlobalLo-1)/sNx + (bi-1))*narr + 1
         dimList(3,1) = ((myXGlobalLo-1)/sNx +   bi  )*narr
         dimList(1,2) = nPy*nSy
         dimList(2,2) = (myYGlobalLo-1)/sNy + bj
         dimList(3,2) = (myYGlobalLo-1)/sNy + bj
         dimList(1,3) = 1
         dimList(2,3) = 1
         dimList(3,3) = 1
         ndims=1
         call MDSWRITEMETA( metaFName, dataFName,
     &     filePrec, ndims, dimList, irecord, myIter, mythid )
        endif
C End of bi,bj loops
ce       enddo
ce      enddo

C If global file was opened then close it
      if (fileIsOpen .AND. globalFile) then
        close( dUnit )
        fileIsOpen = .FALSE.
      endif

C Create meta-file for global file
      if (globalFile) then
       write(metaFName,'(2a)') fName(1:IL),'.meta'
       dimList(1,1) = nPx*nSx*narr
       dimList(2,1) = 1
       dimList(3,1) = nPx*nSx*narr
       dimList(1,2) = nPy*nSy
       dimList(2,2) = 1
       dimList(3,2) = nPy*nSy
       dimList(1,3) = 1
       dimList(2,3) = 1
       dimList(3,3) = 1
       ndims=1
       call MDSWRITEMETA( metaFName, dataFName,
     &   filePrec, ndims, dimList, irecord, myIter, mythid )
      endif

      _END_MASTER( myThid )

#ifdef ALLOW_USE_MPI
C endif useSingleCpuIO
      endif
#endif /* ALLOW_USE_MPI */

C     ------------------------------------------------------------------
      return
      end

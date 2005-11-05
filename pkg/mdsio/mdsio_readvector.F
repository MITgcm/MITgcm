C $Header: /u/gcmpack/MITgcm/pkg/mdsio/Attic/mdsio_readvector.F,v 1.8 2005/11/05 01:05:14 jmc Exp $
C $Name:  $

#include "MDSIO_OPTIONS.h"

      SUBROUTINE MDSREADVECTOR(
     I   fName,
     I   filePrec,
     I   arrType,
     I   narr,
     O   arr,
     I   bi,
     I   bj,
     I   irecord,
     I   myThid )
C
C Arguments:
C
C fName		string	base name for file to read
C filePrec	integer	number of bits per word in file (32 or 64)
C arrType	char(2)	declaration of "arr": either "RS" or "RL"
C narr		integer	size of third dimension: normally either 1 or Nr
C arr		RS/RL	array to read into, arr(narr)
ce bi           integer x tile index
ce bj           integer y tile index
C irecord	integer	record number to read
C myThid	integer thread identifier
C
C Created: 03/26/99 eckert@mit.edu
C Modified: 03/29/99 adcroft@mit.edu + eckert@mit.edu
C           Fixed to work work with _RS and _RL declarations
C Modified: 07/27/99 eckert@mit.edu
C           Customized  for state estimation (--> active_file_control.F)

      implicit none
C Global variables / common blocks
#include "SIZE.h"
#include "EEPARAMS.h"
#include "PARAMS.h"
#include "EESUPPORT.h"

C Routine arguments
      character*(*) fName
      integer filePrec
      character*(2) arrType
      integer narr
      Real arr(narr)
      integer irecord
      integer myThid
ce
      integer bi,bj
ce

C Functions
      integer ILNBLNK
      integer MDS_RECLEN
C Local variables
      character*(MAX_LEN_FNAM) dataFName,pfName
      integer iG,jG,irec,dUnit,IL,pIL
      logical exst
      logical globalFile,fileIsOpen
      integer length_of_rec
      character*(max_len_mbuf) msgbuf

cph(
cph Deal with useSingleCpuIO
cph Not extended here for EXCH2
      integer k,l
      integer nNz
      integer vec_size
      Real*4 xy_buffer_r4(narr*nPx*nPy)
      Real*8 xy_buffer_r8(narr*nPx*nPy)
      Real*8 global   (narr*nPx*nPy)
      _RL    local(narr)
cph)
C     ------------------------------------------------------------------

      vec_size = narr*nPx*nPy
      nNz = 1
      
C Only do I/O if I am the master thread
      _BEGIN_MASTER( myThid )

C Record number must be >= 1
      if (irecord .LT. 1) then
       write(msgbuf,'(a,i9.8)')
     &   ' MDSREADVECTOR: argument irecord = ',irecord
       call print_message( msgbuf, standardmessageunit,
     &                     SQUEEZE_RIGHT , mythid)
       write(msgbuf,'(a)')
     &   ' MDSREADVECTOR: invalid value for irecord'
       call print_error( msgbuf, mythid )
       stop 'ABNORMAL END: S/R MDSREADVECTOR'
      endif

C Assume nothing
      globalFile = .FALSE.
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

      if ( .not. useSingleCPUIO ) then

C Check first for global file with simple name (ie. fName)
      dataFName = fName
      inquire( file=dataFname, exist=exst )
      if (exst) then
       if ( debugLevel .GE. debLevB ) then
        write(msgbuf,'(a,a)')
     &   ' MDSREADVECTOR: opening global file: ',dataFName(1:IL)
        call print_message( msgbuf, standardmessageunit,
     &                     SQUEEZE_RIGHT , mythid)
       endif
       globalFile = .TRUE.
      endif

C If negative check for global file with MDS name (ie. fName.data)
      if (.NOT. globalFile) then
       write(dataFname,'(2a)') fName(1:IL),'.data'
       inquire( file=dataFname, exist=exst )
       if (exst) then
        if ( debugLevel .GE. debLevB ) then
         write(msgbuf,'(a,a)')
     &     ' MDSREADVECTOR: opening global file: ',dataFName(1:IL+5)
         call print_message( msgbuf, standardmessageunit,
     &                       SQUEEZE_RIGHT , mythid)
        endif
        globalFile = .TRUE.
       endif
      endif

C If we are reading from a global file then we open it here
      if (globalFile) then
       length_of_rec=MDS_RECLEN( filePrec, narr, mythid )
       open( dUnit, file=dataFName, status='old',
     &      access='direct', recl=length_of_rec )
       fileIsOpen=.TRUE.
      endif

C Loop over all tiles
ce      do bj=1,nSy
ce       do bi=1,nSx
C If we are reading from a tiled MDS file then we open each one here
        if (.NOT. globalFile) then
         iG=bi+(myXGlobalLo-1)/sNx ! Kludge until unstructered tiles
         jG=bj+(myYGlobalLo-1)/sNy ! Kludge until unstructered tiles
         write(dataFname,'(2a,i3.3,a,i3.3,a)')
     &              pfName(1:pIL),'.',iG,'.',jG,'.data'
         inquire( file=dataFname, exist=exst )
C Of course, we only open the file if the tile is "active"
C (This is a place-holder for the active/passive mechanism)
         if (exst) then
          if ( debugLevel .GE. debLevB ) then
           write(msgbuf,'(a,a)')
     &      ' MDSREADVECTOR: opening file: ',dataFName(1:pIL+13)
           call print_message( msgbuf, standardmessageunit,
     &                        SQUEEZE_RIGHT , mythid)
          endif
          length_of_rec=MDS_RECLEN( filePrec, narr, mythid )
          open( dUnit, file=dataFName, status='old',
     &        access='direct', recl=length_of_rec )
          fileIsOpen=.TRUE.
         else
          fileIsOpen=.FALSE.
          write(msgbuf,'(4a)')
     &      ' MDSREADVECTOR: opening file: ',fName(1:IL),
     &                                 ' , ',dataFName(1:pIL+13)
          call print_message( msgbuf, standardmessageunit,
     &                        SQUEEZE_RIGHT , mythid)
          write(msgbuf,'(a)')
     &      ' MDSREADVECTOR: un-active tiles not implemented yet'
          call print_error( msgbuf, mythid )
          stop 'ABNORMAL END: S/R MDSREADVECTOR'
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
           call MDS_READ_RS_VEC( dUnit, irec, narr, arr, myThid )
          elseif (filePrec .eq. precFloat64) then
           call MDS_READ_RL_VEC( dUnit, irec, narr, arr, myThid )
          else
            write(msgbuf,'(a)')
     &        ' MDSREADVECTOR: illegal value for filePrec'
            call print_error( msgbuf, mythid )
            stop 'ABNORMAL END: S/R MDSREADVECTOR'
          endif
          if (.NOT. globalFile) then
            close( dUnit )
            fileIsOpen = .FALSE.
          endif
        endif
C End of bi,bj loops
ce       enddo
ce      enddo

C If global file was opened then close it
      if (fileIsOpen .AND. globalFile) then
        close( dUnit )
        fileIsOpen = .FALSE.
      endif

      _END_MASTER( myThid )

      endif
c     endif ( .not. useSingleCPUIO )


C     ------------------------------------------------------------------


      if ( useSingleCPUIO ) then

C master thread of process 0, only, opens a global file
       _BEGIN_MASTER( myThid )
#ifdef ALLOW_USE_MPI
        IF( mpiMyId .EQ. 0 ) THEN
#else
        IF ( .TRUE. ) THEN
#endif /* ALLOW_USE_MPI */

C Check first for global file with simple name (ie. fName)
         dataFName = fName
         inquire( file=dataFname, exist=exst )
         if (exst) globalFile = .TRUE.

C If negative check for global file with MDS name (ie. fName.data)
         if (.NOT. globalFile) then
          write(dataFname,'(2a)') fName(1:IL),'.data'
          inquire( file=dataFname, exist=exst )
          if (exst) globalFile = .TRUE.
         endif

C If global file is visible to process 0, then open it here.
C Otherwise stop program.
         if ( globalFile) then
          length_of_rec=MDS_RECLEN( filePrec, vec_size, mythid )
          open( dUnit, file=dataFName, status='old',
     &         access='direct', recl=length_of_rec )
         else
          write(msgbuf,'(2a)') 
     &      ' MDSREADFIELD: filename: ',dataFName(1:IL)
          call print_message( msgbuf, standardmessageunit,
     &                        SQUEEZE_RIGHT , mythid)
          call print_error( msgbuf, mythid )
          write(msgbuf,'(a)')
     &      ' MDSREADFIELD: File does not exist'
          call print_message( msgbuf, standardmessageunit,
     &                        SQUEEZE_RIGHT , mythid)
          call print_error( msgbuf, mythid )
          stop 'ABNORMAL END: S/R MDSREADFIELD'
         endif

        ENDIF
       _END_MASTER( myThid )

       DO k=1,1

        _BEGIN_MASTER( myThid )
#ifdef ALLOW_USE_MPI
         IF( mpiMyId .EQ. 0 ) THEN
#else
         IF ( .TRUE. ) THEN
#endif /* ALLOW_USE_MPI */
          irec = irecord
          if (filePrec .eq. precFloat32) then
           read(dUnit,rec=irec) xy_buffer_r4
#ifdef _BYTESWAPIO
           call MDS_BYTESWAPR4( vec_size, xy_buffer_r4 )
#endif
#if defined(ALLOW_EXCH2) && !defined(MISSING_TILE_IO)
c
#else /* defined(ALLOW_EXCH2) && !defined(MISSING_TILE_IO) */
           DO L=1,narr*nPx*nPy
            global(L) = xy_buffer_r4(L)
           ENDDO
#endif /* defined(ALLOW_EXCH2) && !defined(MISSING_TILE_IO) */
          elseif (filePrec .eq. precFloat64) then
           read(dUnit,rec=irec) xy_buffer_r8
#ifdef _BYTESWAPIO
           call MDS_BYTESWAPR8( vec_size, xy_buffer_r8 )
#endif
#if defined(ALLOW_EXCH2) && !defined(MISSING_TILE_IO)
c
#else /* defined(ALLOW_EXCH2) && !defined(MISSING_TILE_IO) */
           DO L=1,narr*nPx*nPy
            global(L) = xy_buffer_r8(L)
           ENDDO
#endif /* defined(ALLOW_EXCH2) && !defined(MISSING_TILE_IO) */
          else
           write(msgbuf,'(a)')
     &            ' MDSREADFIELD: illegal value for filePrec'
           call print_error( msgbuf, mythid )
           stop 'ABNORMAL END: S/R MDSREADFIELD'
          endif
         ENDIF
        _END_MASTER( myThid )
        CALL SCATTER_VECTOR( narr,global,local,mythid )
        if (arrType .eq. 'RS') then
           call PASStoRSvector( local,arr,narr,k,nNz,mythid )
        elseif (arrType .eq. 'RL') then
           call PASStoRLvector( local,arr,narr,k,nNz,mythid )
        else
           write(msgbuf,'(a)')
     &          ' MDSREADFIELD: illegal value for arrType'
           call print_error( msgbuf, mythid )
           stop 'ABNORMAL END: S/R MDSREADFIELD'
        endif

       ENDDO
c      ENDDO k=1,nNz

       _BEGIN_MASTER( myThid )
        close( dUnit )
       _END_MASTER( myThid )

      endif
c     endif ( useSingleCPUIO )

      return
      end


C     ==================================================================

      subroutine passToRSvector(local,arr,narr,k,nNz,mythid)
      implicit none
#include "EEPARAMS.h"
#include "SIZE.h"

      integer narr
      _RL local(narr)
      _RS arr(narr)
      integer k,nNz
      integer mythid

      integer L

      DO L=1,narr
         arr(L) = local(L)
      ENDDO

      return
      end

      subroutine passToRLvector(local,arr,narr,k,nNz,mythid)
      implicit none
#include "EEPARAMS.h"
#include "SIZE.h"

      integer narr
      _RL local(narr)
      _RS arr(narr)
      integer k,nNz
      integer mythid

      integer L

      DO L=1,narr
         arr(L) = local(L)
      ENDDO

      return
      end

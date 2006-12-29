C $Header: /u/gcmpack/MITgcm/pkg/mdsio/mdsio_wr_metafiles.F,v 1.1 2006/12/29 05:14:11 jmc Exp $
C $Name:  $

#include "MDSIO_OPTIONS.h"

C---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|
CBOP
C !ROUTINE: MDS_WR_METAFILES
C !INTERFACE:
      SUBROUTINE MDS_WR_METAFILES(
     I   fName,
     I   filePrec,
     I   globalFile,
     I   useCurrentDir,
     I   nNx, nNy, nNz,
     I   titleLine,
     I   nFlds, fldList,
     I   nTimRec, timList,
     I   irecord,
     I   myIter,
     I   myThid )

C !DESCRIPTION:
C
C MDS_WR_METAFILES creates either a file of the form "fName.meta" IF the
C LOGICAL flag "globalFile" or "useSingleCPUIO" are set true. Otherwise
C it creates MDS tiled files of the form "fName.xxx.yyy.meta".
C Currently, the meta-files are not read because it is difficult
C to parse files in fortran. We should read meta information before
C adding records to an existing multi-record file.
C The precision of the file is decsribed by filePrec, set either
C to floatPrec32 or floatPrec64.
C nNz=1 implies a 2-D model field and nNz=Nr implies a 3-D model field.
C irecord is the record number to be written and must be >= 1.
C NOTE: It is currently assumed that
C the highest record number in the file was the last record written.
C Nor is there a consistency check between the routine arguments and file.
C ie. IF your WRITE record 2 after record 4 the meta information
C will record the number of records to be 2. This, again, is because
C we have read the meta information. To be fixed.

C     !USES:
      IMPLICIT NONE
C Global variables / COMMON blocks
#include "SIZE.h"
#include "EEPARAMS.h"
#include "PARAMS.h"
#ifdef ALLOW_EXCH2
#include "W2_EXCH2_TOPOLOGY.h"
#include "W2_EXCH2_PARAMS.h"
#endif /* ALLOW_EXCH2 */
C Arguments:
C
C fName     (string)  :: base name for file to write
C filePrec  (integer) :: number of bits per word in file (32 or 64)
C globalFile (logical):: selects between writing a global or tiled file
C useCurrentDir(logic):: always write to the current directory (even if
C                        "mdsioLocalDir" is set)
C nNx,nNy   (integer) :: used for writing YZ or XZ slice
C nNz       (integer) :: number of vertical levels to be written
C titleLine (string)  :: title or any descriptive comments
C nFlds     (integer) :: number of fields from "fldList" to write
C fldList   (string)  :: array of fields name to write
C nTimRec   (integer) :: number of time-info from "fldList" to write
C timList   (real)    :: array of time-info to write
C irecord   (integer) :: record number to write
C myIter    (integer) :: time step number
C myThid    (integer) :: thread identifier
C
C Routine arguments
      CHARACTER*(*) fName
      INTEGER filePrec
      LOGICAL globalFile
      LOGICAL useCurrentDir
      INTEGER nNx, nNy, nNz
      CHARACTER*(*) titleLine
      INTEGER nFlds
      CHARACTER*(8) fldList(*)
      INTEGER nTimRec
      _RL     timList(*)
      INTEGER irecord
      INTEGER myIter
      INTEGER myThid
CEOP

C Functions
      INTEGER  ILNBLNK
      EXTERNAL ILNBLNK
      LOGICAL  MASTER_CPU_IO
      EXTERNAL MASTER_CPU_IO
C Local variables
      CHARACTER*(MAX_LEN_FNAM) dataFName, metaFName, pfName
      INTEGER iG,jG, bi,bj, IL,pIL
      INTEGER dimList(3,3), nDims
      INTEGER xSize, ySize
#if defined(ALLOW_EXCH2) && !defined(MISSING_TILE_IO)
      INTEGER tn
#endif

C---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|
C-    Set dimensions:
      xSize = Nx
      ySize = Ny
#if defined(ALLOW_EXCH2) && !defined(MISSING_TILE_IO)
      xSize = exch2_domain_nxt * sNx
      ySize = exch2_domain_nyt * sNy
#endif
      IF (nNx.EQ.1) xSize = 1
      IF (nNy.EQ.1) ySize = 1

C Only do I/O if I am the master thread (and mpi process 0 IF useSingleCpuIO):
      IF ( MASTER_CPU_IO(myThid) ) THEN

       IF ( useSingleCpuIO .OR. globalFile ) THEN

         IL  = ILNBLNK( fName )
         WRITE(dataFName,'(2A)') fName(1:IL),'.data'
         WRITE(metaFName,'(2A)') fName(1:IL),'.meta'
         dimList(1,1) = xSize
         dimList(2,1) = 1
         dimList(3,1) = xSize
         dimList(1,2) = ySize
         dimList(2,2) = 1
         dimList(3,2) = ySize
         dimList(1,3) = nNz
         dimList(2,3) = 1
         dimList(3,3) = nNz
         nDims=3
         IF (nNz.EQ.1) nDims=2
c        CALL MDSWRITEMETA( metaFName, dataFName, filePrec,
c    &                    nDims, dimList, irecord, myIter, myThid )
         CALL MDS_WRITE_META(
     I              metaFName, dataFName, the_run_name, titleLine,
     I              filePrec, nDims, dimList, nFlds,  fldList,
     I              nTimRec, timList, irecord, myIter, myThid )

       ELSE

C Assign special directory
        pIL = ILNBLNK( mdsioLocalDir )
        IF ( useCurrentDir .OR. pIL.EQ.0 ) THEN
          pfName = fName
        ELSE
          IL  = ILNBLNK( fName )
          WRITE(pfName,'(2A)') mdsioLocalDir(1:pIL), fName(1:IL)
        ENDIF
        pIL=ILNBLNK( pfName )

C Loop over all tiles
        DO bj=1,nSy
         DO bi=1,nSx
C If we are writing to a tiled MDS file then we open each one here
           iG=bi+(myXGlobalLo-1)/sNx
           jG=bj+(myYGlobalLo-1)/sNy
           WRITE(dataFName,'(2a,i3.3,a,i3.3,a)')
     &              pfName(1:pIL),'.',iG,'.',jG,'.data'
C Create meta-file for each tile IF we are tiling
           WRITE(metaFname,'(2a,i3.3,a,i3.3,a)')
     &              pfName(1:pIL),'.',iG,'.',jG,'.meta'
#if defined(ALLOW_EXCH2) && !defined(MISSING_TILE_IO)
           tn = W2_myTileList(bi)
           dimList(1,1) = xSize
           dimList(2,1) = exch2_txGlobalo(tn)
           dimList(3,1) = exch2_txGlobalo(tn)+sNx-1
           dimList(1,2) = ySize
           dimList(2,2) = exch2_tyGlobalo(tn)
           dimList(3,2) = exch2_tyGlobalo(tn)+sNy-1
#else /* defined(ALLOW_EXCH2) && !defined(MISSING_TILE_IO) */
C- jmc: IF MISSING_TILE_IO, keep meta files unchanged
C       to stay consistent with global file structure
           dimList(1,1) = xSize
           dimList(2,1) = myXGlobalLo+(bi-1)*sNx
           dimList(3,1) = myXGlobalLo+bi*sNx-1
           dimList(1,2) = ySize
           dimList(2,2) = myYGlobalLo+(bj-1)*sNy
           dimList(3,2) = myYGlobalLo+bj*sNy-1
#endif /* defined(ALLOW_EXCH2) && !defined(MISSING_TILE_IO) */
           dimList(1,3) = nNz
           dimList(2,3) = 1
           dimList(3,3) = nNz
           nDims=3
           IF (nNz.EQ.1) nDims=2
           IF (nNx.EQ.1) dimList(2,1) = 1
           IF (nNx.EQ.1) dimList(3,1) = 1
           IF (nNy.EQ.1) dimList(2,2) = 1
           IF (nNy.EQ.1) dimList(3,2) = 1
c          CALL MDSWRITEMETA( metaFName, dataFName,
c    &              filePrec, nDims, dimList, irecord, myIter, myThid )
           CALL MDS_WRITE_META(
     I              metaFName, dataFName, the_run_name, titleLine,
     I              filePrec, nDims, dimList, nFlds,  fldList,
     I              nTimRec, timList, irecord, myIter, myThid )
C End of bi,bj loops
         ENDDO
        ENDDO

C endif useSingleCpuIO or globalFile
       ENDIF

C endif MASTER_CPU_IO
      ENDIF

C---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|

      RETURN
      END

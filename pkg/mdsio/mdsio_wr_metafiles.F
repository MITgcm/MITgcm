C $Header: /u/gcmpack/MITgcm/pkg/mdsio/mdsio_wr_metafiles.F,v 1.8 2013/01/13 22:43:53 jmc Exp $
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
     I   misVal,
     I   irecord,
     I   myIter,
     I   myThid )

C !DESCRIPTION:
C
C MDS_WR_METAFILES creates either a file of the form "fName.meta" IF the
C logical flag "globalFile" or "useSingleCPUIO" are set true. Otherwise
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
C ie. if you write record 2 after record 4 the meta information
C will record the number of records to be 2. This, again, is because
C we have read the meta information. To be fixed.

C     !USES:
      IMPLICIT NONE
C Global variables / COMMON blocks
#include "SIZE.h"
#include "EEPARAMS.h"
#include "PARAMS.h"
#ifdef ALLOW_EXCH2
#include "W2_EXCH2_SIZE.h"
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
C misVal    (real)    :: missing value (ignored if = 1.)
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
      _RL     misVal
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
      INTEGER dimList(3,3), nDims, map2gl(2)
      INTEGER xSize, ySize
      INTEGER tBx, tBy
#ifdef ALLOW_EXCH2
      INTEGER tN
#endif /* ALLOW_EXCH2 */

C---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|
C-    Set dimensions:
      xSize = Nx
      ySize = Ny
#ifdef ALLOW_EXCH2
      IF ( W2_useE2ioLayOut ) THEN
        xSize = exch2_global_Nx
        ySize = exch2_global_Ny
      ENDIF
#endif /* ALLOW_EXCH2 */
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
         map2gl(1) = 0
         map2gl(2) = 1
         CALL MDS_WRITE_META(
     I              metaFName, dataFName, the_run_name, titleLine,
     I              filePrec, nDims,dimList,map2gl, nFlds, fldList,
     I              nTimRec, timList, misVal, irecord, myIter, myThid )

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
           tBx = myXGlobalLo-1 + (bi-1)*sNx
           tBy = myYGlobalLo-1 + (bj-1)*sNy
           map2gl(1) = 0
           map2gl(2) = 1
#ifdef ALLOW_EXCH2
           IF ( W2_useE2ioLayOut ) THEN
             tN = W2_myTileList(bi,bj)
             tBx = exch2_txGlobalo(tN) - 1
             tBy = exch2_tyGlobalo(tN) - 1
             IF (nNx.EQ.0 .AND. nNy.EQ.0) THEN
              IF   ( exch2_mydNx(tN) .GT. xSize ) THEN
C-            face x-size larger than glob-size : fold it
                map2gl(1) = 0
                map2gl(2) = exch2_mydNx(tN) / xSize
              ELSEIF ( exch2_tNy(tN) .GT. ySize ) THEN
C-            tile y-size larger than glob-size : make a long line
                map2gl(1) = exch2_mydNx(tN)
                map2gl(2) = 0
              ELSE
C-            default (face fit into global-IO-array)
                map2gl(1) = 0
                map2gl(2) = 1
              ENDIF
             ENDIF
           ENDIF
#endif /* ALLOW_EXCH2 */
           dimList(1,1) = xSize
           dimList(2,1) = tBx + 1
           dimList(3,1) = tBx + sNx
           dimList(1,2) = ySize
           dimList(2,2) = tBy + 1
           dimList(3,2) = tBy + sNy
           dimList(1,3) = nNz
           dimList(2,3) = 1
           dimList(3,3) = nNz
           nDims=3
           IF (nNz.EQ.1) nDims=2
           IF (nNx.EQ.1) dimList(2,1) = 1
           IF (nNx.EQ.1) dimList(3,1) = 1
           IF (nNy.EQ.1) dimList(2,2) = 1
           IF (nNy.EQ.1) dimList(3,2) = 1
           CALL MDS_WRITE_META(
     I              metaFName, dataFName, the_run_name, titleLine,
     I              filePrec, nDims,dimList,map2gl, nFlds, fldList,
     I              nTimRec, timList, misVal, irecord, myIter, myThid )
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

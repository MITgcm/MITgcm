#include "CPP_EEOPTIONS.h"
CStartOfInterface
      SUBROUTINE INI_PROCS
C     /==========================================================\
C     | SUBROUTINE INI_PROCS                                     |
C     | o Initialise multiple concurrent processes environment.  |
C     |==========================================================|
C     | Under MPI this routine calls various MPI service routines|
C     | that map the model grid to MPI processes. The information|
C     | is then stored in a common block for later use.          |
C     | Note: This routine can also be compiled with CPP         |
C     | directives set so that no multi-processing is initialise.|
C     | This is OK and should work fine.                         |
C     \==========================================================/
      IMPLICIT NONE

C     === Global data ===
#include "SIZE.h"
#include "EEPARAMS.h"
#include "EESUPPORT.h"
CEndOfInterface

C     === Local variables ===
#ifdef ALLOW_USE_MPI
C     msgBuffer        - IO buffer
C     myThid           - Dummy thread id
C     mpiRC            - Error code reporting variable used 
C                        with MPI.
C     mpiGridSpec      - No. of processes in X and Y.
C     mpiPeriodicity   - Flag indicating XY priodicity to MPI.
C     arrElSize        - Size of an array element in bytes used
C                        to define MPI datatypes for communication
C                        operations.
C     arrElSep         - Separation in units of array elements between
C                        blocks to be communicated.
C     elCount          - No. of blocks that are associated with MPI
C                        datatype.
C     elLen            - Length of an MPI datatype in terms of preexisting
C                        datatype.
C     elStride         - Distance between starting location of elements
C                        in an MPI datatype - can be bytes of datatype
C                        units.
      CHARACTER*(MAX_LEN_MBUF) msgBuffer
      INTEGER mpiRC
      INTEGER mpiGridSpec(2)
      INTEGER mpiPeriodicity(2)
      INTEGER mpiLProcNam
      CHARACTER*(MPI_MAX_PROCESSOR_NAME) mpiProcNam
      INTEGER arrElSize
      INTEGER arrElSep
      INTEGER elCount
      INTEGER elLen
      INTEGER elStride
#endif /* ALLOW_USE_MPI */
      INTEGER myThid

C--   Default values set to single processor case
C     pid[W-SE] are the MPI process id of the neighbor
C     processes. A process can be its own neighbor!
      myThid      = 1
      myPid       = 1
      nProcs      = 1
      myPx        = 1
      myPy        = 1
      myXGlobalLo = 1
      myYGlobalLo = 1
      pidW        = 1
      pidE        = 1
      pidN        = 1
      pidS        = 1
      errorMessageUnit    = 0
      standardMessageUnit = 6

#ifdef ALLOW_USE_MPI
C--
C--   MPI style full multiple-process initialisation
C--   ==============================================
#ifndef ALWAYS_USE_MPI
      IF ( usingMPI ) THEN
#endif

C--    Arrange MPI processes on a cartesian grid
C      Set variable indicating which MPI process is to the north,
C      south, east, west, south-west, south-east, north-west
C      and north-east of me e.g.
C
C      Plan view of model domain centered on process ME
C      ================================================
C
C               :         :         :        :    
C               :         :         :        :    
C               :         :         :        :    
C          .....------------------------------.....
C               |         |         |        |    
C               |  NW     |   N     |  NE    |    
C               |         |         |        |    
C          .....------------------------------.....
C               |         |         |        |    
C               |  W      |   ME    |  E     |    
C               |         |         |        |    
C          .....------------------------------.....
C               |         |         |        |    
C               |  SW     |   S     |  SE    |    
C               |         |         |        |    
C          .....------------------------------.....
C               :         :         :        :    
C  Y            :         :         :        :    
C / \           :         :         :        :    
C  |
C  |
C  |----> X
C
C--    Set default MPI communicator to XY processor grid
       myThid        = 1
       mpiGridSpec(1) = nPx
       mpiGridSpec(2) = nPy
C      Could be periodic in X and/or Y - set at run time or compile time!
       mpiPeriodicity(1) = _mpiTRUE_
       mpiPeriodicity(2) = _mpiTRUE_
#ifdef CAN_PREVENT_X_PERIODICITY
#ifndef ALWAYS_PREVENT_X_PERIODICITY
       IF ( notUsingXPeriodicity ) THEN
#endif
        mpiPeriodicity(1) = _mpiFALSE_
#ifndef ALWAYS_PREVENT_X_PERIODICITY
       ENDIF
#endif
#endif /* CAN_PREVENT_X_PERIODICITY */
#ifdef  CAN_PREVENT_Y_PERIODICITY
#ifndef ALWAYS_PREVENT_Y_PERIODICITY
       IF ( notUsingYPeriodicity ) THEN
#endif
        mpiPeriodicity(2) = _mpiFALSE_
#ifndef ALWAYS_PREVENT_Y_PERIODICITY
       ENDIF
#endif
#endif /* CAN_PREVENT_Y_PERIODICITY */

       CALL MPI_CART_CREATE(
     I  MPI_COMM_MODEL,2,mpiGridSpec,mpiPeriodicity,_mpiTRUE_,
     O  mpiComm, mpiRC )
       IF ( mpiRC .NE. MPI_SUCCESS ) THEN
        eeBootError = .TRUE.
        WRITE(msgBuffer,'(A,I5)')
     &        'S/R INI_PROCS: MPI_CART_CREATE return code',
     &        mpiRC 
        CALL PRINT_ERROR( msgBuffer , myThid)
        GOTO 999
       ENDIF

C--    Get my location on the grid
       CALL MPI_CART_COORDS( mpiComm, mpiMyId, 2, mpiGridSpec, mpiRC )
       IF ( mpiRC .NE. MPI_SUCCESS ) THEN
        eeBootError = .TRUE.
        WRITE(msgBuffer,'(A,I5)')
     &        'S/R INI_PROCS: MPI_CART_COORDS return code',
     &        mpiRC 
        CALL PRINT_ERROR( msgBuffer , myThid)
        GOTO 999
       ENDIF
       myPid = mpiMyId
       mpiPx = mpiGridSpec(1)
       mpiPy = mpiGridSpec(2)
       mpiXGlobalLo = 1 + sNx*nSx*(mpiPx)
       mpiYGlobalLo = 1 + sNy*nSy*(mpiPy)
       myXGlobalLo  = mpiXGlobalLo
       myYGlobalLo  = mpiYGlobalLo
       myPx = mpiPx+1
       myPy = mpiPy+1
C--    Get MPI id for neighboring procs.
       mpiGridSpec(1) = mpiPx-1
       IF ( mpiPeriodicity(1) .EQ. _mpiTRUE_
     &   .AND. mpiGridSpec(1) .LT. 0 ) 
     &  mpiGridSpec(1) = nPx-1
       mpiGridSpec(2) = mpiPy
       CALL MPI_CART_RANK( mpiComm, mpiGridSpec, mpiPidW , mpiRC )
       IF ( mpiRC .NE. MPI_SUCCESS ) THEN
        eeBootError = .TRUE.
        WRITE(msgBuffer,'(A,I5)')
     &        'S/R INI_PROCS: MPI_CART_RANK (pidW) return code',
     &        mpiRC 
        CALL PRINT_ERROR( msgBuffer , myThid)
        GOTO 999
       ENDIF
       pidW = mpiPidW
       mpiGridSpec(1) = mpiPx+1
       IF ( mpiPeriodicity(1) .EQ. _mpiTRUE_
     &   .AND. mpiGridSpec(1) .GT. nPx-1 )
     &  mpiGridSpec(1) = 0
       mpiGridSpec(2) = mpiPy
       CALL MPI_CART_RANK( mpiComm, mpiGridSpec, mpiPidE , mpiRC )
       IF ( mpiRC .NE. MPI_SUCCESS ) THEN
        eeBootError = .TRUE.
        WRITE(msgBuffer,'(A,I5)')
     &        'S/R INI_PROCS: MPI_CART_RANK (pidE) return code',
     &        mpiRC 
        CALL PRINT_ERROR( msgBuffer , myThid)
        GOTO 999
       ENDIF
       pidE = mpiPidE
       mpiGridSpec(1) = mpiPx
       mpiGridSpec(2) = mpiPy-1
       IF ( mpiPeriodicity(2) .EQ. _mpiTRUE_
     &   .AND. mpiGridSpec(2) .LT. 0 )
     &  mpiGridSpec(2) = nPy - 1
       CALL MPI_CART_RANK( mpiComm, mpiGridSpec, mpiPidS , mpiRC )
       IF ( mpiRC .NE. MPI_SUCCESS ) THEN
        eeBootError = .TRUE.
        WRITE(msgBuffer,'(A,I5)')
     &        'S/R INI_PROCS: MPI_CART_RANK (pidS) return code',
     &        mpiRC 
        CALL PRINT_ERROR( msgBuffer , myThid)
        GOTO 999
       ENDIF
       pidS = mpiPidS
       mpiGridSpec(1) = mpiPx
       mpiGridSpec(2) = mpiPy+1
       IF ( mpiPeriodicity(2) .EQ. _mpiTRUE_
     &   .AND. mpiGridSpec(2) .GT. nPy-1 )
     &  mpiGridSpec(2) = 0
       CALL MPI_CART_RANK( mpiComm, mpiGridSpec, mpiPidN , mpiRC )
       IF ( mpiRC .NE. MPI_SUCCESS ) THEN
        eeBootError = .TRUE.
        WRITE(msgBuffer,'(A,I5)')
     &        'S/R INI_PROCS: MPI_CART_RANK (pidN) return code',
     &        mpiRC 
        CALL PRINT_ERROR( msgBuffer , myThid)
        GOTO 999
       ENDIF
       pidN = mpiPidN

C--    Print summary of processor mapping on standard output
       CALL MPI_GET_PROCESSOR_NAME( mpiProcNam, mpilProcNam, mpiRC )
       IF ( mpiRC .NE. MPI_SUCCESS ) THEN
        eeBootError = .TRUE.
        WRITE(msgBuffer,'(A,I5)')
     &        'S/R INI_PROCS: MPI_GET_PROCESSOR_NAME return code',
     &        mpiRC 
        CALL PRINT_ERROR( msgBuffer , myThid)
        GOTO 999
       ENDIF
       WRITE(msgBuffer,'(A)')
     &   '======= Starting MPI parallel Run ========='
       CALL PRINT_MESSAGE( msgBuffer, standardMessageUnit, 
     &  SQUEEZE_BOTH , myThid)
       WRITE(msgBuffer,'(A,A64)') ' My Processor Name = ', 
     &  mpiProcNam(1:mpilProcNam)
       CALL PRINT_MESSAGE( msgBuffer, standardMessageUnit, 
     &  SQUEEZE_RIGHT , myThid)
       WRITE(msgBuffer,'(A,I3,A,I3,A,I3,A,I3,A)') ' Located at (',
     &  mpiPx,',',mpiPy,
     &  ') on processor grid (0:',nPx-1,',0:',nPy-1,')'
       CALL PRINT_MESSAGE( msgBuffer, standardMessageUnit, 
     &  SQUEEZE_RIGHT , myThid)
       WRITE(msgBuffer,'(A,I4,A,I4,A,I4,A,I4,A)') ' Origin at  (',
     &  mpiXGlobalLo,',',mpiYGLobalLo,
     &  ') on global grid (1:',nPx*sNx*nSx,',1:',nPy*sNy*nSy,')' 
       CALL PRINT_MESSAGE( msgBuffer, standardMessageUnit, 
     &  SQUEEZE_RIGHT , myThid)
       WRITE(msgBuffer,'(A,I4.4)')
     &   ' North neighbor = processor ', mpiPidN
       CALL PRINT_MESSAGE( msgBuffer, standardMessageUnit, 
     &  SQUEEZE_RIGHT , myThid)
       WRITE(msgBuffer,'(A,I4.4)')
     &   ' South neighbor = processor ', mpiPidS
       CALL PRINT_MESSAGE( msgBuffer, standardMessageUnit, 
     &  SQUEEZE_RIGHT , myThid)
       WRITE(msgBuffer,'(A,I4.4)')
     &   '  East neighbor = processor ', mpiPidE
       CALL PRINT_MESSAGE( msgBuffer, standardMessageUnit, 
     &  SQUEEZE_RIGHT , myThid)
       WRITE(msgBuffer,'(A,I4.4)')
     &   '  West neighbor = processor ', mpiPidW
       CALL PRINT_MESSAGE( msgBuffer, standardMessageUnit, 
     &  SQUEEZE_RIGHT , myThid)
C
C--    Create MPI types for transfer of array edges.
C--    Four and eight byte primitive (one block only) datatypes.
C--    These are common to all threads in the process.
C      Notes:
C      ======
C      1. The datatypes MPI_REAL4 and MPI_REAL8 are usually predefined.
C      If they are not defined code must be added to create them -
C      the MPI standard leaves optional whether they exist.
C      2. Per thread datatypes that handle all the edges for a thread
C      are defined based on the type defined here.
C--
C--    xFace datatypes (east<-->west messages)
C--
C      xFace (y=constant) for XY arrays with real*4 declaration.
       arrElSep  = (sNx+OLx*2)
       elCount   = sNy+OLy*2
       elLen     = OLx
       elStride  = arrElSep
       CALL MPI_TYPE_VECTOR(elCount,elLen,elStride,MPI_REAL4,
     &                       mpiTypeXFaceBlock_xy_r4, mpiRC)
       IF ( mpiRC .NE. MPI_SUCCESS ) THEN
        eeBootError = .TRUE.
        WRITE(msgBuffer,'(A,I5)')
     &   'S/R INI_PROCS: MPI_TYPE_VECTOR (mpiTypeXFaceBlock_xy_r4)',
     &        mpiRC 
        CALL PRINT_ERROR( msgBuffer , myThid)
       ENDIF
       CALL MPI_TYPE_COMMIT( mpiTypeXFaceBlock_xy_r4, mpiRC)
       IF ( mpiRC .NE. MPI_SUCCESS ) THEN
        eeBootError = .TRUE.
        WRITE(msgBuffer,'(A,I5)')
     &   'S/R INI_PROCS: MPI_TYPE_COMMIT (mpiTypeXFaceBlock_xy_r4)',
     &        mpiRC 
        CALL PRINT_ERROR( msgBuffer , myThid)
       ENDIF

C      xFace (y=constant) for XY arrays with real*8 declaration.
       CALL MPI_TYPE_VECTOR(elCount,elLen,elStride,MPI_REAL8,
     &                       mpiTypeXFaceBlock_xy_r8, mpiRC)
       IF ( mpiRC .NE. MPI_SUCCESS ) THEN
        eeBootError = .TRUE.
        WRITE(msgBuffer,'(A,I5)')
     &   'S/R INI_PROCS: MPI_TYPE_VECTOR (mpiTypeXFaceBlock_xy_r8)',
     &        mpiRC 
        CALL PRINT_ERROR( msgBuffer , myThid)
       ENDIF
       CALL MPI_TYPE_COMMIT( mpiTypeXFaceBlock_xy_r8, mpiRC)
       IF ( mpiRC .NE. MPI_SUCCESS ) THEN
        eeBootError = .TRUE.
        WRITE(msgBuffer,'(A,I5)')
     &   'S/R INI_PROCS: MPI_TYPE_COMMIT (mpiTypeXFaceBlock_xy_r8)',
     &        mpiRC 
        CALL PRINT_ERROR( msgBuffer , myThid)
       ENDIF

C      xFace (y=constant) for XYZ arrays with real*4 declaration.
       arrElSize = 4
       arrElSep  = (sNx+OLx*2)*(sNy+OLy*2)
       elCount   = Nr
       elLen     = 1
       elStride  = arrElSize*arrElSep
       CALL MPI_TYPE_HVECTOR(elCount,elLen,elStride,
     &                        mpiTypeXFaceBlock_xy_r4,
     &                       mpiTypeXFaceBlock_xyz_r4, mpiRC)
       IF ( mpiRC .NE. MPI_SUCCESS ) THEN
        eeBootError = .TRUE.
        WRITE(msgBuffer,'(A,I5)')
     &   'S/R INI_PROCS: MPI_TYPE_HVECTOR (mpiTypeXFaceBlock_xyz_r4)',
     &        mpiRC 
        CALL PRINT_ERROR( msgBuffer , myThid)
       ENDIF
       CALL MPI_TYPE_COMMIT( mpiTypeXFaceBlock_xyz_r4, mpiRC)
       IF ( mpiRC .NE. MPI_SUCCESS ) THEN
        eeBootError = .TRUE.
        WRITE(msgBuffer,'(A,I5)')
     &   'S/R INI_PROCS: MPI_TYPE_COMMIT  (mpiTypeXFaceBlock_xyz_r4)',
     &        mpiRC 
        CALL PRINT_ERROR( msgBuffer , myThid)
       ENDIF

C      xFace (y=constant) for XYZ arrays with real*8 declaration.
       arrElSize = 8
       elStride  = arrElSize*arrElSep
       CALL MPI_TYPE_HVECTOR(elCount,elLen,elStride,
     &                        mpiTypeXFaceBlock_xy_r8,
     &                       mpiTypeXFaceBlock_xyz_r8, mpiRC)
       IF ( mpiRC .NE. MPI_SUCCESS ) THEN
        eeBootError = .TRUE.
        WRITE(msgBuffer,'(A,I5)')
     &   'S/R INI_PROCS: MPI_TYPE_HVECTOR (mpiTypeXFaceBlock_xyz_r8)',
     &        mpiRC 
        CALL PRINT_ERROR( msgBuffer , myThid)
       ENDIF
       CALL MPI_TYPE_COMMIT( mpiTypeXFaceBlock_xyz_r8, mpiRC)
       IF ( mpiRC .NE. MPI_SUCCESS ) THEN
        eeBootError = .TRUE.
        WRITE(msgBuffer,'(A,I5)')
     &   'S/R INI_PROCS: MPI_TYPE_COMMIT (mpiTypeXFaceBlock_xyz_r8)',
     &        mpiRC 
        CALL PRINT_ERROR( msgBuffer , myThid)
       ENDIF
C--
C--    yFace datatypes (north<-->south messages)
C--
C      yFace (x=constant) for XY arrays with real*4 declaration
       elCount  = OLy*(sNx+OLx*2)
       CALL MPI_TYPE_CONTIGUOUS(elCount,MPI_REAL4,
     &                          mpiTypeYFaceBlock_xy_r4, mpiRC)
       IF ( mpiRC .NE. MPI_SUCCESS ) THEN
        eeBootError = .TRUE.
        WRITE(msgBuffer,'(A,I5)')
     &   'S/R INI_PROCS: MPI_TYPE_CONTIGUOUS (mpiTypeYFaceBlock_xy_r4)',
     &        mpiRC 
        CALL PRINT_ERROR( msgBuffer , myThid)
       ENDIF
       CALL MPI_TYPE_COMMIT( mpiTypeYFaceBlock_xy_r4, mpiRC)
       IF ( mpiRC .NE. MPI_SUCCESS ) THEN
        eeBootError = .TRUE.
        WRITE(msgBuffer,'(A,I5)')
     &   'S/R INI_PROCS: MPI_TYPE_COMMIT (mpiTypeYFaceBlock_xy_r4)',
     &        mpiRC 
        CALL PRINT_ERROR( msgBuffer , myThid)
       ENDIF
C      yFace (x=constant) for XY arrays with real*8 declaration
       CALL MPI_TYPE_CONTIGUOUS(elCount,MPI_REAL8,
     &                          mpiTypeYFaceBlock_xy_r8, mpiRC)
       IF ( mpiRC .NE. MPI_SUCCESS ) THEN
        eeBootError = .TRUE.
        WRITE(msgBuffer,'(A,I5)')
     &   'S/R INI_PROCS: MPI_TYPE_CONTIGUOUS (mpiTypeYFaceBlock_xy_r8)',
     &        mpiRC 
        CALL PRINT_ERROR( msgBuffer , myThid)
       ENDIF
       CALL MPI_TYPE_COMMIT( mpiTypeYFaceBlock_xy_r8, mpiRC)
       IF ( mpiRC .NE. MPI_SUCCESS ) THEN
        eeBootError = .TRUE.
        WRITE(msgBuffer,'(A,I5)')
     &   'S/R INI_PROCS: MPI_TYPE_COMMIT (mpiTypeYFaceBlock_xy_r8)',
     &        mpiRC 
        CALL PRINT_ERROR( msgBuffer , myThid)
       ENDIF
C      yFace (x=constant) for XYZ arrays with real*4 declaration
       arrElSize = 4
       arrElSep  = (sNx+OLx*2)*(sNy+OLy*2)
       elCount   = Nr
       elLen     = 1
       elStride  = arrElSize*arrElSep
       CALL MPI_TYPE_HVECTOR(elCount,elLen,elStride,
     &                        mpiTypeYFaceBlock_xy_r4,
     &                       mpiTypeYFaceBlock_xyz_r4, mpiRC)
       IF ( mpiRC .NE. MPI_SUCCESS ) THEN
        eeBootError = .TRUE.
        WRITE(msgBuffer,'(A,I5)')
     &   'S/R INI_PROCS: MPI_TYPE_HVECTOR (mpiTypeYFaceBlock_xyz_r4)',
     &        mpiRC 
        CALL PRINT_ERROR( msgBuffer , myThid)
       ENDIF
       CALL MPI_TYPE_COMMIT( mpiTypeYFaceBlock_xyz_r4, mpiRC)
       IF ( mpiRC .NE. MPI_SUCCESS ) THEN
        eeBootError = .TRUE.
        WRITE(msgBuffer,'(A,I5)')
     &   'S/R INI_PROCS: MPI_TYPE_COMMIT (mpiTypeYFaceBlock_xyz_r4)',
     &        mpiRC 
        CALL PRINT_ERROR( msgBuffer , myThid)
       ENDIF
C      yFace (x=constant) for XYZ arrays with real*8 declaration
       arrElSize = 8
       elStride  = arrElSize*arrElSep
       CALL MPI_TYPE_HVECTOR(elCount,elLen,elStride,
     &                        mpiTypeYFaceBlock_xy_r8,
     &                       mpiTypeYFaceBlock_xyz_r8, mpiRC)
       IF ( mpiRC .NE. MPI_SUCCESS ) THEN
        eeBootError = .TRUE.
        WRITE(msgBuffer,'(A,I5)')
     &   'S/R INI_PROCS: MPI_TYPE_HVECTOR (mpiTypeYFaceBlock_xyz_r8)',
     &        mpiRC 
        CALL PRINT_ERROR( msgBuffer , myThid)
       ENDIF
       CALL MPI_TYPE_COMMIT( mpiTypeYFaceBlock_xyz_r8, mpiRC)
       IF ( mpiRC .NE. MPI_SUCCESS ) THEN
        eeBootError = .TRUE.
        WRITE(msgBuffer,'(A,I5)')
     &   'S/R INI_PROCS: MPI_TYPE_COMMIT (mpiTypeYFaceBlock_xyz_r8)',
     &        mpiRC 
        CALL PRINT_ERROR( msgBuffer , myThid)
       ENDIF

C--    Assign MPI values used in generating unique tags for messages.
       mpiTagW    = 1
       mpiTagE    = 2
       mpiTagS    = 3
       mpiTagN    = 4

C
       CALL MPI_Barrier(MPI_COMM_MODEL,mpiRC)


C
#ifndef ALWAYS_USE_MPI
      ENDIF
#endif
#endif /* ALLOW_USE_MPI */

 999  CONTINUE

      RETURN
      END

C $Id: ini_procs.F,v 1.11 1999/05/24 15:19:53 adcroft Exp $

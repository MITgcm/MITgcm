C $Header: /u/gcmpack/MITgcm/model/src/ini_model_io.F,v 1.30 2009/05/13 00:39:20 jmc Exp $
C $Name:  $

#include "PACKAGES_CONFIG.h"
#include "CPP_OPTIONS.h"
#ifdef ALLOW_MDSIO
#include "MDSIO_OPTIONS.h"
#endif /* ALLOW_MDSIO */

CBOP
C     !ROUTINE: INI_MODEL_IO

C     !INTERFACE:
      SUBROUTINE INI_MODEL_IO( myThid )

C     !DESCRIPTION:
C     Pass specific setup data for mdsio/rw.

C     !USES:
      IMPLICIT NONE
#include "SIZE.h"
#include "EEPARAMS.h"
#include "EESUPPORT.h"
#include "PARAMS.h"
#include "RESTART.h"
#ifdef ALLOW_EXCH2
# include "W2_EXCH2_SIZE.h"
# include "W2_EXCH2_TOPOLOGY.h"
#endif /* ALLOW_EXCH2 */
#ifdef ALLOW_MDSIO
# include "MDSIO_SCPU.h"
#endif /* ALLOW_MDSIO */

C     !INPUT/OUTPUT PARAMETERS:
C     myThid :: my Thread Id number
      INTEGER myThid

C     !FUNCTIONS
      INTEGER  ILNBLNK
      EXTERNAL ILNBLNK

C     !LOCAL VARIABLES:
C     msgBuf :: Informational/error meesage buffer
      CHARACTER*(MAX_LEN_MBUF) msgBuf
      CHARACTER*(MAX_LEN_FNAM) namBuf
      INTEGER   iL, pIL
#ifdef ALLOW_EXCH2
      INTEGER xySize
#endif /* ALLOW_EXCH2 */
#ifdef ALLOW_USE_MPI
      INTEGER iG,jG,np
#endif /* ALLOW_USE_MPI */
#ifdef ALLOW_MDSIO
      INTEGER i
#endif /* ALLOW_MDSIO */
CEOP

C-    Safety check:
      IF ( nPx*nPy.NE.1 .AND. globalFiles ) THEN
        _BEGIN_MASTER( myThid )
c       WRITE(msgBuf,'(2A)')
c    &   'INI_MODEL_IO: globalFiles=TRUE is not safe',
c    &   ' in Multi-processors (MPI) run'
c       CALL PRINT_ERROR( msgBuf , myThid)
c       WRITE(msgBuf,'(2A)')
c    &   'INI_MODEL_IO: use instead "useSingleCpuIO=.TRUE."'
c       CALL PRINT_ERROR( msgBuf , myThid)
c       STOP 'ABNORMAL END: S/R INI_MODEL_IO'
C------
C   GlobalFiles option with Multi-processors execution (with MPI) is not
C   safe: dependending on the platform & compiler, it may produce:
C    - incomplete output files (wrong size)
C    - wrong isolated values in some output files
C    - missing tiles (all zeros) in some output files.
C   A safe alternative is to set "useSingleCpuIO=.TRUE." in file "data",
C     namelist PARAM01  (and to keep the default value of globalFiles=FALSE)
C   or if you are really sure that the globalFile works well on our platform
C     & compiler, comment out the above "stop"
C-----
        WRITE(msgBuf,'(2A)')
     &   '** WARNING ** INI_MODEL_IO: globalFiles=TRUE is not safe',
     &   ' in Multi-processors (MPI) run'
        CALL PRINT_MESSAGE( msgBuf, errorMessageUnit,
     &                      SQUEEZE_RIGHT , myThid)
        WRITE(msgBuf,'(2A)') '** WARNING ** INI_MODEL_IO:',
     &   ' use instead "useSingleCpuIO=.TRUE."'
        CALL PRINT_MESSAGE( msgBuf, errorMessageUnit,
     &                      SQUEEZE_RIGHT , myThid)
        _END_MASTER( myThid )
      ENDIF

C-    Check size of IO buffers:
      IF ( useSingleCpuIO ) THEN
c       xySize = Nx*Ny
#if defined(ALLOW_EXCH2) && !defined(MISSING_TILE_IO)
        xySize = exch2_global_Nx*exch2_global_Ny
        IF ( xySize.GT.W2_ioBufferSize ) THEN
          WRITE(msgBuf,'(A,I10)')
     &     'INI_MODEL_IO: W2_ioBufferSize=',W2_ioBufferSize
          CALL PRINT_ERROR( msgBuf, myThid )
          WRITE(msgBuf,'(A,I10,A)')
     &     ' <', xySize,' = Size of Global 2-D map'
          CALL PRINT_ERROR( msgBuf, myThid )
          WRITE(msgBuf,'(2A)') ' Must increase "W2_ioBufferSize"',
     &                         ' in "W2_EXCH2_SIZE.h" + recompile'
          CALL PRINT_ERROR( msgBuf, myThid )
          STOP 'ABNORMAL END: S/R INI_MODEL_IO (buffer size)'
        ENDIF
#else /* defined(ALLOW_EXCH2) && !defined(MISSING_TILE_IO) */
#ifdef ALLOW_USE_MPI
C-  Although using mpi_myX,YGlobalLo is more general, if they happen
C   to be different from iG,jG then mapping into global 2D array is
C   very likely to go wrong ; check and stop.
         DO np = 1,nPx*nPy
          iG = MOD(np-1,nPx)
          jG = (np-1)/nPx
          iG = 1+iG*nSx*sNx
          jG = 1+jG*nSy*sNy
          IF ( mpi_myXGlobalLo(np).NE.iG ) THEN
           WRITE(msgBuf,'(2A,I4,A,I8,A,I8,A)') 'INI_MODEL_IO: ',
     &      'myXGlobalLo(np=',np,')=', mpi_myXGlobalLo(np),
     &       ' <>',iG,' =iGLo'
           CALL PRINT_ERROR( msgBuf, myThid )
          ENDIF
          IF ( mpi_myYGlobalLo(np).NE.jG ) THEN
           WRITE(msgBuf,'(2A,I4,A,I8,A,I8,A)') 'INI_MODEL_IO: ',
     &      'myYGlobalLo(np=',np,')=', mpi_myYGlobalLo(np),
     &       ' <>',jG,' =jGLo'
           CALL PRINT_ERROR( msgBuf, myThid )
          ENDIF
          IF ( mpi_myXGlobalLo(np).NE.iG .OR.
     &         mpi_myYGlobalLo(np).NE.jG ) THEN
           STOP 'ABNORMAL END: S/R INI_MODEL_IO wrong iGLo,jGLo'
          ENDIF
         ENDDO
#endif /* ALLOW_USE_MPI */
#endif /* defined(ALLOW_EXCH2) && !defined(MISSING_TILE_IO) */
      ENDIF
C---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|

C-    Only Master-thread updates IO-parameter in Common blocks:
      _BEGIN_MASTER( myThid )

C-    Initialise AB starting level
C      notes: those could be modified when reading a pickup that does
C             correspond to what is actually needed.
      tempStartAB = nIter0
      saltStartAB = nIter0
      mom_StartAB = nIter0
      nHydStartAB = nIter0
      IF ( startFromPickupAB2 ) tempStartAB = MIN( nIter0 , 1 )
      saltStartAB = tempStartAB
      mom_StartAB = tempStartAB

C-    Initialise Alternating pickup-suffix
      nCheckLev         = 1
      checkPtSuff(1)    = 'ckptA'
      checkPtSuff(2)    = 'ckptB'

C-    Flags specific to RW and MDSIO

C-    now we make local directories with myProcessStr appended
      IF ( mdsioLocalDir .NE. ' ' ) THEN
        iL = ILNBLNK( mdsioLocalDir )
        WRITE(namBuf,'(3A)')
     &         ' mkdir -p ', mdsioLocalDir(1:iL),myProcessStr(1:4)
        pIL = 1 + ILNBLNK( namBuf )
        WRITE(standardMessageUnit,'(3A)')
     &  '==> SYSTEM CALL (from INI_MODEL_IO): >',namBuf(1:pIL),'<'
        CALL SYSTEM( namBuf(1:pIL) )
        namBuf(1:iL) = mdsioLocalDir(1:iL)
        WRITE(mdsioLocalDir,'(3A)') namBuf(1:iL),myProcessStr(1:4),'/'
      ENDIF

C-    Initialise MFLDS variables in common block
      CALL READ_MFLDS_INIT( myThid )

C     Set globalFiles flag for READ_WRITE_FLD package
      CALL SET_WRITE_GLOBAL_FLD( globalFiles )
C     Set globalFiles flag for READ_WRITE_REC package
      CALL SET_WRITE_GLOBAL_REC( globalFiles )
C     Set globalFiles flag for READ_WRITE_PICKUP
      CALL SET_WRITE_GLOBAL_PICKUP( globalFiles )

      _END_MASTER( myThid )
C-    Everyone else must wait for the IO-parameters to be set
      _BARRIER

C-    MDSIO IO-buffers initialisation
#ifdef ALLOW_MDSIO
      IF ( useSingleCpuIO ) THEN
        _BEGIN_MASTER( myThid )
        DO i=1,xyBuffer_size
          xy_buffer_r8(i) = 0. _d 0
          xy_buffer_r4(i) = 0.
        ENDDO
        _END_MASTER( myThid )
      ENDIF
#endif /* ALLOW_MDSIO */

C-    MNC model-io initialisation

#ifdef ALLOW_MNC
      IF (useMNC) THEN

C       Write units/set precision/etc for I/O of variables/arrays
C       belonging to the core dynamical model
        CALL INI_MNC_VARS( myThid )

#ifdef ALLOW_AUTODIFF
        CALL AUTODIFF_INI_MODEL_IO( myThid )
#endif

      ENDIF
#endif /* ALLOW_MNC */

C---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|

      RETURN
      END

C $Header: /u/gcmpack/MITgcm/model/src/Attic/read_write.F,v 1.6 1998/06/10 17:05:59 cnh Exp $
#include "CPP_EEOPTIONS.h"

C--  File read_write.F: Routines to handle mid-level I/O interface.
C--   Contents
C--   o READ_FLD_XY_RL  - Read two-dimensional model _RL field.
C--   o READ_FLD_XYZ_RL - Read three-dimensional model _RL field.
C--   o WRITE_1D_I  - Write list of integer values
C--                   Uses MITgcmUV environment file format.
C--   o WRITE_1D_L  - Write list of logical values
C--                   Uses MITgcmUV environment file format.
C--   o WRITE_1D_R8 - Write list of real*8 values
C--                   Uses MITgcmUV environment file format.
C--   o WRITE_FLD_XY_RL  - Write two-dimensional model _RL field.
C--   o WRITE_FLD_XYZ_RL - Write three-dimensional model _RL field.
C--   o WRITE_STATE - Write out model state.
C--   o WRITE_CHECKPOINT - Write out checkpoint files for restarting.

CStartofinterface
      SUBROUTINE READ_CHECKPOINT ( myIter, myThid )
C     /==========================================================\
C     | SUBROUTINE READ_CHECKPOINT                               |
C     | o Controlling routine for IO to write restart file.      |
C     |==========================================================|
C     | Read model checkpoint files for use in restart.          |
C     \==========================================================/

C     == Global variables ===
#include "SIZE.h"
#include "EEPARAMS.h"
#include "PARAMS.h"
#include "DYNVARS.h"
#include "CG2D.h"

      INTEGER  IO_ERRCOUNT
      EXTERNAL IO_ERRCOUNT

C     == Routine arguments ==
C     myThid - Thread number for this instance of the routine.
C     myIter - Iteration number
      INTEGER myThid
      INTEGER myIter
CEndofinterface

C     == Local variables ==
C     suff - Hold suffix part of a filename
C     beginIOErrCount - Begin and end IO error counts
C     endIOErrCount
C     msgBuf - Error message buffer
      CHARACTER*(MAX_LEN_FNAM) suff
      INTEGER beginIOErrCount
      INTEGER endIOErrCount
      CHARACTER*(MAX_LEN_MBUF) msgBuf
      LOGICAL permCheckPoint  

C--    Going to really do some IO. Make everyone except master thread wait.
       _BARRIER
       _BEGIN_MASTER( myThid )

C--     Set suffix for this set of data files.
        WRITE(suff,'(I10.10)') myIter

C--     Set IO "context" for writing state
        CALL DFILE_SET_RO
        CALL DFILE_SET_CONT_ON_ERROR
C       Force 64-bit IO
        readBinaryPrec = precFloat64


C--     Read IO error counter
        beginIOErrCount = IO_ERRCOUNT(myThid)

C--     Write model fields
C       Raw fields
        CALL READ_FLD_XYZ_RL(   'uVel.',suff,      uVel, myIter, myThid)
        CALL READ_FLD_XYZ_RL(     'gU.',suff,        gU, myIter, myThid)
        CALL READ_FLD_XYZ_RL(  'gUNm1.',suff,     gUNm1, myIter, myThid)
        CALL READ_FLD_XYZ_RL(   'vVel.',suff,      vVel, myIter, myThid)
        CALL READ_FLD_XYZ_RL(     'gV.',suff,        gV, myIter, myThid)
        CALL READ_FLD_XYZ_RL(  'gVNm1.',suff,     gVNm1, myIter, myThid)
        CALL READ_FLD_XYZ_RL(  'theta.',suff,     theta, myIter, myThid)
        CALL READ_FLD_XYZ_RL(     'gT.',suff,        gT, myIter, myThid)
        CALL READ_FLD_XYZ_RL(  'gTNm1.',suff,     gTNm1, myIter, myThid)
        CALL READ_FLD_XYZ_RL(   'salt.',suff,      salt, myIter, myThid)
        CALL READ_FLD_XYZ_RL(     'gS.',suff,        gS, myIter, myThid)
        CALL READ_FLD_XYZ_RL(  'gSNm1.',suff,     gSNm1, myIter, myThid)
        CALL READ_FLD_XY_RL ( 'cg2d_x.',suff,    cg2d_x, myIter, myThid)
#ifdef ALLOW_CD
        CALL READ_FLD_XY_RL ( 'cg2d_xNM1.',suff, cg2d_xNM1, myIter, myThid)
        CALL READ_FLD_XYZ_RL(  'uVelD.',suff,    uVelD, myIter, myThid)
        CALL READ_FLD_XYZ_RL(  'vVelD.',suff,    vVelD, myIter, myThid)
        CALL READ_FLD_XYZ_RL(  'uNM1.', suff,     uNM1, myIter, myThid)
        CALL READ_FLD_XYZ_RL(  'vNM1.', suff,     vNM1, myIter, myThid)
        CALL READ_FLD_XYZ_RL(  'guCD.', suff,     guCD, myIter, myThid)
        CALL READ_FLD_XYZ_RL(  'gvCD.', suff,     gvCD, myIter, myThid)
#endif

C--     Reread IO error counter
        endIOErrCount = IO_ERRCOUNT(myThid)

C--     Check for IO errors
        IF ( endIOErrCount .NE. beginIOErrCount ) THEN
         WRITE(msgBuf,'(A)')  'S/R READ_CHECKPOINT'
         CALL PRINT_ERROR( msgBuf, 1 )
         WRITE(msgBuf,'(A)')  'Error reading in model checkpoint'
         CALL PRINT_ERROR( msgBuf, 1 )
         WRITE(msgBuf,'(A,I10)') 'Timestep ',myIter
         CALL PRINT_ERROR( msgBuf, 1 )
         STOP 'ABNORMAL END: S/R READ_CHECKPOINT'
        ELSE
         WRITE(msgBuf,'(A,I10)')  '// Model checkpoint read, timestep', myIter
         CALL PRINT_MESSAGE( msgBuf, standardMessageUnit, SQUEEZE_RIGHT, 1 )
         WRITE(msgBuf,'(A)')  ' '
         CALL PRINT_MESSAGE( msgBuf, standardMessageUnit, SQUEEZE_RIGHT, 1 )
        ENDIF

       _END_MASTER( myThid )
       _BARRIER

C--    Fill in edge regions
      _EXCH_XYZ_R8(uVel  , myThid )
      _EXCH_XYZ_R8(gu    , myThid )
      _EXCH_XYZ_R8(guNM1 , myThid )
      _EXCH_XYZ_R8(vVel  , myThid )
      _EXCH_XYZ_R8(gv    , myThid )
      _EXCH_XYZ_R8(gvNM1 , myThid )
      _EXCH_XYZ_R8(theta , myThid )
      _EXCH_XYZ_R8(gt    , myThid )
      _EXCH_XYZ_R8(gtNM1 , myThid )
      _EXCH_XYZ_R8(salt  , myThid )
      _EXCH_XYZ_R8(gs    , myThid )
      _EXCH_XYZ_R8(gsNM1 , myThid )
      _EXCH_XY_R8 (cg2d_x, myThid )

      RETURN
      END

CStartofinterface
      SUBROUTINE READ_FLD_XY_RL(  pref ,suff, fld, myIter, myThid)
C     /==========================================================\
C     | SUBROUTINE READ_FLD_XY_RL                                |
C     | o Generic two-dimensional field IO routine.              |
C     |==========================================================|
C     | Call low-level routines to read a 2d model field.        |
C     | Handles _RL type data ( generally _RL == REAL*8 )        |
C     \==========================================================/

C     == Global variables ==
#include "SIZE.h"
#include "EEPARAMS.h"
#include "PARAMS.h"
#include "DFILE.h"
    
      INTEGER  IFNBLNK
      EXTERNAL IFNBLNK
      INTEGER  ILNBLNK
      EXTERNAL ILNBLNK
      INTEGER  IO_ERRCOUNT
      EXTERNAL IO_ERRCOUNT
CEndofinterface

C     == Routine arguments ==
C     pref   - File name prefix
C     suff   - File name suffix
C     fld    - Array to be filled
C     myIter - Timestep number
C     myThid - Thread number calling this routine
      CHARACTER*(*) pref
      CHARACTER*(*) suff
      _RL fld(1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      INTEGER myIter
      INTEGER myThid

C     == Local variables ==
C     fNamData   - Filename building strings
C     fNamMeta   
C     fileHandle - Handle used to refer to an open DFILE file.
C     lFilled    - Used to indicate the number of elements in the
C                  IO buffer that have been filled.
C     nXP, nYp   - Processes domain extents in X and Y.
C     iP, jP, kP - Index in processes coordinates.
C     ib         - Index in IO buffer
C     i, j, k, bi, bj - Loop counters
C     s1Lo, s1Hi, s2Lo, s2Hi - Substring indices
C     nDims, dimList - Local and global dataset dimensions
      CHARACTER*(MAX_LEN_FNAM) fNamData
      CHARACTER*(MAX_LEN_FNAM) fNamMeta
      INTEGER fileHandle
      INTEGER lFilled   
      INTEGER nXP, nYP
      INTEGER iP, jP, kP, ib
      INTEGER i,j, k, bi, bj, iG, jG
      INTEGER s1Lo, s1Hi, s2Lo, s2Hi
      INTEGER nDims
      PARAMETER ( nDims = 2 )
      INTEGER dimList(nDims*3)
      INTEGER beginIOErrCount, endIOErrCount
      CHARACTER*(MAX_LEN_MBUF) msgBuf

C--   Track IO errors
      beginIOErrCount = IO_ERRCOUNT(myThid)

C--   Build file name
C     Name has form 'prefix.suffix'
C     e.g. U.0000000100
C          U.0000000100
      s1Lo = IFNBLNK(pref)
      s1Hi = ILNBLNK(pref)
      s2Lo = IFNBLNK(suff)
      s2Hi = ILNBLNK(suff)
      IF ( suff .EQ. ' ' ) THEN
       WRITE( fNamData, '(A)' )
     & pref(s1Lo:s1Hi)
       WRITE( fNamMeta, '(A)' )
     & pref(s1Lo:s1Hi)
      ELSE
       WRITE( fNamData, '(A,A)' )
     & pref(s1Lo:s1Hi),
     & suff(s2Lo:s2Hi)
       WRITE( fNamMeta, '(A,A)' )
     & pref(s1Lo:s1Hi),
     & suff(s2Lo:s2Hi)
      ENDIF

C--   Open file
      CALL DFILE_OPEN( fNamData, fNamMeta, myThid,
     O                 fileHandle )
      IF ( fileHandle .LE. 0 ) GOTO 1000

C--   Set local and global data extents
      nXP=sNx*nSx
      nYP=sNy*nSy
      lFilled = sNx*nSx*nPx * sNy*nSy*nPy
      dimList(1) = nXP*nPx
      dimList(2) = myXGlobalLo
      dimList(3) = myXGlobalLo+nXP-1
      dimList(4) = nYP*nPy
      dimList(5) = myYGlobalLo
      dimList(6) = myYGlobalLo+nYP-1

C--   Read data
      IF   ( readBinaryPrec .EQ. precFloat32 ) THEN
       CALL DFILE_READ_R4( lFilled,
     I                      fileHandle, myThid )
      ELSE
       CALL DFILE_READ_R8( lFilled,
     I                      fileHandle, myThid )
      ENDIF

C--   Copy data from IO buffer. 
C     Also regrid it to i,j,k indexing.
      IF   ( readBinaryPrec .EQ. precFloat32 ) THEN
       DO bj=1,nSy
        DO bi=1,nSx
         DO j=1,sNy
          DO i=1,sNx
           iP = (bi-1)*sNx+i
           jP = (bj-1)*sNy+j
           iG = myXGlobalLo-1+(bi-1)*sNx+I
           jG = myYGlobalLo-1+(bj-1)*sNy+J
           ib = (jG-1)*nXp*nPx+iG
           fld(i,j,bi,bj) = ioBuf_R4(ib)
          ENDDO
         ENDDO
        ENDDO
       ENDDO
      ELSE
       DO bj=1,nSy
        DO bi=1,nSx
         DO j=1,sNy
          DO i=1,sNx
           iP = (bi-1)*sNx+i
           jP = (bj-1)*sNy+j
           iG = myXGlobalLo-1+(bi-1)*sNx+I
           jG = myYGlobalLo-1+(bj-1)*sNy+J
           ib = (jG-1)*nXp*nPx+iG
           fld(i,j,bi,bj) = ioBuf_R8(ib)
          ENDDO
         ENDDO
        ENDDO
       ENDDO
      ENDIF

C--   Close file
      CALL DFILE_CLOSE( fileHandle, myThid )

C--   Check errors
      endIOerrCount = IO_ERRCOUNT(myThid)
      IF ( endIOErrCount .EQ. beginIOErrCount ) THEN
       WRITE(msgBuf,'(A,A,A,A)')  '// Read file(s) ',
     &  pref(s1Lo:s1Hi),'*',suff(s2Lo:s2Hi)
       CALL PRINT_MESSAGE( msgBuf, standardMessageUnit, SQUEEZE_RIGHT, 1 )
      ELSE
       WRITE(msgBuf,'(A,A,A)') 'Error reading file ',pref(s1Lo:s1Hi),suff(s2Lo:s2Hi)
       CALL PRINT_ERROR( msgBuf, 1 )
      ENDIF

 1000 CONTINUE

      RETURN
      END

CStartofinterface
      SUBROUTINE READ_FLD_XY_RS(  pref ,suff, fld, myIter, myThid)
C     /==========================================================\
C     | SUBROUTINE READ_FLD_XY_RS                                |
C     | o Generic two-dimensional field IO routine.              |
C     |==========================================================|
C     | Call low-level routines to read a 2d model field.        |
C     | Handles _RS type data ( generally _RS == REAL*4 )        |
C     \==========================================================/

C     == Global variables ==
#include "SIZE.h"
#include "EEPARAMS.h"
#include "PARAMS.h"
#include "DFILE.h"
    
      INTEGER  IFNBLNK
      EXTERNAL IFNBLNK
      INTEGER  ILNBLNK
      EXTERNAL ILNBLNK
      INTEGER  IO_ERRCOUNT
      EXTERNAL IO_ERRCOUNT
CEndofinterface

C     == Routine arguments ==
C     pref   - File name prefix
C     suff   - File name suffix
C     fld    - Array to be filled
C     myIter - Timestep number
C     myThid - Thread number calling this routine
      CHARACTER*(*) pref
      CHARACTER*(*) suff
      _RS fld(1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      INTEGER myIter
      INTEGER myThid

C     == Local variables ==
C     fNamData   - Filename building strings
C     fNamMeta   
C     fileHandle - Handle used to refer to an open DFILE file.
C     lFilled    - Used to indicate the number of elements in the
C                  IO buffer that have been filled.
C     nXP, nYp   - Processes domain extents in X and Y.
C     iP, jP, kP - Index in processes coordinates.
C     ib         - Index in IO buffer
C     i, j, k, bi, bj - Loop counters
C     s1Lo, s1Hi, s2Lo, s2Hi - Substring indices
C     nDims, dimList - Local and global dataset dimensions
      CHARACTER*(MAX_LEN_FNAM) fNamData
      CHARACTER*(MAX_LEN_FNAM) fNamMeta
      INTEGER fileHandle
      INTEGER lFilled   
      INTEGER nXP, nYP
      INTEGER iP, jP, kP, ib
      INTEGER i,j, k, bi, bj, iG, jG
      INTEGER s1Lo, s1Hi, s2Lo, s2Hi
      INTEGER nDims
      PARAMETER ( nDims = 2 )
      INTEGER dimList(nDims*3)
      INTEGER beginIOErrCount, endIOErrCount
      CHARACTER*(MAX_LEN_MBUF) msgBuf

C--   Track IO errors
      beginIOErrCount = IO_ERRCOUNT(myThid)

C--   Build file name
C     Name has form 'prefix.suffix'
C     e.g. U.0000000100
C          U.0000000100
      s1Lo = IFNBLNK(pref)
      s1Hi = ILNBLNK(pref)
      s2Lo = IFNBLNK(suff)
      s2Hi = ILNBLNK(suff)
      IF     ( pref .EQ. ' ' ) THEN
       WRITE( fNamData, '(A)' )
     & suff(s2Lo:s2Hi)
       WRITE( fNamMeta, '(A)' )
     & suff(s2Lo:s2Hi)
      ELSEIF ( suff .EQ. ' ' ) THEN
       WRITE( fNamData, '(A)' )
     & pref(s1Lo:s1Hi)
       WRITE( fNamMeta, '(A)' )
     & pref(s1Lo:s1Hi)
      ELSE
       WRITE( fNamData, '(A,A)' )
     & pref(s1Lo:s1Hi),
     & suff(s2Lo:s2Hi)
       WRITE( fNamMeta, '(A,A)' )
     & pref(s1Lo:s1Hi),
     & suff(s2Lo:s2Hi)
      ENDIF

C--   Open file
      CALL DFILE_OPEN( fNamData, fNamMeta, myThid,
     O                 fileHandle )
      IF ( fileHandle .LE. 0 ) GOTO 1000

C--   Set local and global data extents
      nXP=sNx*nSx
      nYP=sNy*nSy
      lFilled = sNx*nSx*nPx * sNy*nSy*nPy
      dimList(1) = nXP*nPx
      dimList(2) = myXGlobalLo
      dimList(3) = myXGlobalLo+nXP-1
      dimList(4) = nYP*nPy
      dimList(5) = myYGlobalLo
      dimList(6) = myYGlobalLo+nYP-1

C--   Read data
      IF   ( readBinaryPrec .EQ. precFloat32 ) THEN
       CALL DFILE_READ_R4( lFilled,
     I                      fileHandle, myThid )
      ELSE
       CALL DFILE_READ_R8( lFilled,
     I                      fileHandle, myThid )
      ENDIF

C--   Copy data from IO buffer. 
C     Also regrid it to i,j,k indexing.
      IF   ( readBinaryPrec .EQ. precFloat32 ) THEN
       DO bj=1,nSy
        DO bi=1,nSx
         DO j=1,sNy
          DO i=1,sNx
           iP = (bi-1)*sNx+i
           jP = (bj-1)*sNy+j
           iG = myXGlobalLo-1+(bi-1)*sNx+I
           jG = myYGlobalLo-1+(bj-1)*sNy+J
           ib = (jG-1)*nXp*nPx+iG
           fld(i,j,bi,bj) = ioBuf_R4(ib)
          ENDDO
         ENDDO
        ENDDO
       ENDDO
      ELSE
       DO bj=1,nSy
        DO bi=1,nSx
         DO j=1,sNy
          DO i=1,sNx
           iP = (bi-1)*sNx+i
           jP = (bj-1)*sNy+j
           iG = myXGlobalLo-1+(bi-1)*sNx+I
           jG = myYGlobalLo-1+(bj-1)*sNy+J
           ib = (jG-1)*nXp*nPx+iG
           fld(i,j,bi,bj) = ioBuf_R8(ib)
          ENDDO
         ENDDO
        ENDDO
       ENDDO
      ENDIF

C--   Close file
      CALL DFILE_CLOSE( fileHandle, myThid )

C--   Check errors
      endIOerrCount = IO_ERRCOUNT(myThid)
      IF ( endIOErrCount .EQ. beginIOErrCount ) THEN
       WRITE(msgBuf,'(A,A,A,A)')  '// Read file(s) ',
     &  pref(s1Lo:s1Hi),'*',suff(s2Lo:s2Hi)
       CALL PRINT_MESSAGE( msgBuf, standardMessageUnit, SQUEEZE_RIGHT, 1 )
      ELSE
       WRITE(msgBuf,'(A,A,A)') 'Error reading file ',pref(s1Lo:s1Hi),suff(s2Lo:s2Hi)
       CALL PRINT_ERROR( msgBuf, 1 )
      ENDIF

 1000 CONTINUE

      RETURN
      END

CStartofinterface
      SUBROUTINE READ_FLD_XYZ_RL(  pref ,suff, fld, myIter, myThid)
C     /==========================================================\
C     | SUBROUTINE READ_FLD_XYZ_RL                               |
C     | o Generic three-dimensional field IO routine.            |
C     |==========================================================|
C     | Call low-level routines to read a 3d model field.        |
C     | Handles _RL type data ( generally _RL == REAL*8 )        |
C     \==========================================================/

C     == Global variables ==
#include "SIZE.h"
#include "EEPARAMS.h"
#include "PARAMS.h"
#include "DFILE.h"
    
      INTEGER  IFNBLNK
      EXTERNAL IFNBLNK
      INTEGER  ILNBLNK
      EXTERNAL ILNBLNK
      INTEGER  IO_ERRCOUNT
      EXTERNAL IO_ERRCOUNT
CEndofinterface

C     == Routine arguments ==
C     pref   - File name prefix
C     suff   - File name suffix
C     fld    - Array to be filled
C     myIter - Timestep number
C     myThid - Thread number calling this routine
      CHARACTER*(*) pref
      CHARACTER*(*) suff
      _RL fld(1-OLx:sNx+OLx,1-OLy:sNy+OLy,nZ,nSx,nSy)
      INTEGER myIter
      INTEGER myThid

C     == Local variables ==
C     fNamData   - Filename building strings
C     fNamMeta   
C     fileHandle - Handle used to refer to an open DFILE file.
C     lFilled    - Used to indicate the number of elements in the
C                  IO buffer that have been filled.
C     nXP, nYp   - Processes domain extents in X and Y.
C     iP, jP, kP - Index in processes coordinates.
C     ib         - Index in IO buffer
C     i, j, k, bi, bj - Loop counters
C     s1Lo, s1Hi, s2Lo, s2Hi - Substring indices
C     nDims, dimList - Local and global dataset dimensions
      CHARACTER*(MAX_LEN_FNAM) fNamData
      CHARACTER*(MAX_LEN_FNAM) fNamMeta
      INTEGER fileHandle
      INTEGER lFilled   
      INTEGER nXP, nYP
      INTEGER iP, jP, kP, ib
      INTEGER i,j, k, bi, bj, iG, jG
      INTEGER s1Lo, s1Hi, s2Lo, s2Hi
      INTEGER nDims
      PARAMETER ( nDims = 3 )
      INTEGER dimList(nDims*3)
      INTEGER beginIOErrCount, endIOErrCount
      CHARACTER*(MAX_LEN_MBUF) msgBuf

C--   Track IO errors
      beginIOErrCount = IO_ERRCOUNT(myThid)

C--   Build file name
C     Name has form 'prefix.suffix'
C     e.g. U.0000000100
C          U.0000000100
      s1Lo = IFNBLNK(pref)
      s1Hi = ILNBLNK(pref)
      s2Lo = IFNBLNK(suff)
      s2Hi = ILNBLNK(suff)
      IF ( suff .EQ. ' ' ) THEN
       WRITE( fNamData, '(A)' )
     & pref(s1Lo:s1Hi)
       WRITE( fNamMeta, '(A)' )
     & pref(s1Lo:s1Hi)
      ELSE
       WRITE( fNamData, '(A,A)' )
     & pref(s1Lo:s1Hi),
     & suff(s2Lo:s2Hi)
       WRITE( fNamMeta, '(A,A)' )
     & pref(s1Lo:s1Hi),
     & suff(s2Lo:s2Hi)
      ENDIF

C--   Open file
      CALL DFILE_OPEN( fNamData, fNamMeta, myThid,
     O                 fileHandle )
      IF ( fileHandle .LE. 0 ) GOTO 1000

C--   Set local and global data extents
      nXP=sNx*nSx
      nYP=sNy*nSy
      lFilled = sNx*nSx*nPx * sNy*nSy*nPy * nZ
      dimList(1) = nXP*nPx
      dimList(2) = myXGlobalLo
      dimList(3) = myXGlobalLo+nXP-1
      dimList(4) = nYP*nPy
      dimList(5) = myYGlobalLo
      dimList(6) = myYGlobalLo+nYP-1
      dimList(7) = nZ
      dimList(8) = 1
      dimList(9) = nZ

C--   Read data
      IF   ( readBinaryPrec .EQ. precFloat32 ) THEN
       CALL DFILE_READ_R4( lFilled,
     I                      fileHandle, myThid )
      ELSE
       CALL DFILE_READ_R8( lFilled,
     I                      fileHandle, myThid )
      ENDIF

C--   Copy data from IO buffer. 
C     Also regrid it to i,j,k indexing.
      IF   ( readBinaryPrec .EQ. precFloat32 ) THEN
       DO bj=1,nSy
        DO bi=1,nSx
         DO K=1,nZ
          DO j=1,sNy
           DO i=1,sNx
            iP = (bi-1)*sNx+i
            jP = (bj-1)*sNy+j
            kP = K
            iG = myXGlobalLo-1+(bi-1)*sNx+I
            jG = myYGlobalLo-1+(bj-1)*sNy+J
            ib = (kP-1)*nXp*nPx*nYp*nPy+(jG-1)*nXp*nPx+iG
            fld(i,j,k,bi,bj) = ioBuf_R4(ib)
           ENDDO
          ENDDO
         ENDDO
        ENDDO
       ENDDO
      ELSE
       DO bj=1,nSy
        DO bi=1,nSx
         DO K=1,nZ
          DO j=1,sNy
           DO i=1,sNx
            iP = (bi-1)*sNx+i
            jP = (bj-1)*sNy+j
            kP = K
            iG = myXGlobalLo-1+(bi-1)*sNx+I
            jG = myYGlobalLo-1+(bj-1)*sNy+J
            ib = (kP-1)*nXp*nPx*nYp*nPy+(jG-1)*nXp*nPx+iG
            fld(i,j,k,bi,bj) = ioBuf_R8(ib)
           ENDDO
          ENDDO
         ENDDO
        ENDDO
       ENDDO
      ENDIF

C--   Close file
      CALL DFILE_CLOSE( fileHandle, myThid )

C--   Check errors
      endIOerrCount = IO_ERRCOUNT(myThid)
      IF ( endIOErrCount .EQ. beginIOErrCount ) THEN
       WRITE(msgBuf,'(A,A,A,A)')  '// Read file(s) ',
     &  pref(s1Lo:s1Hi),'*',suff(s2Lo:s2Hi)
       CALL PRINT_MESSAGE( msgBuf, standardMessageUnit, SQUEEZE_RIGHT, 1 )
      ELSE
       WRITE(msgBuf,'(A,A,A)') 'Error reading file ',pref(s1Lo:s1Hi),suff(s2Lo:s2Hi)
       CALL PRINT_ERROR( msgBuf, 1 )
      ENDIF

 1000 CONTINUE

      RETURN
      END

CStartofinterface
      SUBROUTINE READ_FLD_XYZ_RS(  pref ,suff, fld, myIter, myThid)
C     /==========================================================\
C     | SUBROUTINE READ_FLD_XYZ_RS                               |
C     | o Generic three-dimensional field IO routine.            |
C     |==========================================================|
C     | Call low-level routines to read a 3d model field.        |
C     | Handles _RS type data ( generally _RS == REAL*4 )        |
C     \==========================================================/

C     == Global variables ==
#include "SIZE.h"
#include "EEPARAMS.h"
#include "PARAMS.h"
#include "DFILE.h"
    
      INTEGER  IFNBLNK
      EXTERNAL IFNBLNK
      INTEGER  ILNBLNK
      EXTERNAL ILNBLNK
      INTEGER  IO_ERRCOUNT
      EXTERNAL IO_ERRCOUNT
CEndofinterface

C     == Routine arguments ==
C     pref   - File name prefix
C     suff   - File name suffix
C     fld    - Array to be filled
C     myIter - Timestep number
C     myThid - Thread number calling this routine
      CHARACTER*(*) pref
      CHARACTER*(*) suff
      _RS fld(1-OLx:sNx+OLx,1-OLy:sNy+OLy,nZ,nSx,nSy)
      INTEGER myIter
      INTEGER myThid

C     == Local variables ==
C     fNamData   - Filename building strings
C     fNamMeta   
C     fileHandle - Handle used to refer to an open DFILE file.
C     lFilled    - Used to indicate the number of elements in the
C                  IO buffer that have been filled.
C     nXP, nYp   - Processes domain extents in X and Y.
C     iP, jP, kP - Index in processes coordinates.
C     ib         - Index in IO buffer
C     i, j, k, bi, bj - Loop counters
C     s1Lo, s1Hi, s2Lo, s2Hi - Substring indices
C     nDims, dimList - Local and global dataset dimensions
      CHARACTER*(MAX_LEN_FNAM) fNamData
      CHARACTER*(MAX_LEN_FNAM) fNamMeta
      INTEGER fileHandle
      INTEGER lFilled   
      INTEGER nXP, nYP
      INTEGER iP, jP, kP, ib
      INTEGER i,j, k, bi, bj, iG , jG
      INTEGER s1Lo, s1Hi, s2Lo, s2Hi
      INTEGER nDims
      PARAMETER ( nDims = 3 )
      INTEGER dimList(nDims*3)
      INTEGER beginIOErrCount, endIOErrCount
      CHARACTER*(MAX_LEN_MBUF) msgBuf

C--   Track IO errors
      beginIOErrCount = IO_ERRCOUNT(myThid)

C--   Build file name
C     Name has form 'prefix.suffix'
C     e.g. U.0000000100
C          U.0000000100
      s1Lo = IFNBLNK(pref)
      s1Hi = ILNBLNK(pref)
      s2Lo = IFNBLNK(suff)
      s2Hi = ILNBLNK(suff)
      IF ( suff .EQ. ' ' ) THEN
       WRITE( fNamData, '(A)' )
     & pref(s1Lo:s1Hi)
       WRITE( fNamMeta, '(A)' )
     & pref(s1Lo:s1Hi)
      ELSE
       WRITE( fNamData, '(A,A)' )
     & pref(s1Lo:s1Hi),
     & suff(s2Lo:s2Hi)
       WRITE( fNamMeta, '(A,A)' )
     & pref(s1Lo:s1Hi),
     & suff(s2Lo:s2Hi)
      ENDIF
      
C--   Open file
      CALL DFILE_OPEN( fNamData, fNamMeta, myThid,
     O                 fileHandle )
      IF ( fileHandle .LE. 0 ) GOTO 1000

C--   Set local and global data extents
      nXP=sNx*nSx
      nYP=sNy*nSy
      lFilled = sNx*nSx*nPx * sNy*nSy*nPy * nZ
      dimList(1) = nXP*nPx
      dimList(2) = myXGlobalLo
      dimList(3) = myXGlobalLo+nXP-1
      dimList(4) = nYP*nPy
      dimList(5) = myYGlobalLo
      dimList(6) = myYGlobalLo+nYP-1
      dimList(7) = nZ
      dimList(8) = 1
      dimList(9) = nZ

C--   Read data
      IF   ( readBinaryPrec .EQ. precFloat32 ) THEN
       CALL DFILE_READ_R4( lFilled,
     I                      fileHandle, myThid )
      ELSE
       CALL DFILE_READ_R8( lFilled,
     I                      fileHandle, myThid )
      ENDIF

C--   Copy data from IO buffer. 
C     Also regrid it to i,j,k indexing.
      IF   ( readBinaryPrec .EQ. precFloat32 ) THEN
       DO bj=1,nSy
        DO bi=1,nSx
         DO K=1,nZ
          DO j=1,sNy
           DO i=1,sNx
            iP = (bi-1)*sNx+i
            jP = (bj-1)*sNy+j
            kP = K
            iG = myXGlobalLo-1+(bi-1)*sNx+I
            jG = myYGlobalLo-1+(bj-1)*sNy+J
            ib = (kP-1)*nXp*nPx*nYp*nPy+(jG-1)*nXp*nPx+iG
            fld(i,j,k,bi,bj) = ioBuf_R4(ib)
           ENDDO
          ENDDO
         ENDDO
        ENDDO
       ENDDO
      ELSE
       DO bj=1,nSy
        DO bi=1,nSx
         DO K=1,nZ
          DO j=1,sNy
           DO i=1,sNx
            iP = (bi-1)*sNx+i
            jP = (bj-1)*sNy+j
            kP = K
            iG = myXGlobalLo-1+(bi-1)*sNx+I
            jG = myYGlobalLo-1+(bj-1)*sNy+J
            ib = (kP-1)*nXp*nPx*nYp*nPy+(jG-1)*nXp*nPx+iG
            fld(i,j,k,bi,bj) = ioBuf_R8(ib)
           ENDDO
          ENDDO
         ENDDO
        ENDDO
       ENDDO
      ENDIF

C--   Close file
      CALL DFILE_CLOSE( fileHandle, myThid )

C--   Check errors
      endIOerrCount = IO_ERRCOUNT(myThid)
      IF ( endIOErrCount .EQ. beginIOErrCount ) THEN
       WRITE(msgBuf,'(A,A,A,A)')  '// Read file(s) ',
     &  pref(s1Lo:s1Hi),'*',suff(s2Lo:s2Hi)
       CALL PRINT_MESSAGE( msgBuf, standardMessageUnit, SQUEEZE_RIGHT, 1 )
      ELSE
       WRITE(msgBuf,'(A,A,A)') 'Error reading file ',pref(s1Lo:s1Hi),suff(s2Lo:s2Hi)
       CALL PRINT_ERROR( msgBuf, 1 )
      ENDIF

 1000 CONTINUE

      RETURN
      END

CStartofinterface
      SUBROUTINE WRITE_1D_I( fld, lFld, index_type, head, comment )
C     /==========================================================\
C     | o SUBROUTINE WRITE_1D_I                                  |
C     | Controls formatted, tabular I/O for a one-dimensional    |
C     | INTEGER field.                                           |
C     |==========================================================|
C     | This routine produces a standard format for list         |
C     | one-dimensional INTEGER data in textual form. The format |
C     | is designed to be readily parsed by a post-processing    |
C     | utility.                                                 |
C     \==========================================================/

C     == Global data ==
#include "SIZE.h"
#include "EEPARAMS.h"

C     == Routine arguments ==
C     fld  - Field to be printed
C     lFld - Number of elements in field fld.
C     index_type - Type of index labelling (I=,J=,...) to use
C     head - Statement start e.g. phi =
C     comment - Descriptive comment for field
      INTEGER lFld
      INTEGER fld(lFld)
      INTEGER index_type
      CHARACTER*(*) head
      CHARACTER*(*) comment
CEndofinterface

C     == Local variables ==
      CHARACTER*(MAX_LEN_MBUF) msgBuf

      WRITE(msgBuf,'(A,A)') head, comment
      CALL PRINT_MESSAGE( msgBuf, standardMessageUnit, SQUEEZE_RIGHT , 1)
      CALL PRINT_LIST_I( fld, lFld, index_type, standardMessageUnit )
      WRITE(msgBuf,'(A)')   '    ;     '
      CALL PRINT_MESSAGE( msgBuf, standardMessageUnit, SQUEEZE_RIGHT , 1)
C
      RETURN
      END

CStartofinterface
      SUBROUTINE WRITE_1D_L( fld, lFld, index_type, head, comment )
C     /==========================================================\
C     | o SUBROUTINE WRITE_1D_L                                  |
C     | Controls formatted, tabular I/O for a one-dimensional    |
C     | LOGICAL field.                                           |
C     |==========================================================|
C     | This routine produces a standard format for list         |
C     | one-dimensional LOGICAL data in textual form. The format |
C     | is designed to be readily parsed by a post-processing    |
C     | utility.                                                 |
C     \==========================================================/

C     == Global data ==
#include "SIZE.h"
#include "EEPARAMS.h"

C     == Routine arguments ==
C     fld  - Field to be printed
C     lFld - Number of elements in field fld.
C     index_type - Type of index labelling (I=,J=,...) to use
C     head - Statement start e.g. phi =
C     comment - Descriptive comment for field
      INTEGER lFld
      LOGICAL fld(lFld)
      INTEGER index_type
      CHARACTER*(*) head
      CHARACTER*(*) comment
CEndofinterface

C     == Local variables ==
      CHARACTER*(MAX_LEN_MBUF) msgBuf

      WRITE(msgBuf,'(A,A)') head, comment
      CALL PRINT_MESSAGE( msgBuf, standardMessageUnit, SQUEEZE_RIGHT , 1)
      CALL PRINT_LIST_L( fld, lFld, index_type, standardMessageUnit )
      WRITE(msgBuf,'(A)')   '    ;     '
      CALL PRINT_MESSAGE( msgBuf, standardMessageUnit, SQUEEZE_RIGHT , 1)
C
      RETURN
      END

CStartofinterface
      SUBROUTINE WRITE_1D_R8( fld, lFld, index_type, head, comment )
C     /==========================================================\
C     | o SUBROUTINE WRITE_1D_R8                                 |
C     | Controls formatted, tabular I/O for a one-dimensional    |
C     | real*8 field.                                            |
C     |==========================================================|
C     | This routine produces a standard format for list         |
C     | one-dimensional real*8 data in textual form. The format  |
C     | is designed to be readilya parsed by a post-processing   |
C     | utility.                                                 |
C     \==========================================================/

C     == Global data ==
#include "SIZE.h"
#include "EEPARAMS.h"

C     == Routine arguments ==
C     fld  - Field to be printed
C     lFld - Number of elements in field fld.
C     index_type - Type of index labelling (I=,J=,...) to use
C     head - Statement start e.g. phi =
C     comment - Descriptive comment for field
      INTEGER lFld
      Real*8 fld(lFld)
      INTEGER index_type
      CHARACTER*(*) head
      CHARACTER*(*) comment
CEndofinterface

C     == Local variables ==
      CHARACTER*(MAX_LEN_MBUF) msgBuf

      WRITE(msgBuf,'(A,A)') head, comment
      CALL PRINT_MESSAGE( msgBuf, standardMessageUnit, SQUEEZE_RIGHT , 1)
      CALL PRINT_LIST_R8( fld, lFld, index_type, standardMessageUnit )
      WRITE(msgBuf,'(A)')   '    ;     '
      CALL PRINT_MESSAGE( msgBuf, standardMessageUnit, SQUEEZE_RIGHT , 1)
C
      RETURN
      END

CStartofinterface
      SUBROUTINE WRITE_FLD_XY_RL(  pref ,suff, fld, myIter, myThid)
C     /==========================================================\
C     | SUBROUTINE WRITE_FLD_XY_RL                               |
C     | o Generic two-dimensional field IO routine.              |
C     |==========================================================|
C     | Call low-level routines to write a model 2d model field. |
C     | Handles _RL type data ( generally _RL == REAL*8 )        |
C     \==========================================================/

C     == Global variables ==
#include "SIZE.h"
#include "EEPARAMS.h"
#include "PARAMS.h"
#include "DFILE.h"
    
      INTEGER  IFNBLNK
      EXTERNAL IFNBLNK
      INTEGER  ILNBLNK
      EXTERNAL ILNBLNK
      INTEGER  IO_ERRCOUNT
      EXTERNAL IO_ERRCOUNT
CEndofinterface

C     == Routine arguments ==
C     pref   - File name prefix
C     suff   - File name suffix
C     fld    - Data to be written
C     myIter - Timestep number
C     myThid - Thread number calling this routine
      CHARACTER*(*) pref
      CHARACTER*(*) suff
      _RL fld(1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      INTEGER myIter
      INTEGER myThid

C     == Local variables ==
C     fNamData   - Filename building strings
C     fNamMeta   
C     fileHandle - Handle used to refer to an open DFILE file.
C     lFilled    - Used to indicate the number of elements in the
C                  IO buffer that have been filled.
C     nXP, nYp   - Processes domain extents in X and Y.
C     iP, jP, kP - Index in processes coordinates.
C     ib         - Index in IO buffer
C     i, j, k, bi, bj - Loop counters
C     s1Lo, s1Hi, s2Lo, s2Hi - Substring indices
C     nDims, dimList - Local and global dataset dimensions
      CHARACTER*(MAX_LEN_FNAM) fNamData
      CHARACTER*(MAX_LEN_FNAM) fNamMeta
      INTEGER fileHandle
      INTEGER lFilled   
      INTEGER nXP, nYP
      INTEGER iP, jP, kP, ib
      INTEGER i,j, k, bi, bj
      INTEGER s1Lo, s1Hi, s2Lo, s2Hi
      INTEGER nDims
      PARAMETER ( nDims = 2 )
      INTEGER dimList(nDims*3)
      INTEGER beginIOErrCount, endIOErrCount
      CHARACTER*(MAX_LEN_MBUF) msgBuf

C--   Track IO errors
      beginIOErrCount = IO_ERRCOUNT(myThid)

C--   Build file name
C     Name has form 'prefix.pPID.tTID.class.suffix'
C     e.g. U.p0001.t0001.data.0000000100
C          U.p0001.t0001.meta.0000000100
      s1Lo = IFNBLNK(pref)
      s1Hi = ILNBLNK(pref)
      s2Lo = IFNBLNK(suff)
      s2Hi = ILNBLNK(suff)
      WRITE( fNamData, '(A,A,I4.4,A,I4.4,A,A)' )
     & pref(s1Lo:s1Hi),
     & 'p',myProcId,'.t',myThid, '.data.', 
     & suff(s2Lo:s2Hi)
      WRITE( fNamMeta, '(A,A,I4.4,A,I4.4,A,A)' )
     & pref(s1Lo:s1Hi),
     & 'p',myProcId,'.t',myThid, '.meta.', 
     & suff(s2Lo:s2Hi)

C--   Open file
      CALL DFILE_OPEN( fNamData, fNamMeta, myThid,
     O                 fileHandle )
      IF ( fileHandle .LE. 0 ) GOTO 1000

C--   Copy data to IO buffer. 
C     Also regrid it to i,j,k indexing.
      nXP=sNx*nSx
      nYP=sNy*nSy
      lFilled = sNx*nSx * sNy*nSy
      IF   ( writeBinaryPrec .EQ. precFloat32 ) THEN
       DO bj=1,nSy
        DO bi=1,nSx
         DO j=1,sNy
          DO i=1,sNx
           iP = (bi-1)*sNx+i
           jP = (bj-1)*sNy+j
           ib = (jP-1)*nXP + iP
           ioBuf_R4(ib) = fld(i,j,bi,bj)
          ENDDO
         ENDDO
        ENDDO
       ENDDO
      ELSE
       DO bj=1,nSy
        DO bi=1,nSx
         DO j=1,sNy
          DO i=1,sNx
           iP = (bi-1)*sNx+i
           jP = (bj-1)*sNy+j
           ib = (jP-1)*nXP + iP
           ioBuf_R8(ib) = fld(i,j,bi,bj)
          ENDDO
         ENDDO
        ENDDO
       ENDDO
      ENDIF

C--   Set local and global data extents
      dimList(1) = nXP*nPx
      dimList(2) = myXGlobalLo
      dimList(3) = myXGlobalLo+nXP-1
      dimList(4) = nYP*nPy
      dimList(5) = myYGlobalLo
      dimList(6) = myYGlobalLo+nYP-1

C--   Write data
      IF   ( writeBinaryPrec .EQ. precFloat32 ) THEN
       CALL DFILE_WRITE_R4( lFilled,
     I                      nDims, dimList,
     I                      fileHandle, myIter, myThid )
      ELSE
       CALL DFILE_WRITE_R8( lFilled,
     I                      nDims, dimList,
     I                      fileHandle, myIter, myThid )
      ENDIF

C--   Close file
      CALL DFILE_CLOSE( fileHandle, myThid )

C--   Check errors
      endIOerrCount = IO_ERRCOUNT(myThid)
      IF ( endIOErrCount .EQ. beginIOErrCount ) THEN
       WRITE(msgBuf,'(A,A,A,A)')  '// Wrote file(s) ',
     &  pref(s1Lo:s1Hi),'*',suff(s2Lo:s2Hi)
       CALL PRINT_MESSAGE( msgBuf, standardMessageUnit, SQUEEZE_RIGHT, 1 )
      ELSE
       WRITE(msgBuf,'(A,A,A)') 'Error writing file ',pref(s1Lo:s1Hi),suff(s2Lo:s2Hi)
       CALL PRINT_ERROR( msgBuf, 1 )
      ENDIF

 1000 CONTINUE

      RETURN
      END

CStartofinterface
      SUBROUTINE WRITE_FLD_XYZ_RL(  pref ,suff, fld, myIter, myThid)
C     /==========================================================\
C     | SUBROUTINE WRITE_FLD_XYZ_RL                              |
C     | o Generic three-dimensional field IO routine.            |
C     |==========================================================|
C     | Call low-level routines to write a model 3d model field. |
C     | Handles _RL type data ( generally _RL == REAL*8 )        |
C     \==========================================================/

C     == Global variables ==
#include "SIZE.h"
#include "EEPARAMS.h"
#include "PARAMS.h"
#include "DFILE.h"
    
      INTEGER  IFNBLNK
      EXTERNAL IFNBLNK
      INTEGER  ILNBLNK
      EXTERNAL ILNBLNK
      INTEGER  IO_ERRCOUNT
      EXTERNAL IO_ERRCOUNT
CEndofinterface

C     == Routine arguments ==
C     pref   - File name prefix
C     suff   - File name suffix
C     fld    - Data to be written
C     myIter - Timestep number
C     myThid - Thread number calling this routine
      CHARACTER*(*) pref
      CHARACTER*(*) suff
      _RL fld(1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nz,nSx,nSy)
      INTEGER myThid
      INTEGER myIter

C     == Local variables ==
C     fNamData   - Filename building strings
C     fNamMeta   
C     fileHandle - Handle used to refer to an open DFILE file.
C     lFilled    - Used to indicate the number of elements in the
C                  IO buffer that have been filled.
C     nXP, nYp   - Processes domain extents in X and Y.
C     iP, jP, kP - Index in processes coordinates.
C     ib         - Index in IO buffer
C     i, j, k, bi, bj - Loop counters
C     s1Lo, s1Hi, s2Lo, s2Hi - Substring indices
C     nDims, dimList - Local and global dataset dimensions
      CHARACTER*(MAX_LEN_FNAM) fNamData
      CHARACTER*(MAX_LEN_FNAM) fNamMeta
      INTEGER fileHandle
      INTEGER lFilled   
      INTEGER nXP, nYP
      INTEGER iP, jP, kP, ib
      INTEGER i,j, k, bi, bj
      INTEGER s1Lo, s1Hi, s2Lo, s2Hi
      INTEGER nDims
      PARAMETER ( nDims = 3 )
      INTEGER dimList(nDims*3)
      INTEGER beginIOErrCount, endIOErrCount
      CHARACTER*(MAX_LEN_MBUF) msgBuf

C--   Track IO errors
      beginIOErrCount = IO_ERRCOUNT(myThid)

C--   Build file name
C     Name has form 'prefix.pPID.tTID.class.suffix'
C     e.g. U.p0001.t0001.data.0000000100
C          U.p0001.t0001.meta.0000000100
      s1Lo = IFNBLNK(pref)
      s1Hi = ILNBLNK(pref)
      s2Lo = IFNBLNK(suff)
      s2Hi = ILNBLNK(suff)
      WRITE( fNamData, '(A,A,I4.4,A,I4.4,A,A)' )
     & pref(s1Lo:s1Hi),
     & 'p',myProcId,'.t',myThid, '.data.', 
     & suff(s2Lo:s2Hi)
      WRITE( fNamMeta, '(A,A,I4.4,A,I4.4,A,A)' )
     & pref(s1Lo:s1Hi),
     & 'p',myProcId,'.t',myThid, '.meta.', 
     & suff(s2Lo:s2Hi)

C--   Open file
      CALL DFILE_OPEN( fNamData, fNamMeta, myThid,
     O                 fileHandle )
      IF ( fileHandle .LE. 0 ) GOTO 1000

C--   Copy data to IO buffer. 
C     Also regrid it to i,j,k indexing.
      nXP=sNx*nSx
      nYP=sNy*nSy
      lFilled = sNx*nSx * sNy*nSy * Nz
      IF   ( writeBinaryPrec .EQ. precFloat32 ) THEN
       DO bj=1,nSy
        DO bi=1,nSx
         DO k=1,Nz
          DO j=1,sNy
           DO i=1,sNx
            iP = (bi-1)*sNx+i
            jP = (bj-1)*sNy+j
            kP = k
            ib = (kP-1)*nXP*nYP + (jP-1)*nXP + iP
            ioBuf_R4(ib) = fld(i,j,k,bi,bj)
           ENDDO
          ENDDO
         ENDDO
        ENDDO
       ENDDO
      ELSE
       DO bj=1,nSy
        DO bi=1,nSx
         DO k=1,Nz
          DO j=1,sNy
           DO i=1,sNx
            iP = (bi-1)*sNx+i
            jP = (bj-1)*sNy+j
            kP = k
            ib = (kP-1)*nXP*nYP + (jP-1)*nXP + iP
            ioBuf_R8(ib) = fld(i,j,k,bi,bj)
           ENDDO
          ENDDO
         ENDDO
        ENDDO
       ENDDO
      ENDIF

C--   Set local and global data extents
      dimList(1) = nXP*nPx
      dimList(2) = myXGlobalLo
      dimList(3) = myXGlobalLo+nXP-1
      dimList(4) = nYP*nPy
      dimList(5) = myYGlobalLo
      dimList(6) = myYGlobalLo+nYP-1
      dimList(7) = nZ
      dimList(8) = 1
      dimList(9) = nZ

C--   Write data
      IF   ( writeBinaryPrec .EQ. precFloat32 ) THEN
       CALL DFILE_WRITE_R4( lFilled,
     I                      nDims, dimList,
     I                      fileHandle, myIter, myThid )
      ELSE
       CALL DFILE_WRITE_R8( lFilled,
     I                      nDims, dimList,
     I                      fileHandle, myIter, myThid )
      ENDIF

C--   Close file
      CALL DFILE_CLOSE( fileHandle, myThid )

C--   Check errors
      endIOerrCount = IO_ERRCOUNT(myThid)
      IF ( endIOErrCount .EQ. beginIOErrCount ) THEN
       WRITE(msgBuf,'(A,A,A,A)')  '// Wrote file(s) ',
     &  pref(s1Lo:s1Hi),'*',suff(s2Lo:s2Hi)
       CALL PRINT_MESSAGE( msgBuf, standardMessageUnit, SQUEEZE_RIGHT, 1 )
      ELSE
       WRITE(msgBuf,'(A,A,A)') 'Error writing file ',pref(s1Lo:s1Hi),suff(s2Lo:s2Hi)
       CALL PRINT_ERROR( msgBuf, 1 )
      ENDIF

 1000 CONTINUE

      RETURN
      END

CStartofinterface
      SUBROUTINE WRITE_CHECKPOINT ( modelEnd, myCurrentTime, myIter, myThid )
C     /==========================================================\
C     | SUBROUTINE WRITE_CHECKPOINT                              |
C     | o Controlling routine for IO to write restart file.      |
C     |==========================================================|
C     | Write model checkpoint files for use in restart.         |
C     | This routine writes both "rolling-checkpoint" files      |
C     | and permanent checkpoint files. A rolling checkpoint     |
C     | works through a circular list of suffices. Generally the |
C     | circular list has two entries so that a rolling          |
C     | checkpoint will overwrite the last rolling checkpoint    |
C     | but one. This is useful for running long jobs without    |
C     | filling too much disk space.                             |
C     |  In a permanent checkpoint data is written suffixed by   |
C     | the current timestep number. This sort of checkpoint can |
C     | be used to provided a snap-shot from which the model     |
C     | can be rerun.                                            |
C     \==========================================================/

C     == Global variables ===
#include "SIZE.h"
#include "EEPARAMS.h"
#include "PARAMS.h"
#include "DYNVARS.h"
#include "CG2D.h"

      LOGICAL  DIFFERENT_MULTIPLE
      EXTERNAL DIFFERENT_MULTIPLE
      INTEGER  IO_ERRCOUNT
      EXTERNAL IO_ERRCOUNT

C     == Routine arguments ==
C     modelEnd    - Checkpoint call at end of model run.
C     myThid - Thread number for this instance of the routine.
C     myIter - Iteration number
C     myCurrentTime - Current time of simulation ( s )
      LOGICAL modelEnd    
      INTEGER myThid
      INTEGER myIter
      REAL    myCurrentTime
CEndofinterface

C     == Local variables ==
C     suff - Hold suffix part of a filename
C     beginIOErrCount - Begin and end IO error counts
C     endIOErrCount
C     msgBuf - Error message buffer
C     permCheckPoint - Flag indicating whether a permanent checkpoint will
C                      be written.
      CHARACTER*(MAX_LEN_FNAM) suff
      INTEGER beginIOErrCount
      INTEGER endIOErrCount
      CHARACTER*(MAX_LEN_MBUF) msgBuf
      LOGICAL permCheckPoint  

      permCheckPoint = .FALSE.
      permCheckPoint=
     & DIFFERENT_MULTIPLE(pChkptFreq,myCurrentTime,myCurrentTime-deltaTClock)

      IF (
     &    (.NOT. modelEnd .AND. (
     &     permCheckPoint
     &     .OR.
     &     DIFFERENT_MULTIPLE(chkptFreq,myCurrentTime,myCurrentTime-deltaTClock)
     &     ) 
     &    ) 
     &     .OR.
     &    (
     &     modelEnd 
     &     .AND. .NOT.
     &     permCheckPoint 
     &     .AND. .NOT.
     &     DIFFERENT_MULTIPLE(chkptFreq,myCurrentTime,myCurrentTime-deltaTClock)
     &    )
     & ) THEN

C--    Going to really do some IO. Make everyone except master thread wait.
       _BARRIER
       _BEGIN_MASTER( myThid )

C--     Set suffix for this set of data files.
        suff = checkPtSuff(nCheckLev)
        IF ( permCheckPoint ) THEN
         WRITE(suff,'(I10.10)') myIter
        ENDIF

C--     Set IO "context" for writing state
        CALL DFILE_SET_RW
        CALL DFILE_SET_CONT_ON_ERROR
C       Force 64-bit IO
        writeBinaryPrec = precFloat64


C--     Read IO error counter
        beginIOErrCount = IO_ERRCOUNT(myThid)

C--     Write model fields
C       Raw fields
        CALL WRITE_FLD_XYZ_RL(   'uVel.',suff,      uVel, myIter, myThid)
        CALL WRITE_FLD_XYZ_RL(     'gU.',suff,        gU, myIter, myThid)
        CALL WRITE_FLD_XYZ_RL(  'gUNm1.',suff,     gUNm1, myIter, myThid)
        CALL WRITE_FLD_XYZ_RL(   'vVel.',suff,      vVel, myIter, myThid)
        CALL WRITE_FLD_XYZ_RL(     'gV.',suff,        gV, myIter, myThid)
        CALL WRITE_FLD_XYZ_RL(  'gVNm1.',suff,     gVNm1, myIter, myThid)
        CALL WRITE_FLD_XYZ_RL(  'theta.',suff,     theta, myIter, myThid)
        CALL WRITE_FLD_XYZ_RL(     'gT.',suff,        gT, myIter, myThid)
        CALL WRITE_FLD_XYZ_RL(  'gTNm1.',suff,     gTNm1, myIter, myThid)
        CALL WRITE_FLD_XYZ_RL(   'salt.',suff,      salt, myIter, myThid)
        CALL WRITE_FLD_XYZ_RL(     'gS.',suff,        gS, myIter, myThid)
        CALL WRITE_FLD_XYZ_RL(  'gSNm1.',suff,     gSNm1, myIter, myThid)
        CALL WRITE_FLD_XY_RL ( 'cg2d_x.',suff,    cg2d_x, myIter, myThid)
#ifdef ALLOW_CD
        CALL WRITE_FLD_XY_RL ( 'cg2d_xNM1.',suff, cg2d_xNM1, myIter, myThid)
        CALL WRITE_FLD_XYZ_RL(  'uVelD.',suff,    uVelD, myIter, myThid)
        CALL WRITE_FLD_XYZ_RL(  'vVelD.',suff,    vVelD, myIter, myThid)
        CALL WRITE_FLD_XYZ_RL(  'uNM1.', suff,     uNM1, myIter, myThid)
        CALL WRITE_FLD_XYZ_RL(  'vNM1.', suff,     vNM1, myIter, myThid)
        CALL WRITE_FLD_XYZ_RL(  'guCD.', suff,     guCD, myIter, myThid)
        CALL WRITE_FLD_XYZ_RL(  'gvCD.', suff,     gvCD, myIter, myThid)
#endif


C--     Reread IO error counter
        endIOErrCount = IO_ERRCOUNT(myThid)

C--     Check for IO errors
        IF ( endIOErrCount .NE. beginIOErrCount ) THEN
         WRITE(msgBuf,'(A)')  'S/R WRITE_CHECKPOINT'
         CALL PRINT_ERROR( msgBuf, 1 )
         WRITE(msgBuf,'(A)')  'Error writing out model checkpoint'
         CALL PRINT_ERROR( msgBuf, 1 )
         WRITE(msgBuf,'(A,I10)') 'Timestep ',myIter
         CALL PRINT_ERROR( msgBuf, 1 )
        ELSE
         WRITE(msgBuf,'(A,I10)')  '// Model checkpoint written, timestep', myIter
         CALL PRINT_MESSAGE( msgBuf, standardMessageUnit, SQUEEZE_RIGHT, 1 )
         WRITE(msgBuf,'(A)')  ' '
         CALL PRINT_MESSAGE( msgBuf, standardMessageUnit, SQUEEZE_RIGHT, 1 )
C        Wrote OK so step forward to use next checkpoint in loop.
         IF ( .NOT. permCheckPoint ) THEN
          nCheckLev = MOD(nCheckLev, maxNoChkptLev)+1
         ENDIF
        ENDIF

       _END_MASTER( myThid )
       _BARRIER

      ENDIF

      RETURN
      END

CStartofinterface
      SUBROUTINE WRITE_STATE ( myCurrentTime, myIter, myThid )
C     /==========================================================\
C     | SUBROUTINE WRITE_STATE                                   |
C     | o Controlling routine for IO to dump model state.        |
C     |==========================================================|
C     | Write model state files for post-processing. This file   |
C     | includes code for diagnosing W and RHO for output.       |
C     \==========================================================/

C     == Global variables ===
#include "SIZE.h"
#include "EEPARAMS.h"
#include "PARAMS.h"
#include "DYNVARS.h"
#include "CG2D.h"

      LOGICAL  DIFFERENT_MULTIPLE
      EXTERNAL DIFFERENT_MULTIPLE
      INTEGER  IO_ERRCOUNT
      EXTERNAL IO_ERRCOUNT

C     == Routine arguments ==
C     myThid - Thread number for this instance of the routine.
C     myIter - Iteration number
C     myCurrentTime - Current time of simulation ( s )
      INTEGER myThid
      INTEGER myIter
      REAL    myCurrentTime
CEndofinterface

C     == Local variables ==
C     suff - Hold suffix part of a filename
C     beginIOErrCount - Begin and end IO error counts
C     endIOErrCount
C     msgBuf - Error message buffer
      CHARACTER*(MAX_LEN_FNAM) suff
      INTEGER beginIOErrCount
      INTEGER endIOErrCount
      CHARACTER*(MAX_LEN_MBUF) msgBuf

      IF ( .NOT.
     &  DIFFERENT_MULTIPLE(dumpFreq,myCurrentTime,myCurrentTime-deltaTClock)
     & ) RETURN

C--    Going to really do some IO. Make everyone except master thread wait.
       _BARRIER
       _BEGIN_MASTER( myThid )

C--     Set suffix for this set of data files.
        WRITE(suff,'(I10.10)') myIter

C--     Set IO "context" for writing state
        CALL DFILE_SET_RW
        CALL DFILE_SET_CONT_ON_ERROR
        writeBinaryPrec = writeStatePrec

C--     Read IO error counter
        beginIOErrCount = IO_ERRCOUNT(myThid)

C--     Write model fields
C       Raw fields
        CALL WRITE_FLD_XYZ_RL(  'U.',suff,      uVel, myIter, myThid)
        CALL WRITE_FLD_XYZ_RL(  'V.',suff,      vVel, myIter, myThid)
        CALL WRITE_FLD_XYZ_RL(  'T.',suff,      theta, myIter, myThid)
        CALL WRITE_FLD_XYZ_RL(  'S.',suff,      salt, myIter, myThid)
        CALL WRITE_FLD_XY_RL (  'H.',suff,    cg2d_x, myIter, myThid)
C       Hmmm.... what to do atbout these huh
C       need to calculate them but remember we are already within a 
C       _MASTER section. So we can not use multithreaded code.
C       We can still code as blocked but the block loop will be
C       bj=1,nSy and bi=1,nSx.
C       CALL WRITE_FLD_XYZ_RL(   'W.',suff,   arr3d  , myIter, myThid)
C       CALL WRITE_FLD_XYZ_RL( 'RHO.',suff,   arr3d  , myIter, myThid)
C       CALL WRITE_FLD_XYZ_RL('RHOP.',suff,   arr3d  , myIter, myThid)
C       CALL WRITE_FLD_XYZ_RL(  'PH.',suff,   arr3d  , myIter, myThid)

C--     Reread IO error counter
        endIOErrCount = IO_ERRCOUNT(myThid)

C--     Check for IO errors
        IF ( endIOErrCount .NE. beginIOErrCount ) THEN
         WRITE(msgBuf,'(A)')  'S/R WRITE_STATE'
         CALL PRINT_ERROR( msgBuf, 1 )
         WRITE(msgBuf,'(A)')  'Error writing out model state'
         CALL PRINT_ERROR( msgBuf, 1 )
         WRITE(msgBuf,'(A,I10)') 'Timestep ',myIter
         CALL PRINT_ERROR( msgBuf, 1 )
        ELSE
         WRITE(msgBuf,'(A,I10)')  '// Model state written, timestep', myIter
         CALL PRINT_MESSAGE( msgBuf, standardMessageUnit, SQUEEZE_RIGHT, 1 )
         WRITE(msgBuf,'(A)')  ' '
         CALL PRINT_MESSAGE( msgBuf, standardMessageUnit, SQUEEZE_RIGHT, 1 )
        ENDIF

       _END_MASTER( myThid )
       _BARRIER

      RETURN
      END

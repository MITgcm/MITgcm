C $Header: /u/gcmpack/MITgcm/pkg/autodiff/active_file_control_slice.F,v 1.13 2014/04/04 23:03:59 jmc Exp $
C $Name:  $

#include "AUTODIFF_OPTIONS.h"
#ifdef ALLOW_CTRL
# include "CTRL_OPTIONS.h"
#endif

C     ==================================================================
C     active_file_control_slice.F: Routines to handle the I/O of active
C                                  variables for the adjoint calculations.
C                                  All files are diRect access files.
C     Routines:
C     o  ACTIVE_READ_XZ_RL  : Basic routine to handle active XZ read operations
C     o  ACTIVE_READ_XZ_RS  : Basic routine to handle active XZ read operations
C     o  ACTIVE_READ_YZ_RL  : Basic routine to handle active YZ read operations
C     o  ACTIVE_READ_YZ_RS  : Basic routine to handle active YZ read operations

C     o  ACTIVE_WRITE_XZ_RL : Basic routine to handle active XZ write operations
C     o  ACTIVE_WRITE_XZ_RS : Basic routine to handle active XZ write operations
C     o  ACTIVE_WRITE_YZ_RL : Basic routine to handle active YZ write operations
C     o  ACTIVE_WRITE_YZ_RS : Basic routine to handle active YZ write operations

C     ==================================================================

C---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|
CBOP
C     !ROUTINE: ACTIVE_READ_XZ_RL
C     !INTERFACE:
      SUBROUTINE ACTIVE_READ_XZ_RL(
     I                          activeVar_file,
     O                          active_var,
     I                          globalFile,
     I                          useCurrentDir,
     I                          lAdInit,
     I                          iRec,
     I                          myNr,
     I                          theSimulationMode,
     I                          myOptimIter,
     I                          myThid )
C     !DESCRIPTION: \bv
C     ==================================================================
C     SUBROUTINE ACTIVE_READ_XZ_RL
C     ==================================================================
C     o Read an active XZ _RL variable from file.
C     The variable *globalfile* can be used as a switch, which allows
C     to read from a global file. The adjoint files are, however, always
C     treated as tiled files.
C     started: heimbach@mit.edu 05-Mar-2001
C     ==================================================================
C     SUBROUTINE ACTIVE_READ_XZ_RL
C     ==================================================================
C     \ev

C     !USES:
      IMPLICIT NONE

C     == global variables ==
#include "EEPARAMS.h"
#include "SIZE.h"
#include "PARAMS.h"
#include "ctrl.h"

C     !INPUT/OUTPUT PARAMETERS:
C     activeVar_file :: filename
C     active_var     :: array
C     globalFile     ::
C     useCurrentDir  :: always read from the current directory
C                        (even if "mdsioLocalDir" is set)
C     lAdInit        :: initialisation of corresponding adjoint variable
C                        and write to active file
C     iRec           :: record number
C     myNr           :: vertical array dimension
C     theSimulationMode :: forward mode or reverse mode simulation
C     myOptimIter    :: number of optimization iteration (default: 0)
C     myThid         :: thread number for this instance
      CHARACTER*(*) activeVar_file
      INTEGER  myNr
      _RL      active_var(1-OLx:sNx+OLx,myNr,nSx,nSy)
      LOGICAL  globalFile
      LOGICAL  useCurrentDir
      LOGICAL  lAdInit
      INTEGER  iRec
      INTEGER  theSimulationMode
      INTEGER  myOptimIter
      INTEGER  myThid

C     !FUNCTIONS:
      INTEGER  ILNBLNK
      EXTERNAL ILNBLNK

C     !LOCAL VARIABLES:
      CHARACTER*(2)  adpref
      CHARACTER*(80) adfname
      INTEGER bi,bj
      INTEGER i,k
      INTEGER prec
      INTEGER il
      LOGICAL w_globFile
      _RS  dummyRS(1)
      _RL  active_data_t(1-OLx:sNx+OLx,myNr,nSx,nSy)
CEOP

C     force 64-bit io
      prec = ctrlprec

      adpref = 'ad'
      il = ILNBLNK( activeVar_file )
      WRITE(adfname(1:80),'(80a)') ' '
      WRITE(adfname(1:il+2),'(2A)') adpref, activeVar_file(1:il)

C     >>>>>>>>>>>>>>>>>>> FORWARD RUN <<<<<<<<<<<<<<<<<<<
      IF (theSimulationMode .EQ. FORWARD_SIMULATION) THEN

C     Read the active variable from file.
        CALL MDS_READ_SEC_XZ(
     I                activeVar_file, prec, useCurrentDir,
     I                'RL', myNr,
     O                active_var, dummyRS,
     I                iRec, myThid )

        IF ( lAdInit ) THEN
C     Initialise the corresponding adjoint variable on the
C     adjoint variable file. These files are tiled.

          DO bj = myByLo(myThid), myByHi(myThid)
           DO bi = myBxLo(myThid), myBxHi(myThid)
            DO k = 1, myNr
              DO i=1,sNx
                active_data_t(i,k,bi,bj) = 0. _d 0
              ENDDO
            ENDDO
           ENDDO
          ENDDO

          CALL MDS_WRITE_SEC_XZ(
     I                adfname, prec, globalFile, useCurrentDir,
     I                'RL', myNr,
     I                active_data_t, dummyRS,
     I                iRec, myOptimIter, myThid )

        ENDIF

      ENDIF

C     >>>>>>>>>>>>>>>>>>> ADJOINT RUN <<<<<<<<<<<<<<<<<<<
      IF (theSimulationMode .EQ. REVERSE_SIMULATION) THEN

        CALL MDS_READ_SEC_XZ(
     I                activeVar_file, prec, useCurrentDir,
     I                'RL', myNr,
     O                active_data_t, dummyRS,
     I                iRec, myThid )

C     Add active_var from appropriate location to data.
        DO bj = myByLo(myThid), myByHi(myThid)
         DO bi = myBxLo(myThid), myBxHi(myThid)
          DO k = 1, myNr
            DO i=1,sNx
              active_data_t(i,k,bi,bj) = active_data_t(i,k,bi,bj)
     &                                 + active_var(i,k,bi,bj)
            ENDDO
          ENDDO
         ENDDO
        ENDDO

C     Store the result on disk.
        w_globFile = .FALSE.
        CALL MDS_WRITE_SEC_XZ(
     I                activeVar_file, prec, w_globFile, useCurrentDir,
     I                'RL', myNr,
     I                active_data_t, dummyRS,
     I                iRec, myOptimIter, myThid )

C     Set active_var to zero.
        DO bj = myByLo(myThid), myByHi(myThid)
         DO bi = myBxLo(myThid), myBxHi(myThid)
          DO k = 1, myNr
            DO i=1,sNx
              active_var(i,k,bi,bj) = 0 _d 0
            ENDDO
          ENDDO
         ENDDO
        ENDDO

      ENDIF

C     >>>>>>>>>>>>>>>>>>> TANGENT RUN <<<<<<<<<<<<<<<<<<<
      IF (theSimulationMode .EQ. TANGENT_SIMULATION) THEN
C     Read the active variable from file.
        CALL MDS_READ_SEC_XZ(
     I                activeVar_file, prec, useCurrentDir,
     I                'RL', myNr,
     O                active_var, dummyRS,
     I                iRec, myThid )
      ENDIF

      RETURN
      END

C---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|
CBOP
C     !ROUTINE: ACTIVE_READ_XZ_RS
C     !INTERFACE:
      SUBROUTINE ACTIVE_READ_XZ_RS(
     I                          activeVar_file,
     O                          active_var,
     I                          globalFile,
     I                          useCurrentDir,
     I                          lAdInit,
     I                          iRec,
     I                          myNr,
     I                          theSimulationMode,
     I                          myOptimIter,
     I                          myThid )

C     !DESCRIPTION: \bv
C     ==================================================================
C     SUBROUTINE ACTIVE_READ_XZ_RS
C     ==================================================================
C     o Read an active XZ _RS variable from file.
C     The variable *globalfile* can be used as a switch, which allows
C     to read from a global file. The adjoint files are, however, always
C     treated as tiled files.
C     started: heimbach@mit.edu 05-Mar-2001
C     ==================================================================
C     SUBROUTINE ACTIVE_READ_XZ_RS
C     ==================================================================
C     \ev

C     !USES:
      IMPLICIT NONE

C     == global variables ==
#include "EEPARAMS.h"
#include "SIZE.h"
#include "PARAMS.h"
#include "ctrl.h"

C     !INPUT/OUTPUT PARAMETERS:
C     activeVar_file :: filename
C     active_var     :: array
C     globalFile     ::
C     useCurrentDir  :: always read from the current directory
C                        (even if "mdsioLocalDir" is set)
C     lAdInit        :: initialisation of corresponding adjoint variable
C                        and write to active file
C     iRec           :: record number
C     myNr           :: vertical array dimension
C     theSimulationMode :: forward mode or reverse mode simulation
C     myOptimIter    :: number of optimization iteration (default: 0)
C     myThid         :: thread number for this instance
      CHARACTER*(*) activeVar_file
      INTEGER  myNr
      _RS      active_var(1-OLx:sNx+OLx,myNr,nSx,nSy)
      LOGICAL  globalFile
      LOGICAL  useCurrentDir
      LOGICAL  lAdInit
      INTEGER  iRec
      INTEGER  theSimulationMode
      INTEGER  myOptimIter
      INTEGER  myThid

C     !FUNCTIONS:
      INTEGER  ILNBLNK
      EXTERNAL ILNBLNK

C     !LOCAL VARIABLES:
      CHARACTER*(2)  adpref
      CHARACTER*(80) adfname
      INTEGER bi,bj
      INTEGER i,k
      INTEGER prec
      INTEGER il
      LOGICAL w_globFile
      _RS  active_data_t(1-OLx:sNx+OLx,myNr,nSx,nSy)
      _RL  dummyRL(1)
CEOP

C     force 64-bit io
      prec = ctrlprec

      adpref = 'ad'
      il = ILNBLNK( activeVar_file )
      WRITE(adfname(1:80),'(80a)') ' '
      WRITE(adfname(1:il+2),'(2A)') adpref, activeVar_file(1:il)

C     >>>>>>>>>>>>>>>>>>> FORWARD RUN <<<<<<<<<<<<<<<<<<<
      IF (theSimulationMode .EQ. FORWARD_SIMULATION) THEN

C     Read the active variable from file.
        CALL MDS_READ_SEC_XZ(
     I                activeVar_file, prec, useCurrentDir,
     I                'RS', myNr,
     O                dummyRL, active_var,
     I                iRec, myThid )

        IF ( lAdInit ) THEN
C     Initialise the corresponding adjoint variable on the
C     adjoint variable file. These files are tiled.

          DO bj = myByLo(myThid), myByHi(myThid)
           DO bi = myBxLo(myThid), myBxHi(myThid)
            DO k = 1, myNr
              DO i=1,sNx
                active_data_t(i,k,bi,bj) = 0. _d 0
              ENDDO
            ENDDO
           ENDDO
          ENDDO

          CALL MDS_WRITE_SEC_XZ(
     I                adfname, prec, globalFile, useCurrentDir,
     I                'RS', myNr,
     I                dummyRL, active_data_t,
     I                iRec, myOptimIter, myThid )

        ENDIF

      ENDIF

C     >>>>>>>>>>>>>>>>>>> ADJOINT RUN <<<<<<<<<<<<<<<<<<<
      IF (theSimulationMode .EQ. REVERSE_SIMULATION) THEN

        CALL MDS_READ_SEC_XZ(
     I                activeVar_file, prec, useCurrentDir,
     I                'RS', myNr,
     O                dummyRL, active_data_t,
     I                iRec, myThid )

C     Add active_var from appropriate location to data.
        DO bj = myByLo(myThid), myByHi(myThid)
         DO bi = myBxLo(myThid), myBxHi(myThid)
          DO k = 1, myNr
            DO i=1,sNx
              active_data_t(i,k,bi,bj) = active_data_t(i,k,bi,bj)
     &                                 + active_var(i,k,bi,bj)
            ENDDO
          ENDDO
         ENDDO
        ENDDO

C     Store the result on disk.
        w_globFile = .FALSE.
        CALL MDS_WRITE_SEC_XZ(
     I                activeVar_file, prec, w_globFile, useCurrentDir,
     I                'RS', myNr,
     I                dummyRL, active_data_t,
     I                iRec, myOptimIter, myThid )

C     Set active_var to zero.
        DO bj = myByLo(myThid), myByHi(myThid)
         DO bi = myBxLo(myThid), myBxHi(myThid)
          DO k = 1, myNr
            DO i=1,sNx
              active_var(i,k,bi,bj) = 0 _d 0
            ENDDO
          ENDDO
         ENDDO
        ENDDO

      ENDIF

C     >>>>>>>>>>>>>>>>>>> TANGENT RUN <<<<<<<<<<<<<<<<<<<
      IF (theSimulationMode .EQ. TANGENT_SIMULATION) THEN
C     Read the active variable from file.
        CALL MDS_READ_SEC_XZ(
     I                activeVar_file, prec, useCurrentDir,
     I                'RS', myNr,
     O                dummyRL, active_var,
     I                iRec, myThid )
      ENDIF

      RETURN
      END

C---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|
CBOP
C     !ROUTINE: ACTIVE_READ_YZ_RL
C     !INTERFACE:
      SUBROUTINE ACTIVE_READ_YZ_RL(
     I                          activeVar_file,
     O                          active_var,
     I                          globalFile,
     I                          useCurrentDir,
     I                          lAdInit,
     I                          iRec,
     I                          myNr,
     I                          theSimulationMode,
     I                          myOptimIter,
     I                          myThid )
C     !DESCRIPTION: \bv
C     ==================================================================
C     SUBROUTINE ACTIVE_READ_YZ_RL
C     ==================================================================
C     o Read an active YZ _RL variable from file.
C     The variable *globalfile* can be used as a switch, which allows
C     to read from a global file. The adjoint files are, however, always
C     treated as tiled files.
C     started: heimbach@mit.edu 05-Mar-2001
C     ==================================================================
C     SUBROUTINE ACTIVE_READ_YZ_RL
C     ==================================================================
C     \ev

C     !USES:
      IMPLICIT NONE

C     == global variables ==
#include "EEPARAMS.h"
#include "SIZE.h"
#include "PARAMS.h"
#include "ctrl.h"

C     !INPUT/OUTPUT PARAMETERS:
C     activeVar_file :: filename
C     active_var     :: array
C     globalFile     ::
C     useCurrentDir  :: always read from the current directory
C                        (even if "mdsioLocalDir" is set)
C     lAdInit        :: initialisation of corresponding adjoint variable
C                        and write to active file
C     iRec           :: record number
C     myNr           :: vertical array dimension
C     theSimulationMode :: forward mode or reverse mode simulation
C     myOptimIter    :: number of optimization iteration (default: 0)
C     myThid         :: thread number for this instance
      CHARACTER*(*) activeVar_file
      INTEGER  myNr
      _RL      active_var(1-OLy:sNy+OLy,myNr,nSx,nSy)
      LOGICAL  globalFile
      LOGICAL  useCurrentDir
      LOGICAL  lAdInit
      INTEGER  iRec
      INTEGER  theSimulationMode
      INTEGER  myOptimIter
      INTEGER  myThid

C     !FUNCTIONS:
      INTEGER  ILNBLNK
      EXTERNAL ILNBLNK

C     !LOCAL VARIABLES:
      CHARACTER*(2)  adpref
      CHARACTER*(80) adfname
      INTEGER bi,bj
      INTEGER j,k
      INTEGER prec
      INTEGER il
      LOGICAL w_globFile
      _RS  dummyRS(1)
      _RL  active_data_t(1-OLy:sNy+OLy,myNr,nSx,nSy)
CEOP

C     force 64-bit io
      prec = ctrlprec

      adpref = 'ad'
      il = ILNBLNK( activeVar_file )
      WRITE(adfname(1:80),'(80a)') ' '
      WRITE(adfname(1:il+2),'(2A)') adpref, activeVar_file(1:il)

C     >>>>>>>>>>>>>>>>>>> FORWARD RUN <<<<<<<<<<<<<<<<<<<
      IF (theSimulationMode .EQ. FORWARD_SIMULATION) THEN

C     Read the active variable from file.
        CALL MDS_READ_SEC_YZ(
     I                activeVar_file, prec, useCurrentDir,
     I                'RL', myNr,
     O                active_var, dummyRS,
     I                iRec, myThid )

        IF ( lAdInit ) THEN
C     Initialise the corresponding adjoint variable on the
C     adjoint variable file. These files are tiled.

          DO bj = myByLo(myThid), myByHi(myThid)
           DO bi = myBxLo(myThid), myBxHi(myThid)
            DO k = 1, myNr
             DO j=1,sNy
                active_data_t(j,k,bi,bj) = 0. _d 0
             ENDDO
            ENDDO
           ENDDO
          ENDDO

          CALL MDS_WRITE_SEC_YZ(
     I                adfname, prec, globalFile, useCurrentDir,
     I                'RL', myNr,
     I                active_data_t, dummyRS,
     I                iRec, myOptimIter, myThid )

        ENDIF

      ENDIF

C     >>>>>>>>>>>>>>>>>>> ADJOINT RUN <<<<<<<<<<<<<<<<<<<
      IF (theSimulationMode .EQ. REVERSE_SIMULATION) THEN

        CALL MDS_READ_SEC_YZ(
     I                activeVar_file, prec, useCurrentDir,
     I                'RL', myNr,
     O                active_data_t, dummyRS,
     I                iRec, myThid )

C     Add active_var from appropriate location to data.
        DO bj = myByLo(myThid), myByHi(myThid)
         DO bi = myBxLo(myThid), myBxHi(myThid)
          DO k = 1, myNr
           DO j=1,sNy
              active_data_t(j,k,bi,bj) = active_data_t(j,k,bi,bj)
     &                                 + active_var(j,k,bi,bj)
           ENDDO
          ENDDO
         ENDDO
        ENDDO

C     Store the result on disk.
        w_globFile = .FALSE.
        CALL MDS_WRITE_SEC_YZ(
     I                activeVar_file, prec, w_globFile, useCurrentDir,
     I                'RL', myNr,
     I                active_data_t, dummyRS,
     I                iRec, myOptimIter, myThid )

C     Set active_var to zero.
        DO bj = myByLo(myThid), myByHi(myThid)
         DO bi = myBxLo(myThid), myBxHi(myThid)
          DO k = 1, myNr
           DO j=1,sNy
              active_var(j,k,bi,bj) = 0 _d 0
           ENDDO
          ENDDO
         ENDDO
        ENDDO

      ENDIF

C     >>>>>>>>>>>>>>>>>>> TANGENT RUN <<<<<<<<<<<<<<<<<<<
      IF (theSimulationMode .EQ. TANGENT_SIMULATION) THEN
C     Read the active variable from file.
        CALL MDS_READ_SEC_YZ(
     I                activeVar_file, prec, useCurrentDir,
     I                'RL', myNr,
     O                active_var, dummyRS,
     I                iRec, myThid )
      ENDIF

      RETURN
      END

C---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|
CBOP
C     !ROUTINE: ACTIVE_READ_YZ_RS
C     !INTERFACE:
      SUBROUTINE ACTIVE_READ_YZ_RS(
     I                          activeVar_file,
     O                          active_var,
     I                          globalFile,
     I                          useCurrentDir,
     I                          lAdInit,
     I                          iRec,
     I                          myNr,
     I                          theSimulationMode,
     I                          myOptimIter,
     I                          myThid )

C     !DESCRIPTION: \bv
C     ==================================================================
C     SUBROUTINE ACTIVE_READ_YZ_RS
C     ==================================================================
C     o Read an active YZ _RS variable from file.
C     The variable *globalfile* can be used as a switch, which allows
C     to read from a global file. The adjoint files are, however, always
C     treated as tiled files.
C     started: heimbach@mit.edu 05-Mar-2001
C     ==================================================================
C     SUBROUTINE ACTIVE_READ_YZ_RS
C     ==================================================================
C     \ev

C     !USES:
      IMPLICIT NONE

C     == global variables ==
#include "EEPARAMS.h"
#include "SIZE.h"
#include "PARAMS.h"
#include "ctrl.h"

C     !INPUT/OUTPUT PARAMETERS:
C     activeVar_file :: filename
C     active_var     :: array
C     globalFile     ::
C     useCurrentDir  :: always read from the current directory
C                        (even if "mdsioLocalDir" is set)
C     lAdInit        :: initialisation of corresponding adjoint variable
C                        and write to active file
C     iRec           :: record number
C     myNr           :: vertical array dimension
C     theSimulationMode :: forward mode or reverse mode simulation
C     myOptimIter    :: number of optimization iteration (default: 0)
C     myThid         :: thread number for this instance
      CHARACTER*(*) activeVar_file
      INTEGER  myNr
      _RS      active_var(1-OLy:sNy+OLy,myNr,nSx,nSy)
      LOGICAL  globalFile
      LOGICAL  useCurrentDir
      LOGICAL  lAdInit
      INTEGER  iRec
      INTEGER  theSimulationMode
      INTEGER  myOptimIter
      INTEGER  myThid

C     !FUNCTIONS:
      INTEGER  ILNBLNK
      EXTERNAL ILNBLNK

C     !LOCAL VARIABLES:
      CHARACTER*(2)  adpref
      CHARACTER*(80) adfname
      INTEGER bi,bj
      INTEGER j,k
      INTEGER prec
      INTEGER il
      LOGICAL w_globFile
      _RS  active_data_t(1-OLy:sNy+OLy,myNr,nSx,nSy)
      _RL  dummyRL(1)
CEOP

C     force 64-bit io
      prec = ctrlprec

      adpref = 'ad'
      il = ILNBLNK( activeVar_file )
      WRITE(adfname(1:80),'(80a)') ' '
      WRITE(adfname(1:il+2),'(2A)') adpref, activeVar_file(1:il)

C     >>>>>>>>>>>>>>>>>>> FORWARD RUN <<<<<<<<<<<<<<<<<<<
      IF (theSimulationMode .EQ. FORWARD_SIMULATION) THEN

C     Read the active variable from file.
        CALL MDS_READ_SEC_YZ(
     I                activeVar_file, prec, useCurrentDir,
     I                'RS', myNr,
     O                dummyRL, active_var,
     I                iRec, myThid )

        IF ( lAdInit ) THEN
C     Initialise the corresponding adjoint variable on the
C     adjoint variable file. These files are tiled.

          DO bj = myByLo(myThid), myByHi(myThid)
           DO bi = myBxLo(myThid), myBxHi(myThid)
            DO k = 1, myNr
             DO j=1,sNy
                active_data_t(j,k,bi,bj) = 0. _d 0
             ENDDO
            ENDDO
           ENDDO
          ENDDO

          CALL MDS_WRITE_SEC_YZ(
     I                adfname, prec, globalFile, useCurrentDir,
     I                'RS', myNr,
     I                dummyRL, active_data_t,
     I                iRec, myOptimIter, myThid )

        ENDIF

      ENDIF

C     >>>>>>>>>>>>>>>>>>> ADJOINT RUN <<<<<<<<<<<<<<<<<<<
      IF (theSimulationMode .EQ. REVERSE_SIMULATION) THEN

        CALL MDS_READ_SEC_YZ(
     I                activeVar_file, prec, useCurrentDir,
     I                'RS', myNr,
     O                dummyRL, active_data_t,
     I                iRec, myThid )

C     Add active_var from appropriate location to data.
        DO bj = myByLo(myThid), myByHi(myThid)
         DO bi = myBxLo(myThid), myBxHi(myThid)
          DO k = 1, myNr
           DO j=1,sNy
              active_data_t(j,k,bi,bj) = active_data_t(j,k,bi,bj)
     &                                 + active_var(j,k,bi,bj)
           ENDDO
          ENDDO
         ENDDO
        ENDDO

C     Store the result on disk.
        w_globFile = .FALSE.
        CALL MDS_WRITE_SEC_YZ(
     I                activeVar_file, prec, w_globFile, useCurrentDir,
     I                'RS', myNr,
     I                dummyRL, active_data_t,
     I                iRec, myOptimIter, myThid )

C     Set active_var to zero.
        DO bj = myByLo(myThid), myByHi(myThid)
         DO bi = myBxLo(myThid), myBxHi(myThid)
          DO k = 1, myNr
           DO j=1,sNy
              active_var(j,k,bi,bj) = 0 _d 0
           ENDDO
          ENDDO
         ENDDO
        ENDDO

      ENDIF

C     >>>>>>>>>>>>>>>>>>> TANGENT RUN <<<<<<<<<<<<<<<<<<<
      IF (theSimulationMode .EQ. TANGENT_SIMULATION) THEN
C     Read the active variable from file.
        CALL MDS_READ_SEC_YZ(
     I                activeVar_file, prec, useCurrentDir,
     I                'RS', myNr,
     O                dummyRL, active_var,
     I                iRec, myThid )
      ENDIF

      RETURN
      END

C---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|
CBOP
C     !ROUTINE: ACTIVE_WRITE_XZ_RL
C     !INTERFACE:
      SUBROUTINE ACTIVE_WRITE_XZ_RL(
     I                          activeVar_file,
     I                          active_var,
     I                          globalFile,
     I                          useCurrentDir,
     I                          iRec,
     I                          myNr,
     I                          theSimulationMode,
     I                          myOptimIter,
     I                          myThid )

C     !DESCRIPTION: \bv
C     ==================================================================
C     SUBROUTINE ACTIVE_WRITE_XZ_RL
C     ==================================================================
C     o Write an active XZ _RL variable to a file.
C     started: heimbach@mit.edu 05-Mar-2001
C     ==================================================================
C     SUBROUTINE ACTIVE_WRITE_XZ_RL
C     ==================================================================
C     \ev

C     !USES:
      IMPLICIT NONE

C     == global variables ==
#include "EEPARAMS.h"
#include "SIZE.h"
#include "PARAMS.h"
#include "ctrl.h"

C     !INPUT/OUTPUT PARAMETERS:
C     activeVar_file :: filename
C     active_var     :: array
C     globalFile     ::
C     useCurrentDir  :: always write to the current directory
C                        (even if "mdsioLocalDir" is set)
C     iRec           :: record number
C     myNr           :: vertical array dimension
C     theSimulationMode :: forward mode or reverse mode simulation
C     myOptimIter    :: number of optimization iteration (default: 0)
C     myThid         :: thread number for this instance
      CHARACTER*(*) activeVar_file
      INTEGER  myNr
      _RL      active_var(1-OLx:sNx+OLx,myNr,nSx,nSy)
      LOGICAL  globalFile
      LOGICAL  useCurrentDir
      INTEGER  iRec
      INTEGER  theSimulationMode
      INTEGER  myOptimIter
      INTEGER  myThid

C     !LOCAL VARIABLES:
      INTEGER  i,k
      INTEGER  bi,bj
      INTEGER  prec
      _RS  dummyRS(1)
      _RL  active_data_t(1-OLx:sNx+OLx,myNr,nSx,nSy)
CEOP

C     force 64-bit io
      prec = ctrlprec

C     >>>>>>>>>>>>>>>>>>> FORWARD RUN <<<<<<<<<<<<<<<<<<<
      IF (theSimulationMode .EQ. FORWARD_SIMULATION) THEN
        CALL MDS_WRITE_SEC_XZ(
     I                activeVar_file, prec, globalFile, useCurrentDir,
     I                'RL', myNr,
     I                active_var, dummyRS,
     I                iRec, myOptimIter, myThid )
      ENDIF

C     >>>>>>>>>>>>>>>>>>> ADJOINT RUN <<<<<<<<<<<<<<<<<<<
      IF (theSimulationMode .EQ. REVERSE_SIMULATION) THEN

        CALL MDS_READ_SEC_XZ(
     I                activeVar_file, prec, useCurrentDir,
     I                'RL', myNr,
     O                active_data_t, dummyRS,
     I                iRec, myThid )

C     Add active_var from appropriate location to data.
        DO bj = myByLo(myThid), myByHi(myThid)
         DO bi = myBxLo(myThid), myBxHi(myThid)
          DO k = 1, myNr
            DO i=1,sNx
              active_var(i,k,bi,bj) = active_var(i,k,bi,bj)
     &                              + active_data_t(i,k,bi,bj)
              active_data_t(i,k,bi,bj) = 0. _d 0
            ENDDO
          ENDDO
         ENDDO
        ENDDO
        CALL MDS_WRITE_SEC_XZ(
     I                activeVar_file, prec, globalFile, useCurrentDir,
     I                'RL', myNr,
     I                active_data_t, dummyRS,
     I                iRec, myOptimIter, myThid )

      ENDIF

C     >>>>>>>>>>>>>>>>>>> TANGENT RUN <<<<<<<<<<<<<<<<<<<
      IF (theSimulationMode .EQ. TANGENT_SIMULATION) THEN
        CALL MDS_WRITE_SEC_XZ(
     I                activeVar_file, prec, globalFile, useCurrentDir,
     I                'RL', myNr,
     I                active_var, dummyRS,
     I                iRec, myOptimIter, myThid )
      ENDIF

      RETURN
      END

C---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|
CBOP
C     !ROUTINE: ACTIVE_WRITE_XZ_RS
C     !INTERFACE:
      SUBROUTINE ACTIVE_WRITE_XZ_RS(
     I                          activeVar_file,
     I                          active_var,
     I                          globalFile,
     I                          useCurrentDir,
     I                          iRec,
     I                          myNr,
     I                          theSimulationMode,
     I                          myOptimIter,
     I                          myThid )

C     !DESCRIPTION: \bv
C     ==================================================================
C     SUBROUTINE ACTIVE_WRITE_XZ_RS
C     ==================================================================
C     o Write an active XZ _RS variable to a file.
C     started: heimbach@mit.edu 05-Mar-2001
C     ==================================================================
C     SUBROUTINE ACTIVE_WRITE_XZ_RS
C     ==================================================================
C     \ev

C     !USES:
      IMPLICIT NONE

C     == global variables ==
#include "EEPARAMS.h"
#include "SIZE.h"
#include "PARAMS.h"
#include "ctrl.h"

C     !INPUT/OUTPUT PARAMETERS:
C     activeVar_file :: filename
C     active_var     :: array
C     globalFile     ::
C     useCurrentDir  :: always write to the current directory
C                        (even if "mdsioLocalDir" is set)
C     iRec           :: record number
C     myNr           :: vertical array dimension
C     theSimulationMode :: forward mode or reverse mode simulation
C     myOptimIter    :: number of optimization iteration (default: 0)
C     myThid         :: thread number for this instance
      CHARACTER*(*) activeVar_file
      INTEGER  myNr
      _RS      active_var(1-OLx:sNx+OLx,myNr,nSx,nSy)
      LOGICAL  globalFile
      LOGICAL  useCurrentDir
      INTEGER  iRec
      INTEGER  theSimulationMode
      INTEGER  myOptimIter
      INTEGER  myThid

C     !LOCAL VARIABLES:
      INTEGER  i,k
      INTEGER  bi,bj
      INTEGER  prec
      _RS  active_data_t(1-OLx:sNx+OLx,myNr,nSx,nSy)
      _RL  dummyRL(1)
CEOP

C     force 64-bit io
      prec = ctrlprec

C     >>>>>>>>>>>>>>>>>>> FORWARD RUN <<<<<<<<<<<<<<<<<<<
      IF (theSimulationMode .EQ. FORWARD_SIMULATION) THEN
        CALL MDS_WRITE_SEC_XZ(
     I                activeVar_file, prec, globalFile, useCurrentDir,
     I                'RS', myNr,
     I                dummyRL, active_var,
     I                iRec, myOptimIter, myThid )
      ENDIF

C     >>>>>>>>>>>>>>>>>>> ADJOINT RUN <<<<<<<<<<<<<<<<<<<
      IF (theSimulationMode .EQ. REVERSE_SIMULATION) THEN

        CALL MDS_READ_SEC_XZ(
     I                activeVar_file, prec, useCurrentDir,
     I                'RS', myNr,
     O                dummyRL, active_data_t,
     I                iRec, myThid )

C     Add active_var from appropriate location to data.
        DO bj = myByLo(myThid), myByHi(myThid)
         DO bi = myBxLo(myThid), myBxHi(myThid)
          DO k = 1, myNr
            DO i=1,sNx
              active_var(i,k,bi,bj) = active_var(i,k,bi,bj)
     &                              + active_data_t(i,k,bi,bj)
              active_data_t(i,k,bi,bj) = 0. _d 0
            ENDDO
          ENDDO
         ENDDO
        ENDDO
        CALL MDS_WRITE_SEC_XZ(
     I                activeVar_file, prec, globalFile, useCurrentDir,
     I                'RS', myNr,
     I                dummyRL, active_data_t,
     I                iRec, myOptimIter, myThid )

      ENDIF

C     >>>>>>>>>>>>>>>>>>> TANGENT RUN <<<<<<<<<<<<<<<<<<<
      IF (theSimulationMode .EQ. TANGENT_SIMULATION) THEN
        CALL MDS_WRITE_SEC_XZ(
     I                activeVar_file, prec, globalFile, useCurrentDir,
     I                'RS', myNr,
     I                dummyRL, active_var,
     I                iRec, myOptimIter, myThid )
      ENDIF

      RETURN
      END

C---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|
CBOP
C     !ROUTINE: ACTIVE_WRITE_YZ_RL
C     !INTERFACE:
      SUBROUTINE ACTIVE_WRITE_YZ_RL(
     I                          activeVar_file,
     I                          active_var,
     I                          globalFile,
     I                          useCurrentDir,
     I                          iRec,
     I                          myNr,
     I                          theSimulationMode,
     I                          myOptimIter,
     I                          myThid )

C     !DESCRIPTION: \bv
C     ==================================================================
C     SUBROUTINE ACTIVE_WRITE_YZ_RL
C     ==================================================================
C     o Write an active YZ _RL variable to a file.
C     started: heimbach@mit.edu 05-Mar-2001
C     ==================================================================
C     SUBROUTINE ACTIVE_WRITE_YZ_RL
C     ==================================================================
C     \ev

C     !USES:
      IMPLICIT NONE

C     == global variables ==
#include "EEPARAMS.h"
#include "SIZE.h"
#include "PARAMS.h"
#include "ctrl.h"

C     !INPUT/OUTPUT PARAMETERS:
C     activeVar_file :: filename
C     active_var     :: array
C     globalFile     ::
C     useCurrentDir  :: always write to the current directory
C                        (even if "mdsioLocalDir" is set)
C     iRec           :: record number
C     myNr           :: vertical array dimension
C     theSimulationMode :: forward mode or reverse mode simulation
C     myOptimIter    :: number of optimization iteration (default: 0)
C     myThid         :: thread number for this instance
      CHARACTER*(*) activeVar_file
      INTEGER  myNr
      _RL      active_var(1-OLy:sNy+OLy,myNr,nSx,nSy)
      LOGICAL  globalFile
      LOGICAL  useCurrentDir
      INTEGER  iRec
      INTEGER  theSimulationMode
      INTEGER  myOptimIter
      INTEGER  myThid

C     !LOCAL VARIABLES:
      INTEGER  j,k
      INTEGER  bi,bj
      INTEGER  prec
      _RS  dummyRS(1)
      _RL  active_data_t(1-OLy:sNy+OLy,myNr,nSx,nSy)
CEOP

C     force 64-bit io
      prec = ctrlprec

C     >>>>>>>>>>>>>>>>>>> FORWARD RUN <<<<<<<<<<<<<<<<<<<
      IF (theSimulationMode .EQ. FORWARD_SIMULATION) THEN
        CALL MDS_WRITE_SEC_YZ(
     I                activeVar_file, prec, globalFile, useCurrentDir,
     I                'RL', myNr,
     I                active_var, dummyRS,
     I                iRec, myOptimIter, myThid )
      ENDIF

C     >>>>>>>>>>>>>>>>>>> ADJOINT RUN <<<<<<<<<<<<<<<<<<<
      IF (theSimulationMode .EQ. REVERSE_SIMULATION) THEN

        CALL MDS_READ_SEC_YZ(
     I                activeVar_file, prec, useCurrentDir,
     I                'RL', myNr,
     O                active_data_t, dummyRS,
     I                iRec, myThid )

C     Add active_var from appropriate location to data.
        DO bj = myByLo(myThid), myByHi(myThid)
         DO bi = myBxLo(myThid), myBxHi(myThid)
          DO k = 1, myNr
           DO j=1,sNy
              active_var(j,k,bi,bj) = active_var(j,k,bi,bj)
     &                              + active_data_t(j,k,bi,bj)
              active_data_t(j,k,bi,bj) = 0. _d 0
           ENDDO
          ENDDO
         ENDDO
        ENDDO
        CALL MDS_WRITE_SEC_YZ(
     I                activeVar_file, prec, globalFile, useCurrentDir,
     I                'RL', myNr,
     I                active_data_t, dummyRS,
     I                iRec, myOptimIter, myThid )

      ENDIF

C     >>>>>>>>>>>>>>>>>>> TANGENT RUN <<<<<<<<<<<<<<<<<<<
      IF (theSimulationMode .EQ. TANGENT_SIMULATION) THEN
        CALL MDS_WRITE_SEC_YZ(
     I                activeVar_file, prec, globalFile, useCurrentDir,
     I                'RL', myNr,
     I                active_var, dummyRS,
     I                iRec, myOptimIter, myThid )
      ENDIF

      RETURN
      END

C---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|
CBOP
C     !ROUTINE: ACTIVE_WRITE_YZ_RS
C     !INTERFACE:
      SUBROUTINE ACTIVE_WRITE_YZ_RS(
     I                          activeVar_file,
     I                          active_var,
     I                          globalFile,
     I                          useCurrentDir,
     I                          iRec,
     I                          myNr,
     I                          theSimulationMode,
     I                          myOptimIter,
     I                          myThid )

C     !DESCRIPTION: \bv
C     ==================================================================
C     SUBROUTINE ACTIVE_WRITE_YZ_RS
C     ==================================================================
C     o Write an active YZ _RS variable to a file.
C     started: heimbach@mit.edu 05-Mar-2001
C     ==================================================================
C     SUBROUTINE ACTIVE_WRITE_YZ_RS
C     ==================================================================
C     \ev

C     !USES:
      IMPLICIT NONE

C     == global variables ==
#include "EEPARAMS.h"
#include "SIZE.h"
#include "PARAMS.h"
#include "ctrl.h"

C     !INPUT/OUTPUT PARAMETERS:
C     activeVar_file :: filename
C     active_var     :: array
C     globalFile     ::
C     useCurrentDir  :: always write to the current directory
C                        (even if "mdsioLocalDir" is set)
C     iRec           :: record number
C     myNr           :: vertical array dimension
C     theSimulationMode :: forward mode or reverse mode simulation
C     myOptimIter    :: number of optimization iteration (default: 0)
C     myThid         :: thread number for this instance
      CHARACTER*(*) activeVar_file
      INTEGER  myNr
      _RS      active_var(1-OLy:sNy+OLy,myNr,nSx,nSy)
      LOGICAL  globalFile
      LOGICAL  useCurrentDir
      INTEGER  iRec
      INTEGER  theSimulationMode
      INTEGER  myOptimIter
      INTEGER  myThid

C     !LOCAL VARIABLES:
      INTEGER  j,k
      INTEGER  bi,bj
      INTEGER  prec
      _RS  active_data_t(1-OLy:sNy+OLy,myNr,nSx,nSy)
      _RL  dummyRL(1)
CEOP

C     force 64-bit io
      prec = ctrlprec

C     >>>>>>>>>>>>>>>>>>> FORWARD RUN <<<<<<<<<<<<<<<<<<<
      IF (theSimulationMode .EQ. FORWARD_SIMULATION) THEN
        CALL MDS_WRITE_SEC_YZ(
     I                activeVar_file, prec, globalFile, useCurrentDir,
     I                'RS', myNr,
     I                dummyRL, active_var,
     I                iRec, myOptimIter, myThid )
      ENDIF

C     >>>>>>>>>>>>>>>>>>> ADJOINT RUN <<<<<<<<<<<<<<<<<<<
      IF (theSimulationMode .EQ. REVERSE_SIMULATION) THEN

        CALL MDS_READ_SEC_YZ(
     I                activeVar_file, prec, useCurrentDir,
     I                'RS', myNr,
     O                dummyRL, active_data_t,
     I                iRec, myThid )

C     Add active_var from appropriate location to data.
        DO bj = myByLo(myThid), myByHi(myThid)
         DO bi = myBxLo(myThid), myBxHi(myThid)
          DO k = 1, myNr
           DO j=1,sNy
              active_var(j,k,bi,bj) = active_var(j,k,bi,bj)
     &                              + active_data_t(j,k,bi,bj)
              active_data_t(j,k,bi,bj) = 0. _d 0
           ENDDO
          ENDDO
         ENDDO
        ENDDO
        CALL MDS_WRITE_SEC_YZ(
     I                activeVar_file, prec, globalFile, useCurrentDir,
     I                'RS', myNr,
     I                dummyRL, active_data_t,
     I                iRec, myOptimIter, myThid )

      ENDIF

C     >>>>>>>>>>>>>>>>>>> TANGENT RUN <<<<<<<<<<<<<<<<<<<
      IF (theSimulationMode .EQ. TANGENT_SIMULATION) THEN
        CALL MDS_WRITE_SEC_YZ(
     I                activeVar_file, prec, globalFile, useCurrentDir,
     I                'RS', myNr,
     I                dummyRL, active_var,
     I                iRec, myOptimIter, myThid )
      ENDIF

      RETURN
      END

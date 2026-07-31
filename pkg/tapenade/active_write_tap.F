#include "CPP_EEOPTIONS.h"
C--   File active_write_tap.F: Active write with Tapenade.
C--    Contents:
C--    o ACTIVE_WRITE_XY_D
C--    o ACTIVE_WRITE_XYZ_D
C--    o ACTIVE_WRITE_XY_B
C--    o ACTIVE_WRITE_XYZ_B
C--    o ACTIVE_WRITE_XY_BWD (for use without checkpointing)
C--    o ACTIVE_WRITE_XY_FWD (for use without checkpointing)

C---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|

CBOP
C     !ROUTINE: ACTIVE_WRITE_XY_D
C     !INTERFACE:
      SUBROUTINE ACTIVE_WRITE_XY_D(
     I                            active_var_file,
     I                            active_var,
     I                            active_vard,
     I                            iRec,
     I                            myOptimIter,
     I                            myThid,
     I                            dummy,
     I                            dummyd
     &                          )

C     *==========================================================*
C     | SUBROUTINE ACTIVE_WRITE_XY_D
C     | o Forward differentiation of ACTIVE_WRITE_XY.
C     *==========================================================*
C     | written by Shreyas Gaikwad, Laurent Hascoet in Nov 2022
C     *==========================================================*
C     \ev

C     !USES:
      IMPLICIT NONE
C     == global variables ==
#include "SIZE.h"

C     !INPUT/OUTPUT PARAMETERS:
C     active_var_file:: filename
C     active_var     :: array
C     active_vard    :: forward derivative of active_var
C     iRec           :: record number
C     myOptimIter    :: number of optimization iteration (default: 0)
C     myThid         :: thread number for this instance
      CHARACTER*(*) active_var_file
      Real*8     active_var(1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      Real*8     active_vard(1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      INTEGER iRec
      INTEGER myOptimIter
      INTEGER myThid
      Real*8     dummy
      Real*8     dummyd

C     !LOCAL VARIABLES:
      CHARACTER*(2) pref
      CHARACTER*(80) fname
      INTEGER myNr
      LOGICAL globalFile
      LOGICAL useCurrentDir
CEOP

      pref = 'g_'
      CALL ADD_PREFIX( pref, active_var_file, fname )
      myNr = 1
      globalFile = .FALSE.
      useCurrentDir = .FALSE.

      CALL ACTIVE_WRITE_3D_RL(
     &                 active_var_file, active_var, globalFile,
     &                 useCurrentDir, iRec, myNr,
     &                 0, myOptimIter, myThid )

      CALL ACTIVE_WRITE_3D_RL(
     &                 fname, active_vard, globalFile,
     &                 useCurrentDir, iRec, myNr,
     &                 2, myOptimIter, myThid )

      RETURN
      END

C---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|

CBOP
C     !ROUTINE: ACTIVE_WRITE_XYZ_D
C     !INTERFACE:
      SUBROUTINE ACTIVE_WRITE_XYZ_D(
     I                            active_var_file,
     I                            active_var,
     I                            active_vard,
     I                            iRec,
     I                            myOptimIter,
     I                            myThid,
     I                            dummy,
     I                            dummyd
     &                          )

C     *==========================================================*
C     | SUBROUTINE ACTIVE_WRITE_XYZ_D
C     | o Forward differentiation of ACTIVE_WRITE_XYZ.
C     *==========================================================*
C     | written by Shreyas Gaikwad, Laurent Hascoet in Nov 2022
C     *==========================================================*
C     \ev

C     !USES:
      IMPLICIT NONE
C     == global variables ==
#include "SIZE.h"

C     !INPUT/OUTPUT PARAMETERS:
C     active_var_file:: filename
C     active_var     :: array
C     active_vard    :: forward derivative of active_var
C     iRec           :: record number
C     myOptimIter    :: number of optimization iteration (default: 0)
C     myThid         :: thread number for this instance
      CHARACTER*(*) active_var_file
      Real*8     active_var(1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nr,nSx,nSy)
      Real*8     active_vard(1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nr,nSx,nSy)
      INTEGER iRec
      INTEGER myOptimIter
      INTEGER myThid
      Real*8     dummy
      Real*8     dummyd

C     !LOCAL VARIABLES:
      CHARACTER*(2) pref
      CHARACTER*(80) fname
      INTEGER myNr
      LOGICAL globalFile
      LOGICAL useCurrentDir
CEOP

      pref = 'g_'
      CALL ADD_PREFIX( pref, active_var_file, fname )
      myNr = Nr
      globalFile = .FALSE.
      useCurrentDir = .FALSE.

      CALL ACTIVE_WRITE_3D_RL(
     &                 active_var_file, active_var, globalFile,
     &                 useCurrentDir, iRec, myNr,
     &                 0, myOptimIter, myThid )

      CALL ACTIVE_WRITE_3D_RL(
     &                 fname, active_vard, globalFile,
     &                 useCurrentDir, iRec, myNr,
     &                 2, myOptimIter, myThid )

      RETURN
      END

C---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|

CBOP
C     !ROUTINE: ACTIVE_WRITE_XY_B
C     !INTERFACE:
      SUBROUTINE ACTIVE_WRITE_XY_B(
     I                              active_var_file,
     I                              active_var,
     U                              active_varb,
     I                              iRec,
     I                              myOptimIter,
     I                              myThid,
     I                              dummy,
     O                              dummyb
     &                            )

C     *==========================================================*
C     | SUBROUTINE ACTIVE_WRITE_XY_B
C     | o Reverse differentiation of ACTIVE_WRITE_XY.
C     *==========================================================*
C     | written by Shreyas Gaikwad, Laurent Hascoet in Nov 2022
C     *==========================================================*
C     \ev

C     !USES:
      IMPLICIT NONE
C     == global variables ==
#include "EEPARAMS.h"
#include "SIZE.h"

C     !INPUT/OUTPUT PARAMETERS:
C     active_var_file:: filename
C     active_var     :: array
C     active_varb    :: reverse derivative of active_var
C     iRec           :: record number
C     myOptimIter    :: number of optimization iteration (default: 0)
C     myThid         :: thread number for this instance
      CHARACTER*(80) active_var_file
      Real*8     active_var(1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      Real*8     active_varb(1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      INTEGER iRec
      INTEGER myOptimIter
      INTEGER myThid
      Real*8     dummy
      Real*8     dummyb

C     !LOCAL VARIABLES:
      CHARACTER*(2) adpref
      CHARACTER*(80) fname
      INTEGER myNr
      LOGICAL globalFile
      LOGICAL useCurrentDir
CEOP
      adpref = 'ad'
      CALL ADD_PREFIX( adpref, active_var_file, fname )
      myNr = 1
      globalFile = .FALSE.
      useCurrentDir = .FALSE.
      CALL ACTIVE_WRITE_3D_RL(
     &                 fname, active_varb, globalFile,
     &                 useCurrentDir, iRec, myNr,
     &                 REVERSE_SIMULATION, myOptimIter, myThid )

      RETURN
      END

C---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|

CBOP
C     !ROUTINE: ACTIVE_WRITE_XYZ_B
C     !INTERFACE:
      SUBROUTINE ACTIVE_WRITE_XYZ_B(
     I                              active_var_file,
     I                              active_var,
     U                              active_varb,
     I                              iRec,
     I                              myOptimIter,
     I                              myThid,
     I                              dummy,
     O                              dummyb
     &                            )

C     *==========================================================*
C     | SUBROUTINE ACTIVE_WRITE_XYZ_B
C     | o Reverse differentiation of ACTIVE_WRITE_XYZ.
C     *==========================================================*
C     | written by Shreyas Gaikwad, Laurent Hascoet in Nov 2022
C     *==========================================================*
C     \ev

C     !USES:
      IMPLICIT NONE
C     == global variables ==
#include "EEPARAMS.h"
#include "SIZE.h"

C     !INPUT/OUTPUT PARAMETERS:
C     active_var_file:: filename
C     active_var     :: array
C     active_varb    :: reverse derivative of active_var
C     iRec           :: record number
C     myOptimIter    :: number of optimization iteration (default: 0)
C     myThid         :: thread number for this instance
      CHARACTER*(80) active_var_file
      Real*8     active_var(1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nr,nSx,nSy)
      Real*8     active_varb(1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nr,nSx,nSy)
      INTEGER iRec
      INTEGER myOptimIter
      INTEGER myThid
      Real*8     dummy
      Real*8     dummyb

C     !FUNCTIONS:
      INTEGER  ILNBLNK
      EXTERNAL ILNBLNK

C     !LOCAL VARIABLES:
      CHARACTER*(2) adpref
      CHARACTER*(80) fname
      INTEGER il
      INTEGER myNr
      LOGICAL globalFile
      LOGICAL useCurrentDir
CEOP
      adpref = 'ad'
      CALL ADD_PREFIX( adpref, active_var_file, fname )
      myNr = Nr
      globalFile = .FALSE.
      useCurrentDir = .FALSE.
      CALL ACTIVE_WRITE_3D_RL(
     &                 fname, active_varb, globalFile,
     &                 useCurrentDir, iRec, myNr,
     &                 REVERSE_SIMULATION, myOptimIter, myThid )

      RETURN
      END

C---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|

CBOP
C     !ROUTINE: ACTIVE_WRITE_XY_FWD
C     !INTERFACE:
      SUBROUTINE ACTIVE_WRITE_XY_FWD(
     I                              active_var_file,
     I                              active_var,
     I                              iRec,
     I                              myOptimIter,
     I                              myThid,
     I                              dummy
     &                            )

C     *==========================================================*
C     | SUBROUTINE ACTIVE_WRITE_XY_FWD
C     | o Reverse differentiation of ACTIVE_WRITE_XY.
C     *==========================================================*
C     | written by Shreyas Gaikwad, Laurent Hascoet in Nov 2022
C     *==========================================================*
C     \ev

C     !USES:
      IMPLICIT NONE

C     == global variables ==
#include "SIZE.h"

C     !INPUT/OUTPUT PARAMETERS:
C     active_var_file:: filename
C     active_var     :: array
C     iRec           :: record number
C     myOptimIter    :: number of optimization iteration (default: 0)
C     myThid         :: thread number for this instance
      CHARACTER*(80) active_var_file
      Real*8     active_var(1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      INTEGER iRec
      INTEGER myOptimIter
      INTEGER myThid
      Real*8     dummy

      CALL ACTIVE_WRITE_XY(
     I                 active_var_file,
     I                 active_var,
     I                 iRec,
     I                 myOptimIter,
     I                 myThid,
     I                 dummy
     &                    )

      RETURN
      END

C---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|

CBOP
C     !ROUTINE: ACTIVE_WRITE_XY_BWD
C     !INTERFACE:
      SUBROUTINE ACTIVE_WRITE_XY_BWD(
     I                              active_var_file,
     I                              active_var,
     U                              active_varb,
     I                              iRec,
     I                              myOptimIter,
     I                              myThid,
     I                              dummy,
     O                              dummyb
     &                            )

C     *==========================================================*
C     | SUBROUTINE ACTIVE_WRITE_XY_BWD
C     | o Reverse differentiation of ACTIVE_WRITE_XY.
C     *==========================================================*
C     | written by Shreyas Gaikwad, Laurent Hascoet in Nov 2022
C     *==========================================================*
C     \ev

C     !USES:
      IMPLICIT NONE
C     == global variables ==
#include "SIZE.h"

C     !INPUT/OUTPUT PARAMETERS:
C     active_var_file:: filename
C     active_var     :: array
C     active_varb    :: reverse derivative of active_var
C     iRec           :: record number
C     myOptimIter    :: number of optimization iteration (default: 0)
C     myThid         :: thread number for this instance
      CHARACTER*(80) active_var_file
      Real*8     active_var(1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      Real*8     active_varb(1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      INTEGER iRec
      INTEGER myOptimIter
      INTEGER myThid
      Real*8     dummy
      Real*8     dummyb

      CALL ACTIVE_WRITE_XY_B(
     I                 active_var_file,
     I                 active_var,
     U                 active_varb,
     I                 iRec,
     I                 myOptimIter,
     I                 myThid,
     I                 dummy,
     O                 dummyb )

      RETURN
      END

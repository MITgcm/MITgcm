#include "CPP_EEOPTIONS.h"
C--   File active_read_tap.F: Active read with Tapenade.
C--    Contents:
C--    o ACTIVE_READ_XY_D
C--    o ACTIVE_READ_XYZ_D
C--    o ACTIVE_READ_XY_B
C--    o ACTIVE_READ_XYZ_B
C--    o ACTIVE_READ_XY_BWD (for use without checkpointing)
C--    o ACTIVE_READ_XY_FWD (for use without checkpointing)

C---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|

CBOP
C     !ROUTINE: ACTIVE_READ_XY_D
C     !INTERFACE:
      SUBROUTINE ACTIVE_READ_XY_D(active_var_file,
     &                 active_var, active_vard,
     &                 iRec, doglobalread, lAdInit,
     &                 myOptimIter, myThid, dummy, dummyd)

C     *==========================================================*
C     | SUBROUTINE ACTIVE_READ_XY_D
C     | o Tangent linear differentiation of ACTIVE_READ_XY.
C     *==========================================================*
C     | written by Shreyas Gaikwad, Laurent Hascoet in Nov 2022
C     *==========================================================*
C     \ev

C     !USES:
      IMPLICIT NONE
C     === Global variables ===
#include "SIZE.h"
#include "EEPARAMS.h"

C     !INPUT/OUTPUT PARAMETERS:
C     active_var_file:: filename
C     active_var     :: array
C     active_vard     :: forward derivative of active_var
C     iRec           :: record number
C     doglobalread   :: flag for global or local read/write
C                       (default: .false.)
C     lAdInit        :: initialisation of corresponding adjoint
C                       variable and write to active file
C     myOptimIter    :: number of optimization iteration (default: 0)
C     myThid     :: my Thread Id. number
      CHARACTER*(*) active_var_file
      _RL     active_vard(1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      _RL     active_var(1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      INTEGER iRec
      LOGICAL doglobalread
      LOGICAL lAdInit
      INTEGER myOptimIter
      INTEGER myThid
      _RL  dummy
      _RL dummyd

C     !LOCAL VARIABLES:
      CHARACTER*(80) fname
      CHARACTER*(2) tlmpref
CEOP

      tlmpref = 'g_'
      CALL ADD_PREFIX( tlmpref, active_var_file, fname )

      CALL ACTIVE_READ_XY(active_var_file, active_var,
     &                    iRec, doglobalread, lAdInit,
     &                    myOptimIter, myThid, dummy)
      CALL ACTIVE_READ_XY(fname, active_vard, iRec,
     &                    doglobalread, lAdInit,
     &                    myOptimIter, myThid, dummyd)

      RETURN
      END

C---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|

CBOP
C     !ROUTINE: ACTIVE_READ_XYZ_D
C     !INTERFACE:
      SUBROUTINE ACTIVE_READ_XYZ_D(active_var_file,
     &                 active_var, active_vard,
     &                 iRec, doglobalread, lAdInit,
     &                 myOptimIter, myThid, dummy, dummyd)

C     *==========================================================*
C     | SUBROUTINE ACTIVE_READ_XYZ_D
C     | o Tangent linear differentiation of ACTIVE_READ_XYZ.
C     *==========================================================*
C     | written by Shreyas Gaikwad, Laurent Hascoet in Nov 2022
C     *==========================================================*
C     \ev

C     !USES:
      IMPLICIT NONE
C     === Global variables ===
#include "SIZE.h"
#include "EEPARAMS.h"

C     !INPUT/OUTPUT PARAMETERS:
C     active_var_file:: filename
C     active_var     :: array
C     active_vard     :: forward derivative of active_var
C     iRec           :: record number
C     doglobalread   :: flag for global or local read/write
C                       (default: .false.)
C     lAdInit        :: initialisation of corresponding adjoint
C                       variable and write to active file
C     myOptimIter    :: number of optimization iteration (default: 0)
C     myThid     :: my Thread Id. number
      CHARACTER*(*) active_var_file
      _RL     active_vard(1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nr,nSx,nSy)
      _RL     active_var(1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nr,nSx,nSy)
      INTEGER iRec
      LOGICAL doglobalread
      LOGICAL lAdInit
      INTEGER myOptimIter
      INTEGER myThid
      _RL  dummy
      _RL dummyd

C     !LOCAL VARIABLES:
      CHARACTER*(80) fname
      CHARACTER*(2) tlmpref
CEOP

      tlmpref = 'g_'
      CALL ADD_PREFIX( tlmpref, active_var_file, fname )

      CALL ACTIVE_READ_XYZ(active_var_file, active_var,
     &                    iRec, doglobalread, lAdInit,
     &                    myOptimIter, myThid, dummy)
      CALL ACTIVE_READ_XYZ(fname, active_vard, iRec,
     &                    doglobalread, lAdInit,
     &                    myOptimIter, myThid, dummyd)

      RETURN
      END

C---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|

CBOP
C     !ROUTINE: ACTIVE_READ_XY_B
C     !INTERFACE:
      SUBROUTINE ACTIVE_READ_XY_B(active_var_file,
     &                 active_var, active_varb,
     &                 iRec, doglobalread, lAdInit,
     &                 myOptimIter, myThid, dummy, dummyb)

C     *==========================================================*
C     | SUBROUTINE ACTIVE_READ_XY_B
C     | o Reverse differentiation of ACTIVE_READ_XY.
C     *==========================================================*
C     | written by Shreyas Gaikwad, Laurent Hascoet in Nov 2022
C     *==========================================================*
C     \ev

C     !USES:
      IMPLICIT NONE
C     === Global variables ===
#include "SIZE.h"
#include "EEPARAMS.h"

C     !INPUT/OUTPUT PARAMETERS:
C     active_var_file:: filename
C     active_var     :: array
C     active_varb     :: reverse derivative of active_var
C     iRec           :: record number
C     doglobalread   :: flag for global or local read/write
C                       (default: .false.)
C     lAdInit        :: initialisation of corresponding adjoint
C                       variable and write to active file
C     myOptimIter    :: number of optimization iteration (default: 0)
C     myThid     :: my Thread Id. number
      CHARACTER*(*) active_var_file
      _RL     active_var(1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      _RL     active_varb(1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      INTEGER iRec
      INTEGER myIter, myThid
      LOGICAL doglobalread
      LOGICAL lAdInit
      INTEGER myOptimIter
C Tags for IO: ctrl input and adjoint (gradient) output
      _RL  dummy, dummyb

C     !LOCAL VARIABLES:
      CHARACTER*(2) adpref
      CHARACTER*(80) fname
      INTEGER myNr
      LOGICAL useCurrentDir
CEOP

      adpref = 'ad'
      CALL ADD_PREFIX( adpref, active_var_file, fname )
      myNr = 1
      useCurrentDir = .FALSE.

      CALL ACTIVE_READ_3D_RL(
     &                 fname, active_varb, doglobalread,
     &                 useCurrentDir, lAdInit, iRec, myNr,
     &                 REVERSE_SIMULATION, myIter, myThid )

      RETURN
      END

C---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|

CBOP
C     !ROUTINE: ACTIVE_READ_XYZ_B
C     !INTERFACE:
      SUBROUTINE ACTIVE_READ_XYZ_B(active_var_file,
     &                 active_var, active_varb,
     &                 iRec, doglobalread, lAdInit,
     &                 myOptimIter, myThid, dummy, dummyb)

C     *==========================================================*
C     | SUBROUTINE ACTIVE_READ_XYZ_B
C     | o Reverse differentiation of ACTIVE_READ_XYZ.
C     *==========================================================*
C     | written by Shreyas Gaikwad, Laurent Hascoet in Nov 2022
C     *==========================================================*
C     \ev

C     !USES:
      IMPLICIT NONE
C     === Global variables ===
#include "SIZE.h"
#include "EEPARAMS.h"

C     !INPUT/OUTPUT PARAMETERS:
C     active_var_file:: filename
C     active_var     :: array
C     active_varb     :: reverse derivative of active_var
C     iRec           :: record number
C     doglobalread   :: flag for global or local read/write
C                       (default: .false.)
C     lAdInit        :: initialisation of corresponding adjoint
C                       variable and write to active file
C     myOptimIter    :: number of optimization iteration (default: 0)
C     myThid     :: my Thread Id. number
      CHARACTER*(*) active_var_file
      _RL     active_var(1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nr,nSx,nSy)
      _RL     active_varb(1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nr,nSx,nSy)
      INTEGER iRec
      INTEGER myIter, myThid
      LOGICAL doglobalread
      LOGICAL lAdInit
      INTEGER myOptimIter
C Tags for IO: ctrl input and adjoint (gradient) output
      _RL  dummy, dummyb

C     !LOCAL VARIABLES:
      CHARACTER*(2) adpref
      CHARACTER*(80) fname
      INTEGER myNr
      LOGICAL useCurrentDir
CEOP

      adpref = 'ad'
      CALL ADD_PREFIX( adpref, active_var_file, fname )
      myNr = Nr
      useCurrentDir = .FALSE.

      CALL ACTIVE_READ_3D_RL(
     &                 fname, active_varb, doglobalread,
     &                 useCurrentDir, lAdInit, iRec, myNr,
     &                 REVERSE_SIMULATION, myIter, myThid )

      RETURN
      END

C---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|

CBOP
C     !ROUTINE: ACTIVE_READ_XY_FWD
C     !INTERFACE:
      SUBROUTINE ACTIVE_READ_XY_FWD(active_var_file,
     &                 active_var,
     &                 iRec, doglobalread, lAdInit,
     &                 myOptimIter, myThid, dummy)

C     *==========================================================*
C     | SUBROUTINE ACTIVE_READ_XY_FWD
C     | o Reverse differentiation of ACTIVE_READ_XY.
C     *==========================================================*
C     | written by Shreyas Gaikwad, Laurent Hascoet in Nov 2022
C     *==========================================================*
C     \ev

C     !USES:
      IMPLICIT NONE
C     === Global variables ===
#include "SIZE.h"
#include "EEPARAMS.h"

C     !INPUT/OUTPUT PARAMETERS:
C     active_var_file:: filename
C     active_var     :: array
C     active_varb     :: reverse derivative of active_var
C     iRec           :: record number
C     doglobalread   :: flag for global or local read/write
C                       (default: .false.)
C     lAdInit        :: initialisation of corresponding adjoint
C                       variable and write to active file
C     myOptimIter    :: number of optimization iteration (default: 0)
C     myThid     :: my Thread Id. number
      CHARACTER*(*) active_var_file
      _RL     active_var(1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      INTEGER iRec
      INTEGER myIter, myThid
      LOGICAL doglobalread
      LOGICAL lAdInit
      INTEGER myOptimIter
C Tags for IO: ctrl input and adjoint (gradient) output
      _RL  dummy

      CALL ACTIVE_READ_XY(active_var_file, active_var,
     &                    iRec, doglobalread, lAdInit,
     &                    myOptimIter, myThid, dummy)

      RETURN
      END

C---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|

CBOP
C     !ROUTINE: ACTIVE_READ_XY_BWD
C     !INTERFACE:
      SUBROUTINE ACTIVE_READ_XY_BWD(active_var_file,
     &                 active_var, active_varb,
     &                 iRec, doglobalread, lAdInit,
     &                 myOptimIter, myThid, dummy, dummyb)

C     *==========================================================*
C     | SUBROUTINE ACTIVE_READ_XY_BWD
C     | o Reverse differentiation of ACTIVE_READ_XY.
C     *==========================================================*
C     | written by Shreyas Gaikwad, Laurent Hascoet in Nov 2022
C     *==========================================================*
C     \ev

C     !USES:
      IMPLICIT NONE
C     === Global variables ===
#include "SIZE.h"
#include "EEPARAMS.h"

C     !INPUT/OUTPUT PARAMETERS:
C     active_var_file:: filename
C     active_var     :: array
C     active_varb     :: reverse derivative of active_var
C     iRec           :: record number
C     doglobalread   :: flag for global or local read/write
C                       (default: .false.)
C     lAdInit        :: initialisation of corresponding adjoint
C                       variable and write to active file
C     myOptimIter    :: number of optimization iteration (default: 0)
C     myThid     :: my Thread Id. number
      CHARACTER*(*) active_var_file
      _RL     active_var(1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      _RL     active_varb(1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      INTEGER iRec
      INTEGER myIter, myThid
      LOGICAL doglobalread
      LOGICAL lAdInit
      INTEGER myOptimIter
C Tags for IO: ctrl input and adjoint (gradient) output
      _RL  dummy, dummyb

      CALL ACTIVE_READ_XY_B(active_var_file,
     &                 active_var, active_varb,
     &                 iRec, doglobalread, lAdInit,
     &                 myOptimIter, myThid, dummy, dummyb)

      RETURN
      END

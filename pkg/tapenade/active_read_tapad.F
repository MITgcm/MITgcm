C--   File active_read_tapad.F: Active read with Tapenade.
C--    Contents:
C--    o ACTIVE_READ_XY_D
C--    o ACTIVE_READ_XYZ_D
C--    o ACTIVE_READ_XY_B
C--    o ACTIVE_READ_XYZ_B

C---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|

CBOP
C     !ROUTINE: ACTIVE_READ_XY_D
C     !INTERFACE:
      SUBROUTINE ACTIVE_READ_XY_D(active_var_file,
     +                 active_var, active_vard,
     +                 iRec, doglobalread, lAdInit,
     +                 myOptimIter, myThid, dummy, dummyd)

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

C     !INPUT/OUTPUT PARAMETERS:
C     === Routine arguments ===
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
      Real*8     active_vard(1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      Real*8     active_var(1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      INTEGER iRec
      LOGICAL doglobalread
      LOGICAL lAdInit
      INTEGER myOptimIter 
      INTEGER myThid
      Real*8  dummy(1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      Real*8 dummyd(1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)

C     !LOCAL VARIABLES:
      CHARACTER*(80) fname
      CHARACTER*(2) tlmpref
      INTEGER il
      INTEGER ILNBLNK
      EXTERNAL ILNBLNK
CEOP

      tlmpref = 'g_'
      il   = ILNBLNK( active_var_file )
      WRITE(fname(1:80),'(A)') ' '
      WRITE(fname(1:il+2),'(2A)') tlmpref,active_var_file(1:il)

      call active_read_xy(active_var_file, active_var,
     +                    iRec, doglobalread, lAdInit,
     +                    myOptimIter, myThid, dummy)
      call active_read_xy(fname, dummyd, iRec,
     &                    doglobalread, lAdInit,
     &                    myOptimIter, myThid, dummy)

      active_var = 0.d0
      active_vard = dummyd

      END

C---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|

CBOP
C     !ROUTINE: ACTIVE_READ_XYZ_D
C     !INTERFACE:
      subroutine ACTIVE_READ_XYZ_D(active_var_file,
     +                 active_var, active_vard,
     +                 iRec, doglobalread, lAdInit,
     +                 myOptimIter, myThid, dummy, dummyd)

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

C     !INPUT/OUTPUT PARAMETERS:
C     === Routine arguments ===
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
      Real*8     active_vard(1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nr,nSx,nSy)
      Real*8     active_var(1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nr,nSx,nSy)
      INTEGER iRec
      LOGICAL doglobalread
      LOGICAL lAdInit
      INTEGER myOptimIter
      INTEGER myThid
      Real*8  dummy(1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nr,nSx,nSy)
      Real*8 dummyd(1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nr,nSx,nSy)

C     !LOCAL VARIABLES:
      CHARACTER*(80) fname
      CHARACTER*(2) tlmpref
      INTEGER il
      INTEGER ILNBLNK
      EXTERNAL ILNBLNK
CEOP

      tlmpref = 'g_'
      il   = ILNBLNK( active_var_file )
      WRITE(fname(1:80),'(A)') ' '
      WRITE(fname(1:il+2),'(2A)') tlmpref,active_var_file(1:il)

      call active_read_xyz(active_var_file, active_var,
     +                    iRec, doglobalread, lAdInit,
     +                    myOptimIter, myThid, dummy)
      call active_read_xyz(fname, dummyd, iRec,
     &                    doglobalread, lAdInit,
     &                    myOptimIter, myThid, dummy)

      active_var = 0.d0
      active_vard = dummyd

      END

C---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|

CBOP
C     !ROUTINE: ACTIVE_READ_XY_B
C     !INTERFACE:
      SUBROUTINE ACTIVE_READ_XY_B(active_var_file, 
     +                 active_var, active_varb,
     +                 iRec, doglobalread, lAdInit, 
     +                 myOptimIter, myThid, dummy, dummyb)

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

C     !INPUT/OUTPUT PARAMETERS:
C     === Routine arguments ===
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
      Real*8     active_varb(1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      Real*8     active_var(1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      INTEGER iRec
      LOGICAL doglobalread
      LOGICAL lAdInit
      INTEGER myOptimIter
      INTEGER myThid
      Real*8  dummy(1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      Real*8 dummyb(1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)

C     !LOCAL VARIABLES:
      CHARACTER*(2) adpref
      CHARACTER*(80) fname
      INTEGER il
      INTEGER myNr
      LOGICAL useCurrentDir
      INTEGER ILNBLNK
      EXTERNAL ILNBLNK
CEOP

      dummyb = dummyb + active_varb
      active_varb = 0.d0

      adpref = 'ad'
      il   = ILNBLNK( active_var_file )
      WRITE(fname(1:80),'(A)') ' '
      WRITE(fname(1:il+2),'(2A)') adpref,active_var_file(1:il)

      myNr = 1
      useCurrentDir = .FALSE.

      call active_read_3d_rl(
     &                 fname, dummyb, doglobalread,
     &                 useCurrentDir, lAdInit, iRec, myNr,
     &                 1, myOptimIter, myThid )

      END

C---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|

CBOP
C     !ROUTINE: ACTIVE_READ_XYZ_B
C     !INTERFACE:
      SUBROUTINE ACTIVE_READ_XYZ_B(active_var_file, 
     +                 active_var, active_varb,
     +                 iRec, doglobalread, lAdInit, 
     +                 myOptimIter, myThid, dummy, dummyb)

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

C     !INPUT/OUTPUT PARAMETERS:
C     === Routine arguments ===
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
      Real*8     active_varb(1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nr,nSx,nSy)
      Real*8     active_var(1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nr,nSx,nSy)
      INTEGER iRec
      LOGICAL doglobalread
      LOGICAL lAdInit
      INTEGER myOptimIter
      INTEGER myThid
      Real*8  dummy(1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nr,nSx,nSy)
      Real*8 dummyb(1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nr,nSx,nSy)

C     !LOCAL VARIABLES:
      CHARACTER*(2) adpref
      CHARACTER*(80) fname
      INTEGER il
      INTEGER myNr
      LOGICAL useCurrentDir
      INTEGER ILNBLNK
      EXTERNAL ILNBLNK
CEOP

      dummyb = dummyb + active_varb
      active_varb = 0.d0

      adpref = 'ad'
      il   = ILNBLNK( active_var_file )
      WRITE(fname(1:80),'(A)') ' '
      WRITE(fname(1:il+2),'(2A)') adpref,active_var_file(1:il)

      myNr = Nr
      useCurrentDir = .FALSE.

      call active_read_3d_rl(
     &                 fname, dummyb, doglobalread,
     &                 useCurrentDir, lAdInit, iRec, myNr,
     &                 1, myOptimIter, myThid )

      RETURN
      END


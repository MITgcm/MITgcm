C $Header: /u/gcmpack/MITgcm/pkg/exf/exf_set_obcs.F,v 1.17 2011/09/02 18:22:50 jmc Exp $
C $Name:  $

#include "EXF_OPTIONS.h"

C--  File exf_set_obcs.F:
C--   Contents
C--   o EXF_SET_OBCS_XZ
C--   o EXF_SET_OBCS_YZ
C--   o EXF_SET_OBCS_X   <- no longer maintained ; use SET_OBCS_XZ with nNz=1
C--   o EXF_SET_OBCS_Y   <- no longer maintained ; use SET_OBCS_YZ with nNz=1

C---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|

      SUBROUTINE EXF_SET_OBCS_XZ (
     U       obcs_fld_xz, obcs_xz_0, obcs_xz_1,
     I       obcs_file, obcsmask, nNz,
     I       fac, first, changed, useYearlyFields, obcs_period,
     I       count0, count1, year0, year1,
     I       myTime, myIter, myThid )

C     ==================================================================
C     SUBROUTINE EXF_SET_OBCS_XZ
C     ==================================================================
C
C     o set open boundary conditions
C
C     started: heimbach@mit.edu 01-May-2001
C     mods for pkg/seaice: menemenlis@jpl.nasa.gov 20-Dec-2002

C     ==================================================================
C     SUBROUTINE EXF_SET_OBCS_XZ
C     ==================================================================

      IMPLICIT NONE

C     == global variables ==
#include "EEPARAMS.h"
#include "SIZE.h"
#include "GRID.h"
#include "EXF_PARAM.h"
#include "EXF_CONSTANTS.h"

C     == routine arguments ==
C     nNz   :: number of levels to process
      INTEGER nNz
      _RL obcs_fld_xz(1-OLx:sNx+OLx,nNz,nSx,nSy)
      _RL obcs_xz_0(1-OLx:sNx+OLx,nNz,nSx,nSy)
      _RL obcs_xz_1(1-OLx:sNx+OLx,nNz,nSx,nSy)

      CHARACTER*(128) obcs_file
      CHARACTER*1 obcsmask
      LOGICAL first, changed
      LOGICAL useYearlyFields
      _RL     obcs_period
      INTEGER count0, count1, year0, year1
      _RL     fac
      _RL     myTime
      INTEGER myIter
      INTEGER myThid

#ifdef ALLOW_OBCS

C     == local variables ==

      CHARACTER*(128) obcs_file0, obcs_file1
      INTEGER bi, bj
      INTEGER i, k

C     == end of interface ==

      IF ( obcs_file .NE. ' ' ) THEN

         IF ( first ) THEN

            CALL exf_GetYearlyFieldName(
     I         useYearlyFields, twoDigitYear, obcs_period, year0,
     I         obcs_file,
     O         obcs_file0,
     I         myTime, myIter, myThid )

            _BARRIER
            CALL READ_REC_XZ_RL( obcs_file0, exf_iprec_obcs, nNz,
     &                           obcs_xz_1, count0, myIter, myThid )
            _BARRIER
         ENDIF

         IF ( first .OR. changed ) THEN
            CALL exf_swapffields_xz( obcs_xz_0, obcs_xz_1, nNz,myThid )

            CALL exf_GetYearlyFieldName(
     I         useYearlyFields, twoDigitYear, obcs_period, year1,
     I         obcs_file,
     O         obcs_file1,
     I         myTime, myIter, myThid )

            _BARRIER
            CALL READ_REC_XZ_RL( obcs_file1, exf_iprec_obcs, nNz,
     &                           obcs_xz_1, count1, myIter, myThid )
            _BARRIER
         ENDIF

         DO bj = myByLo(myThid),myByHi(myThid)
            DO bi = myBxLo(myThid),myBxHi(myThid)
               DO k = 1,nNz
                  DO i = 1,sNx
                     obcs_fld_xz(i,k,bi,bj) =
     &                    fac * obcs_xz_0(i,k,bi,bj) +
     &                    (exf_one - fac) * obcs_xz_1(i,k,bi,bj)
                  ENDDO
               ENDDO
            ENDDO
         ENDDO

      ENDIF

#endif /* ALLOW_OBCS */

      RETURN
      END

C---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|

      SUBROUTINE EXF_SET_OBCS_YZ (
     U       obcs_fld_yz, obcs_yz_0, obcs_yz_1,
     I       obcs_file, obcsmask, nNz,
     I       fac, first, changed, useYearlyFields, obcs_period,
     I       count0, count1, year0, year1,
     I       myTime, myIter, myThid)

C     ==================================================================
C     SUBROUTINE EXF_SET_OBCS_YZ
C     ==================================================================
C
C     o set open boundary conditions
C
C     started: heimbach@mit.edu 01-May-2001

C     ==================================================================
C     SUBROUTINE EXF_SET_OBCS_YZ
C     ==================================================================

      IMPLICIT NONE

C     == global variables ==
#include "EEPARAMS.h"
#include "SIZE.h"
#include "GRID.h"
#include "EXF_PARAM.h"
#include "EXF_CONSTANTS.h"

C     == routine arguments ==
C     nNz   :: number of levels to process
      INTEGER nNz
      _RL obcs_fld_yz(1-OLy:sNy+OLy,nNz,nSx,nSy)
      _RL obcs_yz_0(1-OLy:sNy+OLy,nNz,nSx,nSy)
      _RL obcs_yz_1(1-OLy:sNy+OLy,nNz,nSx,nSy)
      CHARACTER*(MAX_LEN_FNAM) obcs_file
      CHARACTER*1 obcsmask
      LOGICAL first, changed
      LOGICAL useYearlyFields
      _RL     obcs_period
      INTEGER count0, count1, year0, year1
      _RL     fac
      _RL     myTime
      INTEGER myIter
      INTEGER myThid

#ifdef ALLOW_OBCS

C     == local variables ==

      CHARACTER*(128) obcs_file0, obcs_file1
      INTEGER bi, bj
      INTEGER j, k

C     == end of interface ==

      IF ( obcs_file .NE. ' ' ) THEN

         IF ( first ) THEN

            CALL exf_GetYearlyFieldName(
     I         useYearlyFields, twoDigitYear, obcs_period, year0,
     I         obcs_file,
     O         obcs_file0,
     I         myTime, myIter, myThid )

            _BARRIER
            CALL READ_REC_YZ_RL( obcs_file0, exf_iprec_obcs, nNz,
     &                           obcs_yz_1, count0, myIter, myThid )
            _BARRIER
         ENDIF

         IF ( first .OR. changed ) THEN
            CALL exf_swapffields_yz( obcs_yz_0, obcs_yz_1, nNz,myThid )

            CALL exf_GetYearlyFieldName(
     I         useYearlyFields, twoDigitYear, obcs_period, year1,
     I         obcs_file,
     O         obcs_file1,
     I         myTime, myIter, myThid )

            _BARRIER
            CALL READ_REC_YZ_RL( obcs_file1, exf_iprec_obcs, nNz,
     &                           obcs_yz_1, count1, myIter, myThid )
            _BARRIER
         ENDIF

         DO bj = myByLo(myThid),myByHi(myThid)
            DO bi = myBxLo(myThid),myBxHi(myThid)
               DO k = 1,nNz
                  DO j = 1,sNy
                     obcs_fld_yz(j,k,bi,bj) =
     &                    fac             *obcs_yz_0(j,k,bi,bj) +
     &                    (exf_one - fac) *obcs_yz_1(j,k,bi,bj)
                  ENDDO
               ENDDO
            ENDDO
         ENDDO

      ENDIF

#endif /* ALLOW_OBCS */

      RETURN
      END

C---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|

      SUBROUTINE EXF_SET_OBCS_X (
     U       obcs_fld_x, obcs_x_0, obcs_x_1,
     I       obcs_file, obcsmask,
     I       fac, first, changed, useYearlyFields, obcs_period,
     I       count0, count1, year0, year1,
     I       myTime, myIter, myThid )

C     ==================================================================
C     SUBROUTINE EXF_SET_OBCS_X
C     ==================================================================
C
C     o set open boundary conditions
C       same as EXF_SET_OBCS_XZ but for Nr=1
C
C     ==================================================================
C     SUBROUTINE EXF_SET_OBCS_X
C     ==================================================================

      IMPLICIT NONE

C     == global variables ==

#include "EEPARAMS.h"
#include "SIZE.h"
#include "GRID.h"
#include "EXF_PARAM.h"
#include "EXF_CONSTANTS.h"

C     == routine arguments ==

      _RL obcs_fld_x(1-OLx:sNx+OLx,nSx,nSy)
      _RL obcs_x_0(1-OLx:sNx+OLx,nSx,nSy)
      _RL obcs_x_1(1-OLx:sNx+OLx,nSx,nSy)

      CHARACTER*(128) obcs_file
      CHARACTER*1 obcsmask
      LOGICAL first, changed
      LOGICAL useYearlyFields
      _RL     obcs_period
      INTEGER count0, count1, year0, year1
      _RL     fac
      _RL     myTime
      INTEGER myIter
      INTEGER myThid

#ifdef ALLOW_OBCS

C     == local variables ==

      CHARACTER*(128) obcs_file0, obcs_file1
      INTEGER bi, bj, i

C     == end of interface ==

      STOP 'S/R EXF_SET_OBCS_X no longer maintained'

      IF ( obcs_file .NE. ' ' ) THEN

         IF ( first ) THEN

            CALL exf_GetYearlyFieldName(
     I         useYearlyFields, twoDigitYear, obcs_period, year0,
     I         obcs_file,
     O         obcs_file0,
     I         myTime, myIter, myThid )

            CALL READ_REC_XZ_RL( obcs_file0, exf_iprec_obcs, 1,
     &                           obcs_x_1, count0, myIter, myThid )
         ENDIF

         IF (( first ) .OR. ( changed )) THEN
            CALL exf_swapffields_xz( obcs_x_0, obcs_x_1, 1,myThid )

            CALL exf_GetYearlyFieldName(
     I         useYearlyFields, twoDigitYear, obcs_period, year1,
     I         obcs_file,
     O         obcs_file1,
     I         myTime, myIter, myThid )

            CALL READ_REC_XZ_RL( obcs_file1, exf_iprec_obcs, 1,
     &                           obcs_x_1, count1, myIter, myThid )
         ENDIF

         DO bj = myByLo(myThid),myByHi(myThid)
            DO bi = myBxLo(myThid),myBxHi(myThid)
               DO i = 1,sNx
                  obcs_fld_x(i,bi,bj) =
     &                 fac * obcs_x_0(i,bi,bj) +
     &                 (exf_one - fac) * obcs_x_1(i,bi,bj)
               ENDDO
            ENDDO
         ENDDO

      ENDIF

#endif /* ALLOW_OBCS */

      RETURN
      END

C---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|

      SUBROUTINE EXF_SET_OBCS_Y (
     U       obcs_fld_y, obcs_y_0, obcs_y_1,
     I       obcs_file, obcsmask,
     I       fac, first, changed, useYearlyFields, obcs_period,
     I       count0, count1, year0, year1,
     I       myTime, myIter, myThid )

C     ==================================================================
C     SUBROUTINE EXF_SET_OBCS_Y
C     ==================================================================
C
C     o set open boundary conditions
C       same as EXF_SET_OBCS_YZ but for Nr=1
C
C     ==================================================================
C     SUBROUTINE EXF_SET_OBCS_Y
C     ==================================================================

      IMPLICIT NONE

C     == global variables ==

#include "EEPARAMS.h"
#include "SIZE.h"
#include "GRID.h"
#include "EXF_PARAM.h"
#include "EXF_CONSTANTS.h"

C     == routine arguments ==

      _RL obcs_fld_y(1-OLy:sNy+OLy,nSx,nSy)
      _RL obcs_y_0(1-OLy:sNy+OLy,nSx,nSy)
      _RL obcs_y_1(1-OLy:sNy+OLy,nSx,nSy)
      CHARACTER*(MAX_LEN_FNAM) obcs_file
      CHARACTER*1 obcsmask
      LOGICAL first, changed
      LOGICAL useYearlyFields
      _RL     obcs_period
      INTEGER count0, count1, year0, year1
      _RL     fac
      _RL     myTime
      INTEGER myIter
      INTEGER myThid

#ifdef ALLOW_OBCS

C     == local variables ==

      CHARACTER*(128) obcs_file0, obcs_file1
      INTEGER bi, bj, j

C     == end of interface ==

      STOP 'S/R EXF_SET_OBCS_X no longer maintained'

      IF ( obcs_file .NE. ' ' ) THEN

         IF ( first ) THEN

            CALL exf_GetYearlyFieldName(
     I         useYearlyFields, twoDigitYear, obcs_period, year0,
     I         obcs_file,
     O         obcs_file0,
     I         myTime, myIter, myThid )

            CALL READ_REC_YZ_RL( obcs_file0, exf_iprec_obcs, 1,
     &                           obcs_y_1, count0, myIter, myThid )
         ENDIF

         IF (( first ) .OR. ( changed )) THEN
            CALL exf_swapffields_yz( obcs_y_0, obcs_y_1, 1,myThid )

            CALL exf_GetYearlyFieldName(
     I         useYearlyFields, twoDigitYear, obcs_period, year1,
     I         obcs_file,
     O         obcs_file1,
     I         myTime, myIter, myThid )

            CALL READ_REC_YZ_RL( obcs_file1, exf_iprec_obcs, 1,
     &                           obcs_y_1, count1, myIter, myThid )
         ENDIF

         DO bj = myByLo(myThid),myByHi(myThid)
            DO bi = myBxLo(myThid),myBxHi(myThid)
               DO j = 1,sNy
                  obcs_fld_y(j,bi,bj) =
     &                 fac             *obcs_y_0(j,bi,bj) +
     &                 (exf_one - fac) *obcs_y_1(j,bi,bj)
               ENDDO
            ENDDO
         ENDDO

      ENDIF

#endif /* ALLOW_OBCS */

      RETURN
      END

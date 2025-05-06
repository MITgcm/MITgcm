#include "PROFILES_OPTIONS.h"

C---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|
CBOP
C !ROUTINE: PROFILES_INIT_VARIA

C !INTERFACE:
      SUBROUTINE PROFILES_INIT_VARIA( myThid )

C     !DESCRIPTION:
C     Initialise the variable cost function part.
C     Started: Christian Eckert eckert@mit.edu 30-Jun-1999
C     Changed: Christian Eckert eckert@mit.edu 18-Apr-2000
C              - Restructured the code in order to create a package
C                for the MITgcmUV.
C              heimbach@mit.edu 05-Nov-2003
C              - Now ecco part of cost

C     !USES:
      IMPLICIT NONE
C     == Global variables ===
#include "EEPARAMS.h"
#include "SIZE.h"
#include "GRID.h"

#ifdef ALLOW_PROFILES
# include "PROFILES_SIZE.h"
# include "profiles.h"
#endif

#ifdef ALLOW_GRDCHK
# include "netcdf.inc"
#endif

C     !INPUT/OUTPUT PARAMETERS:
C     myThid: my thread ID number
      INTEGER mythid
CEOP

C     !LOCAL VARIABLES:
      INTEGER bi,bj
      INTEGER num_file,num_var
#ifdef ALLOW_GRDCHK
      INTEGER err
#endif /* ALLOW_GRDCHK */

C Initialize tiled cost function contributions.
      DO bj = myByLo(mythid), myByHi(mythid)
        DO bi = myBxLo(mythid), myBxHi(mythid)
          DO num_file = 1, NFILESPROFMAX
            DO num_var = 1, NVARMAX
              objf_profiles(num_file,num_var,bi,bj) = 0. _d 0
              num_profiles(num_file,num_var,bi,bj)  = 0. _d 0
              profiles_dummy(num_file,num_var,bi,bj) = 0. _d 0
            ENDDO !DO num_var
          ENDDO !DO num_file

          DO num_var = 1,NVARMAX
            objf_profiles_mean(num_var,bi,bj) = 0. _d 0
            num_profiles_mean(num_var,bi,bj) = 0. _d 0
          ENDDO !DO num_var

#ifdef ALLOW_GRDCHK
          IF (profilesDoNcOutput) THEN
            DO num_file = 1, NFILESPROFMAX
              IF (ProfNo(num_file,bi,bj).GT.0) THEN
C Write data from buffer to .nc file
                err = NF_SYNC( fidforward(num_file,bi,bj) )
                CALL PROFILES_NF_ERROR(
     &               'INIT_VARIA: NF_SYNC fidforward',
     &               err,bi,bj,myThid )

# ifdef ALLOW_TANGENTLINEAR_RUN
                IF (TANGENT_SIMULATION) THEN
                  err = NF_SYNC( fidtangent(num_file,bi,bj) )
                  CALL PROFILES_NF_ERROR(
     &                 'INIT_VARIA: NF_SYNC fidtangent',
     &                 err,bi,bj,myThid )
                ENDIF
# endif /* ALLOW_TANGENTLINEAR_RUN */

              ENDIF !IF (ProfNo
            ENDDO !DO num_file
          ENDIF !IF (profilesDoNcOutput)
#endif /* ALLOW_GRDCHK */

        ENDDO !DO bi
      ENDDO !DO bj

      _BARRIER

      RETURN
      END

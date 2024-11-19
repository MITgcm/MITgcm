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

C     !FUNCTIONS:
#ifdef ALLOW_GRDCHK
      INTEGER ILNBLNK
      EXTERNAL ILNBLNK
#endif /* ALLOW_GRDCHK */

C     !LOCAL VARIABLES:
      INTEGER bi,bj
      INTEGER itlo,ithi
      INTEGER jtlo,jthi
      INTEGER num_file,num_var
#ifdef ALLOW_GRDCHK
      INTEGER err, iG, jG, IL, JL
      CHARACTER*(MAX_LEN_FNAM) profilesfile
      CHARACTER*(MAX_LEN_FNAM) fnameequinc
#endif /* ALLOW_GRDCHK */

      jtlo = mybylo(mythid)
      jthi = mybyhi(mythid)
      itlo = mybxlo(mythid)
      ithi = mybxhi(mythid)

C Initialize tiled cost function contributions.
      DO bj = jtlo, jthi
        DO bi = itlo, ithi
          DO num_file = 1, NFILESPROFMAX
            DO num_var = 1, NVARMAX
              objf_profiles(num_file,num_var,bi,bj) = 0. _d 0
              num_profiles(num_file,num_var,bi,bj)  = 0. _d 0
              profiles_dummy(num_file,num_var,bi,bj) = 0. _d 0
            ENDDO
          ENDDO

          DO num_var = 1,NVARMAX
            objf_profiles_mean(num_var,bi,bj) = 0. _d 0
            num_profiles_mean(num_var,bi,bj) = 0. _d 0
          ENDDO
#ifdef ALLOW_GRDCHK
          IF (profilesDoNcOutput) then
            DO num_file = 1, NFILESPROFMAX
              IF (ProfNo(num_file,bi,bj).GT.0) then
C Close .equi file so data in buffer is not lost when run finishes
                err = NF_CLOSE( fidforward(num_file,bi,bj) )
                CALL PROFILES_NF_ERROR( 'INIT_VARIA: NF_CLOSE',
     &               err,bi,bj,myThid )

C Determine model equivalent file name
                iG=bi+(myXGlobalLo-1)/sNx ! Kludge until unstructered tiles
                jG=bj+(myYGlobalLo-1)/sNy ! Kludge until unstructered tiles

                IL = ILNBLNK( profilesfiles(num_file) )
                WRITE(profilesfile,'(A)') profilesfiles(num_file)(1:IL)

                IL = ILNBLNK( profilesfile )
                JL = ILNBLNK( profilesDir )
                WRITE(fnameequinc,'(3A,2(I3.3,A))')
     &           profilesDir(1:JL),profilesfile(1:IL),'.',
     &           iG,'.',jG,'.equi.nc'

C re-open to reset write permissions
                JL = ILNBLNK( fnameequinc )
                err = NF_OPEN( fnameequinc(1:JL),NF_WRITE,
     &          fidforward(num_file,bi,bj) )
                CALL PROFILES_NF_ERROR(
     &               'INIT_VARIA: NF_OPEN('//fnameequinc(1:JL)//')',
     &               err,bi,bj,myThid )

              ENDIF !IF (ProfNo
            ENDDO !DO num_file
          ENDIF !IF (profilesDoNcOutput)
#endif /* ALLOW_GRDCHK */

        ENDDO !DO bi
      ENDDO !DO bj

      _BARRIER

      RETURN
      END

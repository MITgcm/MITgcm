#include "PROFILES_OPTIONS.h"
#include "AD_CONFIG.h"

C---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|
CBOP
C !ROUTINE: PROFILES_INIT_NCFILE

C !INTERFACE:
      SUBROUTINE PROFILES_INIT_NCFILE(
     & num_file,fid1,file2,fid2,length,Zlength,bi,bj,myThid)

C     !DESCRIPTION:
C     Initialization of model counterparts files
C     for netcdf profiles data
C     Started: Gael Forget 15-March-2006

C     !USES:
      IMPLICIT NONE
C     == Global variables ===
#include "EEPARAMS.h"
#include "SIZE.h"
#include "GRID.h"
#include "DYNVARS.h"
#ifdef ALLOW_PROFILES
# include "PROFILES_SIZE.h"
# include "profiles.h"
# include "netcdf.inc"
#endif

C     !INPUT/OUTPUT PARAMETERS:
C     myThid: my thread ID number
      INTEGER fid1,fid2,dimid1,dimid2,varid(2*NVARMAX),varid0
      INTEGER myThid,bi,bj,err,vecid(2), length, Zlength
      CHARACTER*(*) file2
      INTEGER irec,num_var,num_file
      _RL tmp_vec(Zlength+1)
CEOP

C     !LOCAL VARIABLES:
#ifdef ALLOW_PROFILES
      CHARACTER*(MAX_LEN_MBUF) msgBuf

      IF (profilesDoNcOutput) THEN
C1) Creation :
        err = NF_CREATE(file2 , NF_CLOBBER, fid2)
        CALL PROFILES_NF_ERROR( 'INIT_NCFILE 1: NF_CREATE',
     &       err,bi,bj,myThid )
        err = NF_DEF_DIM(fid2,'iDEPTH', Zlength ,dimid1)
        CALL PROFILES_NF_ERROR( 'INIT_NCFILE 2: NF_DEF_DIM',
     &       err,bi,bj,myThid )
        err = NF_DEF_DIM(fid2,'iPROF',length,dimid2)
        CALL PROFILES_NF_ERROR( 'INIT_NCFILE 3: NF_DEF_DIM',
     &       err,bi,bj,myThid )
        vecid(1)=dimid1
        vecid(2)=dimid2

        err = NF_DEF_VAR (fid2,'prof_ind_glob', NF_INT, 1,vecid(2),
     &       varid(1))
        CALL PROFILES_NF_ERROR( 'INIT_NCFILE 4: NF_DEF_VAR',
     &       err,bi,bj,myThid )
        err = NF_PUT_ATT_INT(fid2,varid(1),'_FillValue',NF_INT,1,0)
        CALL PROFILES_NF_ERROR( 'INIT_NCFILE 5: NF_PUT_ATT_INT',
     &       err,bi,bj,myThid )

        DO num_var = 1, NVARMAX
          err = NF_INQ_VARID( fid1,prof_names(num_file,num_var),varid0 )
          IF (err.EQ.NF_NOERR) THEN
            WRITE(msgBuf,'(2A)')
     &       'S/R PROFILES_INIT_NCFILE: defining new variable ',
     &       prof_names(num_file,num_var)
            CALL PRINT_MESSAGE( msgBuf,
     &           standardMessageUnit, SQUEEZE_RIGHT, myThid )

            err = NF_DEF_VAR( fid2,prof_names(num_file,num_var),
     &            NF_DOUBLE,2,vecid,varid(2+(num_var-1)*2) )
            CALL PROFILES_NF_ERROR( 'INIT_NCFILE 7: NF_DEF_VAR',
     &           err,bi,bj,myThid )
            err = NF_PUT_ATT_DOUBLE( fid2,varid(2+(num_var-1)*2),
     &           '_FillValue',NF_DOUBLE,1,0. _d 0 )
            CALL PROFILES_NF_ERROR( 'INIT_NCFILE 8: NF_PUT_ATT_DOUBLE',
     &           err,bi,bj,myThid )
            err = NF_DEF_VAR( fid2,prof_namesmask(num_file,num_var),
     &           NF_DOUBLE,2,vecid,varid(3+(num_var-1)*2) )
            CALL PROFILES_NF_ERROR( 'INIT_NCFILE 9: NF_DEF_VAR',
     &           err,bi,bj,myThid )
            err = NF_PUT_ATT_DOUBLE( fid2,varid(3+(num_var-1)*2),
     &           '_FillValue',NF_DOUBLE,1,0. _d 0)
            CALL PROFILES_NF_ERROR( 'INIT_NCFILE10: NF_PUT_ATT_DOUBLE',
     &           err,bi,bj,myThid )

          ELSE
            WRITE(msgBuf,'(2A)')
     &       'S/R PROFILES_INIT_NCFILE: no variable ',
     &       prof_names(num_file,num_var)
            CALL PRINT_MESSAGE( msgBuf,
     &           standardMessageUnit, SQUEEZE_RIGHT, myThid )

          ENDIF
        ENDDO !DO num_var

        err = NF_ENDDEF( fid2 )
        CALL PROFILES_NF_ERROR( 'INIT_NCFILE11: NF_ENDDEF',
     &       err,bi,bj,myThid )
        err = NF_CLOSE( fid2 )
        CALL PROFILES_NF_ERROR( 'INIT_NCFILE11: NF_CLOSE',
     &       err,bi,bj,myThid )

        err = NF_OPEN( file2, NF_WRITE, fid2 )
        CALL PROFILES_NF_ERROR( 'INIT_NCFILE: NF_OPEN'//file2//')',
     &       err,bi,bj,myThid )

      ELSE !IF (profilesDoNcOutput)
        OPEN( fid2, FILE = file2, FORM = 'unformatted', 
     &   STATUS = 'unknown', ACCESS = 'direct', 
     &   RECL = (Zlength + 1)*WORDLENGTH*2 )

        DO irec = 1, Zlength+1
          tmp_vec(irec) = 0
        ENDDO

# ifdef _BYTESWAPIO
        CALL MDS_BYTESWAPR8(Zlength+1,tmp_vec)
# endif
        DO irec = length, 1, -1
          DO num_var = prof_num_var_tot(num_file,bi,bj), 1, -1
            WRITE(fid2,rec=((irec-1)*prof_num_var_tot(num_file,bi,bj)
     &       +num_var-1)*2 +1) tmp_vec
            WRITE(fid2,rec=((irec-1)*prof_num_var_tot(num_file,bi,bj)
     &       +num_var-1)*2 +2) tmp_vec
          ENDDO
        ENDDO

      ENDIF !IF (profilesNoNcOutput)
#endif /* ALLOW_PROFILES */

      RETURN
      END

C---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|
CBOP
C !ROUTINE: PROFILES_NF_ERROR

C !INTERFACE:
      SUBROUTINE PROFILES_NF_ERROR( message, STATUS, bi, bj, myThid )

C     !DESCRIPTION:
C     Print NetCDF error message

C     !USES:
      IMPLICIT NONE
C     == Global variables ===
#ifdef ALLOW_PROFILES
# include "SIZE.h"
# include "EEPARAMS.h"
# include "PARAMS.h"
# include "netcdf.inc"
#endif

C     !INPUT PARAMETERS:
C     message   :: optional message
C     STATUS    :: NetCDF error status
C     bi,bj     :: Tile indices
C     myThid: my thread ID number
      CHARACTER*(*) message
      INTEGER STATUS, bi, bj, myThid

C     !OUTPUT PARAMETERS:
CEOP

C     !FUNCTIONS:
      INTEGER ILNBLNK
      EXTERNAL ILNBLNK

C     !LOCAL VARIABLES:
#ifdef ALLOW_PROFILES
      INTEGER IL
      CHARACTER*(MAX_LEN_MBUF) msgBuf

      IF (debugLevel .GE. debLevA .AND. STATUS .NE. NF_NOERR) THEN
        IL = ILNBLNK(message)
        IF ( IL .GT. 0 ) THEN
          WRITE(msgBuf,'(A,A,2I3,1X,A)')
     &     'NF_MESSAGE: PROFILES_', message(1:IL),
     &     bi, bj, NF_STRERROR(STATUS)
        ELSE
          WRITE(msgBuf,'(A,2I3,1X,A)') 'NF_MESSAGE: PROFILES_',
     &     bi, bj, NF_STRERROR(STATUS)
        ENDIF
        CALL PRINT_MESSAGE( msgBuf, standardMessageUnit,
     &       SQUEEZE_RIGHT,myThid )
        CALL PRINT_ERROR( msgBuf, myThid )
      ENDIF
#endif /* ALLOW_PROFILES */

      RETURN
      END

C---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|
CBOP
C !ROUTINE: PROFILES_CLOSE_NCFILES

C !INTERFACE:
      SUBROUTINE PROFILES_CLOSE_NCFILES( myThid )

C     !DESCRIPTION:
C     Close NetCDF files

C     !USES:
      IMPLICIT NONE
C     == Global variables ===
#ifdef ALLOW_PROFILES
# include "SIZE.h"
# include "EEPARAMS.h"
# include "PROFILES_SIZE.h"
# include "profiles.h"
# include "netcdf.inc"
#endif

C     !INPUT PARAMETERS:
C     myThid    :: my Thread Id number
      INTEGER myThid

C     !OUTPUT PARAMETERS:
CEOP

C     !FUNCTIONS:
      INTEGER ILNBLNK
      EXTERNAL ILNBLNK

C     !LOCAL VARIABLES:
#ifdef ALLOW_PROFILES
C     bi,bj     :: Tile indices
      INTEGER bi, bj
      INTEGER IL, numFile
      INTEGER STATUS
      CHARACTER*(MAX_LEN_MBUF) msgBuf

      IF (profilesDoNcOutput) THEN
        DO bj = myByLo(myThid), myByHi(myThid)
          DO bi = myBxLo(myThid), myBxHi(myThid)
            DO numFile = 1, NFILESPROFMAX
              IL = ILNBLNK( profilesfiles(numFile) )
              IF (IL.NE.0) THEN
                WRITE(msgBuf,'(A,A,5(1X,I8))') 
     &           'S/R PROFILES_CLOSE_NCFILES:',
     &           ' Closing '//profilesfiles(numFile)(1:IL), numFile,
     &           fiddata(numFile,bi,bj),fidforward(numFile,bi,bj),
     &           fidadjoint(numFile,bi,bj),fidtangent(numFile,bi,bj)
                CALL PRINT_MESSAGE( msgBuf, standardMessageUnit,
     &               SQUEEZE_RIGHT,myThid )
C Data file
                STATUS = NF_CLOSE( fiddata(numFile,bi,bj) )
                WRITE(msgBuf,'(A,I3,I8)')
     &           'CLOSE_NCFILES: NF_CLOSE data unit',
     &           numFile, fiddata(numFile,bi,bj)
                CALL PROFILES_NF_ERROR( msgBuf, STATUS, bi, bj, myThid )

C Forward equivalent (.equi) file
                STATUS = NF_CLOSE( fidforward(numFile,bi,bj) )
                WRITE(msgBuf,'(A,I3,I8)')
     &           'CLOSE_NCFILES: NF_CLOSE fwd unit',
     &           numFile, fidforward(numFile,bi,bj)
                CALL PROFILES_NF_ERROR( msgBuf, STATUS, bi, bj, myThid )
#ifdef ALLOW_ADJOINT_RUN
C Adjoint equivalent (.equi) file
                STATUS = NF_CLOSE( fidadjoint(numFile,bi,bj) )
                WRITE(msgBuf,'(A,I3,I8)') 
     &           'CLOSE_NCFILES: NF_CLOSE adj unit',
     &           numFile, fidadjoint(numFile,bi,bj)
                CALL PROFILES_NF_ERROR( msgBuf, STATUS, bi, bj, myThid )
#endif
#ifdef ALLOW_TANGENTLINEAR_RUN
C Tangent linear equivalent (.equi) file
                STATUS = NF_CLOSE( fidtangent(numFile,bi,bj) )
                WRITE(msgBuf,'(A,I3,I8)') 
     &           'CLOSE_NCFILES: NF_CLOSE tlm unit',
     &           numFile, fidtangent(numFile,bi,bj)
                CALL PROFILES_NF_ERROR( msgBuf, STATUS, bi, bj, myThid )
#endif
              ENDIF
            ENDDO
          ENDDO
        ENDDO
      ENDIF
#endif /* ALLOW_PROFILES */

      RETURN
      END

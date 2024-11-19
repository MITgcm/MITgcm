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
      INTEGER num_file, fid1
      CHARACTER*(*) file2
      INTEGER fid2, length, Zlength
      INTEGER bi, bj, myThid
CEOP

C     !LOCAL VARIABLES:
      INTEGER k,err,vecid(2)
      INTEGER irec,num_var,dimId1,dimId2,varId(2*NVARMAX),varId0
      _RL tmp_vec(Zlength+1)
#ifdef ALLOW_PROFILES
      CHARACTER*(MAX_LEN_MBUF) msgBuf

      IF (profilesDoNcOutput) THEN
C1) Creation :
        err = NF_CREATE( file2, NF_CLOBBER, fid2 )
        CALL PROFILES_NF_ERROR( 'INIT_NCFILE: NF_CREATE fid2',
     &       err,bi,bj,myThid )
        err = NF_DEF_DIM( fid2,'iDEPTH', Zlength, dimId1 )
        CALL PROFILES_NF_ERROR( 'INIT_NCFILE: NF_DEF_DIM Zlength',
     &       err,bi,bj,myThid )
        err = NF_DEF_DIM( fid2,'iPROF', length, dimId2 )
        CALL PROFILES_NF_ERROR( 'INIT_NCFILE: NF_DEF_DIM length',
     &       err,bi,bj,myThid )

        vecid(1)=dimId1
        vecid(2)=dimId2

        err = NF_DEF_VAR( fid2, 'prof_ind_glob',
     &        NF_INT, 1, vecid(2), varId(1) )
        CALL PROFILES_NF_ERROR( 'INIT_NCFILE: NF_DEF_VAR prof_ind_glob',
     &       err,bi,bj,myThid )
        err = NF_PUT_ATT_INT( fid2, varId(1),
     &        '_FillValue', NF_INT, 1, 0 )
        CALL PROFILES_NF_ERROR( 'INIT_NCFILE: NF_PUT_ATT_INT varId(1)',
     &       err,bi,bj,myThid )

        DO num_var = 1, NVARMAX
          err = NF_INQ_VARID( fid1,
     &          prof_names(num_file,num_var), varId0 )
          IF (err.EQ.NF_NOERR) THEN
            WRITE(msgBuf,'(2A)')
     &       'S/R PROFILES_INIT_NCFILE: defining new variable ',
     &       prof_names(num_file,num_var)
            CALL PRINT_MESSAGE( msgBuf,
     &           standardMessageUnit, SQUEEZE_RIGHT, myThid )

            err = NF_DEF_VAR( fid2, prof_names(num_file,num_var),
     &            NF_DOUBLE, 2, vecid, varId(2+(num_var-1)*2) )
            CALL PROFILES_NF_ERROR(
     &           'INIT_NCFILE: NF_DEF_VAR prof_names',
     &           err,bi,bj,myThid )
            err = NF_PUT_ATT_DOUBLE( fid2, varId(2+(num_var-1)*2),
     &           '_FillValue', NF_DOUBLE, 1, 0. _d 0 )
            CALL PROFILES_NF_ERROR(
     &           'INIT_NCFILE: NF_PUT_ATT_DOUBLE varId(2+(num_var-1)*2',
     &           err,bi,bj,myThid )

            err = NF_DEF_VAR( fid2, prof_namesmask(num_file,num_var),
     &            NF_DOUBLE, 2, vecid, varId(3+(num_var-1)*2) )
            CALL PROFILES_NF_ERROR(
     &           'INIT_NCFILE: NF_DEF_VAR prof_namesmask',
     &           err,bi,bj,myThid )
            err = NF_PUT_ATT_DOUBLE( fid2, varId(3+(num_var-1)*2),
     &            '_FillValue', NF_DOUBLE, 1, 0. _d 0 )
            CALL PROFILES_NF_ERROR(
     &           'INIT_NCFILE: NF_PUT_ATT_DOUBLE varId(3+(num_var-1)*2',
     &           err,bi,bj,myThid )

          ELSE
            CALL PROFILES_NF_ERROR(
     &           'INIT_NCFILE: NF_INQ_VARID prof_names',
     &           err,bi,bj,myThid )
            WRITE(msgBuf,'(2A)')
     &       'S/R PROFILES_INIT_NCFILE: no variable ',
     &       prof_names(num_file,num_var)
            CALL PRINT_MESSAGE( msgBuf,
     &           standardMessageUnit, SQUEEZE_RIGHT, myThid )

          ENDIF
        ENDDO !DO num_var

        err = NF_ENDDEF( fid2 )
        CALL PROFILES_NF_ERROR( 'INIT_NCFILE: NF_ENDDEF fid2',
     &       err,bi,bj,myThid )
        err = NF_CLOSE( fid2 )
        CALL PROFILES_NF_ERROR( 'INIT_NCFILE: NF_CLOSE fid2',
     &       err,bi,bj,myThid )

        err = NF_OPEN( file2, NF_WRITE, fid2 )
        CALL PROFILES_NF_ERROR( 'INIT_NCFILE: NF_OPEN'//file2//')',
     &       err,bi,bj,myThid )

      ELSE !IF (profilesDoNcOutput)
        OPEN( fid2, FILE = file2, FORM = 'unformatted',
     &   STATUS = 'unknown', ACCESS = 'direct',
     &   RECL = (Zlength + 1)*WORDLENGTH*2 )

        DO k = 1, Zlength+1
          tmp_vec(k) = 0
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

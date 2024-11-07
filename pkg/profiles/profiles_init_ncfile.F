#include "PROFILES_OPTIONS.h"

C     o==========================================================o
C     | subroutine profiles_init_ncfile                          |
C     | o initialization of model counterparts files             |
C     |   for netcdf profiles data                               |
C     | started: Gael Forget 15-March-2006                       |
C     o==========================================================o

      subroutine profiles_init_ncfile(num_file,fid1,file2,fid2,length,
     & Zlength,bi,bj,myThid)

      implicit none

C ==================== Global Variables ===========================
#include "EEPARAMS.h"
#include "SIZE.h"
#include "GRID.h"
#include "DYNVARS.h"
#ifdef ALLOW_PROFILES
# include "PROFILES_SIZE.h"
# include "profiles.h"
# include "netcdf.inc"
#endif
C ==================== Routine Variables ==========================
      integer fid1,fid2,dimid1,dimid2,varid(2*NVARMAX),varid0
      integer myThid,bi,bj,err,vecid(2), length, Zlength
      character*(MAX_LEN_FNAM) file2
      integer irec, num_var,num_file
      real*8 tmp_vec(Zlength+1)
c     == end of interface ==

#ifdef ALLOW_PROFILES

      if (profilesDoNcOutput) then
c1) creation :
      err = NF_CREATE(file2 , NF_CLOBBER, fid2)
      CALL PROFILES_NF_ERROR( 'INIT_NCFILE 1: NF_CREATE',
     &      err,bi,bj,myThid )
      err = NF_DEF_DIM(fid2,'iDEPTH', Zlength ,dimid1)
      CALL PROFILES_NF_ERROR( 'INIT_NCFILE 2: NF_DEF_DIM',
     &      err,bi,bj,myThid )
      err = NF_DEF_DIM(fid2,'iPROF',length,dimid2)
      CALL PROFILES_NF_ERROR( 'INIT_NCFILE 3: NF_DEF_DIM',
     &      err,bi,bj,myThid )
      vecid(1)=dimid1
      vecid(2)=dimid2

      err = NF_DEF_VAR (fid2,'prof_ind_glob', NF_INT, 1,vecid(2),
     & varid(1))
      CALL PROFILES_NF_ERROR( 'INIT_NCFILE 4: NF_DEF_VAR',
     &      err,bi,bj,myThid )
      err = NF_PUT_ATT_INT(fid2,varid(1),'_FillValue',NF_INT,1,0)
      CALL PROFILES_NF_ERROR( 'INIT_NCFILE 5: NF_PUT_ATT_INT',
     &      err,bi,bj,myThid )

      do num_var=1,NVARMAX
      err = NF_INQ_VARID(fid1,prof_names(num_file,num_var), varid0 )
      CALL PROFILES_NF_ERROR( 'INIT_NCFILE 6: NF_INQ_VARID',
     &      err,bi,bj,myThid )
      if (err.EQ.NF_NOERR) then

      err = NF_DEF_VAR (fid2,prof_names(num_file,num_var), NF_DOUBLE,
     & 2,vecid,varid(2+(num_var-1)*2))
      CALL PROFILES_NF_ERROR( 'INIT_NCFILE 7: NF_DEF_VAR',
     &      err,bi,bj,myThid )
      err = NF_PUT_ATT_DOUBLE(fid2, varid(2+(num_var-1)*2),'_FillValue',
     & NF_DOUBLE,1, 0. _d 0 )
      CALL PROFILES_NF_ERROR( 'INIT_NCFILE 8: NF_PUT_ATT_DOUBLE',
     &      err,bi,bj,myThid )
      err = NF_DEF_VAR (fid2,prof_namesmask(num_file,num_var),
     & NF_DOUBLE, 2,vecid, varid(3+(num_var-1)*2))
      CALL PROFILES_NF_ERROR( 'INIT_NCFILE 9: NF_DEF_VAR',
     &      err,bi,bj,myThid )
      err = NF_PUT_ATT_DOUBLE(fid2,varid(3+(num_var-1)*2),'_FillValue',
     & NF_DOUBLE,1, 0. _d 0)
      CALL PROFILES_NF_ERROR( 'INIT_NCFILE10: NF_PUT_ATT_DOUBLE',
     &      err,bi,bj,myThid )

      endif
      enddo

      err=NF_ENDDEF(fid2)
      CALL PROFILES_NF_ERROR( 'INIT_NCFILE11: NF_ENDDEF',
     &      err,bi,bj,myThid )
      err=NF_CLOSE(fid2)
      CALL PROFILES_NF_ERROR( 'INIT_NCFILE11: NF_CLOSE',
     &      err,bi,bj,myThid )

      err = NF_OPEN(file2, NF_WRITE , fid2)
      CALL PROFILES_NF_ERROR( 'INIT_NCFILE13: NF_OPEN',
     &      err,bi,bj,myThid )

      else

c      call MDSFINDUNIT( fid2 , mythid )
       open( fid2, file=file2, form ='unformatted', status='unknown',
     &      access='direct', recl=  (Zlength + 1)*WORDLENGTH*2 )

       do irec=1,Zlength+1
       tmp_vec(irec)=0
       enddo
#ifdef _BYTESWAPIO
            call MDS_BYTESWAPR8(Zlength+1,tmp_vec)
#endif
       do irec=length,1,-1
       do num_var=prof_num_var_tot(num_file,bi,bj),1,-1
       write(fid2,rec=((irec-1)*prof_num_var_tot(num_file,bi,bj)
     & +num_var-1)*2 +1) tmp_vec
       write(fid2,rec=((irec-1)*prof_num_var_tot(num_file,bi,bj)
     & +num_var-1)*2 +2) tmp_vec
       enddo
       enddo

       endif

#endif

      END

C---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|

CBOP
C !ROUTINE: PROFILES_NF_ERROR

C !INTERFACE: ==========================================================
      SUBROUTINE PROFILES_NF_ERROR( message, STATUS, bi, bj, myThid )

C !DESCRIPTION:
C Print NetCDF error message

C !USES: ===============================================================
      IMPLICIT NONE

#ifdef ALLOW_PROFILES
# include "SIZE.h"
# include "EEPARAMS.h"
# include "PARAMS.h"
# include "netcdf.inc"
#endif

C !INPUT PARAMETERS: ===================================================
C message   :: optional message
C STATUS    :: NetCDF error status
C bi,bj     :: Tile indices
C myThid    :: my Thread Id number
      CHARACTER*(*) message
      INTEGER STATUS, bi, bj, myThid

C !OUTPUT PARAMETERS: ==================================================

#ifdef ALLOW_PROFILES
C !FUNCTIONS: ==========================================================
      INTEGER ILNBLNK
      EXTERNAL ILNBLNK

C !LOCAL VARIABLES: ====================================================
      INTEGER IL
      CHARACTER*(MAX_LEN_MBUF) msgBuf
CEOP

      IF (debugLevel .GE. debLevA .AND. STATUS .NE. NF_NOERR) THEN
       IL = ILNBLNK(message)
       IF ( IL .GT. 0 ) THEN
        WRITE(msgBuf,'(A,A,2I3,1X,A)') 'ESCO: PROFILES_', message(1:IL),
     &       bi, bj, NF_STRERROR(STATUS)
       ELSE
        WRITE(msgBuf,'(A,2I3,1X,A)') 'ESCO: PROFILES_',
     &       bi, bj, NF_STRERROR(STATUS)
       ENDIF
       CALL PRINT_MESSAGE( msgBuf, standardMessageUnit,
     &      SQUEEZE_RIGHT,myThid )
       CALL PRINT_ERROR( msgBuf, myThid )
      ENDIF

#endif /* ALLOW_PROFILES */

      RETURN
      END

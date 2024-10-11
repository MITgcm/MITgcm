#include "OBSFIT_OPTIONS.h"

CBOP
C !ROUTINE: OBSFIT_INIT_EQUIFILES

C !INTERFACE: 
      SUBROUTINE OBSFIT_INIT_EQUIFILES( 
     I                               num_file,
     I                               fid1,
     I                               file2,
     O                               fid2,
     I                               length,
     I                               bi,
     I                               bj,
     I                               myThid )

C !DESCRIPTION:
C Initialization of model counterparts files             
C for obsfit data 

C !USES:
      IMPLICIT NONE
C     == Global variables ===
#include "EEPARAMS.h"
#include "SIZE.h"
#include "GRID.h"
#include "DYNVARS.h"
#ifdef ALLOW_OBSFIT
# include "OBSFIT_SIZE.h"
# include "OBSFIT.h"
# include "netcdf.inc"
#endif
CEOP

C !INPUT PARAMETERS:
      INTEGER num_file, fid1, fid2
      INTEGER bi, bj, myThid, length
      CHARACTER*(MAX_LEN_FNAM) file2

#ifdef ALLOW_OBSFIT
C !LOCAL VARIABLES:
      INTEGER dimid,varid(3),varid0,err
      INTEGER irec
      Real*8  tmp_vec(2)

      IF (obsfitDoNcOutput) THEN
      
C Create a netcdf file
       err = NF_CREATE(file2,NF_CLOBBER,fid2)
       err = NF_DEF_DIM(fid2,'iSAMPLE',length,dimid)
       err = NF_DEF_VAR(fid2,'sample_ind_glob',NF_INT,1,dimid,
     &  varid(1))
       err = NF_PUT_ATT_INT(fid2,varid(1),'_FillValue',NF_INT,1,-9999)

        err = NF_INQ_VARID(fid1,obsfit_nameval,varid0)
       IF (err.EQ.NF_NOERR) THEN

         err = NF_DEF_VAR(fid2,obsfit_nameequi,
     &    NF_DOUBLE,1,dimid,varid(2))
         err = NF_PUT_ATT_DOUBLE(fid2,varid(2),
     &    '_FillValue',NF_DOUBLE,1,-9999. _d 0)
         err = NF_DEF_VAR(fid2,obsfit_namemask,
     &    NF_DOUBLE,1,dimid,varid(3))
         err = NF_PUT_ATT_DOUBLE(fid2,varid(3),
     &    '_FillValue',NF_DOUBLE,1,-9999. _d 0)

       ENDIF

       err=NF_ENDDEF(fid2)
       err=NF_CLOSE(fid2)

       err = NF_OPEN(file2,NF_WRITE,fid2)

      ELSE

C Create a binary file
       OPEN( fid2, file=file2, form ='unformatted', status='unknown',
     &      access='direct', recl=2*WORDLENGTH*2 )

       DO irec=1,2
        tmp_vec(irec)=0
       ENDDO
#ifdef _BYTESWAPIO
       CALL MDS_BYTESWAPR8(2,tmp_vec)
#endif

       DO irec=length,1,-1
         WRITE(fid2,rec=(irec*2-1)) tmp_vec
         WRITE(fid2,rec=(irec*2)) tmp_vec
       ENDDO

      ENDIF

#endif

      END

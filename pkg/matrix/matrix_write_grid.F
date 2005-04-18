C $Header: /u/gcmpack/MITgcm/pkg/matrix/matrix_write_grid.F,v 1.1 2005/04/18 19:41:13 spk Exp $
C $Name:  $

#include "PACKAGES_CONFIG.h"
#include "CPP_OPTIONS.h"

      SUBROUTINE MATRIX_WRITE_GRID( myThid )

      IMPLICIT NONE
 
#include "SIZE.h"
#include "EEPARAMS.h"
#include "PARAMS.h"
#include "GRID.h"
     
      INTEGER myThid
      
#ifdef ALLOW_MATRIX

C     local variables:
      _RS r4seg(Nr)
      INTEGER dUnit, k      

      IF (useMatrix) THEN
        CALL WRITE_FLD_XY_RS( 'DXF',' ',DXF,0,myThid)
        CALL WRITE_FLD_XY_RS( 'DYF',' ',DYF,0,myThid)

        call MDSFINDUNIT( dUnit, mythid )
        do k=1,Nr
           r4seg(k)=rC(k)
        enddo
#ifdef RS_IS_REAL4
        open(dUnit, file='RC', status='unknown',
     &   access='direct', recl=4*Nr )
#ifdef _BYTESWAPIO
        call MDS_BYTESWAPR4( Nr, r4seg )
#endif
#else
        open( dUnit, file='RC', status='unknown',
     &   access='direct', recl=8*Nr )
#ifdef _BYTESWAPIO
        call MDS_BYTESWAPR8( Nr, r4seg )
#endif
#endif
        write(dUnit,rec=1) r4seg
        close(dUnit)

        call MDSFINDUNIT( dUnit, mythid )
        do k=1,Nr
           r4seg(k)=drF(k)
        enddo
#ifdef RS_IS_REAL4
        open( dUnit, file='DRF', status='unknown',
     &   access='direct', recl=4*Nr )
#ifdef _BYTESWAPIO
        call MDS_BYTESWAPR4( Nr, r4seg )
#endif
#else
        open( dUnit, file='DRF', status='unknown',
     &   access='direct', recl=8*Nr )
#ifdef _BYTESWAPIO
        call MDS_BYTESWAPR8( Nr, r4seg )
#endif
#endif
        write(dUnit,rec=1) r4seg
        close(dUnit)
      ENDIF  /* useMatrix */
      
#endif /* ALLOW_MATRIX */
      RETURN 
      END

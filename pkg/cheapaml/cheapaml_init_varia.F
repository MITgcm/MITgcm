C $Header: /u/gcmpack/MITgcm/pkg/cheapaml/cheapaml_init_varia.F,v 1.1 2008/08/05 21:49:31 jmc Exp $
C $Name:  $

#include "CPP_OPTIONS.h"

CStartOfInterface
      SUBROUTINE CHEAPAML_INIT_VARIA( myThid )
C     /==========================================================\
C     | SUBROUTINE CHEAPAML_INIT_VARIA                                      |
C     | o Set cheapaml initial temp field                          |
C     |==========================================================|
      IMPLICIT NONE

C     === Global variables ===
#include "SIZE.h"
#include "EEPARAMS.h"
#include "PARAMS.h"
c #include "GRID.h"
c #include "DYNVARS.h"
#include "FFIELDS.h"
C #include "BULKF.h"
C #include "BULKF_DIAG.h"
C #ifdef CONSERV_BULKF
C #include "BULKF_CONSERV.h"
C #endif
#include "CHEAPAML.h"

C     == Routine arguments ==
C     myThid -  Number of this instance of BULKF_INIT
      INTEGER myThid
CEndOfInterface

C     == Local variables ==
C     bi,bj  - Loop counters
C     I,J
      INTEGER bi, bj
      INTEGER I, J
	INTEGER IG,JG
	_RL localt
c     INTEGER prec
c     CHARACTER*(MAX_LEN_FNAM) fn


        IF ( AirTempFile .NE. ' ' ) THEN
	write(*,*)'Tair initialized from  ->',AirTempFile
         CALL READ_FLD_XY_RL( AirTempFile,Tair,' ',0,myThid )
	ELSE
	write(*,*)'Tair initialized using standard profile'
      DO bj=1,nSy
         DO bi=1,nSx
            DO j=1,sNy
               DO i=1,sNx
                  jG = myYGlobalLo-1+(bj-1)*sNy+j
                  iG=myXGlobalLo-1+(bi-1)*sNx+i
c                  localt=exp(-dfloat((ig-57)**2+(jg-59)**2)/16.d0)
                	localt=25.d0-float((jg-1))/float((ny-1))*10.d0
                  Tair(i,j,bi,bj) = localt
               ENDDO
            ENDDO
         ENDDO
      ENDDO
	_EXCH_XY_RL(Tair, mythid)
	
        ENDIF
c

      RETURN
      END

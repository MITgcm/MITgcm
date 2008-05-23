C $Header: /u/gcmpack/MITgcm/pkg/regrid/regrid_mnc_init.F,v 1.2 2008/05/23 07:22:53 mlosch Exp $
C $Name:  $

#include "REGRID_OPTIONS.h"

C---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|
CBOP 0
C     !ROUTINE: REGRID_MNC_INIT

C     !INTERFACE:
      SUBROUTINE REGRID_MNC_INIT( myThid )

C     !DESCRIPTION:
C     Initialize MNC lookup tables for REGRID.

C     !USES:
      IMPLICIT NONE
#include "SIZE.h"
#include "EEPARAMS.h"
#include "PARAMS.h"
#include "REGRID_SIZE.h"
#include "REGRID.h"
#ifdef ALLOW_MNC
#include "MNC_COMMON.h"
#endif /* ALLOW_MNC */

C     !INPUT PARAMETERS:
      INTEGER myThid
CEOP

C     !LOCAL VARIABLES:
      INTEGER i, k, ndim
#ifdef ALLOW_MNC
      INTEGER CW_MAX_LOC
      PARAMETER ( CW_MAX_LOC = 6 )
      INTEGER dim(CW_MAX_LOC), ib(CW_MAX_LOC), ie(CW_MAX_LOC)
      CHARACTER*(MNC_MAX_CHAR) name
      character*(MNC_MAX_CHAR) dn(CW_MAX_LOC)
      
      IF (useMNC) THEN

C         DO i = 1,CW_MAX_LOC
C           dn(i)(1:MNC_MAX_CHAR)=mnc_blank_name(1:MNC_MAX_CHAR)
C           dim(i) = 0
C           ib(i) = 0
C           ie(i) = 0
C         ENDDO
C         dn(2)(1:1) = 'T'
C         dim(2)     = -1
C         ib(2)      =  1
C         ie(2)      =  1

C         DO k = 1,regrid_ngrids

C           name(1:MNC_MAX_CHAR) = mnc_blank_name(1:MNC_MAX_CHAR)
C           dn(1)(1:MNC_MAX_CHAR) = mnc_blank_name(1:MNC_MAX_CHAR)
C           write(name,'(a,i3)') 'regrid_', k
C           write(dn(1),'(a,i3)') 'regrid_', k
C           dim(1) = REGRID_nout(k)
C           ib(1)  = 1
C           ie(1)  = REGRID_nout(k)
          
C           CALL MNC_CW_ADD_GNAME(name, ndim, dim, dn, ib, ie, myThid)

C         ENDDO
        
      ENDIF

#endif /* ALLOW_MNC */

      RETURN
      END

C---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|

C $Header: /u/gcmpack/MITgcm/pkg/diagnostics/diagnostics_init_varia.F,v 1.3 2005/02/20 05:28:19 edhill Exp $
C $Name:  $

#include "DIAG_OPTIONS.h"
      
C---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|
CBOP 0
C     !ROUTINE: DIAGNOSTICS_INIT_VARIA

C     !INTERFACE:
      SUBROUTINE DIAGNOSTICS_INIT_VARIA(
     I     myThid )

C     !DESCRIPTION:
C     Initialize the qdiag array which accumulates during integration
      
C     !USES:
      IMPLICIT NONE
#include "SIZE.h"
#include "EEPARAMS.h"
#include "PARAMS.h"
#include "DIAGNOSTICS_SIZE.h"
#include "DIAGNOSTICS.h"

C     !INPUT PARAMETERS:
      INTEGER myThid
CEOP

C     !LOCAL VARIABLES:
      INTEGER i,j,n,bi,bj
      CHARACTER*(80) fn
      LOGICAL glf

#ifdef ALLOW_MNC
      INTEGER ii
      CHARACTER*(MAX_LEN_FNAM) diag_mnc_bn
      INTEGER CW_DIMS, NLEN
      PARAMETER ( CW_DIMS = 10 )
      PARAMETER ( NLEN    = 80 )
      INTEGER dim(CW_DIMS), ib(CW_DIMS), ie(CW_DIMS)
      CHARACTER*(NLEN) dn(CW_DIMS)
      CHARACTER*(NLEN) d_cw_name
      CHARACTER*(NLEN) dn_blnk
#endif /*  ALLOW_MNC  */

C     Zero out the qdiag array which accumulates during integration
      DO bj = myByLo(myThid), myByHi(myThid)
        DO bi = myBxLo(myThid), myBxHi(myThid)
          DO n = 1,numdiags
            DO j = 1-Oly,sNy+Oly
              DO i = 1-Olx,sNx+Olx
                qdiag(i,j,n,bi,bj) = 0. _d 0
              ENDDO
            ENDDO
          ENDDO
        ENDDO
      ENDDO


C     Add pickup capability
      IF (diag_pickup_read) THEN

#ifdef ALLOW_MNC
        IF (diag_pickup_read_mnc) THEN
          DO i = 1,NLEN
            dn_blnk(i:i) = ' '
          ENDDO
          DO i = 1,MAX_LEN_FNAM
            diag_mnc_bn(i:i) = ' '
          ENDDO
          WRITE(diag_mnc_bn,'(A)') 'pickup_diagnostics'

C         Update the record dimension by writing the iteration number
          CALL MNC_FILE_CLOSE_ALL_MATCHING(diag_mnc_bn, myThid)
          CALL MNC_CW_SET_UDIM(diag_mnc_bn, 1, myThid)
          
          d_cw_name(1:NLEN) = dn_blnk(1:NLEN)
          DO ii = 1,CW_DIMS
            dn(ii)(1:NLEN) = dn_blnk(1:NLEN)
          ENDDO
          d_cw_name(1:10) = 'diag_state'
          dn(1)(1:3) = 'Xp1'
          dim(1)     = sNx + 2*OLx
          ib(1)      = OLx + 1
          ie(1)      = OLx + sNx + 1
          dn(2)(1:3) = 'Yp1'
          dim(2)     = sNy + 2*OLy
          ib(2)      = OLy + 1
          ie(2)      = OLy + sNy + 1
          dn(3)(1:2) = 'Zd'
          dim(3)     = numdiags
          ib(3)      = 1
          ie(3)      = numdiags
          dn(4)(1:1) = 'T'
          dim(4)     = -1
          ib(4)      = 1
          ie(4)      = 1
          
          CALL MNC_CW_ADD_GNAME(d_cw_name, 4, 
     &         dim, dn, ib, ie, myThid)
          CALL MNC_CW_ADD_VNAME(d_cw_name, d_cw_name, 
     &         4,5, myThid)

          CALL MNC_CW_RL_R('D',diag_mnc_bn,0,0,
     &         d_cw_name, qdiag, myThid)
          
          CALL MNC_CW_DEL_VNAME(d_cw_name, myThid)
          CALL MNC_CW_DEL_GNAME(d_cw_name, myThid)
          
        ENDIF
#endif /* ALLOW_MNC */
           
        IF (diag_pickup_read_mdsio) THEN
          DO i = 1,80
            fn(i:i) = ' '
          ENDDO
          write(fn,'(a)') 'pickup_diagnostics'
          glf = globalFiles
          CALL MDSREADFIELD(fn,readBinaryPrec,glf,'RL',
     &         numdiags,qdiag,1,myThid)
        ENDIF

      ENDIF

      RETURN 
      END

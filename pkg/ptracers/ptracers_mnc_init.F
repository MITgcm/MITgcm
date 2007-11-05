C $Header: /u/gcmpack/MITgcm/pkg/ptracers/ptracers_mnc_init.F,v 1.5 2007/11/05 18:48:04 jmc Exp $
C $Name:  $

#include "PTRACERS_OPTIONS.h"

C---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|
CBOP
C     !ROUTINE:

C     !INTERFACE:
      SUBROUTINE PTRACERS_MNC_INIT( myThid )

C     !DESCRIPTION:
C     Initialize PTRACERS parameters, read in data.ptracers

C     !USES:
      IMPLICIT NONE
#include "SIZE.h"
#include "EEPARAMS.h"
#include "PTRACERS_SIZE.h"
#include "PTRACERS_PARAMS.h"
#include "PARAMS.h"

C     !INPUT PARAMETERS:
      INTEGER myThid
CEOP

#ifdef ALLOW_PTRACERS
#ifdef ALLOW_MNC

C     !LOCAL VARIABLES:
      INTEGER ii, jj, ilnb
      character*(80) name
      character*(MAX_LEN_MBUF) msgbuf

C     Functions
      integer ILNBLNK

      IF (useMNC) THEN
        DO ii = 1,PTRACERS_numInUse

          DO jj = 1,80
            name(jj:jj) = ' '
          ENDDO

C         Create the tracer
          ilnb = ILNBLNK(PTRACERS_names(ii))
          IF (ilnb .LT. 1) THEN
            write(msgbuf,'(2a,i3,2a)') 'PTRACERS_MNC_INIT ERROR: ',
     &           'PTRACERS_names(',ii,') is blank--please name it in',
     &           ' ''data.ptracers'''
            CALL print_error(msgbuf, mythid)
            stop 'ABNORMAL END: S/R PTRACERS_MNC_INIT'
          ENDIF
          CALL MNC_CW_ADD_VNAME(
     &         PTRACERS_names(ii),'Cen_xy_Hn__C__t',4,5,myThid)
          WRITE(name,'(a,a)') 'surf_',PTRACERS_names(ii)(1:ilnb)
          CALL MNC_CW_ADD_VNAME(name,'Cen_xy_Hn__-__t',3,4,myThid)

C         Add the long_name attribute if its defined
          ilnb = ILNBLNK(PTRACERS_long_names(ii))
          IF (ilnb .GT. 0) THEN
            CALL MNC_CW_ADD_VATTR_TEXT(PTRACERS_names(ii),
     &           'long_name',PTRACERS_long_names(ii),myThid)
          ENDIF

C         Add the units attribute if its defined
          ilnb = ILNBLNK(PTRACERS_units(ii))
          IF (ilnb .GT. 0) THEN
            CALL MNC_CW_ADD_VATTR_TEXT(PTRACERS_names(ii),'units',
     &           PTRACERS_units(ii),myThid)
          ENDIF

        ENDDO

      ENDIF
#endif /*  ALLOW_MNC  */
#endif /* ALLOW_PTRACERS */

      RETURN
      END


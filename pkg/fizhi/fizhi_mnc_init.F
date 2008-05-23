C $Header: /u/gcmpack/MITgcm/pkg/fizhi/fizhi_mnc_init.F,v 1.9 2008/05/23 07:22:29 mlosch Exp $
C $Name:  $
      
#include "FIZHI_OPTIONS.h"

C---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|
CBOP 0
C     !ROUTINE: FIZHI_MNC_INIT

C     !INTERFACE:
      SUBROUTINE FIZHI_MNC_INIT( myThid )

C     !DESCRIPTION:
C     Create some pre-defined MNC grid types and variable types useful
C     for the FIZHI package.  Borrows heavily from MNC_CW_INIT().

C     !USES:
      implicit none
#include "SIZE.h"
#include "fizhi_SIZE.h"
#include "fizhi_land_SIZE.h"
#include "EEPARAMS.h"
#include "PARAMS.h"
#ifdef ALLOW_MNC
#include "MNC_PARAMS.h"
#include "MNC_COMMON.h"
#endif

C     !INPUT PARAMETERS:
      integer myThid
CEOP

#ifdef ALLOW_MNC

C     !LOCAL VARIABLES:
      integer CW_MAX_LOC
      parameter ( CW_MAX_LOC = 5 )
      integer i, ihorz,ihsub,ivert,itime,ihalo, is,ih, n,ntot
      integer ndim, ncomb, nvch
      character*(MNC_MAX_CHAR) name
      character*(MNC_MAX_CHAR) dn(CW_MAX_LOC)
      character*(5) horz_dat(CW_MAX_LOC), hsub_dat(CW_MAX_LOC),
     &     vert_dat(CW_MAX_LOC), time_dat(CW_MAX_LOC), 
     &     halo_dat(CW_MAX_LOC)
      integer dim(CW_MAX_LOC), ib(CW_MAX_LOC), ie(CW_MAX_LOC)

C     Functions
      integer ILNBLNK
      external ILNBLNK

C     ......12345....12345....12345....12345....12345...
      data horz_dat /
     &     '-    ', 'U    ', 'V    ', 'Cen  ', 'Cor  '  /
      data hsub_dat /
     &     'xy   ', 'x    ', 'y    ', '-    ', '     '  /
      data halo_dat /
     &     'Hn   ', 'Hy   ', '--   ', '     ', '     '  /
      data vert_dat /
     &     'Zph  ', 'Zphi ', '     ', '     ', '     '  /
      data time_dat /
     &     '-    ', 't    ', '     ', '     ', '     '  /

      IF (useMNC) THEN

C     Define a few "standard" or "convenience" types
      ncomb = 0
      DO ihorz = 1,5
        DO is = 1,3
          DO ih = 1,2
            
C           Loop just ONCE if the Horiz component is "-"
            ihsub = is
            ihalo = ih
            IF (ihorz .EQ. 1) THEN
              IF ((is .EQ. 1) .AND. (ih .EQ. 1)) THEN
                ihsub = 4
                ihalo = 3
              ELSE
                GOTO 10
              ENDIF
            ENDIF
            
            DO ivert = 1,2
              DO itime = 1,2
                
C               horiz and hsub
                name(1:MNC_MAX_CHAR) = mnc_blank_name(1:MNC_MAX_CHAR)
                n = ILNBLNK(horz_dat(ihorz))
                name(1:n) = horz_dat(ihorz)(1:n)
                ntot = n + 1              
                name(ntot:ntot) = '_'
                n = ILNBLNK(hsub_dat(ihsub))
                name((ntot+1):(ntot+n)) = hsub_dat(ihsub)(1:n)
                ntot = ntot + n

C               halo, vert, and time
                write(name((ntot+1):(ntot+5)), '(a1,2a2)')
     &               '_', halo_dat(ihalo)(1:2), '__'
                nvch = ILNBLNK(vert_dat(ivert))
                n = ntot+6+nvch-1
                name((ntot+6):(n)) = vert_dat(ivert)(1:nvch)
                write(name((n+1):(n+3)), '(a2,a1)') 
     &               '__', time_dat(itime)(1:1)

                ndim = 0
                DO i = 1,CW_MAX_LOC
                  dn(i)(1:MNC_MAX_CHAR)=mnc_blank_name(1:MNC_MAX_CHAR)
                  dim(i) = 0
                  ib(i) = 0
                  ie(i) = 0
                ENDDO

C               Horizontal dimensions
                IF (halo_dat(ihalo)(1:5) .EQ. 'Hn   ') THEN

                  IF (hsub_dat(ihsub)(1:1) .EQ. 'x') THEN
                    ndim = ndim + 1
                    IF ( (horz_dat(ihorz)(1:3) .EQ. 'Cen')
     &                   .OR. (horz_dat(ihorz)(1:1) .EQ. 'V') ) THEN
                      dn(ndim)(1:1) = 'X'
                      dim(ndim) = sNx + 2*OLx
                      ib(ndim)  = OLx + 1
                      ie(ndim)  = OLx + sNx
                    ENDIF
                    IF ( (horz_dat(ihorz)(1:3) .EQ. 'Cor')
     &                   .OR. (horz_dat(ihorz)(1:1) .EQ. 'U') ) THEN
                      dn(ndim)(1:3) = 'Xp1'
                      dim(ndim) = sNx + 2*OLx
                      ib(ndim)  = OLx + 1
                      ie(ndim)  = OLx + sNx + 1
                    ENDIF
                  ENDIF
                  IF ((hsub_dat(ihsub)(1:1) .EQ. 'y')
     &                 .OR. (hsub_dat(ihsub)(2:2) .EQ. 'y')) THEN
                    ndim = ndim + 1
                    IF ( (horz_dat(ihorz)(1:3) .EQ. 'Cen')
     &                   .OR. (horz_dat(ihorz)(1:1) .EQ. 'U') ) THEN
                      dn(ndim)(1:1) = 'Y'
                      dim(ndim) = sNy + 2*OLy
                      ib(ndim)  = OLy + 1
                      ie(ndim)  = OLy + sNy
                    ENDIF
                    IF ( (horz_dat(ihorz)(1:3) .EQ. 'Cor')
     &                   .OR. (horz_dat(ihorz)(1:1) .EQ. 'V') ) THEN
                      dn(ndim)(1:3) = 'Yp1'
                      dim(ndim) = sNy + 2*OLy
                      ib(ndim)  = OLy + 1
                      ie(ndim)  = OLy + sNy + 1
                    ENDIF
                  ENDIF

                ELSEIF (halo_dat(ihalo)(1:5) .EQ. 'Hy   ') THEN

                  IF (hsub_dat(ihsub)(1:1) .EQ. 'x') THEN
                    ndim = ndim + 1
                    dn(ndim)(1:3) = 'Xwh'
                    dim(ndim) = sNx + 2*OLx
                    ib(ndim)  = 1
                    ie(ndim)  = sNx + 2*OLx
                  ENDIF
                  IF ((hsub_dat(ihsub)(1:1) .EQ. 'y')
     &                 .OR. (hsub_dat(ihsub)(2:2) .EQ. 'y')) THEN
                    ndim = ndim + 1
                    dn(ndim)(1:3) = 'Ywh'
                    dim(ndim) = sNy + 2*OLy
                    ib(ndim)  = 1
                    ie(ndim)  = sNy + 2*OLy
                  ENDIF

                ENDIF

C               Vertical dimension
                IF (vert_dat(ivert)(1:5) .EQ. 'Phys ') THEN
                  ndim = ndim + 1
                  dn(ndim)(1:5) = 'Zphys'
                  dim(ndim) = NrPhys
                  ib(ndim)  = 1
                  ie(ndim)  = NrPhys
                ENDIF
                IF (vert_dat(ivert)(1:5) .EQ. 'PhysI') THEN
                  ndim = ndim + 1
                  dn(ndim)(1:7) = 'Zphysm1'
                  dim(ndim) = NrPhys - 1
                  ib(ndim)  = 1
                  ie(ndim)  = NrPhys - 1
                ENDIF

C               Time dimension
                IF ( (time_dat(itime)(1:1) .EQ. 't')
     &               .and. (ndim .ne. 0) ) THEN
                  ndim = ndim + 1
                  dn(ndim)(1:1) = 'T'
                  dim(ndim) = -1
                  ib(ndim)  = 1
                  ie(ndim)  = 1
                ENDIF

                IF (ndim .GT. 0) THEN

                  CALL MNC_CW_ADD_GNAME(name, ndim, 
     &                 dim, dn, ib, ie, myThid)
                ENDIF

              ENDDO
            ENDDO

 10         CONTINUE
          ENDDO
        ENDDO
      ENDDO

C     Define a "fizhi_veg" type to be used in FIZHI_INIT_VEG()
C
C     name           1234567890
      ndim = 3
      dn(1)(1:10) = 'X         '
      dim(1)      = sNx + 2*OLx
      ib(1)       = OLx + 1
      ie(1)       = OLx + sNx
      dn(2)(1:10) = 'Y         '
      dim(2)      = sNy + 2*OLy
      ib(2)       = OLy + 1
      ie(2)       = OLy + sNy
      dn(3)(1:10) = 'VegType   '
      dim(3)      = maxtyp
      ib(3)       = 1
      ie(3)       = maxtyp

      CALL MNC_CW_ADD_GNAME('fizhi_veg', ndim, dim, dn,ib,ie, myThid)
      CALL MNC_CW_ADD_VNAME('surftype', 'fizhi_veg', 4,5, myThid)
      CALL MNC_CW_ADD_VNAME('tilefrac', 'fizhi_veg', 4,5, myThid)
      
C     Used for the pickups
      CALL MNC_CW_ADD_VNAME('uphy', 'U_xy_Hn__Zph__-'  ,4,5,myThid)
      CALL MNC_CW_ADD_VNAME('vphy', 'V_xy_Hn__Zph__-'  ,4,5,myThid)
      CALL MNC_CW_ADD_VNAME('thphy','Cen_xy_Hn__Zph__-',4,5,myThid)
      CALL MNC_CW_ADD_VNAME('sphy', 'Cen_xy_Hn__Zph__-',4,5,myThid)

C     For the veg IO
C     name           1234567890
      ndim = 1
      dn(1)(1:10) = 'chips     '
      dim(1)      = nchp
      ib(1)       = 1
      ie(1)       = nchp
      CALL MNC_CW_ADD_GNAME('fizhi_chip', ndim, dim, dn,ib,ie, myThid)
      CALL MNC_CW_ADD_VNAME('ctmt', 'fizhi_chip', 2,3, myThid)
      CALL MNC_CW_ADD_VNAME('xxmt', 'fizhi_chip', 2,3, myThid)
      CALL MNC_CW_ADD_VNAME('yymt', 'fizhi_chip', 2,3, myThid)
      CALL MNC_CW_ADD_VNAME('zetamt', 'fizhi_chip', 2,3, myThid)
      CALL MNC_CW_ADD_VNAME('tcanopy', 'fizhi_chip', 2,3, myThid)
      CALL MNC_CW_ADD_VNAME('tdeep', 'fizhi_chip', 2,3, myThid)
      CALL MNC_CW_ADD_VNAME('ecanopy', 'fizhi_chip', 2,3, myThid)
      CALL MNC_CW_ADD_VNAME('swetshal', 'fizhi_chip', 2,3, myThid)
      CALL MNC_CW_ADD_VNAME('swetroot', 'fizhi_chip', 2,3, myThid)
      CALL MNC_CW_ADD_VNAME('swetdeep', 'fizhi_chip', 2,3, myThid)
      CALL MNC_CW_ADD_VNAME('snodep', 'fizhi_chip', 2,3, myThid)
      CALL MNC_CW_ADD_VNAME('capac', 'fizhi_chip', 2,3, myThid)
      CALL MNC_CW_ADD_VNAME('chlt', 'fizhi_chip', 2,3, myThid)
      CALL MNC_CW_ADD_VNAME('chlon', 'fizhi_chip', 2,3, myThid)
      CALL MNC_CW_ADD_VNAME('igrd', 'fizhi_chip', 2,3, myThid)
      CALL MNC_CW_ADD_VNAME('ityp', 'fizhi_chip', 2,3, myThid)
      CALL MNC_CW_ADD_VNAME('chfr', 'fizhi_chip', 2,3, myThid)

C     name           1234567890
      ndim = 2
      dn(1)(1:10) = 'chips     '
      dim(1)      = nchp
      ib(1)       = 1
      ie(1)       = nchp
      dn(2)(1:10) = 'Zph       '
      dim(2)      = NrPhys
      ib(2)       = 1
      ie(2)       = NrPhys
      CALL MNC_CW_ADD_GNAME('fizhi_chiplev',ndim,dim,dn,ib,ie,myThid)
      CALL MNC_CW_ADD_VNAME('xlmt', 'fizhi_chiplev', 3,4, myThid)
      CALL MNC_CW_ADD_VNAME('khmt', 'fizhi_chiplev', 3,4, myThid)
      CALL MNC_CW_ADD_VNAME('tke', 'fizhi_chiplev', 3,4, myThid)

      ENDIF

#endif  /*  ALLOW_MNC  */
      
      RETURN
      END

C---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|

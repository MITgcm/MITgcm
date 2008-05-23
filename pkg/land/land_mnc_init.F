C $Header: /u/gcmpack/MITgcm/pkg/land/land_mnc_init.F,v 1.3 2008/05/23 07:22:40 mlosch Exp $
C $Name:  $
      
#include "LAND_OPTIONS.h"

C---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|
CBOP 0
C     !ROUTINE: LAND_MNC_INIT

C     !INTERFACE:
      SUBROUTINE LAND_MNC_INIT( 
     I     sNx,sNy, OLx,OLy, nSx,nSy, nPx,nPy, Nland_lev, 
     I     myThid )

C     !DESCRIPTION:
C     Create the pre-defined grid types and variable types.

C     The grid type is a character string that encodes the presence and
C     types associated with the four possible dimensions.  The character
C     string follows the format
C     \begin{center}
C       \texttt{H0\_H1\_H2\_\_V\_\_T}
C     \end{center}
C     where the terms \textit{H0}, \textit{H1}, \textit{H2}, \textit{V},
C     \textit{T} can be almost any combination of the following:
C     \begin{center}
C       \begin{tabular}[h]{|ccc|c|c|}\hline
C         \multicolumn{3}{|c|}{Horizontal} & Vertical & Time \\
C         \textit{H0}: location & \textit{H1}: dimensions & \textit{H2}: halo 
C               & \textit{V}: location & \textit{T}: level  \\\hline
C         \texttt{-} & xy & Hn & \texttt{-} & \texttt{-} \\
C         U  &  x  &  Hy  &  i  &  t  \\
C         V  &  y  &      &  c  &     \\
C         Cen  &   &      &     &     \\
C         Cor  &   &      &     &     \\\hline
C       \end{tabular}
C     \end{center}

C     !USES:
      implicit none
#ifdef ALLOW_MNC
#include "MNC_COMMON.h"
#endif  /*  ALLOW_MNC  */
#include "EEPARAMS.h"

C     !INPUT PARAMETERS:
      integer myThid
      integer sNx,sNy, OLx,OLy, nSx,nSy, nPx,nPy
      integer Nland_lev
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
     &     'Zland', '     ', '     ', '     ', '     '  /
      data time_dat /
     &     '-    ', 't    ', '     ', '     ', '     '  /


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
            
            DO ivert = 1,1
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
                IF (vert_dat(ivert)(1:5) .EQ. 'Zland') THEN
                  ndim = ndim + 1
                  dn(ndim)(1:5) = 'Zland'
                  dim(ndim) = Nland_lev
                  ib(ndim)  = 1
                  ie(ndim)  = Nland_lev
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

C     Now add the variable definitions
      CALL MNC_CW_ADD_VNAME(
     &     'land_groundT','Cen_xy_Hn__Zland__t',4,5,myThid)
      CALL MNC_CW_ADD_VATTR_TEXT('land_groundT','units','---',myThid)

      CALL MNC_CW_ADD_VNAME(
     &     'land_enthalp','Cen_xy_Hn__Zland__t',4,5,myThid)
      CALL MNC_CW_ADD_VATTR_TEXT('land_enthalp','units','---',myThid)

      CALL MNC_CW_ADD_VNAME(
     &     'land_groundW','Cen_xy_Hn__Zland__t',4,5,myThid)
      CALL MNC_CW_ADD_VATTR_TEXT('land_groundW','units','---',myThid)

      CALL MNC_CW_ADD_VNAME('land_skinT','Cen_xy_Hn__-__t',3,4,myThid)
      CALL MNC_CW_ADD_VATTR_TEXT('land_skinT','units','---',myThid)
      CALL MNC_CW_ADD_VNAME('land_hSnow','Cen_xy_Hn__-__t',3,4,myThid)
      CALL MNC_CW_ADD_VATTR_TEXT('land_hSnow','units','---',myThid)
      CALL MNC_CW_ADD_VNAME('land_snAge','Cen_xy_Hn__-__t',3,4,myThid)
      CALL MNC_CW_ADD_VATTR_TEXT('land_snAge','units','---',myThid)
      CALL MNC_CW_ADD_VNAME('land_RunOff','Cen_xy_Hn__-__t',3,4,myThid)
      CALL MNC_CW_ADD_VATTR_TEXT('land_RunOff','units','---',myThid)
      CALL MNC_CW_ADD_VNAME('land_enRnOf','Cen_xy_Hn__-__t',3,4,myThid)
      CALL MNC_CW_ADD_VATTR_TEXT('land_enRnOf','units','---',myThid)

      CALL MNC_CW_ADD_VNAME('land_HeatFx','Cen_xy_Hn__-__t',3,4,myThid)
      CALL MNC_CW_ADD_VATTR_TEXT('land_HeatFx','units','---',myThid)
      CALL MNC_CW_ADD_VNAME('land_frWaFx','Cen_xy_Hn__-__t',3,4,myThid)
      CALL MNC_CW_ADD_VATTR_TEXT('land_frWaFx','units','---',myThid)
      CALL MNC_CW_ADD_VNAME('land_EnWaFx','Cen_xy_Hn__-__t',3,4,myThid)
      CALL MNC_CW_ADD_VATTR_TEXT('land_EnWaFx','units','---',myThid)

#endif  /*  ALLOW_MNC  */
      
      RETURN
      END

C---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|

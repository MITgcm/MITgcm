C $Header: /u/gcmpack/MITgcm/pkg/mnc/mnc_cw_init.F,v 1.4 2008/05/22 12:21:19 mlosch Exp $
C $Name:  $
      
#include "MNC_OPTIONS.h"
      
C---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|
CBOP 0
C     !ROUTINE: MNC_CW_INIT

C     !INTERFACE:
      SUBROUTINE MNC_CW_INIT( 
     I     sNx,sNy, OLx,OLy, nSx,nSy, nPx,nPy, Nr, 
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
#include "MNC_COMMON.h"
#include "EEPARAMS.h"

C     !INPUT PARAMETERS:
      integer myThid
      integer sNx,sNy, OLx,OLy, nSx,nSy, nPx,nPy, Nr
CEOP

C     !LOCAL VARIABLES:
      integer CW_MAX_LOC
      parameter ( CW_MAX_LOC = 6 )
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
     &     '-    ', 'U    ', 'V    ', 'Cen  ', 'Cor  ', '     '  /
      data hsub_dat /
     &     'xy   ', 'x    ', 'y    ', '-    ', '     ', '     '  /
      data halo_dat /
     &     'Hn   ', 'Hy   ', '--   ', '     ', '     ', '     '  /
      data vert_dat /
     &     '-    ', 'C    ', 'I    ', 'L    ', 'U    ', 'S    '  /
      data time_dat /
     &     '-    ', 't    ', '     ', '     ', '     ', '     '  /

C     Create the types
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
            
            DO ivert = 1,6
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
                IF (vert_dat(ivert)(1:1) .EQ. 'C') THEN
                  ndim = ndim + 1
                  dn(ndim)(1:1) = 'Z'
                  dim(ndim) = Nr
                  ib(ndim)  = 1
                  ie(ndim)  = Nr
                ENDIF
                IF (vert_dat(ivert)(1:1) .EQ. 'I') THEN
                  ndim = ndim + 1
                  dn(ndim)(1:3) = 'Zp1'
                  dim(ndim) = Nr + 1
                  ib(ndim)  = 1
                  ie(ndim)  = Nr + 1
                ENDIF
                IF (vert_dat(ivert)(1:1) .EQ. 'L') THEN
                  ndim = ndim + 1
                  dn(ndim)(1:2) = 'Zl'
                  dim(ndim) = Nr
                  ib(ndim)  = 1
                  ie(ndim)  = Nr
                ENDIF
                IF (vert_dat(ivert)(1:1) .EQ. 'U') THEN
                  ndim = ndim + 1
                  dn(ndim)(1:2) = 'Zu'
                  dim(ndim) = Nr
                  ib(ndim)  = 1
                  ie(ndim)  = Nr
                ENDIF
                IF (vert_dat(ivert)(1:1) .EQ. 'M') THEN
                  ndim = ndim + 1
                  dn(ndim)(1:3) = 'Zm1'
                  dim(ndim) = Nr - 1
                  ib(ndim)  = 1
                  ie(ndim)  = Nr - 1
                ENDIF

C               Time dimension
                IF (time_dat(itime)(1:1) .EQ. 't') THEN
                  ndim = ndim + 1
                  dn(ndim)(1:1) = 'T'
                  dim(ndim) = -1
                  ib(ndim)  = 1
                  ie(ndim)  = 1
                ENDIF

                IF (ndim .GT. 0) THEN
#ifdef MNC_DEBUG
                  ncomb = ncomb + 1
                  write(*,'(i4,a3,a15,i3,a3,5i4,a4,5i4,a4,5i4,6a4)')
     &                 ncomb, ' : ', name(1:15), ndim, 
     &                 ' : ', (dim(i), i=1,5), 
     &                 '  | ', (ib(i), i=1,5),
     &                 '  | ', (ie(i), i=1,5),
     &                 '  | ', (dn(i)(1:4), i=1,5)
#endif

                  CALL MNC_CW_ADD_GNAME(name, ndim, 
     &                 dim, dn, ib, ie, myThid)
                ENDIF

              ENDDO
            ENDDO

 10         CONTINUE
          ENDDO
        ENDDO
      ENDDO
      
      RETURN
      END

C---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|

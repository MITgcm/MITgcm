#include "CPP_OPTIONS.h"

      subroutine ctrl_smooth (
     U     fld ,mask)

c     Apply horizontal smoothing to global _RL 2-D array

      IMPLICIT NONE
#include "SIZE.h"

c     input
c     bi, bj : array indices
c     k      : vertical index used for masking
      integer k, bi, bj
      integer itlo,ithi
      integer jtlo,jthi

c     input/output
c     fld    : 2-D array to be smoothed
      _RL fld( 1-OLx:sNx+OLx, 1-OLy:sNy+OLy ,nSx,nSy)
      _RL mask( 1-OLx:sNx+OLx, 1-OLy:sNy+OLy,nr,nSx,nSy )


c     local
      integer i, j, im1, ip1, jm1, jp1
      _RL tempVar
      _RL fld_tmp( 1-OLx:sNx+OLx, 1-OLy:sNy+OLy )

      integer   imin      , imax          , jmin      , jmax
      parameter(imin=2-OLx, imax=sNx+OLx-1, jmin=2-OLy, jmax=sNy+OLy-1)

      _RL        p0    , p5    , p25     , p125      , p0625
      parameter( p0=0.0, p5=0.5, p25=0.25, p125=0.125, p0625=0.0625 )
      jtlo = 1
      jthi = nsy
      itlo = 1
      ithi = nsx
      k=1
      do bj = jtlo,jthi
         do bi = itlo,ithi
            DO j = jmin, jmax
         jm1 = j-1
         jp1 = j+1
         DO i = imin, imax
            im1 = i-1
            ip1 = i+1
            tempVar =
     &           p25   *   mask(i  ,j  ,k,bi,bj)   +
     &           p125  * ( mask(im1,j  ,k,bi,bj)   +
     &                     mask(ip1,j  ,k,bi,bj)   +
     &                     mask(i  ,jm1,k,bi,bj)   +
     &                     mask(i  ,jp1,k,bi,bj) ) +
     &           p0625 * ( mask(im1,jm1,k,bi,bj)   +
     &                     mask(im1,jp1,k,bi,bj)   +
     &                     mask(ip1,jm1,k,bi,bj)   +
     &                     mask(ip1,jp1,k,bi,bj) )
            IF ( tempVar .GE. p25 ) THEN
               fld_tmp(i,j) = (
     &              p25  * fld(i  ,j,bi,bj  )*mask(i  ,j  ,k,bi,bj) +
     &              p125 *(fld(im1,j ,bi,bj )*mask(im1,j  ,k,bi,bj) +
     &                     fld(ip1,j ,bi,bj )*mask(ip1,j  ,k,bi,bj) +
     &                     fld(i  ,jm1,bi,bj)*mask(i  ,jm1,k,bi,bj) +
     &                     fld(i  ,jp1,bi,bj)*mask(i  ,jp1,k,bi,bj))+
     &              p0625*(fld(im1,jm1,bi,bj)*mask(im1,jm1,k,bi,bj) +
     &                     fld(im1,jp1,bi,bj)*mask(im1,jp1,k,bi,bj) +
     &                     fld(ip1,jm1,bi,bj)*mask(ip1,jm1,k,bi,bj) +
     &                     fld(ip1,jp1,bi,bj)*mask(ip1,jp1,k,bi,bj)))
     &              / tempVar
            ELSE
               fld_tmp(i,j) = fld(i,j,bi,bj)
            ENDIF
         ENDDO
      ENDDO

c     transfer smoothed field to output array
      DO j = jmin, jmax
         DO i = imin, imax
            fld(i,j,bi,bj) = fld_tmp(i,j)
         ENDDO
      ENDDO
         ENDDO
      ENDDO


      return
      end

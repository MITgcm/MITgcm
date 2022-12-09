C$Id$
      program testmemsize
      external countsetbits
      integer countsetbits
      integer i, j, ti(10), ci0(10), ci1(10)
      integer*8 i8, ti8(10), ci80(10), ci81(10)
      real r, tr(10), cr0(10), cr1(10)
      real*4 r4, tr4(10), cr40(10), cr41(10)
      real(4) rp4, trp4(10), crp40(10), crp41(10)
      real*8 r8, tr8(10), cr80(10), cr81(10)
      real(8) rp8, trp8(10), crp80(10), crp81(10)
c      real*16 r16, tr16(10), cr160(10), cr161(10)
      double precision d, td(10), cd0(10), cd1(10)
      complex cx, tcx(10), ccx0(10), ccx1(10)
      complex*8 cx8, tcx8(10), ccx80(10), ccx81(10)
      complex(8) cxp8, tcxp8(10), ccxp80(10), ccxp81(10)
      complex*16 cx16, tcx16(10), ccx160(10), ccx161(10)
      double complex cxd, tcxd(10), ccxd0(10), ccxd1(10)
      logical l, tl(10), cl0(10), cl1(10)
      character c, tc(10), cc0(10), cc1(10)
c
      integer tci0(10000)
      common /cbuf0/ tci0
      equivalence (tci0(1),ci0(1))
      equivalence (tci0(1),ci80(1))
      equivalence (tci0(1),cr0(1))
      equivalence (tci0(1),cr40(1))
      equivalence (tci0(1),crp40(1))
      equivalence (tci0(1),cr80(1))
      equivalence (tci0(1),crp80(1))
c      equivalence (tci0(1),cr160(1))
      equivalence (tci0(1),cd0(1))
      equivalence (tci0(1),ccx0(1))
      equivalence (tci0(1),ccx80(1))
      equivalence (tci0(1),ccxp80(1))
      equivalence (tci0(1),ccx160(1))
      equivalence (tci0(1),ccxd0(1))
      equivalence (tci0(1),cl0(1))
      equivalence (tci0(1),cc0(1))
c
      integer tci1(10000)
      common /cbuf1/ tci1
      equivalence (tci1(1),ci1(1))
      equivalence (tci1(1),ci81(1))
      equivalence (tci1(1),cr1(1))
      equivalence (tci1(1),cr41(1))
      equivalence (tci1(1),crp41(1))
      equivalence (tci1(1),cr81(1))
      equivalence (tci1(1),crp81(1))
c      equivalence (tci1(1),cr161(1))
      equivalence (tci1(1),cd1(1))
      equivalence (tci1(1),ccx1(1))
      equivalence (tci1(1),ccx81(1))
      equivalence (tci1(1),ccxp81(1))
      equivalence (tci1(1),ccx161(1))
      equivalence (tci1(1),ccxd1(1))
      equivalence (tci1(1),cl1(1))
      equivalence (tci1(1),cc1(1))
c
      integer nb
c
      do i = 1,10
         ti(i) = (i-1)*i*i*i*i*i*i*i*i*i
         ti8(i) = (i-1)*i*i*i*i*i*i*i*i*i
         tr(i) = (i-1)*i*exp(2.0*i+0.001)
         tr4(i) = (i-1)*i*exp(2.0*i+0.001)
         trp4(i) = (i-1)*i*exp(2.0*i+0.001)
         tr8(i) = (i-1)*i*exp(2.0*i+0.001)
         trp8(i) = (i-1)*i*exp(2.0*i+0.001)
c         tr16(i) = (i-1)*i*exp(2.0*i+0.001)
         td(i) = (i-1)*i*exp(2.d0*i+1.d-3)
         tcx(i) = cmplx(tr(i),tr(i)+12.3)
         tcxp8(i) = cmplx(trp8(i),tr(i)+12.3)
         tcx8(i) = cmplx(tr8(i),tr(i)+12.3)
         tcx16(i) = cmplx(tr8(i),tr(i)+12.3)
         tcxd(i) = cmplx(td(i),td(i)+12.3d0)
         tl(i) = (((i/3)*3).eq.i)
      enddo
      tc(1) = 'a'
      tc(2) = 'b'
      tc(3) = 'c'
      tc(4) = 'd'
      tc(5) = 'e'
      tc(6) = 'f'
      tc(7) = 'g'
      tc(8) = 'h'
      tc(9) = 'i'
      tc(10) = 'j'
      
      i = 12345
      i8 = 12345
      r = sqrt(12345.0)
      r4 = sqrt(12345.0)
      rp4 = sqrt(12345.0)
      r8 = sqrt(12345.0)
      rp8 = sqrt(12345.0)
c      r16 = sqrt(12345.0)
      d = sqrt(12345.d0)
      cx = cmplx(-12.34,56.78)
      cx8 = cmplx(-12.34,56.78)
      cxp8 = cmplx(-12.34,56.78)
      cx16 = cmplx(-12.34,56.78)
      cxd = cmplx(-12.34d0,56.78d0)
      l = .true.
      c = 'z'
c
      call allzero(tci0(1))
      call allones(tci1(1))
c      call displaybits(tci0(1), 3)
c      call displaybits(tci1(1), 3)
      print *, ' '
c
      call allzero(tci0(1))
      call allones(tci1(1))
      do j=1,8
         ci0(j) = ti(j+2)
         ci1(j) = ti(j+2)
      enddo
      nb = countsetbits(tci0(1), tci1(1), 8, 100)
      print 100, 'INTEGER:', nb, ' bytes'
c
      call allzero(tci0(1))
      call allones(tci1(1))
      do j=1,8
         ci80(j) = ti8(j+2)
         ci81(j) = ti8(j+2)
      enddo
      nb = countsetbits(tci0(1), tci1(1), 8, 100)
      print 100, 'INTEGER*8:', nb, ' bytes'
c
      call allzero(tci0(1))
      call allones(tci1(1))
      do j=1,8
         cr0(j) = tr(j+2)
         cr1(j) = tr(j+2)
      enddo
      nb = countsetbits(tci0(1), tci1(1), 8, 100)
      print 100, 'REAL:', nb, ' bytes'
c
      call allzero(tci0(1))
      call allones(tci1(1))
      do j=1,8
         cr40(j) = tr4(j+2)
         cr41(j) = tr4(j+2)
      enddo
      nb = countsetbits(tci0(1), tci1(1), 8, 100)
      print 100, 'REAL*4:', nb, ' bytes'
c
      call allzero(tci0(1))
      call allones(tci1(1))
      do j=1,8
         crp40(j) = trp4(j+2)
         crp41(j) = trp4(j+2)
      enddo
      nb = countsetbits(tci0(1), tci1(1), 8, 100)
      print 100, 'REAL(4):', nb, ' bytes'
c
      call allzero(tci0(1))
      call allones(tci1(1))
      do j=1,8
         cr80(j) = tr8(j+2)
         cr81(j) = tr8(j+2)
      enddo
      nb = countsetbits(tci0(1), tci1(1), 8, 100)
      print 100, 'REAL*8:', nb, ' bytes'
c
      call allzero(tci0(1))
      call allones(tci1(1))
      do j=1,8
         crp80(j) = trp8(j+2)
         crp81(j) = trp8(j+2)
      enddo
      nb = countsetbits(tci0(1), tci1(1), 8, 100)
      print 100, 'REAL(8):', nb, ' bytes'
c
      call allzero(tci0(1))
      call allones(tci1(1))
      do j=1,8
         cd0(j) = td(j+2)
         cd1(j) = td(j+2)
      enddo
      nb = countsetbits(tci0(1), tci1(1), 8, 100)
      print 100, 'DOUBLE PRECISION:', nb, ' bytes'
c
      call allzero(tci0(1))
      call allones(tci1(1))
      do j=1,8
         ccx0(j) = tcx(j+2)
         ccx1(j) = tcx(j+2)
      enddo
      nb = countsetbits(tci0(1), tci1(1), 8, 100)
      print 100, 'COMPLEX:', nb, ' bytes'
c
      call allzero(tci0(1))
      call allones(tci1(1))
      do j=1,8
         ccx80(j) = tcx8(j+2)
         ccx81(j) = tcx8(j+2)
      enddo
      nb = countsetbits(tci0(1), tci1(1), 8, 100)
      print 100, 'COMPLEX*8:', nb, ' bytes'
c
      call allzero(tci0(1))
      call allones(tci1(1))
      do j=1,8
         ccxp80(j) = tcxp8(j+2)
         ccxp81(j) = tcxp8(j+2)
      enddo
      nb = countsetbits(tci0(1), tci1(1), 8, 100)
      print 100, 'COMPLEX(8):', nb, ' bytes'
c
      call allzero(tci0(1))
      call allones(tci1(1))
      do j=1,8
         ccx160(j) = tcx16(j+2)
         ccx161(j) = tcx16(j+2)
      enddo
      nb = countsetbits(tci0(1), tci1(1), 8, 100)
      print 100, 'COMPLEX*16:', nb, ' bytes'
c
      call allzero(tci0(1))
      call allones(tci1(1))
      do j=1,8
         ccxd0(j) = tcxd(j+2)
         ccxd1(j) = tcxd(j+2)
      enddo
      nb = countsetbits(tci0(1), tci1(1), 8, 100)
      print 100, 'DOUBLE COMPLEX:', nb, ' bytes'
c
      call allzero(tci0(1))
      call allones(tci1(1))
      do j=1,8
         cl0(j) = tl(j+2)
         cl1(j) = tl(j+2)
      enddo
      nb = countsetbits(tci0(1), tci1(1), 8, 100)
      print 100, 'LOGICAL:', nb, ' bytes'
c
      call allzero(tci0(1))
      call allones(tci1(1))
      do j=1,8
         cc0(j) = tc(j+2)
         cc1(j) = tc(j+2)
      enddo
      nb = countsetbits(tci0(1), tci1(1), 8, 100)
      print 100, 'CHARACTER:', nb, ' bytes'
c
 100  format(a18,i3,a6)
      end

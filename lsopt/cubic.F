
      subroutine cubic( t, f, fp, ta, fa, fpa, tlower, tupper )

c-----------------------------------------
c arguments
c-----------------------------------------
      double precision    t, f, fp, ta, fa, fpa, tlower, tupper

c-----------------------------------------
c local variables
c-----------------------------------------
      double precision sign, den, anum
      double precision z1, b, discri

c-----------------------------------------
c Using f and fp at t and ta,
c computes new t by cubic formula
c safeguarded inside [tlower,tupper].
c-----------------------------------------
      z1 = dble(fp) + dble(fpa) - 3.d0*dble(fa-f)/dble(ta-t)
      b  = z1 + dble(fp)

c-----------------------------------------
c first compute the discriminant
c (without overflow)
c-----------------------------------------
      if (abs(z1).le.1.) then
         discri = z1*z1-dble(fp)*dble(fpa)
         if (discri .lt. 0.d0) then
            if (fp.lt.0.) t = tupper
            if (fp.ge.0.) t = tlower
            return
         else
            discri = dsqrt(discri)
         end if
      else
         discri = dble(fp)/z1
         discri = discri*dble(fpa)
         discri = z1-discri
         if (z1.ge.0.d0 .and. discri.ge.0.d0) then
            discri = dsqrt(z1)*dsqrt(discri)
         else if (z1.le.0.d0 .and. discri.le.0.d0) then
            discri = dsqrt(-z1)*dsqrt(-discri)
         else
            if (fp.lt.0.) t = tupper
            if (fp.ge.0.) t = tlower
            return
         end if
      end if

c-----------------------------------------
c discriminant nonnegative,
c compute solution (without overflow)
c-----------------------------------------
      if (t-ta .lt. 0.0) then
         discri = -discri
      end if

      sign = (t-ta)/abs(t-ta)
      if (sngl(b)*sign .gt. 0.0) then
         t    = t + fp*(ta-t)/sngl(b+discri)
      else
         den  = sngl(z1+b+dble(fpa))
         anum = sngl(b-discri)
         if (abs((t-ta)*anum).lt.(tupper-tlower)*abs(den)) then
            t = t + anum*(ta-t)/den
         else
            t = tupper
         end if
      end if

      t = max( t, tlower )
      t = min( t, tupper )
      
      return
      end

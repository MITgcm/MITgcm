      subroutine ad_s_max_i(first,second,result)
      integer first, second, result
      if (first .lt. second) then
         result=second
      else
         result=first
      end if
      end subroutine

      subroutine ad_s_max_r(first,second,result)
      real first, second, result
      if (first .lt. second) then
         result=second
      else
         result=first
      end if
      end subroutine

      subroutine ad_s_max_d(first,second,result)
      double precision first, second, result
      if (first .lt. second) then
         result=second
      else
         result=first
      end if
      end subroutine

      subroutine ad_s_min_i(first,second,result)
      integer first, second, result
      if (first .gt. second) then
         result=second
      else
         result=first
      end if
      end subroutine

      subroutine ad_s_min_r(first,second,result)
      real first, second, result
      if (first .gt. second) then
         result=second
      else
         result=first
      end if
      end subroutine

      subroutine ad_s_min_d(first,second,result)
      double precision first, second, result
      if (first .gt. second) then
         result=second
      else
         result=first
      end if
      end subroutine

subroutine normalize_alpha(alpha,num)
implicit none

real,dimension(:),intent(inout) :: alpha
real,intent(out) :: num

! Begin

num=sum(alpha)
alpha=alpha/num
! in the calling routine, multiply the scaling param by num
return

end subroutine normalize_alpha


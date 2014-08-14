subroutine tri_mat_mult(alpha,beta,gamma,N,x,y)
! performes the multiplication A*x=y
! where A is a tridiagonal matrix stored in packed form {alpha,beta,gamma}
	implicit none
! alpha and gamma have length N-1, beta has length N
	real,dimension(:) :: alpha,beta,gamma
	real,dimension(:) :: x
	real,dimension(:) :: y
	integer,intent(in) :: N

! Local Var's
integer :: i

! Begin
! take care of i=1 and i=N
	y(1)=beta(1)*x(1)+gamma(1)*x(2)
	y(N)=alpha(N-1)*x(N-1)+beta(N)*x(N)
	do i=2,N-1
		y(i)=alpha(i-1)*x(i-1)+beta(i)*x(i)+gamma(i)*x(i+1)
	end do !i
end subroutine tri_mat_mult

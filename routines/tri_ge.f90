subroutine tri_ge(alpha,beta,gamma,N,x,b)
! solve a*x=b by Gaussian elimination where
! the matrix a is a packed tridiagonal matrix
! modified from: Numerical Methods for Physicists
!	Alejandro L. Garcia, Prentice Hall, 1994

! pass this either allocatable arrays or explicit shaped arrays but not array pointers!
	implicit none
	integer,intent(in) :: N
	real,dimension(N-1),intent(in) :: alpha,gamma
	real,dimension(N),intent(in) ::beta
	real,dimension(N),intent(in) :: b
	real,dimension(N),intent(out) :: x


! Local Var's
real,dimension(N) :: w_beta,w_b
integer :: i
real :: coeff
! Begin
! foreward elimination
w_beta=beta
w_b=b
do i=2,N
	coeff=alpha(i-1)/w_beta(i-1)
	w_beta(i)=w_beta(i)-coeff*gamma(i-1)
	w_b(i)=w_b(i)-coeff*w_b(i-1)
end do
!back substitution
x(N)=w_b(N)/w_beta(N)
do i=(N-1),1,-1
	x(i)=(w_b(i) - gamma(i)*x(i+1))/w_beta(i)
end do

return
end subroutine tri_ge

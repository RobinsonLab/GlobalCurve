subroutine first_order_anharmonic(N,rmin,dr,param_ptr,Nnot)
	use h_params
	implicit none

	integer,intent(in) :: N
	real,intent(in) :: rmin,dr
	integer,pointer,dimension(:) :: param_ptr
	real,dimension(:) :: Nnot

! Local Var's
	real :: mu,sigma
	real :: r
	integer :: i

! Begin
	mu=param_ptr(2)
	sigma=param_ptr(3)
	kappa=param_ptr(4)

	do i=1,N
		r=rmin+(i-1)*dr
! work on this later: Nnot=normalization_const*1/exp(phi(r)/kT)
! phi(r)=(1/2)k_1(r-mu)**2 + (1/6)k_2(r-mu)**3
!		Nnot(i)=1/(exp((r - mu)**2/(2.*sigma**2))*Sqrt(2*PI)*sigma)
	enddo !i
end subroutine calc_first_order_anharmonic

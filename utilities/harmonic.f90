function harmonic(N,rmin,dr,params)
! work on this !!!!!

	use h_params ! PI
	implicit none
	integer,intent(in) :: N
	real,intent(in) :: rmin,dr
	real,dimension(:) :: params
	real,dimension(N) :: harmonic

! Local Var's
	real :: mu,sigma
	real :: r
	integer :: i

! Begin
	mu=params(1)
	sigma=params(2)
	do i=1,N
		r=rmin+(i-1)*dr
		harmonic(i)=1/(exp((r - mu)**2/(2.*sigma**2))*Sqrt(2*PI)*sigma)
	enddo !i
end function

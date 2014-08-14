function calc_gamma(r)
	use h_params ! PI
	use h_dist_struct
	use numerical_libraries ! gamma
	implicit none
	real,intent(in) :: r
	real :: calc_gamma

!	s=1/lambda
!	mean=s*t
!	standard deviation=s*sqrt(t)

! intent(in)
!	gp%p1=mu and p2=sigma

! Local Vars
	real :: x,gfn,s,t
	
! Begin

	t=gp%p1
	s=gp%p2

	if (r>0) then
		x=r/s
		gfn=gamma(t)
		calc_gamma=exp(-x)*(x)**(t-1.)/(gfn*s)
	else 
		calc_gamma=0.
	endif
end function calc_gamma








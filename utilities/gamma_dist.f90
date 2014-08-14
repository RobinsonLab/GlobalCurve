function gamma_dist(r)
	use h_params ! PI
	use h_dist_struct
	use numerical_libraries, only: gamma
	implicit none
	real,intent(in) :: r
	real :: gamma_dist

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
		gamma_dist=exp(-x)*(x)**(t-1.)/(gfn*s)
	else 
		gamma_dist=0.
	endif
end function gamma_dist








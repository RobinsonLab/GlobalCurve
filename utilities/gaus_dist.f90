function gaus_dist(r)
	use h_params ! PI
	use h_dist_struct
	implicit none
	real,intent(in) :: r
	real :: gaus_dist

! intent(in)
!	gp%p1=mu and p2=sigma


! Begin
	gaus_dist=exp( -0.5*((r - gp%p1)/gp%p2)**2 )/(sqrt(2*PI)*gp%p2)

end function gaus_dist

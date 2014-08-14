function calc_gaus(r)
	use h_params ! PI
	use h_dist_struct
	implicit none
	real,intent(in) :: r
	real :: calc_gaus

! intent(in)
!	gp%p1=mu and p2=sigma


! Begin
	calc_gaus=exp( -0.5*((r - gp%p1)/gp%p2)**2 )/(sqrt(2*PI)*gp%p2)

end function calc_gaus

function calc_1_mom_gaus(r)
	use h_params ! PI
	use h_dist_struct
	real,dimension(:),intent(in) :: r
	real,dimension(size(r)) :: calc_gaus

! intent(in)
!	gp%p1=mu and p2=sigma

! Begin
	calc_gaus=r*exp( -0.5*((r - gp%p1)/gp%p2)**2 )/(sqrt(2*PI)*gp%p2)

end function calc_1_mom_gaus
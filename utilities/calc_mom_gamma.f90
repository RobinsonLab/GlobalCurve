function calc_1_mom_gamma(r)
	use h_params ! PI
	use h_dist_struct
	use h_utils, only: gamma_dist
	implicit none
	real,intent(in) :: r
	real :: calc_1_mom_gamma

!! intent(in) h_dist_struct
!!	gp%p1=mu and p2=sigma

! Begin
	calc_1_mom_gamma=r*gamma_dist(r)

end function calc_1_mom_gamma

function calc_2_mom_gamma(r)
	use h_params ! PI
	use h_dist_struct
	use h_utils, only: gamma_dist
	implicit none
	real,intent(in) :: r
	real :: calc_2_mom_gamma

! intent(in) h_dist_struct
!	gp%p1=mu and p2=sigma and mean

! Begin
	calc_2_mom_gamma=(r-gp%mean)**2*gamma_dist(r)
end function calc_2_mom_gamma

function gamma_mean_TE(r)
	use h_params ! PI
	use h_dist_struct
	use h_utils, only: gamma_dist
	implicit none
	real,intent(in) :: r
	real :: gamma_mean_TE

! intent(in) h_dist_struct
!	gp%p1=mu and p2=sigma

! Begin
	gamma_mean_TE= gamma_dist(r)*(gp%R0/r)**6

end function gamma_mean_TE
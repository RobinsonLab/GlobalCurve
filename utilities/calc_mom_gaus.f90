function calc_1_mom_gaus(r)
	use h_params ! PI
	use h_dist_struct
	use h_utils, only: gaus_dist
	implicit none
	real,intent(in) :: r
	real :: calc_1_mom_gaus

!! intent(in) h_dist_struct
!!	gp%p1=mu and p2=sigma

! Begin
	calc_1_mom_gaus=r*gaus_dist(r)

end function calc_1_mom_gaus

function calc_2_mom_gaus(r)
	use h_params ! PI
	use h_dist_struct
	use h_utils, only: gaus_dist
	implicit none
	real,intent(in) :: r
	real :: calc_2_mom_gaus

! intent(in) h_dist_struct
!	gp%p1=mu and p2=sigma and mean

! Begin
	calc_2_mom_gaus=(r-gp%mean)**2*gaus_dist(r)
end function calc_2_mom_gaus

function gaus_mean_TE(r)
	use h_params ! PI
	use h_dist_struct
	use h_utils, only: gaus_dist
	implicit none
	real,intent(in) :: r
	real :: gaus_mean_TE

! intent(in) h_dist_struct
!	gp%p1=mu and p2=sigma

! Begin
	gaus_mean_TE= gaus_dist(r)*(gp%R0/r)**6

end function gaus_mean_TE
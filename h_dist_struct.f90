module h_dist_struct
	type dist_param
		real :: t,R0,p1,p2
		real :: mean ! used by calc_2_mom_gaus
		integer :: num_life
		real,dimension(10) :: alpha,tau
	end type dist_param

	type(dist_param) :: gp

end module h_dist_struct
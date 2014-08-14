module h_gaus_struct
	type gaus_param
		real :: t,R0,mu,sigma
		integer :: num_life
		real,dimension(10) :: alpha,tau
	end type gaus_param

	type(gaus_param) :: gp

end module h_gaus_struct
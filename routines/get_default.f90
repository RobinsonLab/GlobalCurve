subroutine get_default ()
	use h_params
	implicit none

! local Vars

    integer :: ios
	real :: TE_fac
! Begin

	
	open (unit = DEFAULT_F_N,file=DEFAULT_FILENAME,status='old',iostat=ios)
	if (ios /= 0) call error_handler('Error opening input file: ',DEFAULT_FILENAME)
!	write(DEFAULT_F_N,*)default%simul
!	close (DEFAULT_F_N)
!      return
	read(DEFAULT_F_N,*,err = 1010)default%simul
	read(DEFAULT_F_N,*,err = 1010)TE_fac
	default%r_min=((1.0/TE_fac)-1)**(1.0/6.0)
	default%r_max=((1.0/(1.0-TE_fac))-1)**(1.0/6.0)
	read(DEFAULT_F_N,*,err = 1010)default%pts_per_dist
	read(DEFAULT_F_N,*,err = 1010)default%log_plot
	read(DEFAULT_F_N,*,err = 1010)default%eps
	read(DEFAULT_F_N,*,err = 1010)default%flambda
	read(DEFAULT_F_N,*,err = 1010)default%maxit
	read(DEFAULT_F_N,*,err = 1010)default%chi_sqr_tol
	read(DEFAULT_F_N,*,err = 1010)default%svd_tol
!	read(DEFAULT_F_N,*,err = 1010)default%flambda_start
	read(DEFAULT_F_N,*,err = 1010)default%flambda_div
	read(DEFAULT_F_N,*,err = 1010)default%verbose
	read(DEFAULT_F_N,*,err = 1010)default%rig_scale
	read(DEFAULT_F_N,*,err = 1010)default%conf_lim
	read(DEFAULT_F_N,*,err = 1010)default%mgs
	read(DEFAULT_F_N,*,err = 1010)default%grid_size
	default%tot_grid_size=2*default%grid_size+1
	read(DEFAULT_F_N,*,err = 1010)default%color
	read(DEFAULT_F_N,*,err = 1010)default%ps
	read(DEFAULT_F_N,*,err = 1010)default%minimizaton_type
	read(DEFAULT_F_N,*,err = 1010)default%num_boot_iter
!	read(DEFAULT_F_N,*,err = 1010)default%skip_minimization

	close (DEFAULT_F_N)

	return



1010	call error_handler('Error reading from file: ',DEFAULT_FILENAME)

end subroutine get_default
subroutine calc_mean_hw(current_model,mean,hw)
	use h_dist_struct
	use h_struct
	use h_utils
	use nr, only: qromb
	use h_params ! default%ET_fac,hw_conv

! intent(out)
!	gp%p1,p2,mean

	implicit none	
	type(model_struct), pointer :: current_model
	real,intent(out) :: mean,hw

! Local Var's

	real :: rmin,rmax,R0,norm,var

! Begin

	gp%p1=param_list(current_model%model%param(4)%param_basis(1))%val
	gp%p2=param_list(current_model%model%param(4)%param_basis(2))%val
	R0=param_list(current_model%model%param(3)%param_basis(1))%val
	rmin=R0/default%ET_fac
	rmax=R0*default%ET_fac

	norm=qromb(calc_gaus,rmin,rmax)
	mean=qromb(calc_1_mom_gaus,rmin,rmax)/norm
	gp%mean=mean
	var=qromb(calc_2_mom_gaus,rmin,rmax)/norm
	hw=hw_conv*sqrt(var)

end subroutine calc_mean_hw


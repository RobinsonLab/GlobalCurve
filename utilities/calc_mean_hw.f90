subroutine calc_mean_hw(current_model,mean,hw)
	use h_dist_struct
	use h_struct
	use h_utils
	use numerical_libraries
	use h_params ! default%ET_fac,hw_conv

! intent(out)
!	gp%p1,p2,mean

	implicit none	
	type(model_struct), pointer :: current_model
	real,intent(out) :: mean,hw

! Local Param's
	integer,parameter :: irule=2
	real,parameter :: errabs=0.0,errel=0.001
! Local Var's

	real :: rmin,rmax,R0,norm,var
	real :: res,errest
! Begin

	gp%p1=param_list(current_model%model%param(4)%param_basis(1))%val
	gp%p2=param_list(current_model%model%param(4)%param_basis(2))%val
	R0=param_list(current_model%model%param(3)%param_basis(1))%val
	rmin=R0/default%ET_fac
	rmax=R0*default%ET_fac
	call qdag(calc_gaus,rmin,rmax,errabs,errel,irule,norm,errest)
	call qdag(calc_1_mom_gaus,rmin,rmax,errabs,errel,irule,res,errest)
	mean=res/norm
	gp%mean=mean
	call qdag(calc_1_mom_gaus,rmin,rmax,errabs,errel,irule,res,errest)
	var=res/norm
	hw=hw_conv*sqrt(var)

end subroutine calc_mean_hw


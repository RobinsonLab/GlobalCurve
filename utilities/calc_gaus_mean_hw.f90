subroutine calc_gaus_mean_hw(current_model,mean,hw,TE)
	use h_dist_struct
	use h_struct
	use h_utils
	use numerical_libraries
	use h_params ! default%ET_fac,hw_conv
! this subroutine is used to calculate the mean and halfwidth of a distribution
! over a range specified by rmin and rmax calculated herein.

! intent(out)
!	gp%p1,p2,mean

	implicit none	
	type(model_struct), pointer :: current_model
	real,dimension(2),intent(out) :: mean,hw,TE ! TE is transfer efficiency

! Local Param's
	integer,parameter :: irule=2
	real,parameter :: errabs=0.0,errel=0.001
! Local Var's

	real :: rmin,rmax,R0,norm,var
	real :: res,errest,term
! Begin
	gp%p1=param_list(current_model%model%param(4)%param_basis(1))%val
	gp%p2=param_list(current_model%model%param(4)%param_basis(2))%val
	R0=param_list(current_model%model%param(3)%param_basis(1))%val
	rmin=default%r_min*R0
	rmax=default%r_max*R0
	gp%R0=R0
	call qdag(gaus_dist,rmin,rmax,errabs,errel,irule,norm,errest)
	call qdag(gaus_mean_TE,rmin,rmax,errabs,errel,irule,res,errest)
	term=res/norm
	TE(1)=term/(1.+term)
	call qdag(calc_1_mom_gaus,rmin,rmax,errabs,errel,irule,res,errest)
	mean(1)=res/norm
	gp%mean=mean(1)
	call qdag(calc_2_mom_gaus,rmin,rmax,errabs,errel,irule,res,errest)
	var=res/norm
	hw(1)=hw_conv*sqrt(var)
!	now calc things for 0..rmax
	rmin=0.
	call qdag(gaus_dist,rmin,rmax,errabs,errel,irule,norm,errest)
!	norm should be very close to 1
	call qdag(gaus_mean_TE,rmin,rmax,errabs,errel,irule,res,errest)
	term=res/norm
	TE(2)=term/(1.+term)
	call qdag(calc_1_mom_gaus,rmin,rmax,errabs,errel,irule,res,errest)
	mean(2)=res/norm ! this should equal mu
	gp%mean=mean(2)
	call qdag(calc_2_mom_gaus,rmin,rmax,errabs,errel,irule,res,errest)
	var=res/norm 
	hw(2)=hw_conv*sqrt(var) ! this should equal hw_conv*sigma
end subroutine calc_gaus_mean_hw


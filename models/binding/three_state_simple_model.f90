function three_state_simple_model(current_model,current_data) result(bind)
! bind represents the unscaled fractional saturation of the protein
! ksi in my papers.
! x_pts should be expressed as -log()
! K's are equilibrium association constants.
	use h_struct
	implicit none	
	type(model_struct), pointer :: current_model
	type (data_struct),pointer :: current_data	
	real,dimension(current_data%len) :: bind

! Global params
	! h_vars,intent(in)
	!	type(param_struct),dimension(:),intent(in) :: param_list

! Local Parameters
	real :: f_min,f_max,K1,K2
	real,dimension(current_data%len) :: x
! Hill eqn.

! Begin
! numchan=expt_data%len
f_min=param_list(current_model%model%param(1)%param_basis(1))%val
f_max=param_list(current_model%model%param(1)%param_basis(2))%val
K1=param_list(current_model%model%param(1)%param_basis(3))%val
K2=param_list(current_model%model%param(1)%param_basis(4))%val
x=10**(-current_data%x_pts) ! x is [Ca^2+]; x_pts are pCa
! K is K_d, typically in units of Molar.
bind(:)=f_min + (f_max-f_min)*x*K1*K2/(1+x*K1*(K2+1))

return
end function

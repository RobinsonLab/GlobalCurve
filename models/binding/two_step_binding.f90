function two_step_binding(current_model,current_data) result(bind)
! bind represents the unscaled fractional saturation of the protein
! x_pts and K should be expressed as -log()
	use h_struct
	implicit none	
	type(model_struct), pointer :: current_model
	type (data_struct),pointer :: current_data	
	real,dimension(current_data%len) :: bind

! Global params
	! h_vars,intent(in)
	!	type(param_struct),dimension(:),intent(in) :: param_list

! Local Parameters
	real :: f_min,f_max,Ka,pKa,Kb,pKb
	real,dimension(current_data%len) :: x


! Begin
! numchan=expt_data%len
f_min=param_list(current_model%model%param(1)%param_basis(1))%val
f_max=param_list(current_model%model%param(1)%param_basis(2))%val
pKa=param_list(current_model%model%param(1)%param_basis(3))%val
pKb=param_list(current_model%model%param(1)%param_basis(4))%val

x=10**(-current_data%x_pts) ! x is [Ca^2+]; x_pts are pCa
Ka=10**(pKa)
Kb=10**(pKb)
! K is K_d, typically in units of Molar.
bind(:)=f_min + (f_max-f_min)*(Ka*Kb*x/(1.+Ka*x+Ka*Kb*x))


return
end function two_step_binding

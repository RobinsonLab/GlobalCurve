function disc_et(current_model,current_data) result(fluor)
	use h_struct
	use h_vars
	implicit none	
	type(model_struct), pointer :: current_model
	type (data_struct),pointer :: current_data	
	real,dimension(current_data%len) :: fluor 

! can come back at a later time and have this function read from a vector 
! x_calc which specifies the x_values to calculate y_vals at.
! for now just let it count from 1 to numchan

! Global params
	! h_vars,intent(in)
	!	type(param_struct),dimension(:),intent(in) :: param_list

! Local Parameters
	real,dimension(current_data%len) :: fluorD,fluorDA
	integer :: numchan ! ,num_life
	integer :: i
	real :: tcal,t
	real,dimension(size(current_model%model%param(1)%param_basis)) :: tau,alpha
	real :: one_plus_rsix,r,R0,rsix,labRatio
	
! Begin 

	numchan=current_data%len
	alpha=param_list(current_model%model%param(1)%param_basis)%val
	tau=param_list(current_model%model%param(2)%param_basis)%val
	R0=param_list(current_model%model%param(3)%param_basis(1))%val
	r=param_list(current_model%model%param(3)%param_basis(2))%val
	labRatio=param_list(current_model%model%param(4)%param_basis(1))%val
	tcal=current_data%tcal

	rsix=(R0/r)**6
	one_plus_rsix=1+rsix
	do i=1,numchan ! i is the time counter
		t=tcal*i
!		k(:)=(1+rsix)/tau(:)
		fluorDA(i)= dot_product(alpha(:),exp(- one_plus_rsix*t/tau(:)))
		fluorD(i)= dot_product(alpha,exp(-t/tau))
	enddo !i
	if (current_model%flag == 1) then
		fluor=fluorDA*rsix
	else
		fluor=labRatio*fluorDA+(1.- labRatio)*fluorD ! index over all times
	endif

	return

end function disc_et

function order_2_anis(current_model,current_data) result(fluor)
! equations from Vergani B. et.al. Biochemistry 2000,39,2759-2768
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
	integer :: numchan ! ,num_life
	integer :: i
	real :: tcal,t
	real,dimension(3) :: theta,r
							! {theta}={theta_c,theta_s,theta_f}
							! {r}={r_0,S_s,S_f}
	
! Begin 
	numchan=current_data%len
	tcal=current_data%tcal
	r=param_list(current_model%model%param(1)%param_basis)%val
	theta=param_list(current_model%model%param(2)%param_basis)%val

	do i=1,numchan ! i is the time counter
		t=tcal*i
		fluor(i)= r(1)*exp(-t/theta(1))*( r(2)**2*r(3)**2+ &
			r(3)**2*(1-r(2)**2)*exp(-t/theta(2))+(1-r(3)**2)*exp(-t/theta(3)) )
	enddo !i	

	return
end function order_2_anis

function lifetime(current_model,current_data) result(fluor)
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
	real,dimension(size(current_model%model%param(1)%param_basis)) :: tau,alpha
	
! Begin 


! num_life=size(current_model%model%param(1)%param_basis)

numchan=current_data%len
tcal=current_data%tcal

alpha=param_list(current_model%model%param(1)%param_basis)%val
tau=param_list(current_model%model%param(2)%param_basis)%val


! scale the alpha's so that the sum of alpha's = 1
!call normalize_alpha(alpha,norm)
!param_list(current_model%model_param)%val=param_list(current_model%model_param)%val*norm
!param_list(current_model%model%param(1)%param_basis)%val=alpha
	
	do i=1,numchan ! i is the time counter
		t=tcal*i
!		temp(:) = exp(-t/tau(:))
		fluor(i)= dot_product(alpha,exp(-t/tau))
	enddo !i	

return
end function lifetime

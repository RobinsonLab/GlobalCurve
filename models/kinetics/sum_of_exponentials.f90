function sum_of_exponentials(current_model,current_data) result(fluor)
! for alpha > 0 we get an exponential decay.  For alpha < 0 we have an exponential rise
! note that everything is referenced to the initial state so we are looking at
! (1-exp(-kt)) instead of exp(-kt)

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

	integer :: numchan ! number of data points
	integer :: i
	real :: t,F0,dt
	real,dimension(size(current_model%model%param(1)%param_basis)) :: alpha,k,z
	
! Begin 


! num_life=size(current_model%model%param(1)%param_basis)

numchan=current_data%len

! tcal=current_data%tcal

alpha=param_list(current_model%model%param(1)%param_basis)%val
k=param_list(current_model%model%param(2)%param_basis)%val
F0=param_list(current_model%model%param(3)%param_basis(1))%val
dt=param_list(current_model%model%param(3)%param_basis(2))%val

! scale the alpha's so that the sum of alpha's = 1
!call normalize_alpha(alpha,norm)
!param_list(current_model%model_param)%val=param_list(current_model%model_param)%val*norm
!param_list(current_model%model%param(1)%param_basis)%val=alpha
	
	

!	forall(i=1:current_data%len)	
	do i=1,numchan ! i is the time counter
		t=current_data%x_pts(i)
		z(:)=-alpha(:)*(1.- exp(-k(:)*(t+dt)))	! note if alpha > 0 then we have an 
												! exponential decay
		fluor(i)= F0 + sum(z)
	enddo !i	
!	end forall
return
end function sum_of_exponentials


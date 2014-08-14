subroutine calc_curvature_gradient()
! use deriv_data to calculate the gradient and curvature
! it is assumed that calc_chi_sqr has already been called and produced the residuals
! that are used to calculate the gradient.
	use h_vars
	use h_params ! default
	use h_struct
	use h_routines, only : expt_fit
	implicit none

! h_vars,intent(out)
!	real(8),dimension(num_free_global_param,num_free_global_param),intent(out) :: curvature
!	real(8),dimension(num_free_global_param),intent(out) :: gradient

! h_vars,intent(in)
!	integer,intent(in) :: num_free_global_param
!	type (data_struct),pointer, dimension(:) :: data_list
!	type(expt_struct), pointer :: expt_list
!	type(param_struct),pointer,dimension(:) :: param_list
!	integer,dimension(:),intent(in) :: map_to

! Local vars
type(expt_struct), pointer :: current_expt
type (data_struct),pointer :: current_data
real(8),dimension(:,:),allocatable :: deriv_data
integer :: i,j,j1,j2,k,start,length
real :: delchange



! Begin
	gradient(:) = 0.
	curvature(:,:) = 0.
	current_expt=>expt_list
	i=1 ! counter over experiments
	expt_loop2: do 
		current_data=>data_list(i)
		length=current_data%num_points
		allocate(deriv_data(length,num_free_global_param))
		deriv_data(:,:)=0.
		start=current_data%len-length+1 ! = lims(3)
		do j=1,current_expt%num_local_free_param 
			j1=current_expt%local_map_from(j) ! gives the global param index
			j2=map_to(j1) ! gives the ls index
			delchange=param_list(j1)%val * default%eps
			if (delchange == 0.) delchange = MIN_DELCHANGE
			param_list(j1)%val = param_list(j1)%val + delchange
			call expt_fit(1,current_data,current_expt)
			param_list(j1)%val = param_list(j1)%val - 2.*delchange
			call expt_fit(-1,current_data,current_expt)
			! for now implement central derivative
			deriv_data(:,j2) = deriv_data(:,j2) + &
				( current_data%cdata_foreward(start: ) - &
				current_data%cdata_backward(start: ) )/(2.*delchange)
			! restore param_temp value
			param_list(j1)%val = param_list(j1)%val + delchange
		enddo !j

		do j=1,num_free_global_param
			gradient(j)=gradient(j)+sum( current_data%resid(:) * &
				deriv_data(:,j) * data_list(i)%weight(:) ) * &
				current_expt%scale / real(length)
			do k=j,num_free_global_param
				curvature(j,k)=curvature(j,k) + &
					sum( deriv_data(:,j)*deriv_data(:,k)*current_data%weight(:) ) * &
					current_expt%scale / real(length)
			enddo !k
		enddo !j
			
		deallocate(deriv_data)
		
		if (.not. associated(current_expt%next)) exit expt_loop2
		! no more experiments left
		! else keep going
		current_expt=>current_expt%next
		i=i+1
	enddo expt_loop2
	return
end subroutine calc_curvature_gradient

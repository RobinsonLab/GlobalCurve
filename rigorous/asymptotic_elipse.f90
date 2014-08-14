function asymptotic_elipse(current_rig) result(res)
	use h_params ! default,STRING_LEN
	use h_vars
	use h_struct
	use h_rigorous, only: f_conf
	use h_utils
	implicit none

	type(rig_logic_struct),pointer :: current_rig
	integer :: res

	! h_vars,intent(in)
	!	real(8),dimension(num_free_global_param,num_free_global_param) :: curvature

	! h_vars,intent(out)
	!	real :: tgt

! Local Var's
interface
	function calc_elipse_val(asymp_mat,asymp_vec)
		real,dimension(:,:) :: asymp_mat
		real,dimension(:) :: asymp_vec
		real :: calc_elipse_val 
	end function
	subroutine get_current_val(current_rig,index_val,current_val)
		use h_struct
		type(rig_logic_struct),pointer :: current_rig
		integer,dimension(:) :: index_val
		real,dimension(:) :: current_val
	end subroutine
end interface
	
	real :: val
	integer :: dim
	integer :: i,i2,j,j2,k
	integer :: ios
	real,dimension(current_rig%num_rig_param) :: asymp_vec,asymp_val
	real,dimension(current_rig%num_rig_param) :: current_val
	real,dimension(current_rig%num_rig_param,current_rig%num_rig_param) :: asymp_mat
	integer,dimension(current_rig%num_rig_param) :: index_val
	character(STRING_LEN) :: out_file_name
! Begin

	dim=current_rig%num_rig_param
	tgt=f_conf(global_reduced_chi_sqr,num_free_global_param,global_DOF, &
		default%conf_lim) 
	! tgt is the target chi^2 value that is used as the level of significance.
	!	it is based on the F statistic.  f_conf is called with the param default%conf_lim
	!	which is the confidence level -- usually .67

	! only the upper triangular portion of the curvature matrix is saved
	asymp_val(:)=param_list(current_rig%param_loc(:))%save_val
	do i=1,dim
		i2=map_to(current_rig%param_loc(i))
		do j=i,dim
			j2=map_to(current_rig%param_loc(j))
			asymp_mat(i,j)=curvature(i2,j2)
			asymp_mat(j,i)=asymp_mat(i,j)
		enddo !j
	enddo !i

	write(*,*)'Outputting asymptotic hyper-elipse calculations'
	ios=make_out_file_name(ans_file_name,'elp',out_file_name) 
	if (ios /= 0) call error_handler('error constructing elp file name')
	open(unit = DATA_FILE,file=out_file_name, &
		status='unknown',iostat=ios)
	if (ios /= 0) call error_handler('Error opening file: ',out_file_name)

check_dim: select case (dim)

case (1)

	do i=-default%grid_size,default%grid_size
		index_val=(/i/)
		call get_current_val(current_rig,index_val,current_val)
		asymp_vec=current_val-asymp_val
		val=calc_elipse_val(asymp_mat,asymp_vec)
		write(DATA_FILE,10)current_val,val/tgt
	enddo


case (2)

	do j=-default%grid_size,default%grid_size
		do i=-default%grid_size,default%grid_size
			index_val=(/i,j/)
			call get_current_val(current_rig,index_val,current_val)
			asymp_vec=current_val-asymp_val
			val=calc_elipse_val(asymp_mat,asymp_vec)
			write(DATA_FILE,10)current_val,val/tgt
		enddo
	enddo


case (3)

	do k=-default%grid_size,default%grid_size
		do j=-default%grid_size,default%grid_size
			do i=-default%grid_size,default%grid_size
				index_val=(/i,j,k/)
				call get_current_val(current_rig,index_val,current_val)
				asymp_vec=current_val-asymp_val
				val=calc_elipse_val(asymp_mat,asymp_vec)
				write(DATA_FILE,10)current_val,val/tgt
			enddo
		enddo
	enddo



end select check_dim


	
	close(DATA_FILE)
	res=0
return

10 format(<dim+1>F15.5)

end function
function calc_elipse_val(asymp_mat,asymp_vec)
	implicit none

	real,dimension(:,:) :: asymp_mat
	real,dimension(:) :: asymp_vec
	real :: calc_elipse_val 

! Begin
	calc_elipse_val=dot_product(asymp_vec,matmul(asymp_mat,asymp_vec))
end function calc_elipse_val

subroutine get_current_val(current_rig,index_val,current_val)
! see update_fixed_param_val subroutine for explanation.
	use h_params
	use h_struct
	implicit none

	type(rig_logic_struct),pointer :: current_rig
	integer,dimension(:) :: index_val
	real,dimension(:) :: current_val

! Begin

	current_val(:) = (current_rig%lim(:,2)-current_rig%lim(:,1)) * &
		real(index_val(:)+default%grid_size)/ &
		real(2*default%grid_size)+current_rig%lim(:,1)

end subroutine

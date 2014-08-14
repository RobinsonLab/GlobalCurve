subroutine output_rig_res(current_rig)
! Outputs the following files
!
! $.tgt : tells where to draw the confidence line
! $.dmp : contents of rig_arr
! $.rig : {x,chi_sqr} where {x} is the array of grid point locations
! $.scl : contents of rig_arr scaled as follows: x values are untouched
!			 and chi_sqr is scaled from 0..inf and 1 is where chi_sqr
!			is equal to the conficence line defined as tgt.

	use h_vars
	use h_params
	use h_utils, only : make_out_file_name,error_handler,calc_gaus_mean_hw
	use h_struct
	use h_fluor_wrapper_models, only: gaus_dist_et
	implicit none

	type(rig_logic_struct),pointer :: current_rig


	! h_vars,intent(in)
	!	character(STRING_LEN),intent(in) :: ans_file_name
	!	ingeger :: rig_arr_size
	!	type(rig_struct),allocatable,dimension(:) :: rig_arr

! Local Vars
	type(expt_struct),pointer :: current_expt
!	type(data_struct),pointer :: current_data
	type(model_struct), pointer :: current_model
	integer :: i,j,k,ios
	character(STRING_LEN) :: out_file_name
!	type(rig_struct),dimension(arr_size) :: arr2 ! only need the point portion of rig_struct
												! but we'll keep it as rig_struct.
	real,dimension(2) :: mean,hw,TE
	integer :: dim,max
	logical :: got_special_case
	real :: scl
! Begin
	write(*,*)'Outputting rigrorous error analysis results.'
	dim=current_rig%num_rig_param
	max=num_free_global_param
	ios=make_out_file_name(ans_file_name,'tgt',out_file_name) 
	if (ios /= 0) call error_handler('error constructing tgt file name')
	open(unit = DATA_FILE,file=out_file_name, &
		status='unknown',iostat=ios)
	if (ios /= 0) call error_handler('Error opening file: ',out_file_name)
	write(DATA_FILE,*)'target chi_sqr = ',tgt
	close(DATA_FILE)


! dmp

	ios=make_out_file_name(ans_file_name,'dmp',out_file_name) 
	if (ios /= 0) call error_handler('error constructing dmp file name')
	open(unit = DATA_FILE,file=out_file_name, &
		status='unknown',iostat=ios)
	if (ios /= 0) call error_handler('Error opening file: ',out_file_name)
! output: fixed_param_vals,chi_sqr,free_param_vals,num_iter
	do i=1,rig_arr_size
		write(DATA_FILE,10)rig_arr(i)%point,rig_arr(i)%free_param,rig_arr(i)%num_iter
	enddo !i
	close(DATA_FILE)

! rig
	got_special_case = .false.
	ios=make_out_file_name(ans_file_name,'rig',out_file_name) 
	if (ios /= 0) call error_handler('error constructing dmp file name')
	open(unit = DATA_FILE,file=out_file_name, &
		status='unknown',iostat=ios)
	if (ios /= 0) call error_handler('Error opening file: ',out_file_name)
	! output: fixed_param_vals,chi_sqr	
		do i=1,rig_arr_size
	! patching things together here
		! if we have distributed ET then we need to do some post processing to
		! get mean distance and half_width.
		! Loop over all experiments and models
		current_expt=>expt_list
		k=1
		expt_loop: do !loop over experiments
			current_model=>current_expt%expt
			j=1
			model_loop: do
				if (current_model%model_addr==loc(gaus_dist_et)) then
					got_special_case = .true.
					! update current model params
	! note: map_from and map_to have been reformed by the grid search constructor logic				
! diagnostics here
					param_list(map_from(:))%val = rig_arr(i)%free_param(:)
					param_list(current_rig%param_loc(:))%val = rig_arr(i)%point(1:dim)
					call calc_gaus_mean_hw(current_model,mean,hw,TE)
	
					! restore values
	!				don't bother restoring param list values
	!				param_list(map_from(:))%val = param_list(map_from(:))%save_val
!					write(*,30)mean(1),hw(1),rig_arr(i)%point(dim+1)
					scl=(rig_arr(i)%point(dim+1) - save_global_reduced_chi_sqr)/ &
						(tgt - save_global_reduced_chi_sqr)
					write(DATA_FILE,30)mean(1),hw(1),rig_arr(i)%point(dim+1),scl

				endif
				if (.not. associated(current_model%next)) exit model_loop
				! no more models left for this expt.
				! else keep going
				current_model=>current_model%next
				j=j+1
			enddo model_loop
			if (.not. associated(current_model%next)) exit expt_loop
			! no more models left for this expt.
			! else keep going
			current_model=>current_model%next
			k=k+1
		enddo expt_loop
	scl=(rig_arr(i)%point(dim+1) - save_global_reduced_chi_sqr)/ &
		(tgt - save_global_reduced_chi_sqr)
	if (.not. got_special_case) write(DATA_FILE,20)rig_arr(i)%point,scl
	enddo !i
	close(DATA_FILE)

! scl

	ios=make_out_file_name(ans_file_name,'scl',out_file_name) 
	if (ios /= 0) call error_handler('error constructing scl file name')
	open(unit = DATA_FILE,file=out_file_name, &
		status='unknown',iostat=ios)
	if (ios /= 0) call error_handler('Error opening file: ',out_file_name)
! output: scaled(fixed_param_vals,chi_sqr)
	forall(i=1:rig_arr_size)
!		rig_arr2(i)%point(1:dim)=rig_arr(i)%point(1:dim)/param_list(param_loc(:))%save_val
!		rig_arr2(i)%free_param(:)=rig_arr(i)%free_param(:)/param_list(map_from(:))%save_val
! we can fiddle with rig_arr b/c we aren't going to use it anymore.
		rig_arr(i)%point(dim+1)=(rig_arr(i)%point(dim+1) - save_global_reduced_chi_sqr)/ &
			(tgt - save_global_reduced_chi_sqr)
	end forall
!	write(DATA_FILE,*)save_global_reduced_chi_sqr
	do i=1,rig_arr_size
		write(DATA_FILE,20)rig_arr(i)%point
	enddo !i
	close(DATA_FILE)

	return

10 format(<dim+max+1>F15.5,I5)
20 format(<dim+2>F15.5)
30 format(<dim+3>F15.5)
end subroutine output_rig_res
function main() result(res)
	use h_params
	use	h_struct
	use h_routines
	use h_rigorous
	use h_vars
	implicit none

	integer :: res

! return values for res
!	0	no errors
!	1	error reading answer file
!	2	minimization failed
!	3	error analysis failed
!	4

! Global Vars

	!h_vars,intent(in)
	!	expt_list
	!	data_list
	!	param_list
	!	rig_list


	!h_vars,intent(out)
	!	character(STRING_LEN) :: ans_file_name

! Local Vars

	logical,allocatable,dimension(:) :: rig_mask
	integer,allocatable,dimension(:) :: save_map_to !dim=num_free_global_param
	type(rig_logic_struct),pointer :: current_rig
	type(expt_struct), pointer :: current_expt
	type(data_struct), pointer :: current_data
	
	integer :: i,j
	real :: t0,elapsed_time
	logical :: add_noise
	real :: lim,lim1,lim2
	integer :: res2

! Begin
	t0 = SECNDS(0.0)
	call rnset(ISEED)
	call get_default()
	write(*,10)'Enter answer file name: '
	read(*,'(a<STRING_LEN>)'),ans_file_name
	! ans_file_name='lifetime.ans'
	res2=readans() !read in the ans file and set up the logic
	if (res2 > 0) then
		! need to deallocate stuff based on the value of res2
		res = 1 ! see error code table
		return
	endif

if (default%simul) then
	write (*,*)'Simulation mode'
	write (*,10)'Add noise? '
	read (*,*) add_noise
	current_expt=>expt_list
	!loop through the experiments
	i=1 ! counter over experiments
	expt_loop: do 
		current_data=>data_list(i)
		call expt_fit(0,current_data,current_expt)
		res=0
		if (add_noise) then
			call add_noise_to_data(current_expt,current_data)
		else
			current_data%datum=current_data%cdata
		endif
		if (.not. associated(current_expt%next)) exit expt_loop
		! no more experiments left
		! else keep going
		current_expt=>current_expt%next
		i=i+1
	enddo expt_loop
	call output_results(elapsed_time,add_noise)
!	stop 'Simulation Complete'
else ! no simulation

	if (.not. skip_minimization) then
		!currently, only marquadt-levenberg minimization is supported
		res2=minimize()
		if (res2 > 0) then ! minimize returned with a reduced num of global free param
			res=2
			return
		endif
			! no use in doing rigorous error analysis under these conditions
		elapsed_time=secnds(t0)
		! save off the params that gave us a global minimum
		param_list(map_from(:))%save_val=param_list(map_from(:))%val
		save_global_reduced_chi_sqr=global_reduced_chi_sqr
		! save_val is used by output_results and by function rigorous 
		call calc_covariance()
		call output_results(elapsed_time)
	endif ! .not. skip_minimization
	select case(error_analysis_type)
	case (0) ! none

	case (1) ! grid search
!	if (associated(rig_list)) then
		allocate(rig_mask(num_total_global_param))
		allocate(save_map_to(num_free_global_param))
		save_map_to(:)=map_to(:) ! save off a copy of map_to for upcomming use
		current_rig=>rig_list
		rig_loop: do
			if (.not. default%mgs) then ! then we've got an automated grid-search
										! and we have to set up the lims			
				! rig_lim containes the standard dev. estimates from the cov matrix.
				do i=1,current_rig%num_rig_param
	!				call constructor_logic(fixed) !restore map_to if it was changed
					j=save_map_to( current_rig%param_loc(i) )
					!addressing the cov matrix which is based on the original num_free_global_param
					lim=default%rig_scale*real(sqrt( abs (cov(j,j))) )
					lim1=param_list( current_rig%param_loc(i) )%save_val - lim
					lim2=param_list( current_rig%param_loc(i) )%save_val + lim
					if ( lim1 < param_list( current_rig%param_loc(i) )%min ) then 
						lim1=param_list( current_rig%param_loc(i) )%min
						lim2=2*param_list( current_rig%param_loc(i) )%save_val - lim1 ! = sv+(sv-lim1)
					endif
					current_rig%lim(i,1)=lim1
					current_rig%lim(i,2)=lim2
				enddo !i
			endif !default%mgs
			! might as well go ahead and generate the hyper-elipse approximating the geometry
			! around the chi_sqr sirface minimum.  It will be interesting to compare this
			! result with the one generated subsequently by a call to the rigorous routine.
			call calc_curvature_gradient()
			res2=asymptotic_elipse(current_rig)
			! test res here
			! now set up for the call to rigorous
			rig_mask(:)=.false.
			rig_mask(current_rig%param_loc(:)) = .true. ! flag the parameters that will be fixed in rig anal
			! set num_free_global_param before calling constructor_logic
			num_free_global_param=num_free_global_param-current_rig%num_rig_param
			call constructor_logic((fixed .or. rig_mask)) ! reform all the logic		
			call constructor_rigorous(current_rig)
			res2=rigorous(current_rig)
			! test res here
			call destructor_rigorous()
			if (res2 > 0) then
				res=3
				return
			endif
			! reset num_free_global_param
			num_free_global_param=num_free_global_param+current_rig%num_rig_param
			if (.not. associated(current_rig%next)) exit rig_loop
			current_rig=>current_rig%next
		enddo rig_loop
	
		deallocate(rig_mask)
		deallocate(save_map_to)
	
	case (2) ! bootstrap

		res2=do_bootstrap()
		if (res2 > 0) then
			res=3
			return
		endif
	end select
endif ! siumlation

! clean up and exit

return ! 'Globals ended normally'

10 format(a,$)

end function main





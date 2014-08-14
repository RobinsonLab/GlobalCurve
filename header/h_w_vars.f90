module h_w_vars

! constructor: readans
! friends: minimize_me

	integer :: w_num_free_global_param
	
	integer,pointer,dimension(:) :: w_map_from
	integer,pointer,dimension(:) :: w_map_to
	logical,pointer,dimension(:) :: w_fixed_vec

end module h_w_vars
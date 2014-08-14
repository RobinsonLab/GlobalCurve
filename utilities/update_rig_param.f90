function update_rig_param(rig_lim,index) result(param)
	! calculate the param values at the grid point speccified by index

	use h_params
	implicit none
	
	real,dimension(:),intent(in) :: rig_lim
	integer,dimension(size(rig_lim)),intent(in) :: index
	real,dimension(size(index)) :: param

! Begin

	param= rig_lim * ( real(index)/real(GRID_SIZE) )

end function update_rig_param

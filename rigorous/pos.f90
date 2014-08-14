function pos(vec)
	! calculates the position in arr for point vec
	! the position of vec = {i,j,k} can be calculated as follows:
	! where i,j,k are in the domain {-GRID_SIZE..GRID_SIZE}
	! pos_i = i+(GRID_SIZE+1)
	! pos_j = (j+(GRID_SIZE))*TOT_GRID_SIZE + pos_i
	! pos_k = (k+(GRID_SIZE))*TOT_GRID_SIZE**2 + pos_j

	! GRID_SIZE is a global param
	! TOT_GRID_SIZE is a global param = 2*GRID_SIZE+1
	use h_params ! default
	implicit none

	integer,dimension(:),intent(in) :: vec
	integer :: pos

! Local vars
	integer :: dim 

! Begin
	dim = size(vec)
	! it is obvious that: if (dim >= !) == .true. so:
	pos = vec(1)+(default%grid_size+1)
	if (dim >= 2) &
		pos = (vec(2)+(default%grid_size))*default%tot_grid_size + pos
	if (dim == 3) &
		pos = (vec(3)+(default%grid_size))*default%tot_grid_size**2 + pos

	return
end function pos


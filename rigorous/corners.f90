function corners_2D(current_rig,count) result(res)
	use h_params
	use h_vars
	use h_struct
	use h_rigorous
	implicit none

	type(rig_logic_struct),pointer :: current_rig
	integer,intent(in) :: count
	integer :: res

	! h_vars,intent(out)
	!	rig_rig_index
	!	real,dimension(num_free_global_param) :: free_param_temp


! Local Vars
	integer :: i,j
	integer :: pos1,pos2,pos3
	integer :: dim

! Begin
		dim=current_rig%num_rig_param
		i=count
		j=count
		rig_index=(/i,j/)
		write(*,90)rig_index,pos((/i,j/))
		pos1=pos( (/i-1,j-1/) )
		pos2=pos( (/i-1,j/) ) ! up
		pos3=pos( (/i,j-1/) ) ! right
		free_param_temp(:) = rig_arr(pos2)%free_param(:) + &
			rig_arr(pos3)%free_param(:) - rig_arr(pos1)%free_param(:)
		res=crunch(current_rig)

	! lower right
		i=count
		j=-count
		rig_index=(/i,j/)
		write(*,90)rig_index,pos((/i,j/))
		pos1=pos( (/i-1,j+1/) )
		pos2=pos( (/i-1,j/) ) ! down
		pos3=pos( (/i,j+1/) ) ! right
		free_param_temp(:) = rig_arr(pos2)%free_param(:) + &
			rig_arr(pos3)%free_param(:) - rig_arr(pos1)%free_param(:)
		res=crunch(current_rig)

	! lower left
		i=-count
		j=-count
		rig_index=(/i,j/)
		write(*,90)rig_index,pos((/i,j/))
		pos1=pos( (/i+1,j+1/) )
		pos2=pos( (/i,j+1/) ) ! left
		pos3=pos( (/i+1,j/) ) ! down
		free_param_temp(:) = rig_arr(pos2)%free_param(:) + &
			rig_arr(pos3)%free_param(:) - rig_arr(pos1)%free_param(:)
		res=crunch(current_rig)

	! upper left
		i=-count
		j=count
		rig_index=(/i,j/)
		write(*,90)rig_index,pos((/i,j/))
		pos1=pos( (/i+1,j-1/) )
		pos2=pos( (/i,j-1/) ) ! left
		pos3=pos( (/i+1,j/) ) ! up
		free_param_temp(:) = rig_arr(pos2)%free_param(:) + &
			rig_arr(pos3)%free_param(:) - rig_arr(pos1)%free_param(:)
		res=crunch(current_rig)

	return
90 format('grid point: {',<dim>I4,' }')
end function corners_2D

function corners_3D(current_rig,count) result(res)
! to calculate the function (vector valued) i.e. map_from(:) of param_list
! start at the origin.  Jump to (1,0,0) by taking df/dx in the (1,0,0) 
! direction.  From (1,0,0) jump to (1,1,0) by heading df/dx in the 
! (0,1,0) direction.  Finally jump from (1,1,0) to (1,1,1) by taking a
! df/dx step in the (0,0,1) direction.  If f_1 is the vector valued function
! at position (1,0,0) then the first step size will be f_1 - f_0 where f_0 is
! the function evaluated at the origin.  The next step size will be f_2 - f_0, and
! the third will be f_3 - f_0.  Now f_n which is the function value at
! point (1,1,1) will be: f_n = f_0 + (f_1 - f_0) + (f_2 - f_0) + (f_3 - f_0)
! ergo: f_n = f_1 + f_2 + f_3 - 2*f_0.

	use h_params
	use h_vars
	use h_struct
	use h_rigorous
	implicit none

	type(rig_logic_struct),pointer :: current_rig
	integer,intent(in) :: count
	integer :: res

	! h_vars,intent(out)
	!	rig_rig_index
	!	real,dimension(num_free_global_param) :: free_param_temp


! Local Vars
	integer :: i,j,k
	integer :: pos0,pos1,pos2,pos3
	integer :: dim

! Begin

! corner (1)
	dim=current_rig%num_rig_param
	i=count
	j=count
	k=count
	rig_index=(/i,j,k/)
	write(*,90)rig_index,pos((/i,j,k/))
	pos0=pos( (/i-1,j-1,k-1/) )
	pos1=pos( (/i,j-1,k-1/) )
	pos2=pos( (/i-1,j,k-1/) )
	pos3=pos( (/i-1,j-1,k/) ) 
	free_param_temp(:) = rig_arr(pos1)%free_param(:) + &
		rig_arr(pos2)%free_param(:) + rig_arr(pos3)%free_param(:) - &
		2*rig_arr(pos0)%free_param(:)
	res=crunch(current_rig)

! corner (2)

	i=-count
	j=count
	k=count
	rig_index=(/i,j,k/)
	write(*,90)rig_index,pos((/i,j,k/))
	pos0=pos( (/i+1,j-1,k-1/) )
	pos1=pos( (/i,j-1,k-1/) )
	pos2=pos( (/i+1,j,k-1/) )
	pos3=pos( (/i+1,j-1,k/) ) 
	free_param_temp(:) = rig_arr(pos1)%free_param(:) + &
		rig_arr(pos2)%free_param(:) + rig_arr(pos3)%free_param(:) - &
		2*rig_arr(pos0)%free_param(:)
	res=crunch(current_rig)

! corner (3)

	i=-count
	j=-count
	k=count
	rig_index=(/i,j,k/)
	write(*,90)rig_index,pos((/i,j,k/))
	pos0=pos( (/i+1,j+1,k-1/) )
	pos1=pos( (/i,j+1,k-1/) )
	pos2=pos( (/i+1,j,k-1/) )
	pos3=pos( (/i+1,j+1,k/) ) 
	free_param_temp(:) = rig_arr(pos1)%free_param(:) + &
		rig_arr(pos2)%free_param(:) + rig_arr(pos3)%free_param(:) - &
		2*rig_arr(pos0)%free_param(:)
	res=crunch(current_rig)
	
! corner (4)

	i=count
	j=-count
	k=count
	rig_index=(/i,j,k/)
	write(*,90)rig_index,pos((/i,j,k/))
	pos0=pos( (/i-1,j+1,k-1/) )
	pos1=pos( (/i,j+1,k-1/) )
	pos2=pos( (/i-1,j,k-1/) )
	pos3=pos( (/i-1,j+1,k/) ) 
	free_param_temp(:) = rig_arr(pos1)%free_param(:) + &
		rig_arr(pos2)%free_param(:) + rig_arr(pos3)%free_param(:) - &
		2*rig_arr(pos0)%free_param(:)
	res=crunch(current_rig)

! corner (5)
	i=count
	j=count
	k=-count
	rig_index=(/i,j,k/)
	write(*,90)rig_index,pos((/i,j,k/))
	pos0=pos( (/i-1,j-1,k+1/) )
	pos1=pos( (/i,j-1,k+1/) )
	pos2=pos( (/i-1,j,k+1/) )
	pos3=pos( (/i-1,j-1,k/) ) 
	free_param_temp(:) = rig_arr(pos1)%free_param(:) + &
		rig_arr(pos2)%free_param(:) + rig_arr(pos3)%free_param(:) - &
		2*rig_arr(pos0)%free_param(:)
	res=crunch(current_rig)

! corner (6)
	i=-count
	j=count
	k=-count
	rig_index=(/i,j,k/)
	write(*,90)rig_index,pos((/i,j,k/))
	pos0=pos( (/i+1,j-1,k+1/) )
	pos1=pos( (/i,j-1,k+1/) )
	pos2=pos( (/i+1,j,k+1/) )
	pos3=pos( (/i+1,j-1,k/) ) 
	free_param_temp(:) = rig_arr(pos1)%free_param(:) + &
		rig_arr(pos2)%free_param(:) + rig_arr(pos3)%free_param(:) - &
		2*rig_arr(pos0)%free_param(:)
	res=crunch(current_rig)

! corner (7)
	i=-count
	j=-count
	k=-count
	rig_index=(/i,j,k/)
	write(*,90)rig_index,pos((/i,j,k/))
	pos0=pos( (/i+1,j+1,k+1/) )
	pos1=pos( (/i,j+1,k+1/) )
	pos2=pos( (/i+1,j,k+1/) )
	pos3=pos( (/i+1,j+1,k/) ) 
	free_param_temp(:) = rig_arr(pos1)%free_param(:) + &
		rig_arr(pos2)%free_param(:) + rig_arr(pos3)%free_param(:) - &
		2*rig_arr(pos0)%free_param(:)
	res=crunch(current_rig)

! corner (8)
	i=count
	j=-count
	k=-count
	rig_index=(/i,j,k/)
	write(*,90)rig_index,pos((/i,j,k/))
	pos0=pos( (/i-1,j+1,k+1/) )
	pos1=pos( (/i,j+1,k+1/) )
	pos2=pos( (/i-1,j,k+1/) )
	pos3=pos( (/i-1,j+1,k/) ) 
	free_param_temp(:) = rig_arr(pos1)%free_param(:) + &
		rig_arr(pos2)%free_param(:) + rig_arr(pos3)%free_param(:) - &
		2*rig_arr(pos0)%free_param(:)
	res=crunch(current_rig)

	return
90 format('grid point: {',<dim>I4,' }',I5)
end function corners_3D
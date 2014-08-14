function sides_3D(current_rig,count) result(res)
! logic checked 3/30/00 jmr
	use h_params
	use h_vars
	use h_struct
	use h_rigorous
	implicit none

	type(rig_logic_struct),pointer :: current_rig
	integer,intent(in) :: count
	integer :: res

	! h_vars,intent(out)
	!	rig_index
	!	real,dimension(num_free_global_param) :: free_param_temp

! Local Vars
integer :: i,j,k
integer :: pos0,pos1,pos2,pos3
integer :: dim

! Begin
! 12 strips in all: 1 for each edge of the cube.
! Let's tackle it this way.  First calculate the strips around the + & -
! k faces.

! strip (1) b/t +i and +k
! this means that j varies b/t -(count-1) and +(count-1)
	dim=current_rig%num_rig_param
	i=count
	k=count
	do j=-(count-1),(count-1)
		rig_index=(/i,j,k/) ! this is the pos we're calculating
		write(*,90)rig_index,pos((/i,j,k/))
		pos0=pos( (/i-1,j,k-1/) )
		pos1=pos( (/i-1,j,k/) )
		pos2=pos( (/i,j,k-1/) )
		free_param_temp(:) = rig_arr(pos1)%free_param(:) + &
			rig_arr(pos2)%free_param(:) - rig_arr(pos0)%free_param(:)
		res=crunch(current_rig)
	enddo !j
	
! strip (2) b/t -i and +k
! this means that j again varies b/t -(count-1) and +(count-1)

	i=-count
	k=count
	do j=-(count-1),(count-1)
		rig_index=(/i,j,k/) ! this is the pos we're calculating
		write(*,90)rig_index,pos((/i,j,k/))
		pos0=pos( (/i+1,j,k-1/) )
		pos1=pos( (/i+1,j,k/) )
		pos2=pos( (/i,j,k-1/) )
		free_param_temp(:) = rig_arr(pos1)%free_param(:) + &
			rig_arr(pos2)%free_param(:) - rig_arr(pos0)%free_param(:)
		res=crunch(current_rig)
	enddo !j	
	
! strip (3) b/t +j and +k
! this means that i varies b/t -(count-1) and +(count-1)

	j=count
	k=count
	do i=-(count-1),(count-1)
		rig_index=(/i,j,k/) ! this is the pos we're calculating
		write(*,90)rig_index,pos((/i,j,k/))
		pos0=pos( (/i,j-1,k-1/) )
		pos1=pos( (/i,j-1,k/) )
		pos2=pos( (/i,j,k-1/) )
		free_param_temp(:) = rig_arr(pos1)%free_param(:) + &
			rig_arr(pos2)%free_param(:) - rig_arr(pos0)%free_param(:)
		res=crunch(current_rig)
	enddo !i
	
! strip (4) b/t -j and +k
! this means that i varies b/t -(count-1) and +(count-1)

	j=-count
	k=count
	do i=-(count-1),(count-1)
		rig_index=(/i,j,k/) ! this is the pos we're calculating
		write(*,90)rig_index,pos((/i,j,k/))
		pos0=pos( (/i,j+1,k-1/) )
		pos1=pos( (/i,j+1,k/) )
		pos2=pos( (/i,j,k-1/) )
		free_param_temp(:) = rig_arr(pos1)%free_param(:) + &
			rig_arr(pos2)%free_param(:) - rig_arr(pos0)%free_param(:)
		res=crunch(current_rig)
	enddo !i

! strip (5) b/t +i and -k
! this means that j varies b/t -(count-1) and +(count-1)

	i=count
	k=-count
	do j=-(count-1),(count-1)
		rig_index=(/i,j,k/) ! this is the pos we're calculating
		write(*,90)rig_index,pos((/i,j,k/))
		pos0=pos( (/i-1,j,k+1/) )
		pos1=pos( (/i-1,j,k/) )
		pos2=pos( (/i,j,k+1/) )
		free_param_temp(:) = rig_arr(pos1)%free_param(:) + &
			rig_arr(pos2)%free_param(:) - rig_arr(pos0)%free_param(:)
		res=crunch(current_rig)
	enddo !j
	
! strip (6) b/t -i and -k
! this means that j again varies b/t -(count-1) and +(count-1)

	i=-count
	k=-count
	do j=-(count-1),(count-1)
		rig_index=(/i,j,k/) ! this is the pos we're calculating
		write(*,90)rig_index,pos((/i,j,k/))
		pos0=pos( (/i+1,j,k+1/) )
		pos1=pos( (/i+1,j,k/) )
		pos2=pos( (/i,j,k+1/) )
		free_param_temp(:) = rig_arr(pos1)%free_param(:) + &
			rig_arr(pos2)%free_param(:) - rig_arr(pos0)%free_param(:)
		res=crunch(current_rig)
	enddo !j	
	
! strip (7) b/t +j and -k
! this means that i varies b/t -(count-1) and +(count-1)

	j=count
	k=-count
	do i=-(count-1),(count-1)
		rig_index=(/i,j,k/) ! this is the pos we're calculating
		write(*,90)rig_index,pos((/i,j,k/))
		pos0=pos( (/i,j-1,k+1/) )
		pos1=pos( (/i,j-1,k/) )
		pos2=pos( (/i,j,k+1/) )
		free_param_temp(:) = rig_arr(pos1)%free_param(:) + &
			rig_arr(pos2)%free_param(:) - rig_arr(pos0)%free_param(:)
		res=crunch(current_rig)
	enddo !i
	
! strip (8) b/t -j and -k
! this means that i varies b/t -(count-1) and +(count-1)

	j=-count
	k=-count
	do i=-(count-1),(count-1)
		rig_index=(/i,j,k/) ! this is the pos we're calculating
		write(*,90)rig_index,pos((/i,j,k/))
		pos0=pos( (/i,j+1,k+1/) )
		pos1=pos( (/i,j+1,k/) )
		pos2=pos( (/i,j,k+1/) )
		free_param_temp(:) = rig_arr(pos1)%free_param(:) + &
			rig_arr(pos2)%free_param(:) - rig_arr(pos0)%free_param(:)
		res=crunch(current_rig)
	enddo !i

! strip (9) b/t +i and +j
! this means that k varies b/t -(count-1) and +(count-1)

	i=count
	j=count
	do k=-(count-1),(count-1)
		rig_index=(/i,j,k/) ! this is the pos we're calculating
		write(*,90)rig_index,pos((/i,j,k/))
		pos0=pos( (/i-1,j-1,k/) )
		pos1=pos( (/i-1,j,k/) )
		pos2=pos( (/i,j-1,k/) )
		free_param_temp(:) = rig_arr(pos1)%free_param(:) + &
			rig_arr(pos2)%free_param(:) - rig_arr(pos0)%free_param(:)
		res=crunch(current_rig)
	enddo !k

! strip (10) b/t -i and +j
! this means that k varies b/t -(count-1) and +(count-1)

	i=-count
	j=count
	do k=-(count-1),(count-1)
		rig_index=(/i,j,k/) ! this is the pos we're calculating
		write(*,90)rig_index,pos((/i,j,k/))
		pos0=pos( (/i+1,j-1,k/) )
		pos1=pos( (/i+1,j,k/) )
		pos2=pos( (/i,j-1,k/) )
		free_param_temp(:) = rig_arr(pos1)%free_param(:) + &
			rig_arr(pos2)%free_param(:) - rig_arr(pos0)%free_param(:)
		res=crunch(current_rig)
	enddo !k
! strip (11) b/t +i and -j
! this means that k varies b/t -(count-1) and +(count-1)

	i=count
	j=-count
	do k=-(count-1),(count-1)
		rig_index=(/i,j,k/) ! this is the pos we're calculating
		write(*,90)rig_index,pos((/i,j,k/))
		pos0=pos( (/i-1,j+1,k/) )
		pos1=pos( (/i-1,j,k/) )
		pos2=pos( (/i,j+1,k/) )
		free_param_temp(:) = rig_arr(pos1)%free_param(:) + &
			rig_arr(pos2)%free_param(:) - rig_arr(pos0)%free_param(:)
		res=crunch(current_rig)
	enddo !k
! strip (12) b/t -i and -j
! this means that k varies b/t -(count-1) and +(count-1)

	i=-count
	j=-count
	do k=-(count-1),(count-1)
		rig_index=(/i,j,k/) ! this is the pos we're calculating
		write(*,90)rig_index,pos((/i,j,k/))
		pos0=pos( (/i+1,j+1,k/) )
		pos1=pos( (/i+1,j,k/) )
		pos2=pos( (/i,j+1,k/) )
		free_param_temp(:) = rig_arr(pos1)%free_param(:) + &
			rig_arr(pos2)%free_param(:) - rig_arr(pos0)%free_param(:)
		res=crunch(current_rig)
	enddo !k

	return
90 format('grid point: {',<dim>I4,' }',I5)
end function sides_3D


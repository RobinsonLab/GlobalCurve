subroutine write_rig_array(rig_array,dim,pos,y,x)

implicit none
	type(rig_struct),dimension(:) :: rig_array
	integer,intent(in) :: dim,pos
	real,intent(in) :: y
	real,dimension(:),intent(in) :: x
	
! Begin

	rig_array(pos)%y=y
	rig_array(pos)%x(1:dim)=x

	return
end subroutine write_rig_array

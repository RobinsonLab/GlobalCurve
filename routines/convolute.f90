function convolute(p,fluor,q_shift) result (intensity)
	use h_struct
!	use h_utils, only : simple_convolute
	use numerical_libraries !IMSL routines
	use h_routines, only : shift
	implicit none

	type (data_struct),pointer :: p
	real,dimension(:),intent(in) :: fluor
	real,dimension(size(fluor)) :: intensity !,intensity2,difference
	real,intent(in) :: q_shift

! Local Vars
	integer :: nx
! Begin
! to increase performance, use explicit workspace

! ido = 0 ! for now
! nx = ny = p%len
! x = p%s_lamp
! y = fluor
! ipad = 1 (nonperiodic data)
! nz = p%nz set in readans
! z = p%z
! zhat = p%zhat
! xwk = p%xwk
! ywk = p%ywk
! wk = p%wk


	nx=p%len
	p%s_lamp=shift(p%lamp,p%d_lamp,q_shift)
!	intensity=simple_convolute(s_lamp,fluor)
	call r2onv(0,nx,p%s_lamp,nx,fluor,1,p%nz,p%z,p%zhat, &
		p%xwk,p%ywk,p%wk)
	intensity(:)=p%z(1:nx)

return
end function convolute
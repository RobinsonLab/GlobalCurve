subroutine dump_raw(len,x_pts,species,g)
	use h_struct
	use h_params ! default, DATA_FILE
	use h_utils, only: make_out_file_name,error_handler
	implicit none


	integer, intent(in) :: len
	real,dimension(len), intent(in) :: g,x_pts
	real,dimension(len,3), intent(in) :: species

! local vars
	type(expt_struct),pointer :: current_expt
	character(STRING_LEN) :: out_file_name
	integer :: i,ios

! Begin


	current_expt=>expt_list
	ios=make_out_file_name(current_expt%file_name,'raw',out_file_name) 
	if (ios /= 0) call error_handler('error constructing output file name')
	open(unit = DATA_FILE,file=out_file_name, &
		status='unknown',iostat=ios)
	if (ios /= 0) call error_handler('Error opening file: ',out_file_name)
	! write the header
	write(DATA_FILE,*)'time     I     II     III     g'
!	pause
	do i=1,len
		write(DATA_FILE,50),x_pts(i),species(i,:),g(i)
	enddo ! i
	close(DATA_FILE)

50 format(5ES15.5)

end subroutine



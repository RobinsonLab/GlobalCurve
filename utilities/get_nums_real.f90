function get_nums_real (line,nums) result (res)
 	use diffuse_params
 	use diffuse_header
	
	implicit none

	character(*), intent(in) :: line
!	integer,intent(in) :: n
	real,dimension(:),intent(out) :: nums

	integer :: res,n

	res=size(nums)
	n=nitems(line)
	if (res > n) then
		res = n !can't get that many integers
		return
	end if
	read(line,*) nums((/1:res/))
	return
end function get_nums_real
function get_nums_integer (line,nums) result (res)
	use h_utils
	implicit none

	character(*), intent(in) :: line
!	integer,intent(in) :: n
	integer,dimension(:),intent(out) :: nums

	integer :: res,n

	res=size(nums)
	n=nitems(line)
	if (res > n) then
		res = n !can't get that many integers
		return
	end if
	read(line,*) nums((/1:res/))
	return
end function get_nums_integer

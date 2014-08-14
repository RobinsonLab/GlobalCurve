function trim_string(str)
	use h_params
	implicit none
	character(STRING_LEN), intent(in) :: str
	character(STRING_LEN) :: trim_string
! Local vars

	character(STRING_LEN) :: temp
	integer :: count,count2
! Begin

	count=1
	do 
		if((str(count:count) /= ' ') .and. (str(count:count) /= '/t')) exit
		! couldn't get the tab thing to work so just don't put tab's in the ans file
		count=count+1
	enddo
	count2=len_trim(str)
	trim_string=str(count:count2)
	return       
end function trim_string
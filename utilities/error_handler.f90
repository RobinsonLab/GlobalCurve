subroutine error_handler(str1,str2)
 	use h_params
	implicit none
	character(*), intent(in) :: str1
	character(*),optional, intent(in) :: str2


	if (present(str2)) then
		print *, str1,trim(str2)
	else
		print *, str1
	end if
	stop 'Program terminated upon error'
end subroutine error_handler
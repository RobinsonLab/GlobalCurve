function nitems(line) result(number)
	implicit none

	character(*),intent(in) ::line
	integer :: number

	integer :: icount,lineLen,icmax

! figure out number of numbers seperated by blank in line
	lineLen=len(line)
	number = 0
    icount = 0
	icmax = len(line)
	icloop: do 
		icount = icount+1
        if(icount > lineLen) exit icloop
        if(line(icount:icount) /= ' ' .and. line(icount:icount) /= ',')then !in a number
	        number = number + 1
		    ic2loop: do 
				icount = icount + 1
                if(icount >= icmax) exit icloop
                if(line(icount:icount) == ' ' .or. line(icount:icount) == ',') exit ic2loop !just came out of a num
 			end do ic2loop
        end if
	end do icloop
    return
end function nitems
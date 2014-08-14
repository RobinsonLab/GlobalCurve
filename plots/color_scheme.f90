function color_scheme(bow,color) result(colors)
	use h_struct
	implicit none
	logical,intent(in) :: bow,color
	type(color_struct) :: colors

! Begin

	! default screen color scheme is white on black
	colors%WHITE = 1
	colors%BLACK = 0
	if (bow) then
		call pgscr(0,1.,1.,1.) ! black becomes white
		call pgscr(1,0.,0.,0.) ! white becomes black	
	endif
	if (color) then
		colors%RED = 2
		colors%GREEN = 3
		colors%BLUE = 4
		colors%YELLOW = 7
	else !black & white
		colors%RED = colors%WHITE
		colors%GREEN = colors%WHITE
		colors%BLUE = colors%WHITE
		colors%YELLOW = colors%WHITE
	endif

	return
end function color_scheme

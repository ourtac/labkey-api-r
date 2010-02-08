.parseString <- function(chars, i)
{
	str_start = i
	if (chars[i] != "\"") 
	stop("error")

	## optimization for most string cases.  Use R array functions instead of iterating through character by character
	## We test frame by frame whether we have a double quote ending the string.  Frame size determined by testing to be
	## optimal in the range 25 - 75, probably data-dependent, but not that sensitive to frame size.  
	## if we find a double quote, we look back to see if there are any escaped chars in the string we found, If there are, or
	## if there aren't enought chars left for a full frame, we revert to the old code that goes one char at a time and
	## calls eval(parse())) for every string.

	frame_size = 50
	qts = "\""
	chr_end = length(chars)
	frame_st = i + 1 	
	frame_end = frame_st + frame_size - 1
	qt_pos = 0
	while (frame_end <= chr_end)
	{			
		if(any(qts == chars[frame_st:frame_end]))
		{
			next_qt = which.max(qts == chars[frame_st:frame_end])
			qt_pos = frame_st + next_qt -1
			break
		}
		else
		{
			frame_shift = frame_size
		}
		frame_st = frame_st + frame_shift
		frame_end = frame_st + frame_size -1
	}

	if(qt_pos > 0)
	{			 
		if(!any("\\"==chars[str_start:qt_pos]))
		{		
		return(list(val = paste(chars[(str_start+1):(qt_pos-1)], collapse=""), size = (qt_pos +1)))
		}
	}	    

	i = i + 1
	if (i > length(chars)) 
	return(list(incomplete = TRUE))

	while (TRUE) {
	while (chars[i] != "\\" && chars[i] != "\"") {
	i = i + 1
	if (i > length(chars)) 
	return(list(incomplete = TRUE))
	}
	if (chars[i] == "\\") {
	i = i + 2
	if (i > length(chars)) 
	return(list(incomplete = TRUE))
	}
	else break
	}

	str_end = i
	i = i + 1

	return(list(val = eval(parse(text = paste(chars[str_start:str_end], collapse = ""))), size = i))


	#		   if (!is.null(myval) && (myval$val != theirval$val || myval$size != theirval$size))
	#			{ 
	#				cat(str(myval), str(theirval))
	#				cat("\n" , chars[str_start:str_end], "\n", sep="")
	#				cat ("str_start ", str_start, " my str_end ", mystr_end, " next_qt ", next_qt," str_end ", str_end, sep="")
	#				stop("\n mismatch")
	#			}

}

.onLoad <-
   function(libname, pkgname)
{
   require(rjson)
   assign(".parseString", .parseString, pos="package:rjson")
}


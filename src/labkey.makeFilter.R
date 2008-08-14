

labkey.makeFilter <- function(fcol, fvalue, foperator)
{	# Confirm all variables are of same length
	if(length(fcol)==length(fvalue) || length(fvalue)==length(foperator))
		{	
			# Construct filters
			filters <- array(0, dim=c(length(fcol),1))
			for(i in 1:length(fcol))
				{	# Match the operator
					fop  <- switch(EXPR=foperator[i], 	
									EQUALS="eq",
									NOT_EQUALS="neq", 	
									GREATER_THAN="gt",
									GREATER_THAN_OR_EQUAL_TO="gte",
									LESS_THAN="lt",
									LESS_THAN_OR_EQUAL_TO="lte",
									DATE_EQUAL="dateeq",
									DATE_NOT_EQUAL="dateneq",
									NOT_EQUAL_OR_NULL="neqornull",
									IS_NULL="isblank",
									IS_NOT_NULL="isnonblank",
									CONTAINS="contains",
									DOES_NOT_CONTAIN="doesnotcontain")
					
					if(is.null(fop)==TRUE) stop ("Invalid operator name.")
					filters[i] <- paste(fcol[i],"~",fop,"=",fvalue[i],sep="")
				}
		} else {stop ("The lengths of fcol, fvalue, and foperator must be equal.")}

	return(filters)
}
	
 

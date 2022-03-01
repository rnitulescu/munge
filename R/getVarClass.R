# Get the class of a field in a data set
#
# Returns the class of a column inside a data table.
# @param name character variable representing name of the field.
# @param x data.table object containing field.
# @keywords getVarClass
getVarClass <- function(name, x) {
    ## Used to be called "getClass", but renamed to avoid namespace issues
    ## (Added "flatten" here to get rid of "ordered" component to some factors' classes)
	return(flatten(class(x[[name]]))[1])
}


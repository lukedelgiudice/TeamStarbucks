# helper function to compute the score.  Takes field position.

compute_score <- function(fp){
  
  # order here is important.  If the first condition is satisfied, it does not
  # look at the other statements.  So the only time it looks at the other
  # statements is if fp > 100, so we need not check any more if fp > 100!  If 
  # the second condition is not satisfied, then we know that the fp must be >110
  # and by construction of our output, the only thing it can be is a field goal.
  
  if(fp <= 100) {
    0
  } else if (fp <= 110) {
    7
  } else {
    3
  }

}

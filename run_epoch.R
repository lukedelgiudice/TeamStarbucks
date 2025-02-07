source("run_drive.R")
source("utils.R")

# Now we will focus on the epoch function.  Once we are done, we can more or
# less leave it alone unless there are some project-specific changes we need to
# make.  

run_epoch <- function(down, ytg, fp) {
  # We need to do 3 things: run drives, track which team is on offense during
  # the drive, and determine if there was a score or not.  What is getting fed 
  # into the run_epoch function is the "state" we are interested in calculating
  # an EP for, so that team is the "original offense."  If that team scores,
  # the score for the epoch should be positive.  If the other team scores, the
  # score for the epoch should be negative.  This suggests we can keep track of 
  # the team in possession of the ball with a status of 1 ("original offense") 
  # and -1 (opponent). Then, we can just multiply the team status and the score
  # (when there is one) to obtain our output from this function.
  
  # initialize team status to -1.  Why not 1, since the "original offense" runs
  # the first drive?  The while loop will always have to toggle the team in 
  # possession, regardless of if there was a score or not (I suppose it doesn't
  # _have_ to, but that is overly complicated).  So we will set it to -1 and 
  # then immediately switch it when we enter the while loop.
  team_status <- -1
  
  # we can use a while loop to run our epoch.  In general, I like to use
  # flags when things are not as straightforward, and not check conditions 
  # as a part of the while loop.  This is a little tidyer.  So instead of
  # having the while loop check if the output from the drive is a score,
  # I will set a flag has_scored and monitor it.  We want to upper bound the 
  # number of iterations we are allowing our while loop to run, otherwise it
  # could run forever (in theory, but if we set everything up right this would
  # be highly unlucky!).  So for now we will restrict our while loop to run
  # a maximum of ten drives
  
  # set max number of drives
  max_drives <- 10 # again, avoid hardcoding
  
  # set drive counter
  cumulative_drives <- 0 # based on the code below, it's a little less 
  # cumbersome to index starting at 0.  Main thing is to
  # make sure your indexing and your verification yield
  # a maximum of max_drives.  We can interpret this 
  # counter as "the number of drives that have already 
  # been run."
  
  # initialize flag
  no_score <- TRUE 
  
  # Quick reminder when reading the printed output that the starting state 
  # should be the first state.
  
  print("Remember, the state of the first drive should be the input state.")
  
  # run loop
  while(no_score & (cumulative_drives < max_drives)) {
    
    # flip the team status immediately!  The first iteration of the while loop
    # will set team_status to 1, which is the "original offense."  If we kick
    # out of the while loop after just one drive, then the score multiplier
    # will be 1, which is exactly what we want.
    team_status <- team_status * -1
    
    # for similar reasons, we can go ahead and update the cumulative drives now.
    # This only serves to make our print statments nice...
    
    # increment the run drive
    cumulative_drives <- cumulative_drives + 1
    
    
    # for testing purposes, we may want to track the state of the "game" as it
    # progresses, so we will print here to show that we are running a drive, 
    # and the state of the game.  Since our "run_drive" function has only one 
    # value for fp that will instigate another call to the function, we should
    # always see fp=80, except for the first printout, which will be the state
    # we are deriving an EP for.  If we see anything else, we are not exiting 
    # the while loop properly!
    
    print(paste0("starting down: ", down, ", ytg: ", ytg, ", fp: ", fp, 
                 ", drive number: ", cumulative_drives,
                 ", team status flag: ", team_status,"."))
    
    # run drive
    tmp_state <- run_drive(down, ytg, fp)
    
    # reassign variables (look at branch "state" in the directory to see a 
    # better (in my opinion) way to do this)
    down <- tmp_state$down
    ytg <- tmp_state$ytg
    fp <- tmp_state$fp
    
    # aside: why don't I just have run_drive return just the fp?  1) I like to 
    # keep the state variables together, and 2) I don't want the run_epoch
    # function to do anything that doesn't fit its story.  1st and 10 is a
    # consequence of the end of a drive, not a part of the management of an 
    # epoch, at least in my opinion.
    
    # flip the score flag if there was a score
    no_score <- (fp <= 100)
    
    
    
    
    # That's it for the while loop!  If cumulative_drives < 10 and no_score is
    # true, the while loop will just run again, this time with the updated
    # downs, ytg, and fp settings.  Otherwise, it will exit.
  }
  
  # Now we just need to compute the points from the field position once the 
  # while loop has exited.  I decided to create a helper function to do this to
  # keep the code uncluttered (my documentation is already clutter enough!).
  # I've called it compute_score() and put it in a utils file.
  
  score <- team_status * compute_score(fp)
  
  # after we compute the score, I'd like to print out the scoring fp (0 
  # indicates loop stopped before a score), as well as the score.  
  # This allows me to check that it is scoring properly.
  
  print(paste0("final fp: ", fp, 
               ", scoring drive number: ", cumulative_drives,
               ", team status flag: ", team_status, ", score: ", score, "."))
  
  # and then finally return the score for the get_EP function to use.
  
  score
  
}


################################################################################
############################# Test that run_epoch works ########################
################################################################################

run_epoch(2, 7, 34) # To check different aspects of the code, you can modify
# the probabilities in the run_drive function to "force"
# certain outcomes to be more probable.
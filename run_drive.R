# This is the run_drive function.  For now, our only goal is to make sure that
# run_epoch is working, so we don't really need anything like a realistic
# run_drive function.  We just need one that will give us outcomes we can
# play with.  The run_drive function will output a state.

run_drive <- function(down, ytg, fp) {
  # the end of a drive will either result in a score or a 1st and 10 for the
  # other team.  We will convey a score through fp (100 < fp <= 110, touchdown; 
  # 110 < fp <= 120, field goal).  If a score happens, down and ytg are
  # irrelevant, so it does not hurt us to still set thouse to 1st and 10.
  
  # so for now, to check our run_epoch function, we really just need to sample
  # a field position.  It doesn't even matter what they are, we just have to
  # sample something that will result in a new drive being run, something that
  # will result in a touchdown, and something that will result in a field goal.
  
  # sample new field position.  Because this is only temporary, I'm not worried
  # about hardcoding here.  We will sample a fp that results in a new drive with
  # probability .9, a touchdown with probability .05, and a field goal with 
  # probability .05
  
  # These probabilities aren't reasonable, but they will produce a variety of
  # outputs so that you can check that your run_epoch function works in a
  # reasonable way.  You can change the probabilities to "force" certain
  # outcomes to check something specific.

  new_fp <- sample(c(80, 105, 115), 1, prob=c(.9, .05, .05))
  
  # and we return the new state, as a list (see branch state, where the state
  # is always a list!)
  
  list(down=1, ytg=10, fp=new_fp)
}

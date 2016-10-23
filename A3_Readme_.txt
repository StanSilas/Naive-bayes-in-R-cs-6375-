#Assignment 3  Read me

First open R studio. 

Now make sure the unzipping folder is the set working directory. 

To set the directory, do the following RStudio>Menu>Session>Set Working Directory >Choose Directory 
(Alternatively you can do CTRL +SHIFT + H also)
Make sure the assignment folder is the working directory. 


After setting directory, Now go to the RStudio>Menu>Tools>Shell  and click on shell. 
This will open up a command prompt , with a path already set. So we don't have to worry about setting the path.


Now in the command prompt type 
Rscript naivebayes_3.R train-win.dat test-win.dat 

This will display all the following:

First it will display some essential information about the data sets that have been input as part of the arguments.

Next it will display the number of zeros and one's that each attribute has corresponding to class 0 and class 1.

Next it will print the probability values of each attribute per each class =0 and for class = 1.

Next it displays the training accuracy
next it displays the testing accuracy.


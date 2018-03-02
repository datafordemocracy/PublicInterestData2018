## Code Review, February 9

1. Is the code replicable? Can you run it as is (with the exception of changing the working directory or path to the shared data file)? If not, where does it break?

2. Does the code appear to read in the data from an encrypted file?

3. Is the code well-commented? Do you understand what the purpose of each step/command? Identify areas that need more clarity.

4. Does the code check that the variables are properly formatted for analysis, and reformat those that aren't? E.g., dates are recognized as dates (not numbers or characters), categorical variables are recognized as factors, numeric variables are recognized as numbers (not IDs, but values on which we may wish to perform mathematical operations; all IDs should be characters so that we can readily match them across files)? 

5. Does the code generate effective variable names (short enough to use, long enough to understand)? If not, what suggestions would you offer?

6. Does the code effectively check the data for errors,  unusual or unexpected values? If so, are these addressed in the code? If so, are they addressed appropriately?

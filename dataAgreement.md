# Practices for Sharing Client Data

To insure the security of [moderately sensitive client data](http://security.virginia.edu/university-data-protection-standards), lab members agree to the following practices and terms.

1. The data will be available on a shared UVA Box folder. Lab members should [sync this folder to their computers](https://virginia.app.box.com/settings/sync), or just download the file to a temporary location on your hard drive when available.
2. Lab members should put the data file in an encrypted folder or volume with password protection on their computers.
  * A common option for Mac, PC, or Linux is the free, open-source encryption software [VeraCrypt](https://www.veracrypt.fr/en/Home.html). Here are instructions for [installing and using VeraCrypt on a PC](https://securityinabox.org/en/guide/veracrypt/windows/); here are [instructions for a Mac](https://securityinabox.org/en/guide/veracrypt/mac/). Once you create a standard volume, store the data file in it.
  * Charlotte also provided [this link for Macs](https://www.hongkiat.com/blog/encrypt-mac-folder/). To use the folder, click on the .dmg file to mount it (if you can't find the .dmg file, search with spotlight); it should be visible in Finder. One potential downside of this method for Macs is that I found after dismounting the folder, I can still see it using the terminal and can call a file from it within R (at least until I restart). Dismounting the volume in VeraCrypt made files truly inaccessible right away.
  * Once the data is in a protected volume or folder, unsync the Box folder or delete the data file from the temporary location. You'll view or call the data from the protected folder only. You will need to mount the folder/volume each time you want to use the data, and dismount the folder/volume when you're done working.
3. Lab members should not make any additional copies of the data file, including no copies on portable storage devices, and no copies on public machines.
4. Lab members should not print the data file.
5. When working with the data in non-private spaces, lab members should not leave their computers unattended. 
6. Lab members should not show or share the data file with colleagues outside the lab>. If seeking help with code or analysis from those outside the lab, do not display the data frame in the data viewer.

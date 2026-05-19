#setting up Github#

##REFERENCES##
#main reference tutorial: https://rfortherestofus.com/2021/02/how-to-use-git-github-with-r/
    #https://happygitwithr.com/new-github-first#new-github-first
#how git/github works: https://r-bio.github.io/intro-git-rstudio/
#nice workflow of how git works: https://www.git-tower.com/blog/workflow-of-git
#other setup refs: https://git-scm.com/book/en/v2/Getting-Started-First-Time-Git-Setup, 
    #https://happygitwithr.com/install-git.html,
    #https://www.geeksforgeeks.org/r-language/link-your-github-account-with-r-studio/,
    #https://docs.posit.co/ide/user/ide/guide/tools/version-control.html

##NOTES##
# when setting up/figuring out github stuff I made a project folder in R called "Philben" that I wasn't able to link to github - now obsolete
# similarly, in Github I made a repository called "Philben-files" that I put my token in lmao and it wouldn't let me push anything because I shouldn't be publishing that lol
# so now using the repository "Philben-project" located in the Philben->Philben-files folder in my computer (accidentally nested it but whatever)
# so "Philben" project in my computer and the "Philben-files" repo/project in Github/my computer are obsolete


#testing github  
#making a token
library(usethis)
create_github_token()
#token expires Aug 17 2026

#linking rstudio to github
library(gitcreds)
gitcreds_set() #use this to link new PAT

#linking this project to github
use_git()
use_github()

##test test       
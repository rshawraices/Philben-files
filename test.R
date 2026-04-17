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


#testing github  
#making a token
library(usethis)
create_github_token()
#token expires May 15 2026

#linking rstudio to github
library(gitcreds)
gitcreds_set()

#linking this project to github
use_git()
use_github()

##test test       
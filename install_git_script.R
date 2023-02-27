install.packages("usethis")
library(usethis)
usethis::use_git_config(user.name = "AntoineBrisacier",
                        user.email = "antoine.brisacier@sanofi.com")
usethis::use_git()
usethis::create_github_token()
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

gitcreds::gitcreds_set()
usethis::use_github()

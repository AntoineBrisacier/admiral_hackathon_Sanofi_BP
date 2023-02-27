install.packages("usethis")
library(usethis)
usethis::use_git_config(user.name = "AntoineBrisacier",
                        user.email = "antoine.brisacier@sanofi.com")
usethis::use_git()
usethis::create_github_token()
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

gitcreds::gitcreds_set()
usethis::use_github()

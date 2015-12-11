###
library("devtools")
dev_mode()

thlcolsample()

install_github("thl-mjv/thlcol")

use_testthat() # run only once

test()
check()
build()

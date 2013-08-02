# LOAD REQUIRED LIBRARIES
library('opal')

# LOGIN TO COLLABORATING SERVERS
server1 <- opal.login('administrator', 'password', 'http://54.242.140.255')
server2 <- opal.login('administrator', 'password', 'http://54.242.46.59')
server3 <- opal.login('administrator', 'password', 'http://23.22.215.42')
opals <- list(server1,server2,server3)

# ASSIGN DATA FROM OPAL DATASOURCE TO R AS A LIST
datashield.assign(server1, 'D', 'CNSIM.CNSIM')
datashield.assign(server2, 'D', 'CNSIM.CNSIM')
datashield.assign(server3, 'D', 'CNSIM.CNSIM')

# test function 'factor.create.3'
datashield.assign(opals, 'sex.f', quote(factor.create.3.ag(D$GENDER)))
# check the length of the above created variable
datashield.aggregate(opals, quote(length(sex.f)))

# test function 'quantile.mean.ds'
datashield.aggregate(opals,quote(quantile.mean.ds.ag(D$LAB_TSC)))

# test function 'mean.ds.ag'
datashield.aggregate(opals,quote(mean.ds.ag(D$LAB_TSC)))

# CTX clock
library(effsize)
library(reshape2)

ctx_stock <-c(24847,22075,20780,97822,51292,32033,31497,23957,19891,18903)
ctx_spectre <-c(1000254707,1000184269,1000185526,1000205446,1000272838,1000213128,1000162010,1000130867,1000180564,1000227332)

median(ctx_stock)
median(ctx_spectre)

shapiro.test(ctx_stock)
wilcox.test(ctx_stock, ctx_spectre, paired = FALSE)

df <- data.frame(ctx_stock, ctx_spectre)
mdf <- melt(df, measure.vars=1:2)
VD.A(mdf$value, mdf$variable)

# Apache
apacheStock <-c(34818736,35841493,34950392,34606053,33999807,35594526,34112769,35176634,35035792,33201776)
apacheMeltdown <-c(28804051,23491253,26029895,24119443,24712474,25952394,24655190,24177664,24915020,25596023)

median(apacheStock)
median(apacheMeltdown)

shapiro.test(apacheStock)
wilcox.test(apacheStock, apacheMeltdown, paired = FALSE)

df <- data.frame(apacheStock, apacheMeltdown)
mdf <- melt(df, measure.vars=1:2)
VD.A(mdf$value, mdf$variable)

# Nginx
nginxStock <-c(26860741,30928485,29622771,34225781,31659482,28956894,28262933,30534316,29629032,30156011)
nginxMeltdown <-c(19482235,19419309,19500908,19452758,19402450,19673618,19101769,19987159,20068118,19389511)

median(nginxStock)
median(nginxMeltdown)

shapiro.test(nginxStock)
wilcox.test(nginxStock, nginxMeltdown, paired = FALSE)

df <- data.frame(nginxStock, nginxMeltdown)
mdf <- melt(df, measure.vars=1:2)
VD.A(mdf$value, mdf$variable)

# Network loopback
network_loopback_stock <-c(77911386,77532221,77546067,78053411,77449946,77833422,77501744,77387627,78233475,78244472)
network_loopback_spectre <-c(3888981,5172808,2185125,4852247,24130108,7818750,2754750,9230290,2495962,20607852)

median(network_loopback_stock)
median(network_loopback_spectre)

shapiro.test(network_loopback_stock)
wilcox.test(network_loopback_stock, network_loopback_spectre, paired = FALSE)

df <- data.frame(network_loopback_stock, network_loopback_spectre)
mdf <- melt(df, measure.vars=1:2)
VD.A(mdf$value, mdf$variable)

# Osbench create files
osbench_create_files_stock <-c(12317497,12336287,12145561,12138039,12209653,12212473,12177768,12255061,12307883,12219211)
osbench_create_files_spectre <-c(7892641,6540607,7963984,7973612,6406440,8273693,7976968,8175393,8126516,8331709)

median(osbench_create_files_stock)
median(osbench_create_files_spectre)

shapiro.test(osbench_create_files_stock)
wilcox.test(osbench_create_files_stock, osbench_create_files_spectre, paired = FALSE)

df <- data.frame(osbench_create_files_stock, osbench_create_files_spectre)
mdf <- melt(df, measure.vars=1:2)
VD.A(mdf$value, mdf$variable)

# Osbench create processes
osbench_create_processes_stock <-c(412653104,418878314,418198006,440901144,434627827,438149919,448829377,456566295,451135188,456989473)
osbench_create_processes_spectre <-c(113055353,116555991,123617676,125529167,129492374,128211842,129522126,126251462,131291702,129478855)

median(osbench_create_processes_stock)
median(osbench_create_processes_spectre)

shapiro.test(osbench_create_processes_stock)
wilcox.test(osbench_create_processes_stock, osbench_create_processes_spectre, paired = FALSE)

df <- data.frame(osbench_create_processes_stock, osbench_create_processes_spectre)
mdf <- melt(df, measure.vars=1:2)
VD.A(mdf$value, mdf$variable)

# Osbench create threads
osbench_create_threads_stock <-c(119011423,123426207,149542404,142607723,146017275,146311972,145047978,132517815,130961593,144258253)
osbench_create_threads_spectre <-c(55375809,52369290,56939420,57878203,60020265,60742987,59794454,65499156,53964365,65653420)

median(osbench_create_threads_stock)
median(osbench_create_threads_spectre)

shapiro.test(osbench_create_threads_stock)
wilcox.test(osbench_create_threads_stock, osbench_create_threads_spectre, paired = FALSE)

df <- data.frame(osbench_create_threads_stock, osbench_create_threads_spectre)
mdf <- melt(df, measure.vars=1:2)
VD.A(mdf$value, mdf$variable)

# Osbench launch programs
osbench_launch_programs_stock <-c(321444926,323182448,329387060,328096722,331484871,334102954,330442676,333097828,333984244,333318803)
osbench_launch_programs_spectre <-c(95785643,97739827,100007733,97661915,102490991,105113293,104095105,102198699,106245967,104220232)

median(osbench_launch_programs_stock)
median(osbench_launch_programs_spectre)

shapiro.test(osbench_launch_programs_stock)
wilcox.test(osbench_launch_programs_stock, osbench_launch_programs_spectre, paired = FALSE)

df <- data.frame(osbench_launch_programs_stock, osbench_launch_programs_spectre)
mdf <- melt(df, measure.vars=1:2)
VD.A(mdf$value, mdf$variable)

# Osbench mem alloc
osbench_mem_alloc_stock <-c(47375648,48435192,47666616,47568720,47804011,47937515,47708100,47915786,48104393,47904636)
osbench_mem_alloc_spectre <-c(39835848,41802310,40873355,43742134,42695404,42381166,42533382,41835787,42745535,42898781)

median(osbench_mem_alloc_stock)
median(osbench_mem_alloc_spectre)

shapiro.test(osbench_mem_alloc_stock)
wilcox.test(osbench_mem_alloc_stock, osbench_mem_alloc_spectre, paired = FALSE)

df <- data.frame(osbench_mem_alloc_stock, osbench_mem_alloc_spectre)
mdf <- melt(df, measure.vars=1:2)
VD.A(mdf$value, mdf$variable)

# Redis get
redis_get_stock <-c(75985276,76483576,77106231,74344072,76107711,75445721,75347496,76301486,76475959,75323780)
redis_get_spectre <-c(19973628,21064279,50128744,20972995,24925772,29440213,24808357,25432164,22395606,44152760)

median(redis_get_stock)
median(redis_get_spectre)

shapiro.test(redis_get_stock)
wilcox.test(redis_get_stock, redis_get_spectre, paired = FALSE)

df <- data.frame(redis_get_stock, redis_get_spectre)
mdf <- melt(df, measure.vars=1:2)
VD.A(mdf$value, mdf$variable)

# Redis lpop
redis_lpop_stock <-c(76680891,74725760,77626806,76202333,75955919,75791520,75805744,76694254,75908514,74860674)
redis_lpop_spectre <-c(39536936,18513046,22059067,17879584,20644512,29792542,39601370,34924888,37411344,49402453)

median(redis_lpop_stock)
median(redis_lpop_spectre)

shapiro.test(redis_lpop_stock)
wilcox.test(redis_lpop_stock, redis_lpop_spectre, paired = FALSE)

df <- data.frame(redis_lpop_stock, redis_lpop_spectre)
mdf <- melt(df, measure.vars=1:2)
VD.A(mdf$value, mdf$variable)

# Redis lpush
redis_lpush_stock <-c(75593118,75734672,77450942,75867278,76225310,75361043,75406428,76657094,76560665,75641204)
redis_lpush_spectre <-c(42253595,25950297,64504940,24380759,18064000,49233471,25695574,32992737,25678107,18469668)

median(redis_lpush_stock)
median(redis_lpush_spectre)

shapiro.test(redis_lpush_stock)
wilcox.test(redis_lpush_stock, redis_lpush_spectre, paired = FALSE)

df <- data.frame(redis_lpush_stock, redis_lpush_spectre)
mdf <- melt(df, measure.vars=1:2)
VD.A(mdf$value, mdf$variable)

# Redis sadd
redis_sadd_stock <-c(75431789,75666492,77047390,74507201,74935657,76456115,77264334,76713940,75224480,76258365)
redis_sadd_spectre <-c(49995424,19389025,51000738,23005702,21333986,22104248,34192136,32610715,16039465,26629995)

median(redis_sadd_stock)
median(redis_sadd_spectre)

shapiro.test(redis_sadd_stock)
wilcox.test(redis_sadd_stock, redis_sadd_spectre, paired = FALSE)

df <- data.frame(redis_sadd_stock, redis_sadd_spectre)
mdf <- melt(df, measure.vars=1:2)
VD.A(mdf$value, mdf$variable)

# Redis set
redis_set_stock <-c(75775347,76596493,75904809,75470060,76645520,75890806,75060294,76033659,76504841,76040491)
redis_set_spectre <-c(27015746,24814006,21629223,29410044,25038841,28080948,20373220,47577492,18640836,32671518)

median(redis_set_stock)
median(redis_set_spectre)

shapiro.test(redis_set_stock)
wilcox.test(redis_set_stock, redis_set_spectre, paired = FALSE)

df <- data.frame(redis_set_stock, redis_set_spectre)
mdf <- melt(df, measure.vars=1:2)
VD.A(mdf$value, mdf$variable)

# Stress-ng fork
stress_ng_fork_stock <-c(720482483,716300095,722745982,736270247,734745360,716876568,711136889,727205313,722267169,727210944)
stress_ng_fork_spectre <-c(251853660,271687916,260128075,276367687,268708873,264560677,267901987,272127735,276392371,268045437)

median(stress_ng_fork_stock)
median(stress_ng_fork_spectre)

shapiro.test(stress_ng_fork_stock)
wilcox.test(stress_ng_fork_stock, stress_ng_fork_spectre, paired = FALSE)

df <- data.frame(stress_ng_fork_stock, stress_ng_fork_spectre)
mdf <- melt(df, measure.vars=1:2)
VD.A(mdf$value, mdf$variable)

# Stress-ng msg
stress_ng_msg_stock <-c(213421632,213005268,213642457,213379647,213674739,213500538,213424800,218166201,213923107,213802013)
stress_ng_msg_spectre <-c(5635749,10075640,4657224,5132285,6229192,6389119,108653563,20303418,7977770,7286532)

median(stress_ng_msg_stock)
median(stress_ng_msg_spectre)

shapiro.test(stress_ng_msg_stock)
wilcox.test(stress_ng_msg_stock, stress_ng_msg_spectre, paired = FALSE)

df <- data.frame(stress_ng_msg_stock, stress_ng_msg_spectre)
mdf <- melt(df, measure.vars=1:2)
VD.A(mdf$value, mdf$variable)

# Stress-ng sem
stress_ng_sem_stock <-c(131766746,131529009,131616975,131983323,132070536,131439801,131968422,131935177,131989461,131960941)
stress_ng_sem_spectre <-c(15425498,23720005,20347222,39937544,20967847,111575061,88005051,61476369,25377533,10734393)

median(stress_ng_sem_stock)
median(stress_ng_sem_spectre)

shapiro.test(stress_ng_sem_stock)
wilcox.test(stress_ng_sem_stock, stress_ng_sem_spectre, paired = FALSE)

df <- data.frame(stress_ng_sem_stock, stress_ng_sem_spectre)
mdf <- melt(df, measure.vars=1:2)
VD.A(mdf$value, mdf$variable)

# Stress-ng switch
stress_ng_switch_stock <-c(84404051,81746009,89894127,93181149,92957999,119501693,102590989,114008311,112462234,131505717)
stress_ng_switch_spectre <-c(79853252,80429760,80268836,79909785,81531525,80439763,79901422,80335689,80389835,79941893)

median(stress_ng_switch_stock)
median(stress_ng_switch_spectre)

shapiro.test(stress_ng_switch_stock)
wilcox.test(stress_ng_switch_stock, stress_ng_switch_spectre, paired = FALSE)

df <- data.frame(stress_ng_switch_stock, stress_ng_switch_spectre)
mdf <- melt(df, measure.vars=1:2)
VD.A(mdf$value, mdf$variable)

# T-test1
t_test_stock <-c(29928488,29157326,28803996,28725199,28450422,28879906,28560455,28870736,30615629,29101927)
t_test_spectre <-c(20945430,20658367,21721722,21847468,21782735,20700557,21050253,21630805,18874647,20891369)

median(t_test_stock)
median(t_test_spectre)

shapiro.test(t_test_stock)
wilcox.test(t_test_stock, t_test_spectre, paired = FALSE)

df <- data.frame(t_test_stock, t_test_spectre)
mdf <- melt(df, measure.vars=1:2)
VD.A(mdf$value, mdf$variable)

# build linux kernel
kernel_stock <-c(11779713043,11822509109,11821776389,11806793002,11828361527,11842912569,11843676595,11844746190,11834487044,11838899634)
kernel_spectre <-c(10935613962,11254648824,11272239090,11241363397,11276696908,11275366645,11254973762,11264845632,11271919971,11288777975)

median(kernel_stock)
median(kernel_spectre)

shapiro.test(kernel_stock)
wilcox.test(kernel_stock, kernel_spectre, paired = FALSE)

df <- data.frame(kernel_stock, kernel_spectre)
mdf <- melt(df, measure.vars=1:2)
VD.A(mdf$value, mdf$variable)

# Mcperf add
mcperf_add_stock <-c(6099126,6057863,6041867,6058618,6069509)
mcperf_add_spectre <-c(3873499,4019486,4039408,4055105,4287854)

median(mcperf_add_stock)
median(mcperf_add_spectre)

shapiro.test(mcperf_add_stock)
wilcox.test(mcperf_add_stock, mcperf_add_spectre, paired = FALSE)

df <- data.frame(mcperf_add_stock, mcperf_add_spectre)
mdf <- melt(df, measure.vars=1:2)
VD.A(mdf$value, mdf$variable)

# Mcperf append
mcperf_append_stock <-c(6047909,6064033,6057132,6059625,6040892)
mcperf_append_spectre <-c(4055830,4045603,4174400,3676021,4030252)

median(mcperf_append_stock)
median(mcperf_append_spectre)

shapiro.test(mcperf_append_stock)
wilcox.test(mcperf_append_stock, mcperf_append_spectre, paired = FALSE)

df <- data.frame(mcperf_append_stock, mcperf_append_spectre)
mdf <- melt(df, measure.vars=1:2)
VD.A(mdf$value, mdf$variable)

# Mcperf delete
mcperf_delete_stock <-c(20100727,20020694,20573299,20005376,20098478)
mcperf_delete_spectre <-c(2008111,2025737,2020070,1975667,2033618)

median(mcperf_delete_stock)
median(mcperf_delete_spectre)

shapiro.test(mcperf_delete_stock)
wilcox.test(mcperf_delete_stock, mcperf_delete_spectre, paired = FALSE)

df <- data.frame(mcperf_delete_stock, mcperf_delete_spectre)
mdf <- melt(df, measure.vars=1:2)
VD.A(mdf$value, mdf$variable)

# Mcperf get 
mcperf_get_stock <-c(19930774,19974794,20012813,19995285,20064557)
mcperf_get_spectre <-c(1955826,2016385,2014095,2027605,2104841)

median(mcperf_get_stock)
median(mcperf_get_spectre)

shapiro.test(mcperf_get_stock)
wilcox.test(mcperf_get_stock, mcperf_get_spectre, paired = FALSE)

df <- data.frame(mcperf_get_stock, mcperf_get_spectre)
mdf <- melt(df, measure.vars=1:2)
VD.A(mdf$value, mdf$variable)

# Mcperf prepend
mcperf_prepend_stock <-c(6056952,6048553,6058024,6050369,6045127)
mcperf_prepend_spectre <-c(4183058,4036851,4038872,3871170,3005546)

median(mcperf_get_stock)
median(mcperf_get_spectre)

shapiro.test(mcperf_get_stock)
wilcox.test(mcperf_get_stock, mcperf_get_spectre, paired = FALSE)

df <- data.frame(mcperf_get_stock, mcperf_get_spectre)
mdf <- melt(df, measure.vars=1:2)
VD.A(mdf$value, mdf$variable)

# Mcperf replace
mcperf_replace_stock <-c(6110124,6052509,6059093,6045623,6468950)
mcperf_replace_spectre <-c(2691216,4031753,2937742,3841019,6000696)

median(mcperf_replace_stock)
median(mcperf_replace_spectre)

shapiro.test(mcperf_replace_stock)
wilcox.test(mcperf_replace_stock, mcperf_replace_spectre, paired = FALSE)

df <- data.frame(mcperf_replace_stock, mcperf_replace_spectre)
mdf <- melt(df, measure.vars=1:2)
VD.A(mdf$value, mdf$variable)

# Mcperf set
mcperf_set_stock <-c(6070839,6042696,6045046,6077200,6048160)
mcperf_set_spectre <-c(4049316,4095597,4228102,4080746,2697702)

median(mcperf_set_stock)
median(mcperf_set_spectre)

shapiro.test(mcperf_set_stock)
wilcox.test(mcperf_set_stock, mcperf_set_spectre, paired = FALSE)

df <- data.frame(mcperf_set_stock, mcperf_set_spectre)
mdf <- melt(df, measure.vars=1:2)
VD.A(mdf$value, mdf$variable)


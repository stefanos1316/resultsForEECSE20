# CTX clock
library(effsize)
library(reshape2)

ctx_stock <-c(1000816132,1000675306,1000986543,1000672260,1000728578,1000555656,1000554851,1000621025,1001058314,1001117058)
ctx_mds <-c(603851,548579,599230,620720,781036,941135,569844,496380,789576,538681)

median(ctx_stock)
median(ctx_mds)

shapiro.test(ctx_stock)
wilcox.test(ctx_stock, ctx_mds, paired = FALSE)

df <- data.frame(ctx_stock, ctx_mds)
mdf <- melt(df, measure.vars=1:2)
VD.A(mdf$value, mdf$variable)

#Apache
apacheStock <-c(1473803920,1593686108,1395414484,1311889639,1431324363,1668698564,1579639504,1551488416,1393416183,1453808181)
apacheSpectre <-c(1437858220,1582858438,1607826208,1425994103,1558632444,1314519914,1496795617,1334371472,1295875317,1342233755)

median(apacheStock)
median(apacheSpectre)

shapiro.test(apacheStock)
wilcox.test(apacheStock, apacheSpectre, paired = FALSE)

df <- data.frame(apacheStock, apacheSpectre)
mdf <- melt(df, measure.vars=1:2)
VD.A(mdf$value, mdf$variable)

# Nginx
nginxStock <-c(1453522563,1504888428,1535771524,1508548938,1481186021,1404395460,1480310595,1424710132,1476031085,1523207705)
nginxSpectre <-c(1369232036,1446767615,1589375009,1434551984,1427792271,1503059008,1508695787,1437218468,1379485063,1510228794)

median(nginxStock)
median(nginxSpectre)

shapiro.test(nginxStock)
wilcox.test(nginxStock, nginxSpectre, paired = FALSE)

df <- data.frame(nginxStock, nginxSpectre)
mdf <- melt(df, measure.vars=1:2)
VD.A(mdf$value, mdf$variable)

# Network loopback
network_loopback_stock <-c(27170357131,27794572579,27490088847,27712783762,27438777296,27533188495,28270900269,27673317503,27421541516,27900895545)
network_loopback_mds <-c(26067075702,25928598925,26244077121,26507348498,26361315360,26615755486,25808404761,26363529895,26219488517,26069187356)

median(network_loopback_stock)
median(network_loopback_mds)

shapiro.test(network_loopback_stock)
wilcox.test(network_loopback_stock, network_loopback_mds, paired = FALSE)

df <- data.frame(network_loopback_stock, network_loopback_mds)
mdf <- melt(df, measure.vars=1:2)
VD.A(mdf$value, mdf$variable)

# Osbench create files
osbench_create_files_stock <-c(20152620,19595097,15174515,18240788,16015035,21039807,16301196,20725105,18348606,16995192)
osbench_create_files_mds <-c(9441069,11454127,10721359,12256677,11074498,5385161,10403925,8634136,13633070,11102161)

median(osbench_create_files_stock)
median(osbench_create_files_mds)

shapiro.test(osbench_create_files_stock)
wilcox.test(osbench_create_files_stock, osbench_create_files_mds, paired = FALSE)

df <- data.frame(osbench_create_files_stock, osbench_create_files_mds)
mdf <- melt(df, measure.vars=1:2)
VD.A(mdf$value, mdf$variable)

# Osbench create processes
osbench_create_processes_stock <-c(4886815330,4424538761,4384930068,4389022124,4621997461,4715004838,4411672497,4453613058,4710621048,4295730738)
osbench_create_processes_mds <-c(4368465538,4450747410,4466758754,4824282928,4692007858,4282164734,4385415245,4489714343,4384037263,4565350771)

median(osbench_create_processes_stock)
median(osbench_create_processes_mds)

shapiro.test(osbench_create_processes_stock)
wilcox.test(osbench_create_processes_stock, osbench_create_processes_mds, paired = FALSE)

df <- data.frame(osbench_create_processes_stock, osbench_create_processes_mds)
mdf <- melt(df, measure.vars=1:2)
VD.A(mdf$value, mdf$variable)

# Osbench create threads
osbench_create_threads_stock <-c(1251002107,1211806511,1183642956,1184067490,1214098139,2025784534,1161740704,1207988848,1194972562,1211628713)
osbench_create_threads_mds <-c(1201225865,1184181823,1203914423,1192882835,1207642317,1164593319,1189314178,1224567072,1198208517,1164685682)

median(osbench_create_threads_stock)
median(osbench_create_threads_mds)

shapiro.test(osbench_create_threads_stock)
wilcox.test(osbench_create_threads_stock, osbench_create_threads_mds, paired = FALSE)

df <- data.frame(osbench_create_threads_stock, osbench_create_threads_mds)
mdf <- melt(df, measure.vars=1:2)
VD.A(mdf$value, mdf$variable)

# Osbench launch programs
osbench_launch_programs_stock <-c(1735676817,1769474055,1683351908,1745574261,1710767150,1699486468,1721943455,1686545133,1693231437,1758996895)
osbench_launch_programs_mds <-c(1786106237,1658899211,1652664805,1686003432,1825609481,1640047728,1686742775,1755702811,1689179488,1644625866)

median(osbench_launch_programs_stock)
median(osbench_launch_programs_mds)

shapiro.test(osbench_launch_programs_stock)
wilcox.test(osbench_launch_programs_stock, osbench_launch_programs_mds, paired = FALSE)

df <- data.frame(osbench_launch_programs_stock, osbench_launch_programs_mds)
mdf <- melt(df, measure.vars=1:2)
VD.A(mdf$value, mdf$variable)

# Osbench mem alloc
osbench_mem_alloc_stock <-c(460257274,481700371,470596106,449056371,460217275,465111485,468882059,455667212,452009601,458384523)
osbench_mem_alloc_mds <-c(472864621,464967768,466653393,464209401,455553551,463803219,467109172,461902245,460099820,452810878)

median(osbench_mem_alloc_stock)
median(osbench_mem_alloc_mds)

shapiro.test(osbench_mem_alloc_stock)
wilcox.test(osbench_mem_alloc_stock, osbench_mem_alloc_mds, paired = FALSE)

df <- data.frame(osbench_mem_alloc_stock, osbench_mem_alloc_mds)
mdf <- melt(df, measure.vars=1:2)
VD.A(mdf$value, mdf$variable)

# Redis get
redis_get_stock <-c(77819256,77462635,77454906,77507854,77437637,77727623,77376969,77487325,76855597,77491227)
redis_get_mds <-c(2854609,2611701,2842571,2467059,2427989,2603948,2609740,2881111,2488081,2633617)

median(redis_get_stock)
median(redis_get_mds)

shapiro.test(redis_get_stock)
wilcox.test(redis_get_stock, redis_get_mds, paired = FALSE)

df <- data.frame(redis_get_stock, redis_get_mds)
mdf <- melt(df, measure.vars=1:2)
VD.A(mdf$value, mdf$variable)

# Redis lpop
redis_lpop_stock <-c(77790482,77513861,77474209,77275203,77197773,76923844,77444276,77740189,77487768,77125928)
redis_lpop_mds <-c(3053989,2589646,2810778,2795588,2479813,2533153,2924220,2522576,2508159,2769859)

median(redis_lpop_stock)
median(redis_lpop_mds)

shapiro.test(redis_lpop_stock)
wilcox.test(redis_lpop_stock, redis_lpop_mds, paired = FALSE)

df <- data.frame(redis_lpop_stock, redis_lpop_mds)
mdf <- melt(df, measure.vars=1:2)
VD.A(mdf$value, mdf$variable)

# Redis lpush
redis_lpush_stock <-c(77047619,77209433,77448989,77357425,77336577,77620559,77582253,77228033,77678225,77716939)
redis_lpush_mds <-c(2756001,2962381,2252756,2581008,2417056,2863721,2502010,2335960,2811559,2964033)

median(redis_lpush_stock)
median(redis_lpush_mds)

shapiro.test(redis_lpush_stock)
wilcox.test(redis_lpush_stock, redis_lpush_mds, paired = FALSE)

df <- data.frame(redis_lpush_stock, redis_lpush_mds)
mdf <- melt(df, measure.vars=1:2)
VD.A(mdf$value, mdf$variable)

# Redis sadd
redis_sadd_stock <-c(77567191,77292414,77129447,77830125,77800371,77784260,77338639,77371045,77842186,77824496)
redis_sadd_mds <-c(2553303,2760323,2648731,2640140,2418845,2532535,2824445,2643235,2994078,2647838)

median(redis_sadd_stock)
median(redis_sadd_mds)

shapiro.test(redis_sadd_stock)
wilcox.test(redis_sadd_stock, redis_sadd_mds, paired = FALSE)

df <- data.frame(redis_sadd_stock, redis_sadd_mds)
mdf <- melt(df, measure.vars=1:2)
VD.A(mdf$value, mdf$variable)

# Redis set
redis_set_stock <-c(77434667,77424934,77370576,77630861,77681510,77262954,77451076,77522745,77426180,77417879)
redis_set_mds <-c(2679566,2558886,2672827,2610195,2459592,3018546,2420046,2881711,2620877,3051974)

median(redis_set_stock)
median(redis_set_mds)

shapiro.test(redis_set_stock)
wilcox.test(redis_set_stock, redis_set_mds, paired = FALSE)

df <- data.frame(redis_set_stock, redis_set_mds)
mdf <- melt(df, measure.vars=1:2)
VD.A(mdf$value, mdf$variable)

# Stress-ng fork
stress_ng_fork_stock <-c(4753157597,5096163141,5062693856,5258556095,4878777394,5007701695,4743063249,4829282999,5248226614,5032162801)
stress_ng_fork_mds <-c(4812232736,4928043286,5101494636,4764782392,4649384220,4918973894,4648482931,4707934316,4956980450,4776914594)

median(stress_ng_fork_stock)
median(stress_ng_fork_mds)

shapiro.test(stress_ng_fork_stock)
wilcox.test(stress_ng_fork_stock, stress_ng_fork_mds, paired = FALSE)

df <- data.frame(stress_ng_fork_stock, stress_ng_fork_mds)
mdf <- melt(df, measure.vars=1:2)
VD.A(mdf$value, mdf$variable)

# Stress-ng msg
stress_ng_msg_stock <-c(224351282,225402889,217226889,225025769,216558868,224727548,216920360,216795166,225696664,217008213)
stress_ng_msg_mds <-c(2521953,11440750,11831359,2740607,11198002,2512052,10774994,2515398,2675059,10644286)

median(stress_ng_msg_stock)
median(stress_ng_msg_mds)

shapiro.test(stress_ng_msg_stock)
wilcox.test(stress_ng_msg_stock, stress_ng_msg_mds, paired = FALSE)

df <- data.frame(stress_ng_msg_stock, stress_ng_msg_mds)
mdf <- melt(df, measure.vars=1:2)
VD.A(mdf$value, mdf$variable)

# Stress-ng sem
stress_ng_sem_stock <-c(134524500,135035390,143333356,135818846,134821201,144499242,134790440,143939556,109611199,134516802)
stress_ng_sem_mds <-c(1635713,12549607,12000437,1662047,1712707,1797466,1715657,1751427,12286765,1789024)

median(stress_ng_sem_stock)
median(stress_ng_sem_mds)

shapiro.test(stress_ng_sem_stock)
wilcox.test(stress_ng_sem_stock, stress_ng_sem_mds, paired = FALSE)

df <- data.frame(stress_ng_sem_stock, stress_ng_sem_mds)
mdf <- melt(df, measure.vars=1:2)
VD.A(mdf$value, mdf$variable)

# Stress-ng switch
stress_ng_switch_stock <-c(88022456,81960711,81722364,91160127,81930533,87154650,81822128,81778038,82205959,82006292)
stress_ng_switch_mds <-c(9677195,857445,877356,827759,1028787,7198078,8100237,7913163,7595411,855705)

median(stress_ng_switch_stock)
median(stress_ng_switch_mds)

shapiro.test(stress_ng_switch_stock)
wilcox.test(stress_ng_switch_stock, stress_ng_switch_mds, paired = FALSE)

df <- data.frame(stress_ng_switch_stock, stress_ng_switch_mds)
mdf <- melt(df, measure.vars=1:2)
VD.A(mdf$value, mdf$variable)

# T-test1
t_test_stock <-c(504993066,500174332,499327190,497332917,498316532,497229258,507538348,498499930,499428179,500007676)
t_test_mds <-c(506307396,506373912,507327294,506199997,505411524,502689343,506249246,503184825,510768371,504864828)

median(t_test_stock)
median(t_test_mds)

shapiro.test(t_test_stock)
wilcox.test(t_test_stock, t_test_mds, paired = FALSE)

df <- data.frame(t_test_stock, t_test_mds)
mdf <- melt(df, measure.vars=1:2)
VD.A(mdf$value, mdf$variable)

# build linux kernel
kernel_stock <-c(117590692276,116268673112,116145717585,116481232094,116399874500,116316123144,116487981484,116337463269,116578778870,116609701842)
kernel_mds <-c(119194652241,118341056325,118212977721,118322086083,118216031803,118214321201,118538246624,118232609371,118327705600,11840451428)

median(kernel_stock)
median(kernel_mds)

shapiro.test(kernel_stock)
wilcox.test(kernel_stock, kernel_mds, paired = FALSE)

df <- data.frame(kernel_stock, kernel_mds)
mdf <- melt(df, measure.vars=1:2)
VD.A(mdf$value, mdf$variable)

# Mcperf add
mcperf_add_stock <-c(775802129,782243522,729280393,799559968,752144822,724903815,776671158,790890129,774981109,771901236)
mcperf_add_mds <-c(792680350,771965087,805161198,790743785,786368544,778107992,746789591,790549004,789756895,779705011)

median(mcperf_add_stock)
median(mcperf_add_mds)

shapiro.test(mcperf_add_stock)
wilcox.test(mcperf_add_stock, mcperf_add_mds, paired = FALSE)

df <- data.frame(mcperf_add_stock, mcperf_add_mds)
mdf <- melt(df, measure.vars=1:2)
VD.A(mdf$value, mdf$variable)

# Mcperf append
mcperf_append_stock <-c(787838430,779822303,775675563,801261789,746066938,773892852,764257981,796511048,777065709,787063804)
mcperf_append_mds <-c(764428311,788049380,792467633,781868755,813774043,811197335,772183111,789335673,773601414,768049126)

median(mcperf_append_stock)
median(mcperf_append_mds)

shapiro.test(mcperf_append_stock)
wilcox.test(mcperf_append_stock, mcperf_append_mds, paired = FALSE)

df <- data.frame(mcperf_append_stock, mcperf_append_mds)
mdf <- melt(df, measure.vars=1:2)
VD.A(mdf$value, mdf$variable)

# Mcperf delete
mcperf_delete_stock <-c(21501828,21444523,21712717,21616190,21533169,21653098,21775735,21155285,21659265,21596265)
mcperf_delete_mds <-c(463036,234874,311459,324000,355908,259405,225828,207879,352599,247944)

median(mcperf_delete_stock)
median(mcperf_delete_mds)

shapiro.test(mcperf_delete_stock)
wilcox.test(mcperf_delete_stock, mcperf_delete_mds, paired = FALSE)

df <- data.frame(mcperf_delete_stock, mcperf_delete_mds)
mdf <- melt(df, measure.vars=1:2)
VD.A(mdf$value, mdf$variable)

# Mcperf get 
mcperf_get_stock <-c(21578659,21920465,21900124,21800808,21614938,21557557,21487116,21624023,21616878,22116702)
mcperf_get_mds <-c(281897,936177,497163,4104710,1090002,648101,875296,981295,1107489,2868992)

median(mcperf_get_stock)
median(mcperf_get_mds)

shapiro.test(mcperf_get_stock)
wilcox.test(mcperf_get_stock, mcperf_get_mds, paired = FALSE)

df <- data.frame(mcperf_get_stock, mcperf_get_mds)
mdf <- melt(df, measure.vars=1:2)
VD.A(mdf$value, mdf$variable)

# Mcperf prepend
mcperf_prepend_stock <-c(816879492,846143658,803317188,794503373,768236329,808090536,763093016,777201066,756251341,745399975)
mcperf_prepend_mds <-c(768221143,772466890,819447686,803360181,799468468,723026710,778979984,801457850,742826580,768977450)

median(mcperf_prepend_stock)
median(mcperf_prepend_mds)

shapiro.test(mcperf_prepend_stock)
wilcox.test(mcperf_prepend_stock, mcperf_prepend_mds, paired = FALSE)

df <- data.frame(mcperf_prepend_stock, mcperf_prepend_mds)
mdf <- melt(df, measure.vars=1:2)
VD.A(mdf$value, mdf$variable)

# Mcperf replace
mcperf_replace_stock <-c(794082107,759203911,816360075,772805601,757706081,818165674,757209849,786123548,793183527,742817534)
mcperf_replace_mds <-c(764955467,776079036,800230859,739697171,723854742,789978211,775092436,772860043,786786622,766466327)

median(mcperf_replace_stock)
median(mcperf_replace_mds)

shapiro.test(mcperf_replace_stock)
wilcox.test(mcperf_replace_stock, mcperf_replace_mds, paired = FALSE)

df <- data.frame(mcperf_replace_stock, mcperf_replace_mds)
mdf <- melt(df, measure.vars=1:2)
VD.A(mdf$value, mdf$variable)

# Mcperf set
mcperf_set_stock <-c(767157227,790907704,764603181,798773450,737225235,769677115,723096704,792579463,795068796,753600159)
mcperf_set_mds <-c(786370470,747313099,812270820,747868789,750197723,791653091,762887180,766193412,800941347,756342589)

median(mcperf_set_stock)
median(mcperf_set_mds)

shapiro.test(mcperf_set_stock)
wilcox.test(mcperf_set_stock, mcperf_set_mds, paired = FALSE)

df <- data.frame(mcperf_set_stock, mcperf_set_mds)
mdf <- melt(df, measure.vars=1:2)
VD.A(mdf$value, mdf$variable)


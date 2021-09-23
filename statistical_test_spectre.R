# CTX clock
library(effsize)
library(reshape2)

ctx_stock <-c(1002451858,1002265433,1002279392,1002493367,1002366518,1002274612,1002317379,1002393740,1002411372,1002164001)
ctx_spectre <-c(1270835,1285963,1246702,1288524,1256466,1256886,1251968,1258582,1297623,1244266)

median(ctx_stock)
median(ctx_spectre)

shapiro.test(ctx_stock)
wilcox.test(ctx_stock, ctx_spectre, paired = FALSE)

df <- data.frame(ctx_stock, ctx_spectre)
mdf <- melt(df, measure.vars=1:2)
VD.A(mdf$value, mdf$variable)

# Apache
apacheStock <-c(454956099,453152922,437564739,445947897,448671904,435662456,438860917,442001752,449491315,449151456)
apacheSpectre <-c(116324780,110499952,113765568,112615763,107587270,112644725,106009302,103285564,114342568,109791113)

median(apacheStock)
median(apacheSpectre)

shapiro.test(apacheStock)
wilcox.test(apacheStock, apacheSpectre, paired = FALSE)

df <- data.frame(apacheStock, apacheSpectre)
mdf <- melt(df, measure.vars=1:2)
VD.A(mdf$value, mdf$variable)

# Nginx
nginxStock <-c(438560852,455294277,448246523,453429641,448422294,449630752,451216525,452715562,448061497,447878236)
nginxSpectre <-c(111213922,115004521,113258053,117360306,118653639,114363145,106663792,116708165,113420248,115646759)

median(nginxStock)
median(nginxSpectre)

shapiro.test(nginxStock)
wilcox.test(nginxStock, nginxSpectre, paired = FALSE)

df <- data.frame(nginxStock, nginxSpectre)
mdf <- melt(df, measure.vars=1:2)
VD.A(mdf$value, mdf$variable)

# Network loopback
network_loopback_stock <-c(1095530265,1095544690,1089279787,1093081512,1096990878,1097006135,1093869343,1092267204,1096588834,1099490988)
network_loopback_spectre <-c(150981309,146485932,150261866,147255553,148739246,149696749,145110262,146979407,144586953,150671219)

median(network_loopback_stock)
median(network_loopback_spectre)

shapiro.test(network_loopback_stock)
wilcox.test(network_loopback_stock, network_loopback_spectre, paired = FALSE)

df <- data.frame(network_loopback_stock, network_loopback_spectre)
mdf <- melt(df, measure.vars=1:2)
VD.A(mdf$value, mdf$variable)

# Osbench create files
osbench_create_files_stock <-c(292872455,286363437,283789564,286904282,298501627,287531617,279561663,293755885,295170701,288487636)
osbench_create_files_spectre <-c(16944287,14398421,13033863,12728271,15436984,12956115,14091640,18438075,10732028,15124562)

median(osbench_create_files_stock)
median(osbench_create_files_spectre)

shapiro.test(osbench_create_files_stock)
wilcox.test(osbench_create_files_stock, osbench_create_files_spectre, paired = FALSE)

df <- data.frame(osbench_create_files_stock, osbench_create_files_spectre)
mdf <- melt(df, measure.vars=1:2)
VD.A(mdf$value, mdf$variable)

# Osbench create processes
osbench_create_processes_stock <-c(375713917,369602814,389456603,380872408,378310449,371340637,399304206,406045311,402541490,379340332)
osbench_create_processes_spectre <-c(307738311,297680192,264426256,248544949,260455389,243305838,242465703,273023071,251632867,266535465)

median(osbench_create_processes_stock)
median(osbench_create_processes_spectre)

shapiro.test(osbench_create_processes_stock)
wilcox.test(osbench_create_processes_stock, osbench_create_processes_spectre, paired = FALSE)

df <- data.frame(osbench_create_processes_stock, osbench_create_processes_spectre)
mdf <- melt(df, measure.vars=1:2)
VD.A(mdf$value, mdf$variable)

# Osbench create threads
osbench_create_threads_stock <-c(119011423,123426207,149542404,142607723,146017275,146311972,145047978,132517815,130961593,144258253)
osbench_create_threads_spectre <-c(113934378,108510577,94817558,99794322,98568746,94272392,98215896,95367936,92090615,95607218)

median(osbench_create_threads_stock)
median(osbench_create_threads_spectre)

shapiro.test(osbench_create_threads_stock)
wilcox.test(osbench_create_threads_stock, osbench_create_threads_spectre, paired = FALSE)

df <- data.frame(osbench_create_threads_stock, osbench_create_threads_spectre)
mdf <- melt(df, measure.vars=1:2)
VD.A(mdf$value, mdf$variable)

# Osbench launch programs
osbench_launch_programs_stock <-c(1183270167,1183148415,1182336411,1186838275,1177249037,1177772243,1180415385,1183207205,1185620022,1186942118)
osbench_launch_programs_spectre <-c(1073339188,1075917121,1078421300,1076074742,1077810586,1072015546,1074750834,1076923079,1078467849,1075402580)

median(osbench_launch_programs_stock)
median(osbench_launch_programs_spectre)

shapiro.test(osbench_launch_programs_stock)
wilcox.test(osbench_launch_programs_stock, osbench_launch_programs_spectre, paired = FALSE)

df <- data.frame(osbench_launch_programs_stock, osbench_launch_programs_spectre)
mdf <- melt(df, measure.vars=1:2)
VD.A(mdf$value, mdf$variable)

# Osbench mem alloc
osbench_mem_alloc_stock <-c(32840673,32877561,32440665,32464035,32798192,32765848,32445658,32407899,32484732,32409158)
osbench_mem_alloc_spectre <-c(24687726,24630604,25737687,24649728,24602307,24548326,26309067,24392033,24490782,24584589)

median(osbench_mem_alloc_stock)
median(osbench_mem_alloc_spectre)

shapiro.test(osbench_mem_alloc_stock)
wilcox.test(osbench_mem_alloc_stock, osbench_mem_alloc_spectre, paired = FALSE)

df <- data.frame(osbench_mem_alloc_stock, osbench_mem_alloc_spectre)
mdf <- melt(df, measure.vars=1:2)
VD.A(mdf$value, mdf$variable)

# Redis get
redis_get_stock <-c(1167987963,1157406875,1155871271,1155313200,1157721169,1152461198,1151507690,1156583797,1157855382,1144985274)
redis_get_spectre <-c(289075426,283870012,290695890,286900670,285937575,287364215,282778964,286534660,284075232,289208559)

median(redis_get_stock)
median(redis_get_spectre)

shapiro.test(redis_get_stock)
wilcox.test(redis_get_stock, redis_get_spectre, paired = FALSE)

df <- data.frame(redis_get_stock, redis_get_spectre)
mdf <- melt(df, measure.vars=1:2)
VD.A(mdf$value, mdf$variable)

# Redis lpop
redis_lpop_stock <-c(1151183749,1145634403,1144843847,1150367146,1161554451,1157783384,1151958720,1155710916,1157540137,1151046198)
redis_lpop_spectre <-c(284446541,287826099,289462124,291470818,285798318,287154450,289679748,291780794,289380168,289468889)

median(redis_lpop_stock)
median(redis_lpop_spectre)

shapiro.test(redis_lpop_stock)
wilcox.test(redis_lpop_stock, redis_lpop_spectre, paired = FALSE)

df <- data.frame(redis_lpop_stock, redis_lpop_spectre)
mdf <- melt(df, measure.vars=1:2)
VD.A(mdf$value, mdf$variable)

# Redis lpush
redis_lpush_stock <-c(1152371892,1158069005,1170622833,1156305726,1159321029,1151308346,1155556928,1157927671,1159790789,1167759341)
redis_lpush_spectre <-c(286429871,282328093,284925729,289733770,283350087,289022959,284888785,284289428,294617498,285669942)

median(redis_lpush_stock)
median(redis_lpush_spectre)

shapiro.test(redis_lpush_stock)
wilcox.test(redis_lpush_stock, redis_lpush_spectre, paired = FALSE)

df <- data.frame(redis_lpush_stock, redis_lpush_spectre)
mdf <- melt(df, measure.vars=1:2)
VD.A(mdf$value, mdf$variable)

# Redis sadd
redis_sadd_stock <-c(1151032603,1157238766,1152740843,1170463095,1158324990,1145753958,1149867282,1154995883,1157179451,1155976307)
redis_sadd_spectre <-c(287707509,290135511,289171012,288814983,283286367,288367564,287815516,287310380,282091634,286505600)

median(redis_sadd_stock)
median(redis_sadd_spectre)

shapiro.test(redis_sadd_stock)
wilcox.test(redis_sadd_stock, redis_sadd_spectre, paired = FALSE)

df <- data.frame(redis_sadd_stock, redis_sadd_spectre)
mdf <- melt(df, measure.vars=1:2)
VD.A(mdf$value, mdf$variable)

# Redis set
redis_set_stock <-c(1155278322,1145195044,1158674207,1165530261,1155825283,1165427018,1154919161,1153946789,1160246326,1151231696)
redis_set_spectre <-c(287869081,287126079,286634792,286915776,282878372,282082172,285058140,286657412,288775899,280455975)

median(redis_set_stock)
median(redis_set_spectre)

shapiro.test(redis_set_stock)
wilcox.test(redis_set_stock, redis_set_spectre, paired = FALSE)

df <- data.frame(redis_set_stock, redis_set_spectre)
mdf <- melt(df, measure.vars=1:2)
VD.A(mdf$value, mdf$variable)

# Stress-ng fork
stress_ng_fork_stock <-c(909228313,928212929,921278498,911205947,930668896,931062515,921342990,931749587,933194745,935449147)
stress_ng_fork_spectre <-c(750249246,726960803,735157743,733674222,737134865,712942456,781225262,750821879,714756166,751607336)

median(stress_ng_fork_stock)
median(stress_ng_fork_spectre)

shapiro.test(stress_ng_fork_stock)
wilcox.test(stress_ng_fork_stock, stress_ng_fork_spectre, paired = FALSE)

df <- data.frame(stress_ng_fork_stock, stress_ng_fork_spectre)
mdf <- melt(df, measure.vars=1:2)
VD.A(mdf$value, mdf$variable)

# Stress-ng msg
stress_ng_msg_stock <-c(662036549,827357535,623497789,608369871,604985030,601681871,610117836,633845200,636386555,1090209581)
stress_ng_msg_spectre <-c(84976447,79520430,77627176,73920769,84384313,87999963,81340009,79437176,76098697,76988173)

median(stress_ng_msg_stock)
median(stress_ng_msg_spectre)

shapiro.test(stress_ng_msg_stock)
wilcox.test(stress_ng_msg_stock, stress_ng_msg_spectre, paired = FALSE)

df <- data.frame(stress_ng_msg_stock, stress_ng_msg_spectre)
mdf <- melt(df, measure.vars=1:2)
VD.A(mdf$value, mdf$variable)

# Stress-ng sem
stress_ng_sem_stock <-c(702808087,710832514,435725249,700454518,703821820,713456903,701192721,703709100,707332295,714641049)
stress_ng_sem_spectre <-c(367372832,367265856,370753866,370884336,374377051,206269227,368476572,369992291,368010542,364052726)

median(stress_ng_sem_stock)
median(stress_ng_sem_spectre)

shapiro.test(stress_ng_sem_stock)
wilcox.test(stress_ng_sem_stock, stress_ng_sem_spectre, paired = FALSE)

df <- data.frame(stress_ng_sem_stock, stress_ng_sem_spectre)
mdf <- melt(df, measure.vars=1:2)
VD.A(mdf$value, mdf$variable)

# Stress-ng switch
stress_ng_switch_stock <-c(1847672424,1838617040,1837626391,1843188320,1825418369,1830914524,1833350716,1835621713,1838276713,1823238194)
stress_ng_switch_spectre <-c(585702861,573947957,596702253,587458009,585338578,579910503,593728330,574068500,584103044,587762622)

median(stress_ng_switch_stock)
median(stress_ng_switch_spectre)

shapiro.test(stress_ng_switch_stock)
wilcox.test(stress_ng_switch_stock, stress_ng_switch_spectre, paired = FALSE)

df <- data.frame(stress_ng_switch_stock, stress_ng_switch_spectre)
mdf <- melt(df, measure.vars=1:2)
VD.A(mdf$value, mdf$variable)

# T-test1
t_test_stock <-c(184480318,183942881,183750135,183799380,183866917,183992097,184201392,184072316,184087544,184195999)
t_test_spectre <-c(177476309,176898611,176554852,176633610,176380219,176734930,176795710,177121332,176580903,177164040)

median(t_test_stock)
median(t_test_spectre)

shapiro.test(t_test_stock)
wilcox.test(t_test_stock, t_test_spectre, paired = FALSE)

df <- data.frame(t_test_stock, t_test_spectre)
mdf <- melt(df, measure.vars=1:2)
VD.A(mdf$value, mdf$variable)

# build linux kernel
kernel_stock <-c(171827743892,172087932203,172109541894,172090321068,172124035794,172088853416,172090245775,172115576964,172107587364,172106362981)
kernel_spectre <-c(170712514107,171069046649,171057514713,171111429349,171073287423,170998885498,171030390550,171093962360,171029725493,171067748112)

median(kernel_stock)
median(kernel_spectre)

shapiro.test(kernel_stock)
wilcox.test(kernel_stock, kernel_spectre, paired = FALSE)

df <- data.frame(kernel_stock, kernel_spectre)
mdf <- melt(df, measure.vars=1:2)
VD.A(mdf$value, mdf$variable)

# Mcperf add
mcperf_add_stock <-c(146003372,142455002,143803282,146834584,148113953,149042201,141900240,145923175,147606426,141898324)
mcperf_add_spectre <-c(22468867,15196318,17669500,19525915,22736329,22167462,21224903,18962321,25644263,15109517)

median(mcperf_add_stock)
median(mcperf_add_spectre)

shapiro.test(mcperf_add_stock)
wilcox.test(mcperf_add_stock, mcperf_add_spectre, paired = FALSE)

df <- data.frame(mcperf_add_stock, mcperf_add_spectre)
mdf <- melt(df, measure.vars=1:2)
VD.A(mdf$value, mdf$variable)

# Mcperf append
mcperf_append_stock <-c(145731036,144179663,151980924,145200071,149677963,146748506,145738549,144604416,149539693,142453681)
mcperf_append_spectre <-c(26488241,18703196,20007177,15416446,18377296,23412154,20736207,18125773,22334429,21854938)

median(mcperf_append_stock)
median(mcperf_append_spectre)

shapiro.test(mcperf_append_stock)
wilcox.test(mcperf_append_stock, mcperf_append_spectre, paired = FALSE)

df <- data.frame(mcperf_append_stock, mcperf_append_spectre)
mdf <- melt(df, measure.vars=1:2)
VD.A(mdf$value, mdf$variable)

# Mcperf delete
mcperf_delete_stock <-c(225039224,226077490,225925917,221391676,226481756,222678079,222156428,228316661,223185677,225244038)
mcperf_delete_spectre <-c(10932321,9546451,6847464,8691634,11192750,9237655,11732391,9096031,8533316,11905670)

median(mcperf_delete_stock)
median(mcperf_delete_spectre)

shapiro.test(mcperf_delete_stock)
wilcox.test(mcperf_delete_stock, mcperf_delete_spectre, paired = FALSE)

df <- data.frame(mcperf_delete_stock, mcperf_delete_spectre)
mdf <- melt(df, measure.vars=1:2)
VD.A(mdf$value, mdf$variable)

# Mcperf get 
mcperf_get_stock <-c(222522090,221048640,223436621,226499269,222424885,222980979,220880218,225523057,221652329,224217052)
mcperf_get_spectre <-c(11160982,9925202,9300741,11295474,9114820,8261281,11730674,11472321,9379614,8610954)

median(mcperf_get_stock)
median(mcperf_get_spectre)

shapiro.test(mcperf_get_stock)
wilcox.test(mcperf_get_stock, mcperf_get_spectre, paired = FALSE)

df <- data.frame(mcperf_get_stock, mcperf_get_spectre)
mdf <- melt(df, measure.vars=1:2)
VD.A(mdf$value, mdf$variable)

# Mcperf prepend
mcperf_prepend_stock <-c(147145377,142684981,144686512,139533005,146241791,147259335,145315788,148247720,147461174,140123967)
mcperf_prepend_spectre <-c(20990775,24147298,22912648,23220626,20810404,19537050,23581298,28529857,19048097,19790575)

median(mcperf_get_stock)
median(mcperf_get_spectre)

shapiro.test(mcperf_get_stock)
wilcox.test(mcperf_get_stock, mcperf_get_spectre, paired = FALSE)

df <- data.frame(mcperf_get_stock, mcperf_get_spectre)
mdf <- melt(df, measure.vars=1:2)
VD.A(mdf$value, mdf$variable)

# Mcperf replace
mcperf_replace_stock <-c(141445692,139636369,143425796,148374192,144459131,141139551,146520568,142228894,143285446,145435150)
mcperf_replace_spectre <-c(16130562,17750854,24939753,20302121,18718296,19775602,23957273,25419746,18139298,21005716)

median(mcperf_replace_stock)
median(mcperf_replace_spectre)

shapiro.test(mcperf_replace_stock)
wilcox.test(mcperf_replace_stock, mcperf_replace_spectre, paired = FALSE)

df <- data.frame(mcperf_replace_stock, mcperf_replace_spectre)
mdf <- melt(df, measure.vars=1:2)
VD.A(mdf$value, mdf$variable)

# Mcperf set
mcperf_set_stock <-c(141350865,147312336,141975623,140667186,145231281,149799986,146936884,142351158,140834907,146033680)
mcperf_set_spectre <-c(19738131,23385924,19987962,18135887,18137341,23027927,21672798,21559226,26367113,19902108)

median(mcperf_set_stock)
median(mcperf_set_spectre)

shapiro.test(mcperf_set_stock)
wilcox.test(mcperf_set_stock, mcperf_set_spectre, paired = FALSE)

df <- data.frame(mcperf_set_stock, mcperf_set_spectre)
mdf <- melt(df, measure.vars=1:2)
VD.A(mdf$value, mdf$variable)


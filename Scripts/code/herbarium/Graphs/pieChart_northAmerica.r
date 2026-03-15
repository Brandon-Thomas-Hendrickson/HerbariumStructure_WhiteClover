#########################
#Pie Chart##############
#########################

#Big Map figure
library(rworldmap)
library(raster)
library(ggplot2)
library(ggmap)
library(mapdata)
library(maps)
library(ggsn)
library(RColorBrewer)
library(mapplots)

#separate out by timebins
#Contemp Post1990 Pre1900 Pre1930 Pre1960 Pre1990
herbgen <- read_csv("~/Documents/herbarium_structure_dataframe.csv")
#Maps
#Subset by individual
timebin<-as.factor(TimeBin)
country<-as.factor(Country)
range<-as.factor(RANGE)
herbgen<-cbind(herbgen, timebin, country, range)
country

pre1900<-herbgen[herbgen$TimeBin == "1838-1877",]
pre1930<-herbgen[herbgen$TimeBin == "1878-1917",]
pre1960<-herbgen[herbgen$TimeBin == "1918-1957",]
pre1990<-herbgen[herbgen$TimeBin == "1958-1997",]
contemp<-herbgen[herbgen$TimeBin == "1998-Present",]


sample1<-herbgen[herbgen$SampleN == "1",]
sample1 <- sample1[!is.na(sample1$K3Q1S1964), ]
sample2<-herbgen[herbgen$SampleN == "2",]
sample2 <- sample2[!is.na(sample2$K3Q1S1964), ]
sample3<-herbgen[herbgen$SampleN == "3",]
sample3 <- sample3[!is.na(sample3$K3Q1S1964), ]
sample4<-herbgen[herbgen$SampleN == "4",]
sample4 <- sample4[!is.na(sample4$K3Q1S1964), ]
sample5<-herbgen[herbgen$SampleN == "5",]
sample5 <- sample5[!is.na(sample5$K3Q1S1964), ]
sample6<-herbgen[herbgen$SampleN == "6",]
sample6 <- sample6[!is.na(sample6$K3Q1S1964), ]
sample7<-herbgen[herbgen$SampleN == "7",]
sample7 <- sample7[!is.na(sample7$K3Q1S1964), ]
sample8<-herbgen[herbgen$SampleN == "8",]
sample8 <- sample8[!is.na(sample8$K3Q1S1964), ]
sample9<-herbgen[herbgen$SampleN == "9",]
sample9 <- sample9[!is.na(sample9$K3Q1S1964), ]
sample10<-herbgen[herbgen$SampleN == "10",]
sample10 <- sample10[!is.na(sample10$K3Q1S1964), ]
sample11<-herbgen[herbgen$SampleN == "11",]
sample11 <- sample11[!is.na(sample11$K3Q1S1964), ]
sample12<-herbgen[herbgen$SampleN == "12",]
sample12 <- sample12[!is.na(sample12$K3Q1S1964), ]
sample13<-herbgen[herbgen$SampleN == "13",]
sample13 <- sample13[!is.na(sample13$K3Q1S1964), ]
sample14<-herbgen[herbgen$SampleN == "14",]
sample14 <- sample14[!is.na(sample14$K3Q1S1964), ]
sample15<-herbgen[herbgen$SampleN == "15",]
sample15 <- sample15[!is.na(sample15$K3Q1S1964), ]
sample16<-herbgen[herbgen$SampleN == "16",]
sample16 <- sample16[!is.na(sample16$K3Q1S1964), ]
sample17<-herbgen[herbgen$SampleN == "17",]
sample17 <- sample17[!is.na(sample17$K3Q1S1964), ]
sample18<-herbgen[herbgen$SampleN == "18",]
sample18 <- sample18[!is.na(sample18$K3Q1S1964), ]
sample19<-herbgen[herbgen$SampleN == "19",]
sample19 <- sample19[!is.na(sample19$K3Q1S1964), ]
sample20<-herbgen[herbgen$SampleN == "20",]
sample20 <- sample20[!is.na(sample20$K3Q1S1964), ]
sample21<-herbgen[herbgen$SampleN == "21",]
sample21 <- sample21[!is.na(sample21$K3Q1S1964), ]
sample22<-herbgen[herbgen$SampleN == "22",]
sample22 <- sample22[!is.na(sample22$K3Q1S1964), ]
sample23<-herbgen[herbgen$SampleN == "23",]
sample23 <- sample23[!is.na(sample23$K3Q1S1964), ]
sample24<-herbgen[herbgen$SampleN == "24",]
sample24 <- sample24[!is.na(sample24$K3Q1S1964), ]
sample25<-herbgen[herbgen$SampleN == "25",]
sample25 <- sample25[!is.na(sample25$K3Q1S1964), ]
sample26<-herbgen[herbgen$SampleN == "26",]
sample26 <- sample26[!is.na(sample26$K3Q1S1964), ]
sample27<-herbgen[herbgen$SampleN == "27",]
sample27 <- sample27[!is.na(sample27$K3Q1S1964), ]
sample28<-herbgen[herbgen$SampleN == "28",]
sample28 <- sample28[!is.na(sample28$K3Q1S1964), ]
sample29<-herbgen[herbgen$SampleN == "29",]
sample29 <- sample29[!is.na(sample29$K3Q1S1964), ]
sample30<-herbgen[herbgen$SampleN == "30",]
sample30 <- sample30[!is.na(sample30$K3Q1S1964), ]
sample31<-herbgen[herbgen$SampleN == "31",]
sample31 <- sample31[!is.na(sample31$K3Q1S1964), ]
sample32<-herbgen[herbgen$SampleN == "32",]
sample32 <- sample32[!is.na(sample32$K3Q1S1964), ]
sample33<-herbgen[herbgen$SampleN == "33",]
sample33 <- sample33[!is.na(sample33$K3Q1S1964), ]
sample34<-herbgen[herbgen$SampleN == "34",]
sample34 <- sample34[!is.na(sample34$K3Q1S1964), ]
sample35<-herbgen[herbgen$SampleN == "35",]
sample35 <- sample35[!is.na(sample35$K3Q1S1964), ]
sample36<-herbgen[herbgen$SampleN == "36",]
sample36 <- sample36[!is.na(sample36$K3Q1S1964), ]
sample37<-herbgen[herbgen$SampleN == "37",]
sample37 <- sample37[!is.na(sample37$K3Q1S1964), ]
sample38<-herbgen[herbgen$SampleN == "38",]
sample38 <- sample38[!is.na(sample38$K3Q1S1964), ]
sample39<-herbgen[herbgen$SampleN == "39",]
sample39 <- sample39[!is.na(sample39$K3Q1S1964), ]
sample40<-herbgen[herbgen$SampleN == "40",]
sample40 <- sample40[!is.na(sample40$K3Q1S1964), ]
sample41<-herbgen[herbgen$SampleN == "41",]
sample41 <- sample41[!is.na(sample41$K3Q1S1964), ]
sample42<-herbgen[herbgen$SampleN == "42",]
sample42 <- sample42[!is.na(sample42$K3Q1S1964), ]
sample43<-herbgen[herbgen$SampleN == "43",]
sample43 <- sample43[!is.na(sample43$K3Q1S1964), ]
sample44<-herbgen[herbgen$SampleN == "44",]
sample44 <- sample44[!is.na(sample44$K3Q1S1964), ]
sample45<-herbgen[herbgen$SampleN == "45",]
sample45 <- sample45[!is.na(sample45$K3Q1S1964), ]
sample46<-herbgen[herbgen$SampleN == "46",]
sample46 <- sample46[!is.na(sample46$K3Q1S1964), ]
sample47<-herbgen[herbgen$SampleN == "47",]
sample47 <- sample47[!is.na(sample47$K3Q1S1964), ]
sample48<-herbgen[herbgen$SampleN == "48",]
sample48 <- sample48[!is.na(sample48$K3Q1S1964), ]
sample49<-herbgen[herbgen$SampleN == "49",]
sample49 <- sample49[!is.na(sample49$K3Q1S1964), ]
sample50<-herbgen[herbgen$SampleN == "50",]
sample50 <- sample50[!is.na(sample50$K3Q1S1964), ]
sample51<-herbgen[herbgen$SampleN == "51",]
sample51 <- sample51[!is.na(sample51$K3Q1S1964), ]
sample52<-herbgen[herbgen$SampleN == "52",]
sample52 <- sample52[!is.na(sample52$K3Q1S1964), ]
sample53<-herbgen[herbgen$SampleN == "53",]
sample53 <- sample53[!is.na(sample53$K3Q1S1964), ]
sample54<-herbgen[herbgen$SampleN == "54",]
sample54 <- sample54[!is.na(sample54$K3Q1S1964), ]
sample55<-herbgen[herbgen$SampleN == "55",]
sample55 <- sample55[!is.na(sample55$K3Q1S1964), ]
sample56<-herbgen[herbgen$SampleN == "56",]
sample56 <- sample56[!is.na(sample56$K3Q1S1964), ]
sample57<-herbgen[herbgen$SampleN == "57",]
sample57 <- sample57[!is.na(sample57$K3Q1S1964), ]
sample58<-herbgen[herbgen$SampleN == "58",]
sample58 <- sample58[!is.na(sample58$K3Q1S1964), ]
sample59<-herbgen[herbgen$SampleN == "59",]
sample59 <- sample59[!is.na(sample59$K3Q1S1964), ]
sample60<-herbgen[herbgen$SampleN == "60",]
sample60 <- sample60[!is.na(sample60$K3Q1S1964), ]
sample61<-herbgen[herbgen$SampleN == "61",]
sample61 <- sample61[!is.na(sample61$K3Q1S1964), ]
sample62<-herbgen[herbgen$SampleN == "62",]
sample62 <- sample62[!is.na(sample62$K3Q1S1964), ]
sample63<-herbgen[herbgen$SampleN == "63",]
sample63 <- sample63[!is.na(sample63$K3Q1S1964), ]
sample64<-herbgen[herbgen$SampleN == "64",]
sample64 <- sample64[!is.na(sample64$K3Q1S1964), ]
sample65<-herbgen[herbgen$SampleN == "65",]
sample65 <- sample65[!is.na(sample65$K3Q1S1964), ]
sample66<-herbgen[herbgen$SampleN == "66",]
sample66 <- sample66[!is.na(sample66$K3Q1S1964), ]
sample67<-herbgen[herbgen$SampleN == "67",]
sample67 <- sample67[!is.na(sample67$K3Q1S1964), ]
sample68<-herbgen[herbgen$SampleN == "68",]
sample68 <- sample68[!is.na(sample68$K3Q1S1964), ]
sample69<-herbgen[herbgen$SampleN == "69",]
sample69 <- sample69[!is.na(sample69$K3Q1S1964), ]
sample70<-herbgen[herbgen$SampleN == "70",]
sample70 <- sample70[!is.na(sample70$K3Q1S1964), ]
sample71<-herbgen[herbgen$SampleN == "71",]
sample71 <- sample71[!is.na(sample71$K3Q1S1964), ]
sample72<-herbgen[herbgen$SampleN == "72",]
sample72 <- sample72[!is.na(sample72$K3Q1S1964), ]
sample73<-herbgen[herbgen$SampleN == "73",]
sample73 <- sample73[!is.na(sample73$K3Q1S1964), ]
sample74<-herbgen[herbgen$SampleN == "74",]
sample74 <- sample74[!is.na(sample74$K3Q1S1964), ]
sample75<-herbgen[herbgen$SampleN == "75",]
sample75 <- sample75[!is.na(sample75$K3Q1S1964), ]
sample76<-herbgen[herbgen$SampleN == "76",]
sample76 <- sample76[!is.na(sample76$K3Q1S1964), ]
sample77<-herbgen[herbgen$SampleN == "77",]
sample77 <- sample77[!is.na(sample77$K3Q1S1964), ]
sample78<-herbgen[herbgen$SampleN == "78",]
sample78 <- sample78[!is.na(sample78$K3Q1S1964), ]
sample79<-herbgen[herbgen$SampleN == "79",]
sample79 <- sample79[!is.na(sample79$K3Q1S1964), ]
sample80<-herbgen[herbgen$SampleN == "80",]
sample80 <- sample80[!is.na(sample80$K3Q1S1964), ]
sample81<-herbgen[herbgen$SampleN == "81",]
sample81 <- sample81[!is.na(sample81$K3Q1S1964), ]
sample82<-herbgen[herbgen$SampleN == "82",]
sample82 <- sample82[!is.na(sample82$K3Q1S1964), ]
sample83<-herbgen[herbgen$SampleN == "83",]
sample83 <- sample83[!is.na(sample83$K3Q1S1964), ]
sample84<-herbgen[herbgen$SampleN == "84",]
sample84 <- sample84[!is.na(sample84$K3Q1S1964), ]
sample85<-herbgen[herbgen$SampleN == "85",]
sample85 <- sample85[!is.na(sample85$K3Q1S1964), ]
sample86<-herbgen[herbgen$SampleN == "86",]
sample86 <- sample86[!is.na(sample86$K3Q1S1964), ]
sample87<-herbgen[herbgen$SampleN == "87",]
sample87 <- sample87[!is.na(sample87$K3Q1S1964), ]
sample88<-herbgen[herbgen$SampleN == "88",]
sample88 <- sample88[!is.na(sample88$K3Q1S1964), ]
sample89<-herbgen[herbgen$SampleN == "89",]
sample89 <- sample89[!is.na(sample89$K3Q1S1964), ]
sample90<-herbgen[herbgen$SampleN == "90",]
sample90 <- sample90[!is.na(sample90$K3Q1S1964), ]
sample91<-herbgen[herbgen$SampleN == "91",]
sample91 <- sample91[!is.na(sample91$K3Q1S1964), ]
sample92<-herbgen[herbgen$SampleN == "92",]
sample92 <- sample92[!is.na(sample92$K3Q1S1964), ]
sample93<-herbgen[herbgen$SampleN == "93",]
sample93 <- sample93[!is.na(sample93$K3Q1S1964), ]
sample94<-herbgen[herbgen$SampleN == "94",]
sample94 <- sample94[!is.na(sample94$K3Q1S1964), ]
sample95<-herbgen[herbgen$SampleN == "95",]
sample95 <- sample95[!is.na(sample95$K3Q1S1964), ]
sample96<-herbgen[herbgen$SampleN == "96",]
sample96 <- sample96[!is.na(sample96$K3Q1S1964), ]
sample97<-herbgen[herbgen$SampleN == "97",]
sample97 <- sample97[!is.na(sample97$K3Q1S1964), ]
sample98<-herbgen[herbgen$SampleN == "98",]
sample98 <- sample98[!is.na(sample98$K3Q1S1964), ]
sample99<-herbgen[herbgen$SampleN == "99",]
sample99 <- sample99[!is.na(sample99$K3Q1S1964), ]
sample100<-herbgen[herbgen$SampleN == "100",]
sample100 <- sample100[!is.na(sample100$K3Q1S1964), ]
sample101<-herbgen[herbgen$SampleN == "101",]
sample101 <- sample101[!is.na(sample101$K3Q1S1964), ]
sample102<-herbgen[herbgen$SampleN == "102",]
sample102 <- sample102[!is.na(sample102$K3Q1S1964), ]
sample103<-herbgen[herbgen$SampleN == "103",]
sample103 <- sample103[!is.na(sample103$K3Q1S1964), ]
sample104<-herbgen[herbgen$SampleN == "104",]
sample104 <- sample104[!is.na(sample104$K3Q1S1964), ]
sample105<-herbgen[herbgen$SampleN == "105",]
sample105 <- sample105[!is.na(sample105$K3Q1S1964), ]
sample106<-herbgen[herbgen$SampleN == "106",]
sample106 <- sample106[!is.na(sample106$K3Q1S1964), ]
sample107<-herbgen[herbgen$SampleN == "107",]
sample107 <- sample107[!is.na(sample107$K3Q1S1964), ]
sample108<-herbgen[herbgen$SampleN == "108",]
sample108 <- sample108[!is.na(sample108$K3Q1S1964), ]
sample109<-herbgen[herbgen$SampleN == "109",]
sample109 <- sample109[!is.na(sample109$K3Q1S1964), ]
sample110<-herbgen[herbgen$SampleN == "110",]
sample110 <- sample110[!is.na(sample110$K3Q1S1964), ]
sample111<-herbgen[herbgen$SampleN == "111",]
sample111 <- sample111[!is.na(sample111$K3Q1S1964), ]
sample112<-herbgen[herbgen$SampleN == "112",]
sample112 <- sample112[!is.na(sample112$K3Q1S1964), ]
sample113<-herbgen[herbgen$SampleN == "113",]
sample113 <- sample113[!is.na(sample113$K3Q1S1964), ]
sample114<-herbgen[herbgen$SampleN == "114",]
sample114 <- sample114[!is.na(sample114$K3Q1S1964), ]
sample115<-herbgen[herbgen$SampleN == "115",]
sample115 <- sample115[!is.na(sample115$K3Q1S1964), ]
sample116<-herbgen[herbgen$SampleN == "116",]
sample116 <- sample116[!is.na(sample116$K3Q1S1964), ]
sample117<-herbgen[herbgen$SampleN == "117",]
sample117 <- sample117[!is.na(sample117$K3Q1S1964), ]
sample118<-herbgen[herbgen$SampleN == "118",]
sample118 <- sample118[!is.na(sample118$K3Q1S1964), ]
sample119<-herbgen[herbgen$SampleN == "119",]
sample119 <- sample119[!is.na(sample119$K3Q1S1964), ]
sample120<-herbgen[herbgen$SampleN == "120",]
sample120 <- sample120[!is.na(sample120$K3Q1S1964), ]
sample121<-herbgen[herbgen$SampleN == "121",]
sample121 <- sample121[!is.na(sample121$K3Q1S1964), ]
sample122<-herbgen[herbgen$SampleN == "122",]
sample122 <- sample122[!is.na(sample122$K3Q1S1964), ]
sample123<-herbgen[herbgen$SampleN == "123",]
sample123 <- sample123[!is.na(sample123$K3Q1S1964), ]
sample124<-herbgen[herbgen$SampleN == "124",]
sample124 <- sample124[!is.na(sample124$K3Q1S1964), ]
sample125<-herbgen[herbgen$SampleN == "125",]
sample125 <- sample125[!is.na(sample125$K3Q1S1964), ]
sample126<-herbgen[herbgen$SampleN == "126",]
sample126 <- sample126[!is.na(sample126$K3Q1S1964), ]
sample127<-herbgen[herbgen$SampleN == "127",]
sample127 <- sample127[!is.na(sample127$K3Q1S1964), ]
sample128<-herbgen[herbgen$SampleN == "128",]
sample128 <- sample128[!is.na(sample128$K3Q1S1964), ]
sample129<-herbgen[herbgen$SampleN == "129",]
sample129 <- sample129[!is.na(sample129$K3Q1S1964), ]
sample130<-herbgen[herbgen$SampleN == "130",]
sample130 <- sample130[!is.na(sample130$K3Q1S1964), ]
sample131<-herbgen[herbgen$SampleN == "131",]
sample131 <- sample131[!is.na(sample131$K3Q1S1964), ]
sample132<-herbgen[herbgen$SampleN == "132",]
sample132 <- sample132[!is.na(sample132$K3Q1S1964), ]
sample133<-herbgen[herbgen$SampleN == "133",]
sample133 <- sample133[!is.na(sample133$K3Q1S1964), ]
sample134<-herbgen[herbgen$SampleN == "134",]
sample134 <- sample134[!is.na(sample134$K3Q1S1964), ]
sample135<-herbgen[herbgen$SampleN == "135",]
sample135 <- sample135[!is.na(sample135$K3Q1S1964), ]
sample136<-herbgen[herbgen$SampleN == "136",]
sample136 <- sample136[!is.na(sample136$K3Q1S1964), ]
sample137<-herbgen[herbgen$SampleN == "137",]
sample137 <- sample137[!is.na(sample137$K3Q1S1964), ]
sample138<-herbgen[herbgen$SampleN == "138",]
sample138 <- sample138[!is.na(sample138$K3Q1S1964), ]
sample139<-herbgen[herbgen$SampleN == "139",]
sample139 <- sample139[!is.na(sample139$K3Q1S1964), ]
sample140<-herbgen[herbgen$SampleN == "140",]
sample140 <- sample140[!is.na(sample140$K3Q1S1964), ]
sample141<-herbgen[herbgen$SampleN == "141",]
sample141 <- sample141[!is.na(sample141$K3Q1S1964), ]
sample142<-herbgen[herbgen$SampleN == "142",]
sample142 <- sample142[!is.na(sample142$K3Q1S1964), ]
sample143<-herbgen[herbgen$SampleN == "143",]
sample143 <- sample143[!is.na(sample143$K3Q1S1964), ]
sample144<-herbgen[herbgen$SampleN == "144",]
sample144 <- sample144[!is.na(sample144$K3Q1S1964), ]
sample145<-herbgen[herbgen$SampleN == "145",]
sample145 <- sample145[!is.na(sample145$K3Q1S1964), ]
sample146<-herbgen[herbgen$SampleN == "146",]
sample146 <- sample146[!is.na(sample146$K3Q1S1964), ]
sample147<-herbgen[herbgen$SampleN == "147",]
sample147 <- sample147[!is.na(sample147$K3Q1S1964), ]
sample148<-herbgen[herbgen$SampleN == "148",]
sample148 <- sample148[!is.na(sample148$K3Q1S1964), ]
sample149<-herbgen[herbgen$SampleN == "149",]
sample149 <- sample149[!is.na(sample149$K3Q1S1964), ]
sample150<-herbgen[herbgen$SampleN == "150",]
sample150 <- sample150[!is.na(sample150$K3Q1S1964), ]
sample151<-herbgen[herbgen$SampleN == "151",]
sample151 <- sample151[!is.na(sample151$K3Q1S1964), ]
sample152<-herbgen[herbgen$SampleN == "152",]
sample152 <- sample152[!is.na(sample152$K3Q1S1964), ]
sample153<-herbgen[herbgen$SampleN == "153",]
sample153 <- sample153[!is.na(sample153$K3Q1S1964), ]
sample154<-herbgen[herbgen$SampleN == "154",]
sample154 <- sample154[!is.na(sample154$K3Q1S1964), ]
sample155<-herbgen[herbgen$SampleN == "155",]
sample155 <- sample155[!is.na(sample155$K3Q1S1964), ]
sample156<-herbgen[herbgen$SampleN == "156",]
sample156 <- sample156[!is.na(sample156$K3Q1S1964), ]
sample157<-herbgen[herbgen$SampleN == "157",]
sample157 <- sample157[!is.na(sample157$K3Q1S1964), ]
sample158<-herbgen[herbgen$SampleN == "158",]
sample158 <- sample158[!is.na(sample158$K3Q1S1964), ]
sample159<-herbgen[herbgen$SampleN == "159",]
sample159 <- sample159[!is.na(sample159$K3Q1S1964), ]
sample160<-herbgen[herbgen$SampleN == "160",]
sample160 <- sample160[!is.na(sample160$K3Q1S1964), ]
sample161<-herbgen[herbgen$SampleN == "161",]
sample161 <- sample161[!is.na(sample161$K3Q1S1964), ]
sample162<-herbgen[herbgen$SampleN == "162",]
sample162 <- sample162[!is.na(sample162$K3Q1S1964), ]
sample163<-herbgen[herbgen$SampleN == "163",]
sample163 <- sample163[!is.na(sample163$K3Q1S1964), ]
sample164<-herbgen[herbgen$SampleN == "164",]
sample164 <- sample164[!is.na(sample164$K3Q1S1964), ]
sample165<-herbgen[herbgen$SampleN == "165",]
sample165 <- sample165[!is.na(sample165$K3Q1S1964), ]
sample166<-herbgen[herbgen$SampleN == "166",]
sample166 <- sample166[!is.na(sample166$K3Q1S1964), ]
sample167<-herbgen[herbgen$SampleN == "167",]
sample167 <- sample167[!is.na(sample167$K3Q1S1964), ]
sample168<-herbgen[herbgen$SampleN == "168",]
sample168 <- sample168[!is.na(sample168$K3Q1S1964), ]
sample169<-herbgen[herbgen$SampleN == "169",]
sample169 <- sample169[!is.na(sample169$K3Q1S1964), ]
sample170<-herbgen[herbgen$SampleN == "170",]
sample170 <- sample170[!is.na(sample170$K3Q1S1964), ]
sample171<-herbgen[herbgen$SampleN == "171",]
sample171 <- sample171[!is.na(sample171$K3Q1S1964), ]
sample172<-herbgen[herbgen$SampleN == "172",]
sample172 <- sample172[!is.na(sample172$K3Q1S1964), ]
sample173<-herbgen[herbgen$SampleN == "173",]
sample173 <- sample173[!is.na(sample173$K3Q1S1964), ]
sample174<-herbgen[herbgen$SampleN == "174",]
sample174 <- sample174[!is.na(sample174$K3Q1S1964), ]
sample175<-herbgen[herbgen$SampleN == "175",]
sample175 <- sample175[!is.na(sample175$K3Q1S1964), ]
sample176<-herbgen[herbgen$SampleN == "176",]
sample176 <- sample176[!is.na(sample176$K3Q1S1964), ]
sample177<-herbgen[herbgen$SampleN == "177",]
sample177 <- sample177[!is.na(sample177$K3Q1S1964), ]
sample178<-herbgen[herbgen$SampleN == "178",]
sample178 <- sample178[!is.na(sample178$K3Q1S1964), ]
sample179<-herbgen[herbgen$SampleN == "179",]
sample179 <- sample179[!is.na(sample179$K3Q1S1964), ]
sample180<-herbgen[herbgen$SampleN == "180",]
sample180 <- sample180[!is.na(sample180$K3Q1S1964), ]
sample181<-herbgen[herbgen$SampleN == "181",]
sample181 <- sample181[!is.na(sample181$K3Q1S1964), ]
sample182<-herbgen[herbgen$SampleN == "182",]
sample182 <- sample182[!is.na(sample182$K3Q1S1964), ]
sample183<-herbgen[herbgen$SampleN == "183",]
sample183 <- sample183[!is.na(sample183$K3Q1S1964), ]
sample184<-herbgen[herbgen$SampleN == "184",]
sample184 <- sample184[!is.na(sample184$K3Q1S1964), ]
sample185<-herbgen[herbgen$SampleN == "185",]
sample185 <- sample185[!is.na(sample185$K3Q1S1964), ]
sample186<-herbgen[herbgen$SampleN == "186",]
sample186 <- sample186[!is.na(sample186$K3Q1S1964), ]
sample187<-herbgen[herbgen$SampleN == "187",]
sample187 <- sample187[!is.na(sample187$K3Q1S1964), ]
sample188<-herbgen[herbgen$SampleN == "188",]
sample188 <- sample188[!is.na(sample188$K3Q1S1964), ]
sample189<-herbgen[herbgen$SampleN == "189",]
sample189 <- sample189[!is.na(sample189$K3Q1S1964), ]
sample190<-herbgen[herbgen$SampleN == "190",]
sample190 <- sample190[!is.na(sample190$K3Q1S1964), ]
sample191<-herbgen[herbgen$SampleN == "191",]
sample191 <- sample191[!is.na(sample191$K3Q1S1964), ]
sample192<-herbgen[herbgen$SampleN == "192",]
sample192 <- sample192[!is.na(sample192$K3Q1S1964), ]
sample193<-herbgen[herbgen$SampleN == "193",]
sample193 <- sample193[!is.na(sample193$K3Q1S1964), ]
sample194<-herbgen[herbgen$SampleN == "194",]
sample194 <- sample194[!is.na(sample194$K3Q1S1964), ]
sample195<-herbgen[herbgen$SampleN == "195",]
sample195 <- sample195[!is.na(sample195$K3Q1S1964), ]
sample196<-herbgen[herbgen$SampleN == "196",]
sample196 <- sample196[!is.na(sample196$K3Q1S1964), ]
sample197<-herbgen[herbgen$SampleN == "197",]
sample197 <- sample197[!is.na(sample197$K3Q1S1964), ]
sample198<-herbgen[herbgen$SampleN == "198",]
sample198 <- sample198[!is.na(sample198$K3Q1S1964), ]
sample199<-herbgen[herbgen$SampleN == "199",]
sample199 <- sample199[!is.na(sample199$K3Q1S1964), ]
sample200<-herbgen[herbgen$SampleN == "200",]
sample200 <- sample200[!is.na(sample200$K3Q1S1964), ]
sample201<-herbgen[herbgen$SampleN == "201",]
sample201 <- sample201[!is.na(sample201$K3Q1S1964), ]
sample202<-herbgen[herbgen$SampleN == "202",]
sample202 <- sample202[!is.na(sample202$K3Q1S1964), ]
sample203<-herbgen[herbgen$SampleN == "203",]
sample203 <- sample203[!is.na(sample203$K3Q1S1964), ]
sample204<-herbgen[herbgen$SampleN == "204",]
sample204 <- sample204[!is.na(sample204$K3Q1S1964), ]
sample205<-herbgen[herbgen$SampleN == "205",]
sample205 <- sample205[!is.na(sample205$K3Q1S1964), ]
sample206<-herbgen[herbgen$SampleN == "206",]
sample206 <- sample206[!is.na(sample206$K3Q1S1964), ]
sample207<-herbgen[herbgen$SampleN == "207",]
sample207 <- sample207[!is.na(sample207$K3Q1S1964), ]
sample208<-herbgen[herbgen$SampleN == "208",]
sample208 <- sample208[!is.na(sample208$K3Q1S1964), ]
sample209<-herbgen[herbgen$SampleN == "209",]
sample209 <- sample209[!is.na(sample209$K3Q1S1964), ]
sample210<-herbgen[herbgen$SampleN == "210",]
sample210 <- sample210[!is.na(sample210$K3Q1S1964), ]
sample211<-herbgen[herbgen$SampleN == "211",]
sample211 <- sample211[!is.na(sample211$K3Q1S1964), ]
sample212<-herbgen[herbgen$SampleN == "212",]
sample212 <- sample212[!is.na(sample212$K3Q1S1964), ]
sample213<-herbgen[herbgen$SampleN == "213",]
sample213 <- sample213[!is.na(sample213$K3Q1S1964), ]
sample214<-herbgen[herbgen$SampleN == "214",]
sample214 <- sample214[!is.na(sample214$K3Q1S1964), ]
sample215<-herbgen[herbgen$SampleN == "215",]
sample215 <- sample215[!is.na(sample215$K3Q1S1964), ]
sample216<-herbgen[herbgen$SampleN == "216",]
sample216 <- sample216[!is.na(sample216$K3Q1S1964), ]
sample217<-herbgen[herbgen$SampleN == "217",]
sample217 <- sample217[!is.na(sample217$K3Q1S1964), ]
sample218<-herbgen[herbgen$SampleN == "218",]
sample218 <- sample218[!is.na(sample218$K3Q1S1964), ]
sample219<-herbgen[herbgen$SampleN == "219",]
sample219 <- sample219[!is.na(sample219$K3Q1S1964), ]
sample220<-herbgen[herbgen$SampleN == "220",]
sample220 <- sample220[!is.na(sample220$K3Q1S1964), ]
sample221<-herbgen[herbgen$SampleN == "221",]
sample221 <- sample221[!is.na(sample221$K3Q1S1964), ]
sample222<-herbgen[herbgen$SampleN == "222",]
sample222 <- sample222[!is.na(sample222$K3Q1S1964), ]
sample223<-herbgen[herbgen$SampleN == "223",]
sample223 <- sample223[!is.na(sample223$K3Q1S1964), ]
sample224<-herbgen[herbgen$SampleN == "224",]
sample224 <- sample224[!is.na(sample224$K3Q1S1964), ]
sample225<-herbgen[herbgen$SampleN == "225",]
sample225 <- sample225[!is.na(sample225$K3Q1S1964), ]
sample226<-herbgen[herbgen$SampleN == "226",]
sample226 <- sample226[!is.na(sample226$K3Q1S1964), ]
sample227<-herbgen[herbgen$SampleN == "227",]
sample227 <- sample227[!is.na(sample227$K3Q1S1964), ]
sample228<-herbgen[herbgen$SampleN == "228",]
sample228 <- sample228[!is.na(sample228$K3Q1S1964), ]
sample229<-herbgen[herbgen$SampleN == "229",]
sample229 <- sample229[!is.na(sample229$K3Q1S1964), ]
sample230<-herbgen[herbgen$SampleN == "230",]
sample230 <- sample230[!is.na(sample230$K3Q1S1964), ]
sample231<-herbgen[herbgen$SampleN == "231",]
sample231 <- sample231[!is.na(sample231$K3Q1S1964), ]
sample232<-herbgen[herbgen$SampleN == "232",]
sample232 <- sample232[!is.na(sample232$K3Q1S1964), ]
sample233<-herbgen[herbgen$SampleN == "233",]
sample233 <- sample233[!is.na(sample233$K3Q1S1964), ]
sample234<-herbgen[herbgen$SampleN == "234",]
sample234 <- sample234[!is.na(sample234$K3Q1S1964), ]
sample235<-herbgen[herbgen$SampleN == "235",]
sample235 <- sample235[!is.na(sample235$K3Q1S1964), ]
sample236<-herbgen[herbgen$SampleN == "236",]
sample236 <- sample236[!is.na(sample236$K3Q1S1964), ]
sample237<-herbgen[herbgen$SampleN == "237",]
sample237 <- sample237[!is.na(sample237$K3Q1S1964), ]
sample238<-herbgen[herbgen$SampleN == "238",]
sample238 <- sample238[!is.na(sample238$K3Q1S1964), ]
sample239<-herbgen[herbgen$SampleN == "239",]
sample239 <- sample239[!is.na(sample239$K3Q1S1964), ]
sample240<-herbgen[herbgen$SampleN == "240",]
sample240 <- sample240[!is.na(sample240$K3Q1S1964), ]
sample241<-herbgen[herbgen$SampleN == "241",]
sample241 <- sample241[!is.na(sample241$K3Q1S1964), ]
sample242<-herbgen[herbgen$SampleN == "242",]
sample242 <- sample242[!is.na(sample242$K3Q1S1964), ]
sample243<-herbgen[herbgen$SampleN == "243",]
sample243 <- sample243[!is.na(sample243$K3Q1S1964), ]
sample244<-herbgen[herbgen$SampleN == "244",]
sample244 <- sample244[!is.na(sample244$K3Q1S1964), ]
sample245<-herbgen[herbgen$SampleN == "245",]
sample245 <- sample245[!is.na(sample245$K3Q1S1964), ]
sample246<-herbgen[herbgen$SampleN == "246",]
sample246 <- sample246[!is.na(sample246$K3Q1S1964), ]
sample247<-herbgen[herbgen$SampleN == "247",]
sample247 <- sample247[!is.na(sample247$K3Q1S1964), ]
sample248<-herbgen[herbgen$SampleN == "248",]
sample248 <- sample248[!is.na(sample248$K3Q1S1964), ]
sample249<-herbgen[herbgen$SampleN == "249",]
sample249 <- sample249[!is.na(sample249$K3Q1S1964), ]
sample250<-herbgen[herbgen$SampleN == "250",]
sample250 <- sample250[!is.na(sample250$K3Q1S1964), ]
sample251<-herbgen[herbgen$SampleN == "251",]
sample251 <- sample251[!is.na(sample251$K3Q1S1964), ]
sample252<-herbgen[herbgen$SampleN == "252",]
sample252 <- sample252[!is.na(sample252$K3Q1S1964), ]
sample253<-herbgen[herbgen$SampleN == "253",]
sample253 <- sample253[!is.na(sample253$K3Q1S1964), ]
sample254<-herbgen[herbgen$SampleN == "254",]
sample254 <- sample254[!is.na(sample254$K3Q1S1964), ]
sample255<-herbgen[herbgen$SampleN == "255",]
sample255 <- sample255[!is.na(sample255$K3Q1S1964), ]
sample256<-herbgen[herbgen$SampleN == "256",]
sample256 <- sample256[!is.na(sample256$K3Q1S1964), ]
sample257<-herbgen[herbgen$SampleN == "257",]
sample257 <- sample257[!is.na(sample257$K3Q1S1964), ]
sample258<-herbgen[herbgen$SampleN == "258",]
sample258 <- sample258[!is.na(sample258$K3Q1S1964), ]
sample259<-herbgen[herbgen$SampleN == "259",]
sample259 <- sample259[!is.na(sample259$K3Q1S1964), ]
sample260<-herbgen[herbgen$SampleN == "260",]
sample260 <- sample260[!is.na(sample260$K3Q1S1964), ]
sample261<-herbgen[herbgen$SampleN == "261",]
sample261 <- sample261[!is.na(sample261$K3Q1S1964), ]
sample262<-herbgen[herbgen$SampleN == "262",]
sample262 <- sample262[!is.na(sample262$K3Q1S1964), ]
sample263<-herbgen[herbgen$SampleN == "263",]
sample263 <- sample263[!is.na(sample263$K3Q1S1964), ]
sample264<-herbgen[herbgen$SampleN == "264",]
sample264 <- sample264[!is.na(sample264$K3Q1S1964), ]
sample265<-herbgen[herbgen$SampleN == "265",]
sample265 <- sample265[!is.na(sample265$K3Q1S1964), ]
sample266<-herbgen[herbgen$SampleN == "266",]
sample266 <- sample266[!is.na(sample266$K3Q1S1964), ]
sample267<-herbgen[herbgen$SampleN == "267",]
sample267 <- sample267[!is.na(sample267$K3Q1S1964), ]
sample268<-herbgen[herbgen$SampleN == "268",]
sample268 <- sample268[!is.na(sample268$K3Q1S1964), ]
sample269<-herbgen[herbgen$SampleN == "269",]
sample269 <- sample269[!is.na(sample269$K3Q1S1964), ]
sample270<-herbgen[herbgen$SampleN == "270",]
sample270 <- sample270[!is.na(sample270$K3Q1S1964), ]
sample271<-herbgen[herbgen$SampleN == "271",]
sample271 <- sample271[!is.na(sample271$K3Q1S1964), ]
sample272<-herbgen[herbgen$SampleN == "272",]
sample272 <- sample272[!is.na(sample272$K3Q1S1964), ]
sample273<-herbgen[herbgen$SampleN == "273",]
sample273 <- sample273[!is.na(sample273$K3Q1S1964), ]
sample274<-herbgen[herbgen$SampleN == "274",]
sample274 <- sample274[!is.na(sample274$K3Q1S1964), ]
sample275<-herbgen[herbgen$SampleN == "275",]
sample275 <- sample275[!is.na(sample275$K3Q1S1964), ]
sample276<-herbgen[herbgen$SampleN == "276",]
sample276 <- sample276[!is.na(sample276$K3Q1S1964), ]
sample277<-herbgen[herbgen$SampleN == "277",]
sample277 <- sample277[!is.na(sample277$K3Q1S1964), ]
sample278<-herbgen[herbgen$SampleN == "278",]
sample278 <- sample278[!is.na(sample278$K3Q1S1964), ]
sample279<-herbgen[herbgen$SampleN == "279",]
sample279 <- sample279[!is.na(sample279$K3Q1S1964), ]
sample280<-herbgen[herbgen$SampleN == "280",]
sample280 <- sample280[!is.na(sample280$K3Q1S1964), ]
sample281<-herbgen[herbgen$SampleN == "281",]
sample281 <- sample281[!is.na(sample281$K3Q1S1964), ]
sample282<-herbgen[herbgen$SampleN == "282",]
sample282 <- sample282[!is.na(sample282$K3Q1S1964), ]
sample283<-herbgen[herbgen$SampleN == "283",]
sample283 <- sample283[!is.na(sample283$K3Q1S1964), ]
sample284<-herbgen[herbgen$SampleN == "284",]
sample284 <- sample284[!is.na(sample284$K3Q1S1964), ]
sample285<-herbgen[herbgen$SampleN == "285",]
sample285 <- sample285[!is.na(sample285$K3Q1S1964), ]
sample286<-herbgen[herbgen$SampleN == "286",]
sample286 <- sample286[!is.na(sample286$K3Q1S1964), ]
sample287<-herbgen[herbgen$SampleN == "287",]
sample287 <- sample287[!is.na(sample287$K3Q1S1964), ]
sample288<-herbgen[herbgen$SampleN == "288",]
sample288 <- sample288[!is.na(sample288$K3Q1S1964), ]
sample289<-herbgen[herbgen$SampleN == "289",]
sample289 <- sample289[!is.na(sample289$K3Q1S1964), ]
sample290<-herbgen[herbgen$SampleN == "290",]
sample290 <- sample290[!is.na(sample290$K3Q1S1964), ]
sample291<-herbgen[herbgen$SampleN == "291",]
sample291 <- sample291[!is.na(sample291$K3Q1S1964), ]
sample292<-herbgen[herbgen$SampleN == "292",]
sample292 <- sample292[!is.na(sample292$K3Q1S1964), ]
sample293<-herbgen[herbgen$SampleN == "293",]
sample293 <- sample293[!is.na(sample293$K3Q1S1964), ]
sample294<-herbgen[herbgen$SampleN == "294",]
sample294 <- sample294[!is.na(sample294$K3Q1S1964), ]
sample295<-herbgen[herbgen$SampleN == "295",]
sample295 <- sample295[!is.na(sample295$K3Q1S1964), ]
sample296<-herbgen[herbgen$SampleN == "296",]
sample296 <- sample296[!is.na(sample296$K3Q1S1964), ]
sample297<-herbgen[herbgen$SampleN == "297",]
sample297 <- sample297[!is.na(sample297$K3Q1S1964), ]
sample298<-herbgen[herbgen$SampleN == "298",]
sample298 <- sample298[!is.na(sample298$K3Q1S1964), ]
sample299<-herbgen[herbgen$SampleN == "299",]
sample299 <- sample299[!is.na(sample299$K3Q1S1964), ]
sample300<-herbgen[herbgen$SampleN == "300",]
sample300 <- sample300[!is.na(sample300$K3Q1S1964), ]
sample301<-herbgen[herbgen$SampleN == "301",]
sample301 <- sample301[!is.na(sample301$K3Q1S1964), ]
sample302<-herbgen[herbgen$SampleN == "302",]
sample302 <- sample302[!is.na(sample302$K3Q1S1964), ]
sample303<-herbgen[herbgen$SampleN == "303",]
sample303 <- sample303[!is.na(sample303$K3Q1S1964), ]
sample304<-herbgen[herbgen$SampleN == "304",]
sample304 <- sample304[!is.na(sample304$K3Q1S1964), ]
sample305<-herbgen[herbgen$SampleN == "305",]
sample305 <- sample305[!is.na(sample305$K3Q1S1964), ]
sample306<-herbgen[herbgen$SampleN == "306",]
sample306 <- sample306[!is.na(sample306$K3Q1S1964), ]
sample307<-herbgen[herbgen$SampleN == "307",]
sample307 <- sample307[!is.na(sample307$K3Q1S1964), ]
sample308<-herbgen[herbgen$SampleN == "308",]
sample308 <- sample308[!is.na(sample308$K3Q1S1964), ]
sample309<-herbgen[herbgen$SampleN == "309",]
sample309 <- sample309[!is.na(sample309$K3Q1S1964), ]
sample310<-herbgen[herbgen$SampleN == "310",]
sample310 <- sample310[!is.na(sample310$K3Q1S1964), ]
sample311<-herbgen[herbgen$SampleN == "311",]
sample311 <- sample311[!is.na(sample311$K3Q1S1964), ]
sample312<-herbgen[herbgen$SampleN == "312",]
sample312 <- sample312[!is.na(sample312$K3Q1S1964), ]
sample313<-herbgen[herbgen$SampleN == "313",]
sample313 <- sample313[!is.na(sample313$K3Q1S1964), ]
sample314<-herbgen[herbgen$SampleN == "314",]
sample314 <- sample314[!is.na(sample314$K3Q1S1964), ]
sample315<-herbgen[herbgen$SampleN == "315",]
sample315 <- sample315[!is.na(sample315$K3Q1S1964), ]
sample316<-herbgen[herbgen$SampleN == "316",]
sample316 <- sample316[!is.na(sample316$K3Q1S1964), ]
sample317<-herbgen[herbgen$SampleN == "317",]
sample317 <- sample317[!is.na(sample317$K3Q1S1964), ]
sample318<-herbgen[herbgen$SampleN == "318",]
sample318 <- sample318[!is.na(sample318$K3Q1S1964), ]
sample319<-herbgen[herbgen$SampleN == "319",]
sample319 <- sample319[!is.na(sample319$K3Q1S1964), ]
sample320<-herbgen[herbgen$SampleN == "320",]
sample320 <- sample320[!is.na(sample320$K3Q1S1964), ]
sample321<-herbgen[herbgen$SampleN == "321",]
sample321 <- sample321[!is.na(sample321$K3Q1S1964), ]
sample322<-herbgen[herbgen$SampleN == "322",]
sample322 <- sample322[!is.na(sample322$K3Q1S1964), ]
sample323<-herbgen[herbgen$SampleN == "323",]
sample323 <- sample323[!is.na(sample323$K3Q1S1964), ]
sample324<-herbgen[herbgen$SampleN == "324",]
sample324 <- sample324[!is.na(sample324$K3Q1S1964), ]
sample325<-herbgen[herbgen$SampleN == "325",]
sample325 <- sample325[!is.na(sample325$K3Q1S1964), ]
sample326<-herbgen[herbgen$SampleN == "326",]
sample326 <- sample326[!is.na(sample326$K3Q1S1964), ]
sample327<-herbgen[herbgen$SampleN == "327",]
sample327 <- sample327[!is.na(sample327$K3Q1S1964), ]
sample328<-herbgen[herbgen$SampleN == "328",]
sample328 <- sample328[!is.na(sample328$K3Q1S1964), ]
sample329<-herbgen[herbgen$SampleN == "329",]
sample329 <- sample329[!is.na(sample329$K3Q1S1964), ]
sample330<-herbgen[herbgen$SampleN == "330",]
sample330 <- sample330[!is.na(sample330$K3Q1S1964), ]
sample331<-herbgen[herbgen$SampleN == "331",]
sample331 <- sample331[!is.na(sample331$K3Q1S1964), ]
sample332<-herbgen[herbgen$SampleN == "332",]
sample332 <- sample332[!is.na(sample332$K3Q1S1964), ]
sample333<-herbgen[herbgen$SampleN == "333",]
sample333 <- sample333[!is.na(sample333$K3Q1S1964), ]
sample334<-herbgen[herbgen$SampleN == "334",]
sample334 <- sample334[!is.na(sample334$K3Q1S1964), ]
sample335<-herbgen[herbgen$SampleN == "335",]
sample335 <- sample335[!is.na(sample335$K3Q1S1964), ]
sample336<-herbgen[herbgen$SampleN == "336",]
sample336 <- sample336[!is.na(sample336$K3Q1S1964), ]
sample337<-herbgen[herbgen$SampleN == "337",]
sample337 <- sample337[!is.na(sample337$K3Q1S1964), ]
sample338<-herbgen[herbgen$SampleN == "338",]
sample338 <- sample338[!is.na(sample338$K3Q1S1964), ]
sample339<-herbgen[herbgen$SampleN == "339",]
sample339 <- sample339[!is.na(sample339$K3Q1S1964), ]
sample340<-herbgen[herbgen$SampleN == "340",]
sample340 <- sample340[!is.na(sample340$K3Q1S1964), ]
sample341<-herbgen[herbgen$SampleN == "341",]
sample341 <- sample341[!is.na(sample341$K3Q1S1964), ]
sample342<-herbgen[herbgen$SampleN == "342",]
sample342 <- sample342[!is.na(sample342$K3Q1S1964), ]
sample343<-herbgen[herbgen$SampleN == "343",]
sample343 <- sample343[!is.na(sample343$K3Q1S1964), ]
sample344<-herbgen[herbgen$SampleN == "344",]
sample344 <- sample344[!is.na(sample344$K3Q1S1964), ]
sample345<-herbgen[herbgen$SampleN == "345",]
sample345 <- sample345[!is.na(sample345$K3Q1S1964), ]
sample346<-herbgen[herbgen$SampleN == "346",]
sample346 <- sample346[!is.na(sample346$K3Q1S1964), ]
sample347<-herbgen[herbgen$SampleN == "347",]
sample347 <- sample347[!is.na(sample347$K3Q1S1964), ]
sample348<-herbgen[herbgen$SampleN == "348",]
sample348 <- sample348[!is.na(sample348$K3Q1S1964), ]
sample349<-herbgen[herbgen$SampleN == "349",]
sample349 <- sample349[!is.na(sample349$K3Q1S1964), ]
sample350<-herbgen[herbgen$SampleN == "350",]
sample350 <- sample350[!is.na(sample350$K3Q1S1964), ]
sample351<-herbgen[herbgen$SampleN == "351",]
sample351 <- sample351[!is.na(sample351$K3Q1S1964), ]
sample352<-herbgen[herbgen$SampleN == "352",]
sample352 <- sample352[!is.na(sample352$K3Q1S1964), ]
sample353<-herbgen[herbgen$SampleN == "353",]
sample353 <- sample353[!is.na(sample353$K3Q1S1964), ]
sample354<-herbgen[herbgen$SampleN == "354",]
sample354 <- sample354[!is.na(sample354$K3Q1S1964), ]
sample355<-herbgen[herbgen$SampleN == "355",]
sample355 <- sample355[!is.na(sample355$K3Q1S1964), ]
sample356<-herbgen[herbgen$SampleN == "356",]
sample356 <- sample356[!is.na(sample356$K3Q1S1964), ]
sample357<-herbgen[herbgen$SampleN == "357",]
sample357 <- sample357[!is.na(sample357$K3Q1S1964), ]
sample358<-herbgen[herbgen$SampleN == "358",]
sample358 <- sample358[!is.na(sample358$K3Q1S1964), ]
sample359<-herbgen[herbgen$SampleN == "359",]
sample359 <- sample359[!is.na(sample359$K3Q1S1964), ]
sample360<-herbgen[herbgen$SampleN == "360",]
sample360 <- sample360[!is.na(sample360$K3Q1S1964), ]
sample361<-herbgen[herbgen$SampleN == "361",]
sample361 <- sample361[!is.na(sample361$K3Q1S1964), ]
sample362<-herbgen[herbgen$SampleN == "362",]
sample362 <- sample362[!is.na(sample362$K3Q1S1964), ]
sample363<-herbgen[herbgen$SampleN == "363",]
sample363 <- sample363[!is.na(sample363$K3Q1S1964), ]
sample364<-herbgen[herbgen$SampleN == "364",]
sample364 <- sample364[!is.na(sample364$K3Q1S1964), ]
sample365<-herbgen[herbgen$SampleN == "365",]
sample365 <- sample365[!is.na(sample365$K3Q1S1964), ]
sample366<-herbgen[herbgen$SampleN == "366",]
sample366 <- sample366[!is.na(sample366$K3Q1S1964), ]
sample367<-herbgen[herbgen$SampleN == "367",]
sample367 <- sample367[!is.na(sample367$K3Q1S1964), ]
sample368<-herbgen[herbgen$SampleN == "368",]
sample368 <- sample368[!is.na(sample368$K3Q1S1964), ]
sample369<-herbgen[herbgen$SampleN == "369",]
sample369 <- sample369[!is.na(sample369$K3Q1S1964), ]
sample370<-herbgen[herbgen$SampleN == "370",]
sample370 <- sample370[!is.na(sample370$K3Q1S1964), ]
sample371<-herbgen[herbgen$SampleN == "371",]
sample371 <- sample371[!is.na(sample371$K3Q1S1964), ]
sample372<-herbgen[herbgen$SampleN == "372",]
sample372 <- sample372[!is.na(sample372$K3Q1S1964), ]
sample373<-herbgen[herbgen$SampleN == "373",]
sample373 <- sample373[!is.na(sample373$K3Q1S1964), ]
sample374<-herbgen[herbgen$SampleN == "374",]
sample374 <- sample374[!is.na(sample374$K3Q1S1964), ]
sample375<-herbgen[herbgen$SampleN == "375",]
sample375 <- sample375[!is.na(sample375$K3Q1S1964), ]
sample376<-herbgen[herbgen$SampleN == "376",]
sample376 <- sample376[!is.na(sample376$K3Q1S1964), ]
sample377<-herbgen[herbgen$SampleN == "377",]
sample377 <- sample377[!is.na(sample377$K3Q1S1964), ]
sample378<-herbgen[herbgen$SampleN == "378",]
sample378 <- sample378[!is.na(sample378$K3Q1S1964), ]
sample379<-herbgen[herbgen$SampleN == "379",]
sample379 <- sample379[!is.na(sample379$K3Q1S1964), ]
sample380<-herbgen[herbgen$SampleN == "380",]
sample380 <- sample380[!is.na(sample380$K3Q1S1964), ]
sample381<-herbgen[herbgen$SampleN == "381",]
sample381 <- sample381[!is.na(sample381$K3Q1S1964), ]
sample382<-herbgen[herbgen$SampleN == "382",]
sample382 <- sample382[!is.na(sample382$K3Q1S1964), ]
sample383<-herbgen[herbgen$SampleN == "383",]
sample383 <- sample383[!is.na(sample383$K3Q1S1964), ]
sample384<-herbgen[herbgen$SampleN == "384",]
sample384 <- sample384[!is.na(sample384$K3Q1S1964), ]
sample385<-herbgen[herbgen$SampleN == "385",]
sample385 <- sample385[!is.na(sample385$K3Q1S1964), ]
sample386<-herbgen[herbgen$SampleN == "386",]
sample386 <- sample386[!is.na(sample386$K3Q1S1964), ]
sample387<-herbgen[herbgen$SampleN == "387",]
sample387 <- sample387[!is.na(sample387$K3Q1S1964), ]
sample388<-herbgen[herbgen$SampleN == "388",]
sample388 <- sample388[!is.na(sample388$K3Q1S1964), ]
sample389<-herbgen[herbgen$SampleN == "389",]
sample389 <- sample389[!is.na(sample389$K3Q1S1964), ]
sample390<-herbgen[herbgen$SampleN == "390",]
sample390 <- sample390[!is.na(sample390$K3Q1S1964), ]
sample391<-herbgen[herbgen$SampleN == "391",]
sample391 <- sample391[!is.na(sample391$K3Q1S1964), ]
sample392<-herbgen[herbgen$SampleN == "392",]
sample392 <- sample392[!is.na(sample392$K3Q1S1964), ]
sample393<-herbgen[herbgen$SampleN == "393",]
sample393 <- sample393[!is.na(sample393$K3Q1S1964), ]
sample394<-herbgen[herbgen$SampleN == "394",]
sample394 <- sample394[!is.na(sample394$K3Q1S1964), ]
sample395<-herbgen[herbgen$SampleN == "395",]
sample395 <- sample395[!is.na(sample395$K3Q1S1964), ]
sample396<-herbgen[herbgen$SampleN == "396",]
sample396 <- sample396[!is.na(sample396$K3Q1S1964), ]
sample397<-herbgen[herbgen$SampleN == "397",]
sample397 <- sample397[!is.na(sample397$K3Q1S1964), ]
sample398<-herbgen[herbgen$SampleN == "398",]
sample398 <- sample398[!is.na(sample398$K3Q1S1964), ]
sample399<-herbgen[herbgen$SampleN == "399",]
sample399 <- sample399[!is.na(sample399$K3Q1S1964), ]
sample400<-herbgen[herbgen$SampleN == "400",]
sample400 <- sample400[!is.na(sample400$K3Q1S1964), ]
sample401<-herbgen[herbgen$SampleN == "401",]
sample401 <- sample401[!is.na(sample401$K3Q1S1964), ]
sample402<-herbgen[herbgen$SampleN == "402",]
sample402 <- sample402[!is.na(sample402$K3Q1S1964), ]
sample403<-herbgen[herbgen$SampleN == "403",]
sample403 <- sample403[!is.na(sample403$K3Q1S1964), ]
sample404<-herbgen[herbgen$SampleN == "404",]
sample404 <- sample404[!is.na(sample404$K3Q1S1964), ]
sample405<-herbgen[herbgen$SampleN == "405",]
sample405 <- sample405[!is.na(sample405$K3Q1S1964), ]
sample406<-herbgen[herbgen$SampleN == "406",]
sample406 <- sample406[!is.na(sample406$K3Q1S1964), ]
sample407<-herbgen[herbgen$SampleN == "407",]
sample407 <- sample407[!is.na(sample407$K3Q1S1964), ]
sample408<-herbgen[herbgen$SampleN == "408",]
sample408 <- sample408[!is.na(sample408$K3Q1S1964), ]
sample409<-herbgen[herbgen$SampleN == "409",]
sample409 <- sample409[!is.na(sample409$K3Q1S1964), ]
sample410<-herbgen[herbgen$SampleN == "410",]
sample410 <- sample410[!is.na(sample410$K3Q1S1964), ]
sample411<-herbgen[herbgen$SampleN == "411",]
sample411 <- sample411[!is.na(sample411$K3Q1S1964), ]
sample412<-herbgen[herbgen$SampleN == "412",]
sample412 <- sample412[!is.na(sample412$K3Q1S1964), ]
sample413<-herbgen[herbgen$SampleN == "413",]
sample413 <- sample413[!is.na(sample413$K3Q1S1964), ]
sample414<-herbgen[herbgen$SampleN == "414",]
sample414 <- sample414[!is.na(sample414$K3Q1S1964), ]

#K=3
#Get worldmap
newmap<- getMap(resolution="low")


pdf("Pre1900.pdf", width=6, height=6)
plot(newmap, xlim=c(-105,-60), ylim=c(25,50), main= "1838-1877")
add.pie(z=c(sample1$K3Q1S1964, sample1$K3Q2S1964, sample1$K3Q3S1964), x=sample1$lon, y=sample1$lat, col=c("#8DD3C7", "#FFFFB3",
"#BEBADA"), labels="", radius=.7)
add.pie(z=c(sample2$K3Q1S1964, sample2$K3Q2S1964, sample2$K3Q3S1964), x=sample2$lon, y=sample2$lat, col=c("#8DD3C7", "#FFFFB3",
"#BEBADA"), labels="", radius=.7)
add.pie(z=c(sample3$K3Q1S1964, sample3$K3Q2S1964, sample3$K3Q3S1964), x=sample3$lon, y=sample3$lat, col=c("#8DD3C7", "#FFFFB3",
"#BEBADA"), labels="", radius=.7)
add.pie(z=c(sample4$K3Q1S1964, sample4$K3Q2S1964, sample4$K3Q3S1964), x=sample4$lon, y=sample4$lat, col=c("#8DD3C7", "#FFFFB3",
"#BEBADA"), labels="", radius=.7)
add.pie(z=c(sample5$K3Q1S1964, sample5$K3Q2S1964, sample5$K3Q3S1964), x=sample5$lon, y=sample5$lat, col=c("#8DD3C7", "#FFFFB3",
"#BEBADA"), labels="", radius=.7)
add.pie(z=c(sample6$K3Q1S1964, sample6$K3Q2S1964, sample6$K3Q3S1964), x=sample6$lon, y=sample6$lat, col=c("#8DD3C7", "#FFFFB3",
"#BEBADA"), labels="", radius=.7)
add.pie(z=c(sample7$K3Q1S1964, sample7$K3Q2S1964, sample7$K3Q3S1964), x=sample7$lon, y=sample7$lat, col=c("#8DD3C7", "#FFFFB3",
"#BEBADA"), labels="", radius=.7)
add.pie(z=c(sample8$K3Q1S1964, sample8$K3Q2S1964, sample8$K3Q3S1964), x=sample8$lon, y=sample8$lat, col=c("#8DD3C7", "#FFFFB3",
"#BEBADA"), labels="", radius=.7)
add.pie(z=c(sample9$K3Q1S1964, sample9$K3Q2S1964, sample9$K3Q3S1964), x=sample9$lon, y=sample9$lat, col=c("#8DD3C7", "#FFFFB3",
"#BEBADA"), labels="", radius=.7)
add.pie(z=c(sample10$K3Q1S1964, sample10$K3Q2S1964, sample10$K3Q3S1964), x=sample10$lon, y=sample10$lat, col=c("#8DD3C7", "#FFFFB3",
"#BEBADA"), labels="", radius=.7)
dev.off()

#pre1930
pre1930$SampleN  #49-90
pdf("Pre1930.pdf", width=6, height=6)
plot(newmap, xlim=c(-105,-60), ylim=c(25,50), main="1878-1917")
add.pie(z=c(sample11$K3Q1S1964, sample11$K3Q2S1964, sample11$K3Q3S1964), x=sample11$lon, y=sample11$lat, col=c("#8DD3C7", "#FFFFB3",
"#BEBADA"), labels="", radius=.7)
add.pie(z=c(sample12$K3Q1S1964, sample12$K3Q2S1964, sample12$K3Q3S1964), x=sample12$lon, y=sample12$lat, col=c("#8DD3C7", "#FFFFB3",
"#BEBADA"), labels="", radius=.7)
add.pie(z=c(sample13$K3Q1S1964, sample13$K3Q2S1964, sample13$K3Q3S1964), x=sample13$lon, y=sample13$lat, col=c("#8DD3C7", "#FFFFB3",
"#BEBADA"), labels="", radius=.7)
add.pie(z=c(sample14$K3Q1S1964, sample14$K3Q2S1964, sample14$K3Q3S1964), x=sample14$lon, y=sample14$lat, col=c("#8DD3C7", "#FFFFB3",
"#BEBADA"), labels="", radius=.7)
add.pie(z=c(sample15$K3Q1S1964, sample15$K3Q2S1964, sample15$K3Q3S1964), x=sample15$lon, y=sample15$lat, col=c("#8DD3C7", "#FFFFB3",
"#BEBADA"), labels="", radius=.7)
add.pie(z=c(sample16$K3Q1S1964, sample16$K3Q2S1964, sample16$K3Q3S1964), x=sample16$lon, y=sample16$lat, col=c("#8DD3C7", "#FFFFB3",
"#BEBADA"), labels="", radius=.7)
add.pie(z=c(sample17$K3Q1S1964, sample17$K3Q2S1964, sample17$K3Q3S1964), x=sample17$lon, y=sample17$lat, col=c("#8DD3C7", "#FFFFB3",
"#BEBADA"), labels="", radius=.7)
add.pie(z=c(sample18$K3Q1S1964, sample18$K3Q2S1964, sample18$K3Q3S1964), x=sample18$lon, y=sample18$lat, col=c("#8DD3C7", "#FFFFB3",
"#BEBADA"), labels="", radius=.7)
add.pie(z=c(sample19$K3Q1S1964, sample19$K3Q2S1964, sample19$K3Q3S1964), x=sample19$lon, y=sample19$lat, col=c("#8DD3C7", "#FFFFB3",
"#BEBADA"), labels="", radius=.7)
add.pie(z=c(sample20$K3Q1S1964, sample20$K3Q2S1964, sample20$K3Q3S1964), x=sample20$lon, y=sample20$lat, col=c("#8DD3C7", "#FFFFB3",
"#BEBADA"), labels="", radius=.7)
add.pie(z=c(sample21$K3Q1S1964, sample21$K3Q2S1964, sample21$K3Q3S1964), x=sample21$lon, y=sample21$lat, col=c("#8DD3C7", "#FFFFB3",
"#BEBADA"), labels="", radius=.7)
add.pie(z=c(sample22$K3Q1S1964, sample22$K3Q2S1964, sample22$K3Q3S1964), x=sample22$lon, y=sample22$lat, col=c("#8DD3C7", "#FFFFB3",
"#BEBADA"), labels="", radius=.7)
add.pie(z=c(sample23$K3Q1S1964, sample23$K3Q2S1964, sample23$K3Q3S1964), x=sample23$lon, y=sample23$lat, col=c("#8DD3C7", "#FFFFB3",
"#BEBADA"), labels="", radius=.7)
add.pie(z=c(sample24$K3Q1S1964, sample24$K3Q2S1964, sample24$K3Q3S1964), x=sample24$lon, y=sample24$lat, col=c("#8DD3C7", "#FFFFB3",
"#BEBADA"), labels="", radius=.7)
add.pie(z=c(sample25$K3Q1S1964, sample25$K3Q2S1964, sample25$K3Q3S1964), x=sample25$lon, y=sample25$lat, col=c("#8DD3C7", "#FFFFB3",
"#BEBADA"), labels="", radius=.7)
add.pie(z=c(sample26$K3Q1S1964, sample26$K3Q2S1964, sample26$K3Q3S1964), x=sample26$lon, y=sample26$lat, col=c("#8DD3C7", "#FFFFB3",
"#BEBADA"), labels="", radius=.7)
add.pie(z=c(sample27$K3Q1S1964, sample27$K3Q2S1964, sample27$K3Q3S1964), x=sample27$lon, y=sample27$lat, col=c("#8DD3C7", "#FFFFB3",
"#BEBADA"), labels="", radius=.7)
add.pie(z=c(sample28$K3Q1S1964, sample28$K3Q2S1964, sample28$K3Q3S1964), x=sample28$lon, y=sample28$lat, col=c("#8DD3C7", "#FFFFB3",
"#BEBADA"), labels="", radius=.7)
add.pie(z=c(sample29$K3Q1S1964, sample29$K3Q2S1964, sample29$K3Q3S1964), x=sample29$lon, y=sample29$lat, col=c("#8DD3C7", "#FFFFB3",
"#BEBADA"), labels="", radius=.7)
add.pie(z=c(sample30$K3Q1S1964, sample30$K3Q2S1964, sample30$K3Q3S1964), x=sample30$lon, y=sample30$lat, col=c("#8DD3C7", "#FFFFB3",
"#BEBADA"), labels="", radius=.7)
add.pie(z=c(sample31$K3Q1S1964, sample31$K3Q2S1964, sample31$K3Q3S1964), x=sample31$lon, y=sample31$lat, col=c("#8DD3C7", "#FFFFB3",
"#BEBADA"), labels="", radius=.7)
add.pie(z=c(sample32$K3Q1S1964, sample32$K3Q2S1964, sample32$K3Q3S1964), x=sample32$lon, y=sample32$lat, col=c("#8DD3C7", "#FFFFB3",
"#BEBADA"), labels="", radius=.7)
add.pie(z=c(sample33$K3Q1S1964, sample33$K3Q2S1964, sample33$K3Q3S1964), x=sample33$lon, y=sample33$lat, col=c("#8DD3C7", "#FFFFB3",
"#BEBADA"), labels="", radius=.7)
add.pie(z=c(sample34$K3Q1S1964, sample34$K3Q2S1964, sample34$K3Q3S1964), x=sample34$lon, y=sample34$lat, col=c("#8DD3C7", "#FFFFB3",
"#BEBADA"), labels="", radius=.7)
add.pie(z=c(sample35$K3Q1S1964, sample35$K3Q2S1964, sample35$K3Q3S1964), x=sample35$lon, y=sample35$lat, col=c("#8DD3C7", "#FFFFB3",
"#BEBADA"), labels="", radius=.7)
add.pie(z=c(sample36$K3Q1S1964, sample36$K3Q2S1964, sample36$K3Q3S1964), x=sample36$lon, y=sample36$lat, col=c("#8DD3C7", "#FFFFB3",
"#BEBADA"), labels="", radius=.7)
add.pie(z=c(sample37$K3Q1S1964, sample37$K3Q2S1964, sample37$K3Q3S1964), x=sample37$lon, y=sample37$lat, col=c("#8DD3C7", "#FFFFB3",
"#BEBADA"), labels="", radius=.7)
add.pie(z=c(sample38$K3Q1S1964, sample38$K3Q2S1964, sample38$K3Q3S1964), x=sample38$lon, y=sample38$lat, col=c("#8DD3C7", "#FFFFB3",
"#BEBADA"), labels="", radius=.7)
add.pie(z=c(sample39$K3Q1S1964, sample39$K3Q2S1964, sample39$K3Q3S1964), x=sample39$lon, y=sample39$lat, col=c("#8DD3C7", "#FFFFB3",
"#BEBADA"), labels="", radius=.7)
add.pie(z=c(sample40$K3Q1S1964, sample40$K3Q2S1964, sample40$K3Q3S1964), x=sample40$lon, y=sample40$lat, col=c("#8DD3C7", "#FFFFB3",
"#BEBADA"), labels="", radius=.7)
add.pie(z=c(sample41$K3Q1S1964, sample41$K3Q2S1964, sample41$K3Q3S1964), x=sample41$lon, y=sample41$lat, col=c("#8DD3C7", "#FFFFB3",
"#BEBADA"), labels="", radius=.7)
add.pie(z=c(sample42$K3Q1S1964, sample42$K3Q2S1964, sample42$K3Q3S1964), x=sample42$lon, y=sample42$lat, col=c("#8DD3C7", "#FFFFB3",
"#BEBADA"), labels="", radius=.7)
add.pie(z=c(sample43$K3Q1S1964, sample43$K3Q2S1964, sample43$K3Q3S1964), x=sample43$lon, y=sample43$lat, col=c("#8DD3C7", "#FFFFB3",
"#BEBADA"), labels="", radius=.7)
add.pie(z=c(sample44$K3Q1S1964, sample44$K3Q2S1964, sample44$K3Q3S1964), x=sample44$lon, y=sample44$lat, col=c("#8DD3C7", "#FFFFB3",
"#BEBADA"), labels="", radius=.7)
add.pie(z=c(sample45$K3Q1S1964, sample45$K3Q2S1964, sample45$K3Q3S1964), x=sample45$lon, y=sample45$lat, col=c("#8DD3C7", "#FFFFB3",
"#BEBADA"), labels="", radius=.7)
add.pie(z=c(sample46$K3Q1S1964, sample46$K3Q2S1964, sample46$K3Q3S1964), x=sample46$lon, y=sample46$lat, col=c("#8DD3C7", "#FFFFB3",
"#BEBADA"), labels="", radius=.7)
add.pie(z=c(sample47$K3Q1S1964, sample47$K3Q2S1964, sample47$K3Q3S1964), x=sample47$lon, y=sample47$lat, col=c("#8DD3C7", "#FFFFB3",
"#BEBADA"), labels="", radius=.7)
add.pie(z=c(sample48$K3Q1S1964, sample48$K3Q2S1964, sample48$K3Q3S1964), x=sample48$lon, y=sample48$lat, col=c("#8DD3C7", "#FFFFB3",
"#BEBADA"), labels="", radius=.7)
add.pie(z=c(sample49$K3Q1S1964, sample49$K3Q2S1964, sample49$K3Q3S1964), x=sample49$lon, y=sample49$lat, col=c("#8DD3C7", "#FFFFB3",
"#BEBADA"), labels="", radius=.7)
add.pie(z=c(sample50$K3Q1S1964, sample50$K3Q2S1964, sample50$K3Q3S1964), x=sample50$lon, y=sample50$lat, col=c("#8DD3C7", "#FFFFB3",
"#BEBADA"), labels="", radius=.7)
add.pie(z=c(sample51$K3Q1S1964, sample51$K3Q2S1964, sample51$K3Q3S1964), x=sample51$lon, y=sample51$lat, col=c("#8DD3C7", "#FFFFB3",
"#BEBADA"), labels="", radius=.7)
add.pie(z=c(sample52$K3Q1S1964, sample52$K3Q2S1964, sample52$K3Q3S1964), x=sample52$lon, y=sample52$lat, col=c("#8DD3C7", "#FFFFB3",
"#BEBADA"), labels="", radius=.7)
add.pie(z=c(sample53$K3Q1S1964, sample53$K3Q2S1964, sample53$K3Q3S1964), x=sample53$lon, y=sample53$lat, col=c("#8DD3C7", "#FFFFB3",
"#BEBADA"), labels="", radius=.7)
add.pie(z=c(sample54$K3Q1S1964, sample54$K3Q2S1964, sample54$K3Q3S1964), x=sample54$lon, y=sample54$lat, col=c("#8DD3C7", "#FFFFB3",
"#BEBADA"), labels="", radius=.7)
add.pie(z=c(sample55$K3Q1S1964, sample55$K3Q2S1964, sample55$K3Q3S1964), x=sample55$lon, y=sample55$lat, col=c("#8DD3C7", "#FFFFB3",
"#BEBADA"), labels="", radius=.7)
add.pie(z=c(sample56$K3Q1S1964, sample56$K3Q2S1964, sample56$K3Q3S1964), x=sample56$lon, y=sample56$lat, col=c("#8DD3C7", "#FFFFB3",
"#BEBADA"), labels="", radius=.7)
add.pie(z=c(sample57$K3Q1S1964, sample57$K3Q2S1964, sample57$K3Q3S1964), x=sample57$lon, y=sample57$lat, col=c("#8DD3C7", "#FFFFB3",
"#BEBADA"), labels="", radius=.7)
add.pie(z=c(sample58$K3Q1S1964, sample58$K3Q2S1964, sample58$K3Q3S1964), x=sample58$lon, y=sample58$lat, col=c("#8DD3C7", "#FFFFB3",
"#BEBADA"), labels="", radius=.7)
add.pie(z=c(sample59$K3Q1S1964, sample59$K3Q2S1964, sample59$K3Q3S1964), x=sample59$lon, y=sample59$lat, col=c("#8DD3C7", "#FFFFB3",
"#BEBADA"), labels="", radius=.7)
add.pie(z=c(sample60$K3Q1S1964, sample60$K3Q2S1964, sample60$K3Q3S1964), x=sample60$lon, y=sample60$lat, col=c("#8DD3C7", "#FFFFB3",
"#BEBADA"), labels="", radius=.7)
dev.off()

#pre1960
pre1960$SampleN #91-209
pdf("Pre1960.pdf", width=6, height=6)
plot(newmap, xlim=c(-105,-60), ylim=c(25,50), main="1918-1957")
add.pie(z=c(sample61$K3Q1S1964, sample61$K3Q2S1964, sample61$K3Q3S1964), x=sample61$lon, y=sample61$lat, col=c("#8DD3C7", "#FFFFB3",
"#BEBADA"), labels="", radius=.7)
add.pie(z=c(sample62$K3Q1S1964, sample62$K3Q2S1964, sample62$K3Q3S1964), x=sample62$lon, y=sample62$lat, col=c("#8DD3C7", "#FFFFB3",
"#BEBADA"), labels="", radius=.7)
add.pie(z=c(sample63$K3Q1S1964, sample63$K3Q2S1964, sample63$K3Q3S1964), x=sample63$lon, y=sample63$lat, col=c("#8DD3C7", "#FFFFB3",
"#BEBADA"), labels="", radius=.7)
add.pie(z=c(sample64$K3Q1S1964, sample64$K3Q2S1964, sample64$K3Q3S1964), x=sample64$lon, y=sample64$lat, col=c("#8DD3C7", "#FFFFB3",
"#BEBADA"), labels="", radius=.7)
add.pie(z=c(sample65$K3Q1S1964, sample65$K3Q2S1964, sample65$K3Q3S1964), x=sample65$lon, y=sample65$lat, col=c("#8DD3C7", "#FFFFB3",
"#BEBADA"), labels="", radius=.7)
add.pie(z=c(sample66$K3Q1S1964, sample66$K3Q2S1964, sample66$K3Q3S1964), x=sample66$lon, y=sample66$lat, col=c("#8DD3C7", "#FFFFB3",
"#BEBADA"), labels="", radius=.7)
add.pie(z=c(sample67$K3Q1S1964, sample67$K3Q2S1964, sample67$K3Q3S1964), x=sample67$lon, y=sample67$lat, col=c("#8DD3C7", "#FFFFB3",
"#BEBADA"), labels="", radius=.7)
add.pie(z=c(sample68$K3Q1S1964, sample68$K3Q2S1964, sample68$K3Q3S1964), x=sample68$lon, y=sample68$lat, col=c("#8DD3C7", "#FFFFB3",
"#BEBADA"), labels="", radius=.7)
add.pie(z=c(sample69$K3Q1S1964, sample69$K3Q2S1964, sample69$K3Q3S1964), x=sample69$lon, y=sample69$lat, col=c("#8DD3C7", "#FFFFB3",
"#BEBADA"), labels="", radius=.7)
add.pie(z=c(sample70$K3Q1S1964, sample70$K3Q2S1964, sample70$K3Q3S1964), x=sample70$lon, y=sample70$lat, col=c("#8DD3C7", "#FFFFB3",
"#BEBADA"), labels="", radius=.7)
add.pie(z=c(sample71$K3Q1S1964, sample71$K3Q2S1964, sample71$K3Q3S1964), x=sample71$lon, y=sample71$lat, col=c("#8DD3C7", "#FFFFB3",
"#BEBADA"), labels="", radius=.7)
add.pie(z=c(sample72$K3Q1S1964, sample72$K3Q2S1964, sample72$K3Q3S1964), x=sample72$lon, y=sample72$lat, col=c("#8DD3C7", "#FFFFB3",
"#BEBADA"), labels="", radius=.7)
add.pie(z=c(sample73$K3Q1S1964, sample73$K3Q2S1964, sample73$K3Q3S1964), x=sample73$lon, y=sample73$lat, col=c("#8DD3C7", "#FFFFB3",
"#BEBADA"), labels="", radius=.7)
add.pie(z=c(sample74$K3Q1S1964, sample74$K3Q2S1964, sample74$K3Q3S1964), x=sample74$lon, y=sample74$lat, col=c("#8DD3C7", "#FFFFB3",
"#BEBADA"), labels="", radius=.7)
add.pie(z=c(sample75$K3Q1S1964, sample75$K3Q2S1964, sample75$K3Q3S1964), x=sample75$lon, y=sample75$lat, col=c("#8DD3C7", "#FFFFB3",
"#BEBADA"), labels="", radius=.7)
add.pie(z=c(sample76$K3Q1S1964, sample76$K3Q2S1964, sample76$K3Q3S1964), x=sample76$lon, y=sample76$lat, col=c("#8DD3C7", "#FFFFB3",
"#BEBADA"), labels="", radius=.7)
add.pie(z=c(sample77$K3Q1S1964, sample77$K3Q2S1964, sample77$K3Q3S1964), x=sample77$lon, y=sample77$lat, col=c("#8DD3C7", "#FFFFB3",
"#BEBADA"), labels="", radius=.7)
add.pie(z=c(sample78$K3Q1S1964, sample78$K3Q2S1964, sample78$K3Q3S1964), x=sample78$lon, y=sample78$lat, col=c("#8DD3C7", "#FFFFB3",
"#BEBADA"), labels="", radius=.7)
add.pie(z=c(sample79$K3Q1S1964, sample79$K3Q2S1964, sample79$K3Q3S1964), x=sample79$lon, y=sample79$lat, col=c("#8DD3C7", "#FFFFB3",
"#BEBADA"), labels="", radius=.7)
add.pie(z=c(sample80$K3Q1S1964, sample80$K3Q2S1964, sample80$K3Q3S1964), x=sample80$lon, y=sample80$lat, col=c("#8DD3C7", "#FFFFB3",
"#BEBADA"), labels="", radius=.7)
add.pie(z=c(sample81$K3Q1S1964, sample81$K3Q2S1964, sample81$K3Q3S1964), x=sample81$lon, y=sample81$lat, col=c("#8DD3C7", "#FFFFB3",
"#BEBADA"), labels="", radius=.7)
add.pie(z=c(sample82$K3Q1S1964, sample82$K3Q2S1964, sample82$K3Q3S1964), x=sample82$lon, y=sample82$lat, col=c("#8DD3C7", "#FFFFB3",
"#BEBADA"), labels="", radius=.7)
add.pie(z=c(sample83$K3Q1S1964, sample83$K3Q2S1964, sample83$K3Q3S1964), x=sample83$lon, y=sample83$lat, col=c("#8DD3C7", "#FFFFB3",
"#BEBADA"), labels="", radius=.7)
add.pie(z=c(sample84$K3Q1S1964, sample84$K3Q2S1964, sample84$K3Q3S1964), x=sample84$lon, y=sample84$lat, col=c("#8DD3C7", "#FFFFB3",
"#BEBADA"), labels="", radius=.7)
add.pie(z=c(sample85$K3Q1S1964, sample85$K3Q2S1964, sample85$K3Q3S1964), x=sample85$lon, y=sample85$lat, col=c("#8DD3C7", "#FFFFB3",
"#BEBADA"), labels="", radius=.7)
add.pie(z=c(sample86$K3Q1S1964, sample86$K3Q2S1964, sample86$K3Q3S1964), x=sample86$lon, y=sample86$lat, col=c("#8DD3C7", "#FFFFB3",
"#BEBADA"), labels="", radius=.7)
add.pie(z=c(sample87$K3Q1S1964, sample87$K3Q2S1964, sample87$K3Q3S1964), x=sample87$lon, y=sample87$lat, col=c("#8DD3C7", "#FFFFB3",
"#BEBADA"), labels="", radius=.7)
add.pie(z=c(sample88$K3Q1S1964, sample88$K3Q2S1964, sample88$K3Q3S1964), x=sample88$lon, y=sample88$lat, col=c("#8DD3C7", "#FFFFB3",
"#BEBADA"), labels="", radius=.7)
add.pie(z=c(sample89$K3Q1S1964, sample89$K3Q2S1964, sample89$K3Q3S1964), x=sample89$lon, y=sample89$lat, col=c("#8DD3C7", "#FFFFB3",
"#BEBADA"), labels="", radius=.7)
add.pie(z=c(sample90$K3Q1S1964, sample90$K3Q2S1964, sample90$K3Q3S1964), x=sample90$lon, y=sample90$lat, col=c("#8DD3C7", "#FFFFB3",
"#BEBADA"), labels="", radius=.7)
add.pie(z=c(sample91$K3Q1S1964, sample91$K3Q2S1964, sample91$K3Q3S1964), x=sample91$lon, y=sample91$lat, col=c("#8DD3C7", "#FFFFB3",
"#BEBADA"), labels="", radius=.7)
add.pie(z=c(sample92$K3Q1S1964, sample92$K3Q2S1964, sample92$K3Q3S1964), x=sample92$lon, y=sample92$lat, col=c("#8DD3C7", "#FFFFB3",
"#BEBADA"), labels="", radius=.7)
add.pie(z=c(sample93$K3Q1S1964, sample93$K3Q2S1964, sample93$K3Q3S1964), x=sample93$lon, y=sample93$lat, col=c("#8DD3C7", "#FFFFB3",
"#BEBADA"), labels="", radius=.7)
add.pie(z=c(sample94$K3Q1S1964, sample94$K3Q2S1964, sample94$K3Q3S1964), x=sample94$lon, y=sample94$lat, col=c("#8DD3C7", "#FFFFB3",
"#BEBADA"), labels="", radius=.7)
add.pie(z=c(sample95$K3Q1S1964, sample95$K3Q2S1964, sample95$K3Q3S1964), x=sample95$lon, y=sample95$lat, col=c("#8DD3C7", "#FFFFB3",
"#BEBADA"), labels="", radius=.7)
add.pie(z=c(sample96$K3Q1S1964, sample96$K3Q2S1964, sample96$K3Q3S1964), x=sample96$lon, y=sample96$lat, col=c("#8DD3C7", "#FFFFB3",
"#BEBADA"), labels="", radius=.7)
add.pie(z=c(sample97$K3Q1S1964, sample97$K3Q2S1964, sample97$K3Q3S1964), x=sample97$lon, y=sample97$lat, col=c("#8DD3C7", "#FFFFB3",
"#BEBADA"), labels="", radius=.7)
add.pie(z=c(sample98$K3Q1S1964, sample98$K3Q2S1964, sample98$K3Q3S1964), x=sample98$lon, y=sample98$lat, col=c("#8DD3C7", "#FFFFB3",
"#BEBADA"), labels="", radius=.7)
add.pie(z=c(sample99$K3Q1S1964, sample99$K3Q2S1964, sample99$K3Q3S1964), x=sample99$lon, y=sample99$lat, col=c("#8DD3C7", "#FFFFB3",
"#BEBADA"), labels="", radius=.7)
add.pie(z=c(sample100$K3Q1S1964, sample100$K3Q2S1964, sample100$K3Q3S1964), x=sample100$lon, y=sample100$lat, col=c("#8DD3C7", "#FFFFB3",
"#BEBADA"), labels="", radius=.7)
add.pie(z=c(sample101$K3Q1S1964, sample101$K3Q2S1964, sample101$K3Q3S1964), x=sample101$lon, y=sample101$lat, col=c("#8DD3C7", "#FFFFB3",
"#BEBADA"), labels="", radius=.7)
add.pie(z=c(sample102$K3Q1S1964, sample102$K3Q2S1964, sample102$K3Q3S1964), x=sample102$lon, y=sample102$lat, col=c("#8DD3C7", "#FFFFB3",
"#BEBADA"), labels="", radius=.7)
add.pie(z=c(sample103$K3Q1S1964, sample103$K3Q2S1964, sample103$K3Q3S1964), x=sample103$lon, y=sample103$lat, col=c("#8DD3C7", "#FFFFB3",
"#BEBADA"), labels="", radius=.7)
add.pie(z=c(sample104$K3Q1S1964, sample104$K3Q2S1964, sample104$K3Q3S1964), x=sample104$lon, y=sample104$lat, col=c("#8DD3C7", "#FFFFB3",
"#BEBADA"), labels="", radius=.7)
add.pie(z=c(sample105$K3Q1S1964, sample105$K3Q2S1964, sample105$K3Q3S1964), x=sample105$lon, y=sample105$lat, col=c("#8DD3C7", "#FFFFB3",
"#BEBADA"), labels="", radius=.7)
add.pie(z=c(sample106$K3Q1S1964, sample106$K3Q2S1964, sample106$K3Q3S1964), x=sample106$lon, y=sample106$lat, col=c("#8DD3C7", "#FFFFB3",
"#BEBADA"), labels="", radius=.7)
add.pie(z=c(sample107$K3Q1S1964, sample107$K3Q2S1964, sample107$K3Q3S1964), x=sample107$lon, y=sample107$lat, col=c("#8DD3C7", "#FFFFB3",
"#BEBADA"), labels="", radius=.7)
add.pie(z=c(sample108$K3Q1S1964, sample108$K3Q2S1964, sample108$K3Q3S1964), x=sample108$lon, y=sample108$lat, col=c("#8DD3C7", "#FFFFB3",
"#BEBADA"), labels="", radius=.7)
add.pie(z=c(sample109$K3Q1S1964, sample109$K3Q2S1964, sample109$K3Q3S1964), x=sample109$lon, y=sample109$lat, col=c("#8DD3C7", "#FFFFB3",
"#BEBADA"), labels="", radius=.7)
add.pie(z=c(sample110$K3Q1S1964, sample110$K3Q2S1964, sample110$K3Q3S1964), x=sample110$lon, y=sample110$lat, col=c("#8DD3C7", "#FFFFB3",
"#BEBADA"), labels="", radius=.7)
add.pie(z=c(sample111$K3Q1S1964, sample111$K3Q2S1964, sample111$K3Q3S1964), x=sample111$lon, y=sample111$lat, col=c("#8DD3C7", "#FFFFB3",
"#BEBADA"), labels="", radius=.7)
add.pie(z=c(sample112$K3Q1S1964, sample112$K3Q2S1964, sample112$K3Q3S1964), x=sample112$lon, y=sample112$lat, col=c("#8DD3C7", "#FFFFB3",
"#BEBADA"), labels="", radius=.7)
add.pie(z=c(sample113$K3Q1S1964, sample113$K3Q2S1964, sample113$K3Q3S1964), x=sample113$lon, y=sample113$lat, col=c("#8DD3C7", "#FFFFB3",
"#BEBADA"), labels="", radius=.7)
add.pie(z=c(sample114$K3Q1S1964, sample114$K3Q2S1964, sample114$K3Q3S1964), x=sample114$lon, y=sample114$lat, col=c("#8DD3C7", "#FFFFB3",
"#BEBADA"), labels="", radius=.7)
add.pie(z=c(sample115$K3Q1S1964, sample115$K3Q2S1964, sample115$K3Q3S1964), x=sample115$lon, y=sample115$lat, col=c("#8DD3C7", "#FFFFB3",
"#BEBADA"), labels="", radius=.7)
add.pie(z=c(sample116$K3Q1S1964, sample116$K3Q2S1964, sample116$K3Q3S1964), x=sample116$lon, y=sample116$lat, col=c("#8DD3C7", "#FFFFB3",
"#BEBADA"), labels="", radius=.7)
add.pie(z=c(sample117$K3Q1S1964, sample117$K3Q2S1964, sample117$K3Q3S1964), x=sample117$lon, y=sample117$lat, col=c("#8DD3C7", "#FFFFB3",
"#BEBADA"), labels="", radius=.7)
add.pie(z=c(sample118$K3Q1S1964, sample118$K3Q2S1964, sample118$K3Q3S1964), x=sample118$lon, y=sample118$lat, col=c("#8DD3C7", "#FFFFB3",
"#BEBADA"), labels="", radius=.7)
add.pie(z=c(sample119$K3Q1S1964, sample119$K3Q2S1964, sample119$K3Q3S1964), x=sample119$lon, y=sample119$lat, col=c("#8DD3C7", "#FFFFB3",
"#BEBADA"), labels="", radius=.7)
add.pie(z=c(sample120$K3Q1S1964, sample120$K3Q2S1964, sample120$K3Q3S1964), x=sample120$lon, y=sample120$lat, col=c("#8DD3C7", "#FFFFB3",
"#BEBADA"), labels="", radius=.7)
add.pie(z=c(sample121$K3Q1S1964, sample121$K3Q2S1964, sample121$K3Q3S1964), x=sample121$lon, y=sample121$lat, col=c("#8DD3C7", "#FFFFB3",
"#BEBADA"), labels="", radius=.7)
add.pie(z=c(sample122$K3Q1S1964, sample122$K3Q2S1964, sample122$K3Q3S1964), x=sample122$lon, y=sample122$lat, col=c("#8DD3C7", "#FFFFB3",
"#BEBADA"), labels="", radius=.7)
add.pie(z=c(sample123$K3Q1S1964, sample123$K3Q2S1964, sample123$K3Q3S1964), x=sample123$lon, y=sample123$lat, col=c("#8DD3C7", "#FFFFB3",
"#BEBADA"), labels="", radius=.7)
add.pie(z=c(sample124$K3Q1S1964, sample124$K3Q2S1964, sample124$K3Q3S1964), x=sample124$lon, y=sample124$lat, col=c("#8DD3C7", "#FFFFB3",
"#BEBADA"), labels="", radius=.7)
add.pie(z=c(sample125$K3Q1S1964, sample125$K3Q2S1964, sample125$K3Q3S1964), x=sample125$lon, y=sample125$lat, col=c("#8DD3C7", "#FFFFB3",
"#BEBADA"), labels="", radius=.7)
add.pie(z=c(sample126$K3Q1S1964, sample126$K3Q2S1964, sample126$K3Q3S1964), x=sample126$lon, y=sample126$lat, col=c("#8DD3C7", "#FFFFB3",
"#BEBADA"), labels="", radius=.7)
add.pie(z=c(sample127$K3Q1S1964, sample127$K3Q2S1964, sample127$K3Q3S1964), x=sample127$lon, y=sample127$lat, col=c("#8DD3C7", "#FFFFB3",
"#BEBADA"), labels="", radius=.7)
add.pie(z=c(sample128$K3Q1S1964, sample128$K3Q2S1964, sample128$K3Q3S1964), x=sample128$lon, y=sample128$lat, col=c("#8DD3C7", "#FFFFB3",
"#BEBADA"), labels="", radius=.7)
add.pie(z=c(sample129$K3Q1S1964, sample129$K3Q2S1964, sample129$K3Q3S1964), x=sample129$lon, y=sample129$lat, col=c("#8DD3C7", "#FFFFB3",
"#BEBADA"), labels="", radius=.7)
add.pie(z=c(sample130$K3Q1S1964, sample130$K3Q2S1964, sample130$K3Q3S1964), x=sample130$lon, y=sample130$lat, col=c("#8DD3C7", "#FFFFB3",
"#BEBADA"), labels="", radius=.7)
add.pie(z=c(sample131$K3Q1S1964, sample131$K3Q2S1964, sample131$K3Q3S1964), x=sample131$lon, y=sample131$lat, col=c("#8DD3C7", "#FFFFB3",
"#BEBADA"), labels="", radius=.7)
add.pie(z=c(sample132$K3Q1S1964, sample132$K3Q2S1964, sample132$K3Q3S1964), x=sample132$lon, y=sample132$lat, col=c("#8DD3C7", "#FFFFB3",
"#BEBADA"), labels="", radius=.7)
add.pie(z=c(sample133$K3Q1S1964, sample133$K3Q2S1964, sample133$K3Q3S1964), x=sample133$lon, y=sample133$lat, col=c("#8DD3C7", "#FFFFB3",
"#BEBADA"), labels="", radius=.7)
add.pie(z=c(sample134$K3Q1S1964, sample134$K3Q2S1964, sample134$K3Q3S1964), x=sample134$lon, y=sample134$lat, col=c("#8DD3C7", "#FFFFB3",
"#BEBADA"), labels="", radius=.7)
add.pie(z=c(sample135$K3Q1S1964, sample135$K3Q2S1964, sample135$K3Q3S1964), x=sample135$lon, y=sample135$lat, col=c("#8DD3C7", "#FFFFB3",
"#BEBADA"), labels="", radius=.7)
add.pie(z=c(sample136$K3Q1S1964, sample136$K3Q2S1964, sample136$K3Q3S1964), x=sample136$lon, y=sample136$lat, col=c("#8DD3C7", "#FFFFB3",
"#BEBADA"), labels="", radius=.7)
add.pie(z=c(sample137$K3Q1S1964, sample137$K3Q2S1964, sample137$K3Q3S1964), x=sample137$lon, y=sample137$lat, col=c("#8DD3C7", "#FFFFB3",
"#BEBADA"), labels="", radius=.7)
add.pie(z=c(sample138$K3Q1S1964, sample138$K3Q2S1964, sample138$K3Q3S1964), x=sample138$lon, y=sample138$lat, col=c("#8DD3C7", "#FFFFB3",
"#BEBADA"), labels="", radius=.7)
add.pie(z=c(sample139$K3Q1S1964, sample139$K3Q2S1964, sample139$K3Q3S1964), x=sample139$lon, y=sample139$lat, col=c("#8DD3C7", "#FFFFB3",
"#BEBADA"), labels="", radius=.7)
add.pie(z=c(sample140$K3Q1S1964, sample140$K3Q2S1964, sample140$K3Q3S1964), x=sample140$lon, y=sample140$lat, col=c("#8DD3C7", "#FFFFB3",
"#BEBADA"), labels="", radius=.7)
add.pie(z=c(sample141$K3Q1S1964, sample141$K3Q2S1964, sample141$K3Q3S1964), x=sample141$lon, y=sample141$lat, col=c("#8DD3C7", "#FFFFB3",
"#BEBADA"), labels="", radius=.7)
add.pie(z=c(sample142$K3Q1S1964, sample142$K3Q2S1964, sample142$K3Q3S1964), x=sample142$lon, y=sample142$lat, col=c("#8DD3C7", "#FFFFB3",
"#BEBADA"), labels="", radius=.7)
add.pie(z=c(sample143$K3Q1S1964, sample143$K3Q2S1964, sample143$K3Q3S1964), x=sample143$lon, y=sample143$lat, col=c("#8DD3C7", "#FFFFB3",
"#BEBADA"), labels="", radius=.7)
add.pie(z=c(sample144$K3Q1S1964, sample144$K3Q2S1964, sample144$K3Q3S1964), x=sample144$lon, y=sample144$lat, col=c("#8DD3C7", "#FFFFB3",
"#BEBADA"), labels="", radius=.7)
add.pie(z=c(sample145$K3Q1S1964, sample145$K3Q2S1964, sample145$K3Q3S1964), x=sample145$lon, y=sample145$lat, col=c("#8DD3C7", "#FFFFB3",
"#BEBADA"), labels="", radius=.7)
add.pie(z=c(sample146$K3Q1S1964, sample146$K3Q2S1964, sample146$K3Q3S1964), x=sample146$lon, y=sample146$lat, col=c("#8DD3C7", "#FFFFB3",
"#BEBADA"), labels="", radius=.7)
add.pie(z=c(sample147$K3Q1S1964, sample147$K3Q2S1964, sample147$K3Q3S1964), x=sample147$lon, y=sample147$lat, col=c("#8DD3C7", "#FFFFB3",
"#BEBADA"), labels="", radius=.7)
add.pie(z=c(sample148$K3Q1S1964, sample148$K3Q2S1964, sample148$K3Q3S1964), x=sample148$lon, y=sample148$lat, col=c("#8DD3C7", "#FFFFB3",
"#BEBADA"), labels="", radius=.7)
add.pie(z=c(sample149$K3Q1S1964, sample149$K3Q2S1964, sample149$K3Q3S1964), x=sample149$lon, y=sample149$lat, col=c("#8DD3C7", "#FFFFB3",
"#BEBADA"), labels="", radius=.7)
add.pie(z=c(sample150$K3Q1S1964, sample150$K3Q2S1964, sample150$K3Q3S1964), x=sample150$lon, y=sample150$lat, col=c("#8DD3C7", "#FFFFB3",
"#BEBADA"), labels="", radius=.7)
add.pie(z=c(sample151$K3Q1S1964, sample151$K3Q2S1964, sample151$K3Q3S1964), x=sample151$lon, y=sample151$lat, col=c("#8DD3C7", "#FFFFB3",
"#BEBADA"), labels="", radius=.7)
add.pie(z=c(sample152$K3Q1S1964, sample152$K3Q2S1964, sample152$K3Q3S1964), x=sample152$lon, y=sample152$lat, col=c("#8DD3C7", "#FFFFB3",
"#BEBADA"), labels="", radius=.7)
add.pie(z=c(sample153$K3Q1S1964, sample153$K3Q2S1964, sample153$K3Q3S1964), x=sample153$lon, y=sample153$lat, col=c("#8DD3C7", "#FFFFB3",
"#BEBADA"), labels="", radius=.7)
add.pie(z=c(sample154$K3Q1S1964, sample154$K3Q2S1964, sample154$K3Q3S1964), x=sample154$lon, y=sample154$lat, col=c("#8DD3C7", "#FFFFB3",
"#BEBADA"), labels="", radius=.7)
add.pie(z=c(sample155$K3Q1S1964, sample155$K3Q2S1964, sample155$K3Q3S1964), x=sample155$lon, y=sample155$lat, col=c("#8DD3C7", "#FFFFB3",
"#BEBADA"), labels="", radius=.7)
add.pie(z=c(sample156$K3Q1S1964, sample156$K3Q2S1964, sample156$K3Q3S1964), x=sample156$lon, y=sample156$lat, col=c("#8DD3C7", "#FFFFB3",
"#BEBADA"), labels="", radius=.7)
add.pie(z=c(sample157$K3Q1S1964, sample157$K3Q2S1964, sample157$K3Q3S1964), x=sample157$lon, y=sample157$lat, col=c("#8DD3C7", "#FFFFB3",
"#BEBADA"), labels="", radius=.7)
add.pie(z=c(sample158$K3Q1S1964, sample158$K3Q2S1964, sample158$K3Q3S1964), x=sample158$lon, y=sample158$lat, col=c("#8DD3C7", "#FFFFB3",
"#BEBADA"), labels="", radius=.7)
add.pie(z=c(sample159$K3Q1S1964, sample159$K3Q2S1964, sample159$K3Q3S1964), x=sample159$lon, y=sample159$lat, col=c("#8DD3C7", "#FFFFB3",
"#BEBADA"), labels="", radius=.7)
add.pie(z=c(sample160$K3Q1S1964, sample160$K3Q2S1964, sample160$K3Q3S1964), x=sample160$lon, y=sample160$lat, col=c("#8DD3C7", "#FFFFB3",
"#BEBADA"), labels="", radius=.7)
add.pie(z=c(sample161$K3Q1S1964, sample161$K3Q2S1964, sample161$K3Q3S1964), x=sample161$lon, y=sample161$lat, col=c("#8DD3C7", "#FFFFB3",
"#BEBADA"), labels="", radius=.7)
add.pie(z=c(sample162$K3Q1S1964, sample162$K3Q2S1964, sample162$K3Q3S1964), x=sample162$lon, y=sample162$lat, col=c("#8DD3C7", "#FFFFB3",
"#BEBADA"), labels="", radius=.7)
add.pie(z=c(sample163$K3Q1S1964, sample163$K3Q2S1964, sample163$K3Q3S1964), x=sample163$lon, y=sample163$lat, col=c("#8DD3C7", "#FFFFB3",
"#BEBADA"), labels="", radius=.7)
add.pie(z=c(sample164$K3Q1S1964, sample164$K3Q2S1964, sample164$K3Q3S1964), x=sample164$lon, y=sample164$lat, col=c("#8DD3C7", "#FFFFB3",
"#BEBADA"), labels="", radius=.7)
add.pie(z=c(sample165$K3Q1S1964, sample165$K3Q2S1964, sample165$K3Q3S1964), x=sample165$lon, y=sample165$lat, col=c("#8DD3C7", "#FFFFB3",
"#BEBADA"), labels="", radius=.7)
add.pie(z=c(sample166$K3Q1S1964, sample166$K3Q2S1964, sample166$K3Q3S1964), x=sample166$lon, y=sample166$lat, col=c("#8DD3C7", "#FFFFB3",
"#BEBADA"), labels="", radius=.7)
add.pie(z=c(sample167$K3Q1S1964, sample167$K3Q2S1964, sample167$K3Q3S1964), x=sample167$lon, y=sample167$lat, col=c("#8DD3C7", "#FFFFB3",
"#BEBADA"), labels="", radius=.7)
add.pie(z=c(sample168$K3Q1S1964, sample168$K3Q2S1964, sample168$K3Q3S1964), x=sample168$lon, y=sample168$lat, col=c("#8DD3C7", "#FFFFB3",
"#BEBADA"), labels="", radius=.7)
add.pie(z=c(sample169$K3Q1S1964, sample169$K3Q2S1964, sample169$K3Q3S1964), x=sample169$lon, y=sample169$lat, col=c("#8DD3C7", "#FFFFB3",
"#BEBADA"), labels="", radius=.7)
add.pie(z=c(sample170$K3Q1S1964, sample170$K3Q2S1964, sample170$K3Q3S1964), x=sample170$lon, y=sample170$lat, col=c("#8DD3C7", "#FFFFB3",
"#BEBADA"), labels="", radius=.7)
add.pie(z=c(sample171$K3Q1S1964, sample171$K3Q2S1964, sample171$K3Q3S1964), x=sample171$lon, y=sample171$lat, col=c("#8DD3C7", "#FFFFB3",
"#BEBADA"), labels="", radius=.7)
add.pie(z=c(sample172$K3Q1S1964, sample172$K3Q2S1964, sample172$K3Q3S1964), x=sample172$lon, y=sample172$lat, col=c("#8DD3C7", "#FFFFB3",
"#BEBADA"), labels="", radius=.7)
add.pie(z=c(sample173$K3Q1S1964, sample173$K3Q2S1964, sample173$K3Q3S1964), x=sample173$lon, y=sample173$lat, col=c("#8DD3C7", "#FFFFB3",
"#BEBADA"), labels="", radius=.7)
add.pie(z=c(sample174$K3Q1S1964, sample174$K3Q2S1964, sample174$K3Q3S1964), x=sample174$lon, y=sample174$lat, col=c("#8DD3C7", "#FFFFB3",
"#BEBADA"), labels="", radius=.7)
add.pie(z=c(sample175$K3Q1S1964, sample175$K3Q2S1964, sample175$K3Q3S1964), x=sample175$lon, y=sample175$lat, col=c("#8DD3C7", "#FFFFB3",
"#BEBADA"), labels="", radius=.7)
add.pie(z=c(sample176$K3Q1S1964, sample176$K3Q2S1964, sample176$K3Q3S1964), x=sample176$lon, y=sample176$lat, col=c("#8DD3C7", "#FFFFB3",
"#BEBADA"), labels="", radius=.7)
add.pie(z=c(sample177$K3Q1S1964, sample177$K3Q2S1964, sample177$K3Q3S1964), x=sample177$lon, y=sample177$lat, col=c("#8DD3C7", "#FFFFB3",
"#BEBADA"), labels="", radius=.7)
add.pie(z=c(sample178$K3Q1S1964, sample178$K3Q2S1964, sample178$K3Q3S1964), x=sample178$lon, y=sample178$lat, col=c("#8DD3C7", "#FFFFB3",
"#BEBADA"), labels="", radius=.7)
add.pie(z=c(sample179$K3Q1S1964, sample179$K3Q2S1964, sample179$K3Q3S1964), x=sample179$lon, y=sample179$lat, col=c("#8DD3C7", "#FFFFB3",
"#BEBADA"), labels="", radius=.7)
add.pie(z=c(sample180$K3Q1S1964, sample180$K3Q2S1964, sample180$K3Q3S1964), x=sample180$lon, y=sample180$lat, col=c("#8DD3C7", "#FFFFB3",
"#BEBADA"), labels="", radius=.7)
dev.off()

#pre1990
#pre1990$SampleN #210-334
pdf("Pre1990.pdf", width=6, height=6)
plot(newmap, xlim=c(-105,-60), ylim=c(25,50), main="1958-1997")
add.pie(z=c(sample181$K3Q1S1964, sample181$K3Q2S1964, sample181$K3Q3S1964), x=sample181$lon, y=sample181$lat, col=c("#8DD3C7", "#FFFFB3",
"#BEBADA"), labels="", radius=.7)
add.pie(z=c(sample182$K3Q1S1964, sample182$K3Q2S1964, sample182$K3Q3S1964), x=sample182$lon, y=sample182$lat, col=c("#8DD3C7", "#FFFFB3",
"#BEBADA"), labels="", radius=.7)
add.pie(z=c(sample183$K3Q1S1964, sample183$K3Q2S1964, sample183$K3Q3S1964), x=sample183$lon, y=sample183$lat, col=c("#8DD3C7", "#FFFFB3",
"#BEBADA"), labels="", radius=.7)
add.pie(z=c(sample184$K3Q1S1964, sample184$K3Q2S1964, sample184$K3Q3S1964), x=sample184$lon, y=sample184$lat, col=c("#8DD3C7", "#FFFFB3",
"#BEBADA"), labels="", radius=.7)
add.pie(z=c(sample185$K3Q1S1964, sample185$K3Q2S1964, sample185$K3Q3S1964), x=sample185$lon, y=sample185$lat, col=c("#8DD3C7", "#FFFFB3",
"#BEBADA"), labels="", radius=.7)
add.pie(z=c(sample186$K3Q1S1964, sample186$K3Q2S1964, sample186$K3Q3S1964), x=sample186$lon, y=sample186$lat, col=c("#8DD3C7", "#FFFFB3",
"#BEBADA"), labels="", radius=.7)
add.pie(z=c(sample187$K3Q1S1964, sample187$K3Q2S1964, sample187$K3Q3S1964), x=sample187$lon, y=sample187$lat, col=c("#8DD3C7", "#FFFFB3",
"#BEBADA"), labels="", radius=.7)
add.pie(z=c(sample188$K3Q1S1964, sample188$K3Q2S1964, sample188$K3Q3S1964), x=sample188$lon, y=sample188$lat, col=c("#8DD3C7", "#FFFFB3",
"#BEBADA"), labels="", radius=.7)
add.pie(z=c(sample189$K3Q1S1964, sample189$K3Q2S1964, sample189$K3Q3S1964), x=sample189$lon, y=sample189$lat, col=c("#8DD3C7", "#FFFFB3",
"#BEBADA"), labels="", radius=.7)
add.pie(z=c(sample190$K3Q1S1964, sample190$K3Q2S1964, sample190$K3Q3S1964), x=sample190$lon, y=sample190$lat, col=c("#8DD3C7", "#FFFFB3",
"#BEBADA"), labels="", radius=.7)
add.pie(z=c(sample191$K3Q1S1964, sample191$K3Q2S1964, sample191$K3Q3S1964), x=sample191$lon, y=sample191$lat, col=c("#8DD3C7", "#FFFFB3",
"#BEBADA"), labels="", radius=.7)
add.pie(z=c(sample192$K3Q1S1964, sample192$K3Q2S1964, sample192$K3Q3S1964), x=sample192$lon, y=sample192$lat, col=c("#8DD3C7", "#FFFFB3",
"#BEBADA"), labels="", radius=.7)
add.pie(z=c(sample193$K3Q1S1964, sample193$K3Q2S1964, sample193$K3Q3S1964), x=sample193$lon, y=sample193$lat, col=c("#8DD3C7", "#FFFFB3",
"#BEBADA"), labels="", radius=.7)
add.pie(z=c(sample194$K3Q1S1964, sample194$K3Q2S1964, sample194$K3Q3S1964), x=sample194$lon, y=sample194$lat, col=c("#8DD3C7", "#FFFFB3",
"#BEBADA"), labels="", radius=.7)
add.pie(z=c(sample195$K3Q1S1964, sample195$K3Q2S1964, sample195$K3Q3S1964), x=sample195$lon, y=sample195$lat, col=c("#8DD3C7", "#FFFFB3",
"#BEBADA"), labels="", radius=.7)
add.pie(z=c(sample196$K3Q1S1964, sample196$K3Q2S1964, sample196$K3Q3S1964), x=sample196$lon, y=sample196$lat, col=c("#8DD3C7", "#FFFFB3",
"#BEBADA"), labels="", radius=.7)
add.pie(z=c(sample197$K3Q1S1964, sample197$K3Q2S1964, sample197$K3Q3S1964), x=sample197$lon, y=sample197$lat, col=c("#8DD3C7", "#FFFFB3",
"#BEBADA"), labels="", radius=.7)
add.pie(z=c(sample198$K3Q1S1964, sample198$K3Q2S1964, sample198$K3Q3S1964), x=sample198$lon, y=sample198$lat, col=c("#8DD3C7", "#FFFFB3",
"#BEBADA"), labels="", radius=.7)
add.pie(z=c(sample199$K3Q1S1964, sample199$K3Q2S1964, sample199$K3Q3S1964), x=sample199$lon, y=sample199$lat, col=c("#8DD3C7", "#FFFFB3",
"#BEBADA"), labels="", radius=.7)
add.pie(z=c(sample200$K3Q1S1964, sample200$K3Q2S1964, sample200$K3Q3S1964), x=sample200$lon, y=sample200$lat, col=c("#8DD3C7", "#FFFFB3",
"#BEBADA"), labels="", radius=.7)
add.pie(z=c(sample201$K3Q1S1964, sample201$K3Q2S1964, sample201$K3Q3S1964), x=sample201$lon, y=sample201$lat, col=c("#8DD3C7", "#FFFFB3",
"#BEBADA"), labels="", radius=.7)
add.pie(z=c(sample202$K3Q1S1964, sample202$K3Q2S1964, sample202$K3Q3S1964), x=sample202$lon, y=sample202$lat, col=c("#8DD3C7", "#FFFFB3",
"#BEBADA"), labels="", radius=.7)
add.pie(z=c(sample203$K3Q1S1964, sample203$K3Q2S1964, sample203$K3Q3S1964), x=sample203$lon, y=sample203$lat, col=c("#8DD3C7", "#FFFFB3",
"#BEBADA"), labels="", radius=.7)
add.pie(z=c(sample204$K3Q1S1964, sample204$K3Q2S1964, sample204$K3Q3S1964), x=sample204$lon, y=sample204$lat, col=c("#8DD3C7", "#FFFFB3",
"#BEBADA"), labels="", radius=.7)
add.pie(z=c(sample205$K3Q1S1964, sample205$K3Q2S1964, sample205$K3Q3S1964), x=sample205$lon, y=sample205$lat, col=c("#8DD3C7", "#FFFFB3",
"#BEBADA"), labels="", radius=.7)
add.pie(z=c(sample206$K3Q1S1964, sample206$K3Q2S1964, sample206$K3Q3S1964), x=sample206$lon, y=sample206$lat, col=c("#8DD3C7", "#FFFFB3",
"#BEBADA"), labels="", radius=.7)
add.pie(z=c(sample207$K3Q1S1964, sample207$K3Q2S1964, sample207$K3Q3S1964), x=sample207$lon, y=sample207$lat, col=c("#8DD3C7", "#FFFFB3",
"#BEBADA"), labels="", radius=.7)
add.pie(z=c(sample208$K3Q1S1964, sample208$K3Q2S1964, sample208$K3Q3S1964), x=sample208$lon, y=sample208$lat, col=c("#8DD3C7", "#FFFFB3",
"#BEBADA"), labels="", radius=.7)
add.pie(z=c(sample209$K3Q1S1964, sample209$K3Q2S1964, sample209$K3Q3S1964), x=sample209$lon, y=sample209$lat, col=c("#8DD3C7", "#FFFFB3",
"#BEBADA"), labels="", radius=.7)
add.pie(z=c(sample210$K3Q1S1964, sample210$K3Q2S1964, sample210$K3Q3S1964), x=sample210$lon, y=sample210$lat, col=c("#8DD3C7", "#FFFFB3",
"#BEBADA"), labels="", radius=.7)
add.pie(z=c(sample211$K3Q1S1964, sample211$K3Q2S1964, sample211$K3Q3S1964), x=sample211$lon, y=sample211$lat, col=c("#8DD3C7", "#FFFFB3",
"#BEBADA"), labels="", radius=.7)
add.pie(z=c(sample212$K3Q1S1964, sample212$K3Q2S1964, sample212$K3Q3S1964), x=sample212$lon, y=sample212$lat, col=c("#8DD3C7", "#FFFFB3",
"#BEBADA"), labels="", radius=.7)
add.pie(z=c(sample213$K3Q1S1964, sample213$K3Q2S1964, sample213$K3Q3S1964), x=sample213$lon, y=sample213$lat, col=c("#8DD3C7", "#FFFFB3",
"#BEBADA"), labels="", radius=.7)
add.pie(z=c(sample214$K3Q1S1964, sample214$K3Q2S1964, sample214$K3Q3S1964), x=sample214$lon, y=sample214$lat, col=c("#8DD3C7", "#FFFFB3",
"#BEBADA"), labels="", radius=.7)
add.pie(z=c(sample215$K3Q1S1964, sample215$K3Q2S1964, sample215$K3Q3S1964), x=sample215$lon, y=sample215$lat, col=c("#8DD3C7", "#FFFFB3",
"#BEBADA"), labels="", radius=.7)
add.pie(z=c(sample216$K3Q1S1964, sample216$K3Q2S1964, sample216$K3Q3S1964), x=sample216$lon, y=sample216$lat, col=c("#8DD3C7", "#FFFFB3",
"#BEBADA"), labels="", radius=.7)
add.pie(z=c(sample217$K3Q1S1964, sample217$K3Q2S1964, sample217$K3Q3S1964), x=sample217$lon, y=sample217$lat, col=c("#8DD3C7", "#FFFFB3",
"#BEBADA"), labels="", radius=.7)
add.pie(z=c(sample218$K3Q1S1964, sample218$K3Q2S1964, sample218$K3Q3S1964), x=sample218$lon, y=sample218$lat, col=c("#8DD3C7", "#FFFFB3",
"#BEBADA"), labels="", radius=.7)
add.pie(z=c(sample219$K3Q1S1964, sample219$K3Q2S1964, sample219$K3Q3S1964), x=sample219$lon, y=sample219$lat, col=c("#8DD3C7", "#FFFFB3",
"#BEBADA"), labels="", radius=.7)
add.pie(z=c(sample220$K3Q1S1964, sample220$K3Q2S1964, sample220$K3Q3S1964), x=sample220$lon, y=sample220$lat, col=c("#8DD3C7", "#FFFFB3",
"#BEBADA"), labels="", radius=.7)
add.pie(z=c(sample221$K3Q1S1964, sample221$K3Q2S1964, sample221$K3Q3S1964), x=sample221$lon, y=sample221$lat, col=c("#8DD3C7", "#FFFFB3",
"#BEBADA"), labels="", radius=.7)
add.pie(z=c(sample222$K3Q1S1964, sample222$K3Q2S1964, sample222$K3Q3S1964), x=sample222$lon, y=sample222$lat, col=c("#8DD3C7", "#FFFFB3",
"#BEBADA"), labels="", radius=.7)
add.pie(z=c(sample223$K3Q1S1964, sample223$K3Q2S1964, sample223$K3Q3S1964), x=sample223$lon, y=sample223$lat, col=c("#8DD3C7", "#FFFFB3",
"#BEBADA"), labels="", radius=.7)
add.pie(z=c(sample224$K3Q1S1964, sample224$K3Q2S1964, sample224$K3Q3S1964), x=sample224$lon, y=sample224$lat, col=c("#8DD3C7", "#FFFFB3",
"#BEBADA"), labels="", radius=.7)
add.pie(z=c(sample225$K3Q1S1964, sample225$K3Q2S1964, sample225$K3Q3S1964), x=sample225$lon, y=sample225$lat, col=c("#8DD3C7", "#FFFFB3",
"#BEBADA"), labels="", radius=.7)
add.pie(z=c(sample226$K3Q1S1964, sample226$K3Q2S1964, sample226$K3Q3S1964), x=sample226$lon, y=sample226$lat, col=c("#8DD3C7", "#FFFFB3",
"#BEBADA"), labels="", radius=.7)
add.pie(z=c(sample227$K3Q1S1964, sample227$K3Q2S1964, sample227$K3Q3S1964), x=sample227$lon, y=sample227$lat, col=c("#8DD3C7", "#FFFFB3",
"#BEBADA"), labels="", radius=.7)
add.pie(z=c(sample228$K3Q1S1964, sample228$K3Q2S1964, sample228$K3Q3S1964), x=sample228$lon, y=sample228$lat, col=c("#8DD3C7", "#FFFFB3",
"#BEBADA"), labels="", radius=.7)
add.pie(z=c(sample229$K3Q1S1964, sample229$K3Q2S1964, sample229$K3Q3S1964), x=sample229$lon, y=sample229$lat, col=c("#8DD3C7", "#FFFFB3",
"#BEBADA"), labels="", radius=.7)
add.pie(z=c(sample230$K3Q1S1964, sample230$K3Q2S1964, sample230$K3Q3S1964), x=sample230$lon, y=sample230$lat, col=c("#8DD3C7", "#FFFFB3",
"#BEBADA"), labels="", radius=.7)
add.pie(z=c(sample231$K3Q1S1964, sample231$K3Q2S1964, sample231$K3Q3S1964), x=sample231$lon, y=sample231$lat, col=c("#8DD3C7", "#FFFFB3",
"#BEBADA"), labels="", radius=.7)
add.pie(z=c(sample232$K3Q1S1964, sample232$K3Q2S1964, sample232$K3Q3S1964), x=sample232$lon, y=sample232$lat, col=c("#8DD3C7", "#FFFFB3",
"#BEBADA"), labels="", radius=.7)
add.pie(z=c(sample233$K3Q1S1964, sample233$K3Q2S1964, sample233$K3Q3S1964), x=sample233$lon, y=sample233$lat, col=c("#8DD3C7", "#FFFFB3",
"#BEBADA"), labels="", radius=.7)
add.pie(z=c(sample234$K3Q1S1964, sample234$K3Q2S1964, sample234$K3Q3S1964), x=sample234$lon, y=sample234$lat, col=c("#8DD3C7", "#FFFFB3",
"#BEBADA"), labels="", radius=.7)
add.pie(z=c(sample235$K3Q1S1964, sample235$K3Q2S1964, sample235$K3Q3S1964), x=sample235$lon, y=sample235$lat, col=c("#8DD3C7", "#FFFFB3",
"#BEBADA"), labels="", radius=.7)
add.pie(z=c(sample236$K3Q1S1964, sample236$K3Q2S1964, sample236$K3Q3S1964), x=sample236$lon, y=sample236$lat, col=c("#8DD3C7", "#FFFFB3",
"#BEBADA"), labels="", radius=.7)
add.pie(z=c(sample237$K3Q1S1964, sample237$K3Q2S1964, sample237$K3Q3S1964), x=sample237$lon, y=sample237$lat, col=c("#8DD3C7", "#FFFFB3",
"#BEBADA"), labels="", radius=.7)
add.pie(z=c(sample238$K3Q1S1964, sample238$K3Q2S1964, sample238$K3Q3S1964), x=sample238$lon, y=sample238$lat, col=c("#8DD3C7", "#FFFFB3",
"#BEBADA"), labels="", radius=.7)
add.pie(z=c(sample239$K3Q1S1964, sample239$K3Q2S1964, sample239$K3Q3S1964), x=sample239$lon, y=sample239$lat, col=c("#8DD3C7", "#FFFFB3",
"#BEBADA"), labels="", radius=.7)
add.pie(z=c(sample240$K3Q1S1964, sample240$K3Q2S1964, sample240$K3Q3S1964), x=sample240$lon, y=sample240$lat, col=c("#8DD3C7", "#FFFFB3",
"#BEBADA"), labels="", radius=.7)
add.pie(z=c(sample241$K3Q1S1964, sample241$K3Q2S1964, sample241$K3Q3S1964), x=sample241$lon, y=sample241$lat, col=c("#8DD3C7", "#FFFFB3",
"#BEBADA"), labels="", radius=.7)
add.pie(z=c(sample242$K3Q1S1964, sample242$K3Q2S1964, sample242$K3Q3S1964), x=sample242$lon, y=sample242$lat, col=c("#8DD3C7", "#FFFFB3",
"#BEBADA"), labels="", radius=.7)
add.pie(z=c(sample243$K3Q1S1964, sample243$K3Q2S1964, sample243$K3Q3S1964), x=sample243$lon, y=sample243$lat, col=c("#8DD3C7", "#FFFFB3",
"#BEBADA"), labels="", radius=.7)
add.pie(z=c(sample244$K3Q1S1964, sample244$K3Q2S1964, sample244$K3Q3S1964), x=sample244$lon, y=sample244$lat, col=c("#8DD3C7", "#FFFFB3",
"#BEBADA"), labels="", radius=.7)
add.pie(z=c(sample245$K3Q1S1964, sample245$K3Q2S1964, sample245$K3Q3S1964), x=sample245$lon, y=sample245$lat, col=c("#8DD3C7", "#FFFFB3",
"#BEBADA"), labels="", radius=.7)
add.pie(z=c(sample246$K3Q1S1964, sample246$K3Q2S1964, sample246$K3Q3S1964), x=sample246$lon, y=sample246$lat, col=c("#8DD3C7", "#FFFFB3",
"#BEBADA"), labels="", radius=.7)
add.pie(z=c(sample247$K3Q1S1964, sample247$K3Q2S1964, sample247$K3Q3S1964), x=sample247$lon, y=sample247$lat, col=c("#8DD3C7", "#FFFFB3",
"#BEBADA"), labels="", radius=.7)
add.pie(z=c(sample248$K3Q1S1964, sample248$K3Q2S1964, sample248$K3Q3S1964), x=sample248$lon, y=sample248$lat, col=c("#8DD3C7", "#FFFFB3",
"#BEBADA"), labels="", radius=.7)
add.pie(z=c(sample249$K3Q1S1964, sample249$K3Q2S1964, sample249$K3Q3S1964), x=sample249$lon, y=sample249$lat, col=c("#8DD3C7", "#FFFFB3",
"#BEBADA"), labels="", radius=.7)
add.pie(z=c(sample250$K3Q1S1964, sample250$K3Q2S1964, sample250$K3Q3S1964), x=sample250$lon, y=sample250$lat, col=c("#8DD3C7", "#FFFFB3",
"#BEBADA"), labels="", radius=.7)
add.pie(z=c(sample251$K3Q1S1964, sample251$K3Q2S1964, sample251$K3Q3S1964), x=sample251$lon, y=sample251$lat, col=c("#8DD3C7", "#FFFFB3",
"#BEBADA"), labels="", radius=.7)
add.pie(z=c(sample252$K3Q1S1964, sample252$K3Q2S1964, sample252$K3Q3S1964), x=sample252$lon, y=sample252$lat, col=c("#8DD3C7", "#FFFFB3",
"#BEBADA"), labels="", radius=.7)
add.pie(z=c(sample253$K3Q1S1964, sample253$K3Q2S1964, sample253$K3Q3S1964), x=sample253$lon, y=sample253$lat, col=c("#8DD3C7", "#FFFFB3",
"#BEBADA"), labels="", radius=.7)
add.pie(z=c(sample254$K3Q1S1964, sample254$K3Q2S1964, sample254$K3Q3S1964), x=sample254$lon, y=sample254$lat, col=c("#8DD3C7", "#FFFFB3",
"#BEBADA"), labels="", radius=.7)
add.pie(z=c(sample255$K3Q1S1964, sample255$K3Q2S1964, sample255$K3Q3S1964), x=sample255$lon, y=sample255$lat, col=c("#8DD3C7", "#FFFFB3",
"#BEBADA"), labels="", radius=.7)
add.pie(z=c(sample256$K3Q1S1964, sample256$K3Q2S1964, sample256$K3Q3S1964), x=sample256$lon, y=sample256$lat, col=c("#8DD3C7", "#FFFFB3",
"#BEBADA"), labels="", radius=.7)
add.pie(z=c(sample257$K3Q1S1964, sample257$K3Q2S1964, sample257$K3Q3S1964), x=sample257$lon, y=sample257$lat, col=c("#8DD3C7", "#FFFFB3",
"#BEBADA"), labels="", radius=.7)
add.pie(z=c(sample258$K3Q1S1964, sample258$K3Q2S1964, sample258$K3Q3S1964), x=sample258$lon, y=sample258$lat, col=c("#8DD3C7", "#FFFFB3",
"#BEBADA"), labels="", radius=.7)
add.pie(z=c(sample259$K3Q1S1964, sample259$K3Q2S1964, sample259$K3Q3S1964), x=sample259$lon, y=sample259$lat, col=c("#8DD3C7", "#FFFFB3",
"#BEBADA"), labels="", radius=.7)
add.pie(z=c(sample260$K3Q1S1964, sample260$K3Q2S1964, sample260$K3Q3S1964), x=sample260$lon, y=sample260$lat, col=c("#8DD3C7", "#FFFFB3",
"#BEBADA"), labels="", radius=.7)
add.pie(z=c(sample261$K3Q1S1964, sample261$K3Q2S1964, sample261$K3Q3S1964), x=sample261$lon, y=sample261$lat, col=c("#8DD3C7", "#FFFFB3",
"#BEBADA"), labels="", radius=.7)
add.pie(z=c(sample262$K3Q1S1964, sample262$K3Q2S1964, sample262$K3Q3S1964), x=sample262$lon, y=sample262$lat, col=c("#8DD3C7", "#FFFFB3",
"#BEBADA"), labels="", radius=.7)
add.pie(z=c(sample263$K3Q1S1964, sample263$K3Q2S1964, sample263$K3Q3S1964), x=sample263$lon, y=sample263$lat, col=c("#8DD3C7", "#FFFFB3",
"#BEBADA"), labels="", radius=.7)
add.pie(z=c(sample264$K3Q1S1964, sample264$K3Q2S1964, sample264$K3Q3S1964), x=sample264$lon, y=sample264$lat, col=c("#8DD3C7", "#FFFFB3",
"#BEBADA"), labels="", radius=.7)
add.pie(z=c(sample265$K3Q1S1964, sample265$K3Q2S1964, sample265$K3Q3S1964), x=sample265$lon, y=sample265$lat, col=c("#8DD3C7", "#FFFFB3",
"#BEBADA"), labels="", radius=.7)
add.pie(z=c(sample266$K3Q1S1964, sample266$K3Q2S1964, sample266$K3Q3S1964), x=sample266$lon, y=sample266$lat, col=c("#8DD3C7", "#FFFFB3",
"#BEBADA"), labels="", radius=.7)
add.pie(z=c(sample267$K3Q1S1964, sample267$K3Q2S1964, sample267$K3Q3S1964), x=sample267$lon, y=sample267$lat, col=c("#8DD3C7", "#FFFFB3",
"#BEBADA"), labels="", radius=.7)
add.pie(z=c(sample268$K3Q1S1964, sample268$K3Q2S1964, sample268$K3Q3S1964), x=sample268$lon, y=sample268$lat, col=c("#8DD3C7", "#FFFFB3",
"#BEBADA"), labels="", radius=.7)
add.pie(z=c(sample269$K3Q1S1964, sample269$K3Q2S1964, sample269$K3Q3S1964), x=sample269$lon, y=sample269$lat, col=c("#8DD3C7", "#FFFFB3",
"#BEBADA"), labels="", radius=.7)
add.pie(z=c(sample270$K3Q1S1964, sample270$K3Q2S1964, sample270$K3Q3S1964), x=sample270$lon, y=sample270$lat, col=c("#8DD3C7", "#FFFFB3",
"#BEBADA"), labels="", radius=.7)
add.pie(z=c(sample271$K3Q1S1964, sample271$K3Q2S1964, sample271$K3Q3S1964), x=sample271$lon, y=sample271$lat, col=c("#8DD3C7", "#FFFFB3",
"#BEBADA"), labels="", radius=.7)
add.pie(z=c(sample272$K3Q1S1964, sample272$K3Q2S1964, sample272$K3Q3S1964), x=sample272$lon, y=sample272$lat, col=c("#8DD3C7", "#FFFFB3",
"#BEBADA"), labels="", radius=.7)
add.pie(z=c(sample273$K3Q1S1964, sample273$K3Q2S1964, sample273$K3Q3S1964), x=sample273$lon, y=sample273$lat, col=c("#8DD3C7", "#FFFFB3",
"#BEBADA"), labels="", radius=.7)
add.pie(z=c(sample274$K3Q1S1964, sample274$K3Q2S1964, sample274$K3Q3S1964), x=sample274$lon, y=sample274$lat, col=c("#8DD3C7", "#FFFFB3",
"#BEBADA"), labels="", radius=.7)
add.pie(z=c(sample275$K3Q1S1964, sample275$K3Q2S1964, sample275$K3Q3S1964), x=sample275$lon, y=sample275$lat, col=c("#8DD3C7", "#FFFFB3",
"#BEBADA"), labels="", radius=.7)
add.pie(z=c(sample276$K3Q1S1964, sample276$K3Q2S1964, sample276$K3Q3S1964), x=sample276$lon, y=sample276$lat, col=c("#8DD3C7", "#FFFFB3",
"#BEBADA"), labels="", radius=.7)
add.pie(z=c(sample277$K3Q1S1964, sample277$K3Q2S1964, sample277$K3Q3S1964), x=sample277$lon, y=sample277$lat, col=c("#8DD3C7", "#FFFFB3",
"#BEBADA"), labels="", radius=.7)
add.pie(z=c(sample278$K3Q1S1964, sample278$K3Q2S1964, sample278$K3Q3S1964), x=sample278$lon, y=sample278$lat, col=c("#8DD3C7", "#FFFFB3",
"#BEBADA"), labels="", radius=.7)
add.pie(z=c(sample279$K3Q1S1964, sample279$K3Q2S1964, sample279$K3Q3S1964), x=sample279$lon, y=sample279$lat, col=c("#8DD3C7", "#FFFFB3",
"#BEBADA"), labels="", radius=.7)
add.pie(z=c(sample280$K3Q1S1964, sample280$K3Q2S1964, sample280$K3Q3S1964), x=sample280$lon, y=sample280$lat, col=c("#8DD3C7", "#FFFFB3",
"#BEBADA"), labels="", radius=.7)
add.pie(z=c(sample281$K3Q1S1964, sample281$K3Q2S1964, sample281$K3Q3S1964), x=sample281$lon, y=sample281$lat, col=c("#8DD3C7", "#FFFFB3",
"#BEBADA"), labels="", radius=.7)
add.pie(z=c(sample282$K3Q1S1964, sample282$K3Q2S1964, sample282$K3Q3S1964), x=sample282$lon, y=sample282$lat, col=c("#8DD3C7", "#FFFFB3",
"#BEBADA"), labels="", radius=.7)
add.pie(z=c(sample283$K3Q1S1964, sample283$K3Q2S1964, sample283$K3Q3S1964), x=sample283$lon, y=sample283$lat, col=c("#8DD3C7", "#FFFFB3",
"#BEBADA"), labels="", radius=.7)
add.pie(z=c(sample284$K3Q1S1964, sample284$K3Q2S1964, sample284$K3Q3S1964), x=sample284$lon, y=sample284$lat, col=c("#8DD3C7", "#FFFFB3",
"#BEBADA"), labels="", radius=.7)
add.pie(z=c(sample285$K3Q1S1964, sample285$K3Q2S1964, sample285$K3Q3S1964), x=sample285$lon, y=sample285$lat, col=c("#8DD3C7", "#FFFFB3",
"#BEBADA"), labels="", radius=.7)
add.pie(z=c(sample286$K3Q1S1964, sample286$K3Q2S1964, sample286$K3Q3S1964), x=sample286$lon, y=sample286$lat, col=c("#8DD3C7", "#FFFFB3",
"#BEBADA"), labels="", radius=.7)
add.pie(z=c(sample287$K3Q1S1964, sample287$K3Q2S1964, sample287$K3Q3S1964), x=sample287$lon, y=sample287$lat, col=c("#8DD3C7", "#FFFFB3",
"#BEBADA"), labels="", radius=.7)
add.pie(z=c(sample288$K3Q1S1964, sample288$K3Q2S1964, sample288$K3Q3S1964), x=sample288$lon, y=sample288$lat, col=c("#8DD3C7", "#FFFFB3",
"#BEBADA"), labels="", radius=.7)
add.pie(z=c(sample289$K3Q1S1964, sample289$K3Q2S1964, sample289$K3Q3S1964), x=sample289$lon, y=sample289$lat, col=c("#8DD3C7", "#FFFFB3",
"#BEBADA"), labels="", radius=.7)
add.pie(z=c(sample290$K3Q1S1964, sample290$K3Q2S1964, sample290$K3Q3S1964), x=sample290$lon, y=sample290$lat, col=c("#8DD3C7", "#FFFFB3",
"#BEBADA"), labels="", radius=.7)
add.pie(z=c(sample291$K3Q1S1964, sample291$K3Q2S1964, sample291$K3Q3S1964), x=sample291$lon, y=sample291$lat, col=c("#8DD3C7", "#FFFFB3",
"#BEBADA"), labels="", radius=.7)
add.pie(z=c(sample292$K3Q1S1964, sample292$K3Q2S1964, sample292$K3Q3S1964), x=sample292$lon, y=sample292$lat, col=c("#8DD3C7", "#FFFFB3",
"#BEBADA"), labels="", radius=.7)
add.pie(z=c(sample293$K3Q1S1964, sample293$K3Q2S1964, sample293$K3Q3S1964), x=sample293$lon, y=sample293$lat, col=c("#8DD3C7", "#FFFFB3",
"#BEBADA"), labels="", radius=.7)
add.pie(z=c(sample294$K3Q1S1964, sample294$K3Q2S1964, sample294$K3Q3S1964), x=sample294$lon, y=sample294$lat, col=c("#8DD3C7", "#FFFFB3",
"#BEBADA"), labels="", radius=.7)
add.pie(z=c(sample295$K3Q1S1964, sample295$K3Q2S1964, sample295$K3Q3S1964), x=sample295$lon, y=sample295$lat, col=c("#8DD3C7", "#FFFFB3",
"#BEBADA"), labels="", radius=.7)
add.pie(z=c(sample296$K3Q1S1964, sample296$K3Q2S1964, sample296$K3Q3S1964), x=sample296$lon, y=sample296$lat, col=c("#8DD3C7", "#FFFFB3",
"#BEBADA"), labels="", radius=.7)
add.pie(z=c(sample297$K3Q1S1964, sample297$K3Q2S1964, sample297$K3Q3S1964), x=sample297$lon, y=sample297$lat, col=c("#8DD3C7", "#FFFFB3",
"#BEBADA"), labels="", radius=.7)
add.pie(z=c(sample298$K3Q1S1964, sample298$K3Q2S1964, sample298$K3Q3S1964), x=sample298$lon, y=sample298$lat, col=c("#8DD3C7", "#FFFFB3",
"#BEBADA"), labels="", radius=.7)
add.pie(z=c(sample299$K3Q1S1964, sample299$K3Q2S1964, sample299$K3Q3S1964), x=sample299$lon, y=sample299$lat, col=c("#8DD3C7", "#FFFFB3",
"#BEBADA"), labels="", radius=.7)
add.pie(z=c(sample300$K3Q1S1964, sample300$K3Q2S1964, sample300$K3Q3S1964), x=sample300$lon, y=sample300$lat, col=c("#8DD3C7", "#FFFFB3",
"#BEBADA"), labels="", radius=.7)
add.pie(z=c(sample301$K3Q1S1964, sample301$K3Q2S1964, sample301$K3Q3S1964), x=sample301$lon, y=sample301$lat, col=c("#8DD3C7", "#FFFFB3",
"#BEBADA"), labels="", radius=.7)
add.pie(z=c(sample302$K3Q1S1964, sample302$K3Q2S1964, sample302$K3Q3S1964), x=sample302$lon, y=sample302$lat, col=c("#8DD3C7", "#FFFFB3",
"#BEBADA"), labels="", radius=.7)
add.pie(z=c(sample303$K3Q1S1964, sample303$K3Q2S1964, sample303$K3Q3S1964), x=sample303$lon, y=sample303$lat, col=c("#8DD3C7", "#FFFFB3",
"#BEBADA"), labels="", radius=.7)
add.pie(z=c(sample304$K3Q1S1964, sample304$K3Q2S1964, sample304$K3Q3S1964), x=sample304$lon, y=sample304$lat, col=c("#8DD3C7", "#FFFFB3",
"#BEBADA"), labels="", radius=.7)
add.pie(z=c(sample305$K3Q1S1964, sample305$K3Q2S1964, sample305$K3Q3S1964), x=sample305$lon, y=sample305$lat, col=c("#8DD3C7", "#FFFFB3",
"#BEBADA"), labels="", radius=.7)
add.pie(z=c(sample306$K3Q1S1964, sample306$K3Q2S1964, sample306$K3Q3S1964), x=sample306$lon, y=sample306$lat, col=c("#8DD3C7", "#FFFFB3",
"#BEBADA"), labels="", radius=.7)
add.pie(z=c(sample307$K3Q1S1964, sample307$K3Q2S1964, sample307$K3Q3S1964), x=sample307$lon, y=sample307$lat, col=c("#8DD3C7", "#FFFFB3",
"#BEBADA"), labels="", radius=.7)
add.pie(z=c(sample308$K3Q1S1964, sample308$K3Q2S1964, sample308$K3Q3S1964), x=sample308$lon, y=sample308$lat, col=c("#8DD3C7", "#FFFFB3",
"#BEBADA"), labels="", radius=.7)
add.pie(z=c(sample309$K3Q1S1964, sample309$K3Q2S1964, sample309$K3Q3S1964), x=sample309$lon, y=sample309$lat, col=c("#8DD3C7", "#FFFFB3",
"#BEBADA"), labels="", radius=.7)
add.pie(z=c(sample310$K3Q1S1964, sample310$K3Q2S1964, sample310$K3Q3S1964), x=sample310$lon, y=sample310$lat, col=c("#8DD3C7", "#FFFFB3",
"#BEBADA"), labels="", radius=.7)
add.pie(z=c(sample311$K3Q1S1964, sample311$K3Q2S1964, sample311$K3Q3S1964), x=sample311$lon, y=sample311$lat, col=c("#8DD3C7", "#FFFFB3",
"#BEBADA"), labels="", radius=.7)
add.pie(z=c(sample312$K3Q1S1964, sample312$K3Q2S1964, sample312$K3Q3S1964), x=sample312$lon, y=sample312$lat, col=c("#8DD3C7", "#FFFFB3",
"#BEBADA"), labels="", radius=.7)
add.pie(z=c(sample313$K3Q1S1964, sample313$K3Q2S1964, sample313$K3Q3S1964), x=sample313$lon, y=sample313$lat, col=c("#8DD3C7", "#FFFFB3",
"#BEBADA"), labels="", radius=.7)
add.pie(z=c(sample314$K3Q1S1964, sample314$K3Q2S1964, sample314$K3Q3S1964), x=sample314$lon, y=sample314$lat, col=c("#8DD3C7", "#FFFFB3",
"#BEBADA"), labels="", radius=.7)
add.pie(z=c(sample315$K3Q1S1964, sample315$K3Q2S1964, sample315$K3Q3S1964), x=sample315$lon, y=sample315$lat, col=c("#8DD3C7", "#FFFFB3",
"#BEBADA"), labels="", radius=.7)
add.pie(z=c(sample316$K3Q1S1964, sample316$K3Q2S1964, sample316$K3Q3S1964), x=sample316$lon, y=sample316$lat, col=c("#8DD3C7", "#FFFFB3",
"#BEBADA"), labels="", radius=.7)
add.pie(z=c(sample317$K3Q1S1964, sample317$K3Q2S1964, sample317$K3Q3S1964), x=sample317$lon, y=sample317$lat, col=c("#8DD3C7", "#FFFFB3",
"#BEBADA"), labels="", radius=.7)
add.pie(z=c(sample318$K3Q1S1964, sample318$K3Q2S1964, sample318$K3Q3S1964), x=sample318$lon, y=sample318$lat, col=c("#8DD3C7", "#FFFFB3",
"#BEBADA"), labels="", radius=.7)
add.pie(z=c(sample319$K3Q1S1964, sample319$K3Q2S1964, sample319$K3Q3S1964), x=sample319$lon, y=sample319$lat, col=c("#8DD3C7", "#FFFFB3",
"#BEBADA"), labels="", radius=.7)
add.pie(z=c(sample320$K3Q1S1964, sample320$K3Q2S1964, sample320$K3Q3S1964), x=sample320$lon, y=sample320$lat, col=c("#8DD3C7", "#FFFFB3",
"#BEBADA"), labels="", radius=.7)
add.pie(z=c(sample321$K3Q1S1964, sample321$K3Q2S1964, sample321$K3Q3S1964), x=sample321$lon, y=sample321$lat, col=c("#8DD3C7", "#FFFFB3",
"#BEBADA"), labels="", radius=.7)
add.pie(z=c(sample322$K3Q1S1964, sample322$K3Q2S1964, sample322$K3Q3S1964), x=sample322$lon, y=sample322$lat, col=c("#8DD3C7", "#FFFFB3",
"#BEBADA"), labels="", radius=.7)
add.pie(z=c(sample323$K3Q1S1964, sample323$K3Q2S1964, sample323$K3Q3S1964), x=sample323$lon, y=sample323$lat, col=c("#8DD3C7", "#FFFFB3",
"#BEBADA"), labels="", radius=.7)
add.pie(z=c(sample324$K3Q1S1964, sample324$K3Q2S1964, sample324$K3Q3S1964), x=sample324$lon, y=sample324$lat, col=c("#8DD3C7", "#FFFFB3",
"#BEBADA"), labels="", radius=.7)
add.pie(z=c(sample325$K3Q1S1964, sample325$K3Q2S1964, sample325$K3Q3S1964), x=sample325$lon, y=sample325$lat, col=c("#8DD3C7", "#FFFFB3",
"#BEBADA"), labels="", radius=.7)
add.pie(z=c(sample326$K3Q1S1964, sample326$K3Q2S1964, sample326$K3Q3S1964), x=sample326$lon, y=sample326$lat, col=c("#8DD3C7", "#FFFFB3",
"#BEBADA"), labels="", radius=.7)
add.pie(z=c(sample327$K3Q1S1964, sample327$K3Q2S1964, sample327$K3Q3S1964), x=sample327$lon, y=sample327$lat, col=c("#8DD3C7", "#FFFFB3",
"#BEBADA"), labels="", radius=.7)
add.pie(z=c(sample328$K3Q1S1964, sample328$K3Q2S1964, sample328$K3Q3S1964), x=sample328$lon, y=sample328$lat, col=c("#8DD3C7", "#FFFFB3",
"#BEBADA"), labels="", radius=.7)
add.pie(z=c(sample329$K3Q1S1964, sample329$K3Q2S1964, sample329$K3Q3S1964), x=sample329$lon, y=sample329$lat, col=c("#8DD3C7", "#FFFFB3",
"#BEBADA"), labels="", radius=.7)
add.pie(z=c(sample330$K3Q1S1964, sample330$K3Q2S1964, sample330$K3Q3S1964), x=sample330$lon, y=sample330$lat, col=c("#8DD3C7", "#FFFFB3",
"#BEBADA"), labels="", radius=.7)
add.pie(z=c(sample331$K3Q1S1964, sample331$K3Q2S1964, sample331$K3Q3S1964), x=sample331$lon, y=sample331$lat, col=c("#8DD3C7", "#FFFFB3",
"#BEBADA"), labels="", radius=.7)
add.pie(z=c(sample332$K3Q1S1964, sample332$K3Q2S1964, sample332$K3Q3S1964), x=sample332$lon, y=sample332$lat, col=c("#8DD3C7", "#FFFFB3",
"#BEBADA"), labels="", radius=.7)
add.pie(z=c(sample333$K3Q1S1964, sample333$K3Q2S1964, sample333$K3Q3S1964), x=sample333$lon, y=sample333$lat, col=c("#8DD3C7", "#FFFFB3",
"#BEBADA"), labels="", radius=.7)
add.pie(z=c(sample334$K3Q1S1964, sample334$K3Q2S1964, sample334$K3Q3S1964), x=sample334$lon, y=sample334$lat, col=c("#8DD3C7", "#FFFFB3",
"#BEBADA"), labels="", radius=.7)
add.pie(z=c(sample335$K3Q1S1964, sample335$K3Q2S1964, sample335$K3Q3S1964), x=sample335$lon, y=sample335$lat, col=c("#8DD3C7", "#FFFFB3",
"#BEBADA"), labels="", radius=.7)
add.pie(z=c(sample336$K3Q1S1964, sample336$K3Q2S1964, sample336$K3Q3S1964), x=sample336$lon, y=sample336$lat, col=c("#8DD3C7", "#FFFFB3",
"#BEBADA"), labels="", radius=.7)
add.pie(z=c(sample337$K3Q1S1964, sample337$K3Q2S1964, sample337$K3Q3S1964), x=sample337$lon, y=sample337$lat, col=c("#8DD3C7", "#FFFFB3",
"#BEBADA"), labels="", radius=.7)
add.pie(z=c(sample338$K3Q1S1964, sample338$K3Q2S1964, sample338$K3Q3S1964), x=sample338$lon, y=sample338$lat, col=c("#8DD3C7", "#FFFFB3",
"#BEBADA"), labels="", radius=.7)
add.pie(z=c(sample339$K3Q1S1964, sample339$K3Q2S1964, sample339$K3Q3S1964), x=sample339$lon, y=sample339$lat, col=c("#8DD3C7", "#FFFFB3",
"#BEBADA"), labels="", radius=.7)
add.pie(z=c(sample340$K3Q1S1964, sample340$K3Q2S1964, sample340$K3Q3S1964), x=sample340$lon, y=sample340$lat, col=c("#8DD3C7", "#FFFFB3",
"#BEBADA"), labels="", radius=.7)
add.pie(z=c(sample341$K3Q1S1964, sample341$K3Q2S1964, sample341$K3Q3S1964), x=sample341$lon, y=sample341$lat, col=c("#8DD3C7", "#FFFFB3",
"#BEBADA"), labels="", radius=.7)
add.pie(z=c(sample342$K3Q1S1964, sample342$K3Q2S1964, sample342$K3Q3S1964), x=sample342$lon, y=sample342$lat, col=c("#8DD3C7", "#FFFFB3",
"#BEBADA"), labels="", radius=.7)
add.pie(z=c(sample343$K3Q1S1964, sample343$K3Q2S1964, sample343$K3Q3S1964), x=sample343$lon, y=sample343$lat, col=c("#8DD3C7", "#FFFFB3",
"#BEBADA"), labels="", radius=.7)
add.pie(z=c(sample344$K3Q1S1964, sample344$K3Q2S1964, sample344$K3Q3S1964), x=sample344$lon, y=sample344$lat, col=c("#8DD3C7", "#FFFFB3",
"#BEBADA"), labels="", radius=.7)
add.pie(z=c(sample345$K3Q1S1964, sample345$K3Q2S1964, sample345$K3Q3S1964), x=sample345$lon, y=sample345$lat, col=c("#8DD3C7", "#FFFFB3",
"#BEBADA"), labels="", radius=.7)
add.pie(z=c(sample346$K3Q1S1964, sample346$K3Q2S1964, sample346$K3Q3S1964), x=sample346$lon, y=sample346$lat, col=c("#8DD3C7", "#FFFFB3",
"#BEBADA"), labels="", radius=.7)
add.pie(z=c(sample347$K3Q1S1964, sample347$K3Q2S1964, sample347$K3Q3S1964), x=sample347$lon, y=sample347$lat, col=c("#8DD3C7", "#FFFFB3",
"#BEBADA"), labels="", radius=.7)
add.pie(z=c(sample348$K3Q1S1964, sample348$K3Q2S1964, sample348$K3Q3S1964), x=sample348$lon, y=sample348$lat, col=c("#8DD3C7", "#FFFFB3",
"#BEBADA"), labels="", radius=.7)
add.pie(z=c(sample349$K3Q1S1964, sample349$K3Q2S1964, sample349$K3Q3S1964), x=sample349$lon, y=sample349$lat, col=c("#8DD3C7", "#FFFFB3",
"#BEBADA"), labels="", radius=.7)
add.pie(z=c(sample350$K3Q1S1964, sample350$K3Q2S1964, sample350$K3Q3S1964), x=sample350$lon, y=sample350$lat, col=c("#8DD3C7", "#FFFFB3",
"#BEBADA"), labels="", radius=.7)
add.pie(z=c(sample351$K3Q1S1964, sample351$K3Q2S1964, sample351$K3Q3S1964), x=sample351$lon, y=sample351$lat, col=c("#8DD3C7", "#FFFFB3",
"#BEBADA"), labels="", radius=.7)
add.pie(z=c(sample352$K3Q1S1964, sample352$K3Q2S1964, sample352$K3Q3S1964), x=sample352$lon, y=sample352$lat, col=c("#8DD3C7", "#FFFFB3",
"#BEBADA"), labels="", radius=.7)
add.pie(z=c(sample353$K3Q1S1964, sample353$K3Q2S1964, sample353$K3Q3S1964), x=sample353$lon, y=sample353$lat, col=c("#8DD3C7", "#FFFFB3",
"#BEBADA"), labels="", radius=.7)
add.pie(z=c(sample354$K3Q1S1964, sample354$K3Q2S1964, sample354$K3Q3S1964), x=sample354$lon, y=sample354$lat, col=c("#8DD3C7", "#FFFFB3",
"#BEBADA"), labels="", radius=.7)
add.pie(z=c(sample355$K3Q1S1964, sample355$K3Q2S1964, sample355$K3Q3S1964), x=sample355$lon, y=sample355$lat, col=c("#8DD3C7", "#FFFFB3",
"#BEBADA"), labels="", radius=.7)
add.pie(z=c(sample356$K3Q1S1964, sample356$K3Q2S1964, sample356$K3Q3S1964), x=sample356$lon, y=sample356$lat, col=c("#8DD3C7", "#FFFFB3",
"#BEBADA"), labels="", radius=.7)
add.pie(z=c(sample357$K3Q1S1964, sample357$K3Q2S1964, sample357$K3Q3S1964), x=sample357$lon, y=sample357$lat, col=c("#8DD3C7", "#FFFFB3",
"#BEBADA"), labels="", radius=.7)
add.pie(z=c(sample358$K3Q1S1964, sample358$K3Q2S1964, sample358$K3Q3S1964), x=sample358$lon, y=sample358$lat, col=c("#8DD3C7", "#FFFFB3",
"#BEBADA"), labels="", radius=.7)
add.pie(z=c(sample359$K3Q1S1964, sample359$K3Q2S1964, sample359$K3Q3S1964), x=sample359$lon, y=sample359$lat, col=c("#8DD3C7", "#FFFFB3",
"#BEBADA"), labels="", radius=.7)
add.pie(z=c(sample360$K3Q1S1964, sample360$K3Q2S1964, sample360$K3Q3S1964), x=sample360$lon, y=sample360$lat, col=c("#8DD3C7", "#FFFFB3",
"#BEBADA"), labels="", radius=.7)
add.pie(z=c(sample361$K3Q1S1964, sample361$K3Q2S1964, sample361$K3Q3S1964), x=sample361$lon, y=sample361$lat, col=c("#8DD3C7", "#FFFFB3",
"#BEBADA"), labels="", radius=.7)
add.pie(z=c(sample362$K3Q1S1964, sample362$K3Q2S1964, sample362$K3Q3S1964), x=sample362$lon, y=sample362$lat, col=c("#8DD3C7", "#FFFFB3",
"#BEBADA"), labels="", radius=.7)
dev.off()

#pre1990
#pre1990$SampleN #210-334
pdf("Contemp.pdf", width=6, height=6)
plot(newmap, xlim=c(-105,-60), ylim=c(25,50), main="1998-Present")
add.pie(z=c(sample363$K3Q1S1964, sample363$K3Q2S1964, sample363$K3Q3S1964), x=sample363$lon, y=sample363$lat, col=c("#8DD3C7", "#FFFFB3",
"#BEBADA"), labels="", radius=.7)
add.pie(z=c(sample364$K3Q1S1964, sample364$K3Q2S1964, sample364$K3Q3S1964), x=sample364$lon, y=sample364$lat, col=c("#8DD3C7", "#FFFFB3",
"#BEBADA"), labels="", radius=.7)
add.pie(z=c(sample365$K3Q1S1964, sample365$K3Q2S1964, sample365$K3Q3S1964), x=sample365$lon, y=sample365$lat, col=c("#8DD3C7", "#FFFFB3",
"#BEBADA"), labels="", radius=.7)
add.pie(z=c(sample366$K3Q1S1964, sample366$K3Q2S1964, sample366$K3Q3S1964), x=sample366$lon, y=sample366$lat, col=c("#8DD3C7", "#FFFFB3",
"#BEBADA"), labels="", radius=.7)
add.pie(z=c(sample367$K3Q1S1964, sample367$K3Q2S1964, sample367$K3Q3S1964), x=sample367$lon, y=sample367$lat, col=c("#8DD3C7", "#FFFFB3",
"#BEBADA"), labels="", radius=.7)
add.pie(z=c(sample368$K3Q1S1964, sample368$K3Q2S1964, sample368$K3Q3S1964), x=sample368$lon, y=sample368$lat, col=c("#8DD3C7", "#FFFFB3",
"#BEBADA"), labels="", radius=.7)
add.pie(z=c(sample369$K3Q1S1964, sample369$K3Q2S1964, sample369$K3Q3S1964), x=sample369$lon, y=sample369$lat, col=c("#8DD3C7", "#FFFFB3",
"#BEBADA"), labels="", radius=.7)
add.pie(z=c(sample370$K3Q1S1964, sample370$K3Q2S1964, sample370$K3Q3S1964), x=sample370$lon, y=sample370$lat, col=c("#8DD3C7", "#FFFFB3",
"#BEBADA"), labels="", radius=.7)
add.pie(z=c(sample371$K3Q1S1964, sample371$K3Q2S1964, sample371$K3Q3S1964), x=sample371$lon, y=sample371$lat, col=c("#8DD3C7", "#FFFFB3",
"#BEBADA"), labels="", radius=.7)
add.pie(z=c(sample372$K3Q1S1964, sample372$K3Q2S1964, sample372$K3Q3S1964), x=sample372$lon, y=sample372$lat, col=c("#8DD3C7", "#FFFFB3",
"#BEBADA"), labels="", radius=.7)
add.pie(z=c(sample373$K3Q1S1964, sample373$K3Q2S1964, sample373$K3Q3S1964), x=sample373$lon, y=sample373$lat, col=c("#8DD3C7", "#FFFFB3",
"#BEBADA"), labels="", radius=.7)
add.pie(z=c(sample374$K3Q1S1964, sample374$K3Q2S1964, sample374$K3Q3S1964), x=sample374$lon, y=sample374$lat, col=c("#8DD3C7", "#FFFFB3",
"#BEBADA"), labels="", radius=.7)
add.pie(z=c(sample375$K3Q1S1964, sample375$K3Q2S1964, sample375$K3Q3S1964), x=sample375$lon, y=sample375$lat, col=c("#8DD3C7", "#FFFFB3",
"#BEBADA"), labels="", radius=.7)
add.pie(z=c(sample376$K3Q1S1964, sample376$K3Q2S1964, sample376$K3Q3S1964), x=sample376$lon, y=sample376$lat, col=c("#8DD3C7", "#FFFFB3",
"#BEBADA"), labels="", radius=.7)
add.pie(z=c(sample377$K3Q1S1964, sample377$K3Q2S1964, sample377$K3Q3S1964), x=sample377$lon, y=sample377$lat, col=c("#8DD3C7", "#FFFFB3",
"#BEBADA"), labels="", radius=.7)
add.pie(z=c(sample378$K3Q1S1964, sample378$K3Q2S1964, sample378$K3Q3S1964), x=sample378$lon, y=sample378$lat, col=c("#8DD3C7", "#FFFFB3",
"#BEBADA"), labels="", radius=.7)
add.pie(z=c(sample379$K3Q1S1964, sample379$K3Q2S1964, sample379$K3Q3S1964), x=sample379$lon, y=sample379$lat, col=c("#8DD3C7", "#FFFFB3",
"#BEBADA"), labels="", radius=.7)
add.pie(z=c(sample380$K3Q1S1964, sample380$K3Q2S1964, sample380$K3Q3S1964), x=sample380$lon, y=sample380$lat, col=c("#8DD3C7", "#FFFFB3",
"#BEBADA"), labels="", radius=.7)
add.pie(z=c(sample381$K3Q1S1964, sample381$K3Q2S1964, sample381$K3Q3S1964), x=sample381$lon, y=sample381$lat, col=c("#8DD3C7", "#FFFFB3",
"#BEBADA"), labels="", radius=.7)
add.pie(z=c(sample382$K3Q1S1964, sample382$K3Q2S1964, sample382$K3Q3S1964), x=sample382$lon, y=sample382$lat, col=c("#8DD3C7", "#FFFFB3",
"#BEBADA"), labels="", radius=.7)
add.pie(z=c(sample383$K3Q1S1964, sample383$K3Q2S1964, sample383$K3Q3S1964), x=sample383$lon, y=sample383$lat, col=c("#8DD3C7", "#FFFFB3",
"#BEBADA"), labels="", radius=.7)
add.pie(z=c(sample384$K3Q1S1964, sample384$K3Q2S1964, sample384$K3Q3S1964), x=sample384$lon, y=sample384$lat, col=c("#8DD3C7", "#FFFFB3",
"#BEBADA"), labels="", radius=.7)
add.pie(z=c(sample385$K3Q1S1964, sample385$K3Q2S1964, sample385$K3Q3S1964), x=sample385$lon, y=sample385$lat, col=c("#8DD3C7", "#FFFFB3",
"#BEBADA"), labels="", radius=.7)
add.pie(z=c(sample386$K3Q1S1964, sample386$K3Q2S1964, sample386$K3Q3S1964), x=sample386$lon, y=sample386$lat, col=c("#8DD3C7", "#FFFFB3",
"#BEBADA"), labels="", radius=.7)
add.pie(z=c(sample387$K3Q1S1964, sample387$K3Q2S1964, sample387$K3Q3S1964), x=sample387$lon, y=sample387$lat, col=c("#8DD3C7", "#FFFFB3",
"#BEBADA"), labels="", radius=.7)
add.pie(z=c(sample388$K3Q1S1964, sample388$K3Q2S1964, sample388$K3Q3S1964), x=sample388$lon, y=sample388$lat, col=c("#8DD3C7", "#FFFFB3",
"#BEBADA"), labels="", radius=.7)
add.pie(z=c(sample389$K3Q1S1964, sample389$K3Q2S1964, sample389$K3Q3S1964), x=sample389$lon, y=sample389$lat, col=c("#8DD3C7", "#FFFFB3",
"#BEBADA"), labels="", radius=.7)
add.pie(z=c(sample390$K3Q1S1964, sample390$K3Q2S1964, sample390$K3Q3S1964), x=sample390$lon, y=sample390$lat, col=c("#8DD3C7", "#FFFFB3",
"#BEBADA"), labels="", radius=.7)
add.pie(z=c(sample391$K3Q1S1964, sample391$K3Q2S1964, sample391$K3Q3S1964), x=sample391$lon, y=sample391$lat, col=c("#8DD3C7", "#FFFFB3",
"#BEBADA"), labels="", radius=.7)
add.pie(z=c(sample392$K3Q1S1964, sample392$K3Q2S1964, sample392$K3Q3S1964), x=sample392$lon, y=sample392$lat, col=c("#8DD3C7", "#FFFFB3",
"#BEBADA"), labels="", radius=.7)
add.pie(z=c(sample393$K3Q1S1964, sample393$K3Q2S1964, sample393$K3Q3S1964), x=sample393$lon, y=sample393$lat, col=c("#8DD3C7", "#FFFFB3",
"#BEBADA"), labels="", radius=.7)
add.pie(z=c(sample394$K3Q1S1964, sample394$K3Q2S1964, sample394$K3Q3S1964), x=sample394$lon, y=sample394$lat, col=c("#8DD3C7", "#FFFFB3",
"#BEBADA"), labels="", radius=.7)
add.pie(z=c(sample395$K3Q1S1964, sample395$K3Q2S1964, sample395$K3Q3S1964), x=sample395$lon, y=sample395$lat, col=c("#8DD3C7", "#FFFFB3",
"#BEBADA"), labels="", radius=.7)
add.pie(z=c(sample396$K3Q1S1964, sample396$K3Q2S1964, sample396$K3Q3S1964), x=sample396$lon, y=sample396$lat, col=c("#8DD3C7", "#FFFFB3",
"#BEBADA"), labels="", radius=.7)
add.pie(z=c(sample397$K3Q1S1964, sample397$K3Q2S1964, sample397$K3Q3S1964), x=sample397$lon, y=sample397$lat, col=c("#8DD3C7", "#FFFFB3",
"#BEBADA"), labels="", radius=.7)
add.pie(z=c(sample398$K3Q1S1964, sample398$K3Q2S1964, sample398$K3Q3S1964), x=sample398$lon, y=sample398$lat, col=c("#8DD3C7", "#FFFFB3",
"#BEBADA"), labels="", radius=.7)
add.pie(z=c(sample399$K3Q1S1964, sample399$K3Q2S1964, sample399$K3Q3S1964), x=sample399$lon, y=sample399$lat, col=c("#8DD3C7", "#FFFFB3",
"#BEBADA"), labels="", radius=.7)
add.pie(z=c(sample400$K3Q1S1964, sample400$K3Q2S1964, sample400$K3Q3S1964), x=sample400$lon, y=sample400$lat, col=c("#8DD3C7", "#FFFFB3",
"#BEBADA"), labels="", radius=.7)
add.pie(z=c(sample401$K3Q1S1964, sample401$K3Q2S1964, sample401$K3Q3S1964), x=sample401$lon, y=sample401$lat, col=c("#8DD3C7", "#FFFFB3",
"#BEBADA"), labels="", radius=.7)
add.pie(z=c(sample402$K3Q1S1964, sample402$K3Q2S1964, sample402$K3Q3S1964), x=sample402$lon, y=sample402$lat, col=c("#8DD3C7", "#FFFFB3",
"#BEBADA"), labels="", radius=.7)
add.pie(z=c(sample403$K3Q1S1964, sample403$K3Q2S1964, sample403$K3Q3S1964), x=sample403$lon, y=sample403$lat, col=c("#8DD3C7", "#FFFFB3",
"#BEBADA"), labels="", radius=.7)
add.pie(z=c(sample404$K3Q1S1964, sample404$K3Q2S1964, sample404$K3Q3S1964), x=sample404$lon, y=sample404$lat, col=c("#8DD3C7", "#FFFFB3",
"#BEBADA"), labels="", radius=.7)
add.pie(z=c(sample405$K3Q1S1964, sample405$K3Q2S1964, sample405$K3Q3S1964), x=sample405$lon, y=sample405$lat, col=c("#8DD3C7", "#FFFFB3",
"#BEBADA"), labels="", radius=.7)
add.pie(z=c(sample406$K3Q1S1964, sample406$K3Q2S1964, sample406$K3Q3S1964), x=sample406$lon, y=sample406$lat, col=c("#8DD3C7", "#FFFFB3",
"#BEBADA"), labels="", radius=.7)
add.pie(z=c(sample407$K3Q1S1964, sample407$K3Q2S1964, sample407$K3Q3S1964), x=sample407$lon, y=sample407$lat, col=c("#8DD3C7", "#FFFFB3",
"#BEBADA"), labels="", radius=.7)
add.pie(z=c(sample408$K3Q1S1964, sample408$K3Q2S1964, sample408$K3Q3S1964), x=sample408$lon, y=sample408$lat, col=c("#8DD3C7", "#FFFFB3",
"#BEBADA"), labels="", radius=.7)
add.pie(z=c(sample409$K3Q1S1964, sample409$K3Q2S1964, sample409$K3Q3S1964), x=sample409$lon, y=sample409$lat, col=c("#8DD3C7", "#FFFFB3",
"#BEBADA"), labels="", radius=.7)
add.pie(z=c(sample410$K3Q1S1964, sample410$K3Q2S1964, sample410$K3Q3S1964), x=sample410$lon, y=sample410$lat, col=c("#8DD3C7", "#FFFFB3",
"#BEBADA"), labels="", radius=.7)
add.pie(z=c(sample411$K3Q1S1964, sample411$K3Q2S1964, sample411$K3Q3S1964), x=sample411$lon, y=sample411$lat, col=c("#8DD3C7", "#FFFFB3",
"#BEBADA"), labels="", radius=.7)
add.pie(z=c(sample412$K3Q1S1964, sample412$K3Q2S1964, sample412$K3Q3S1964), x=sample412$lon, y=sample412$lat, col=c("#8DD3C7", "#FFFFB3",
"#BEBADA"), labels="", radius=.7)
add.pie(z=c(sample413$K3Q1S1964, sample413$K3Q2S1964, sample413$K3Q3S1964), x=sample413$lon, y=sample413$lat, col=c("#8DD3C7", "#FFFFB3",
"#BEBADA"), labels="", radius=.7)
add.pie(z=c(sample414$K3Q1S1964, sample414$K3Q2S1964, sample414$K3Q3S1964), x=sample414$lon, y=sample414$lat, col=c("#8DD3C7", "#FFFFB3",
"#BEBADA"), labels="", radius=.7)
dev.off()

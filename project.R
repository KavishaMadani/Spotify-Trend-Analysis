data=read.csv(file.choose("1 Spotify Trend Analysis Dataset"))
data
View(data)
attach(data)
Top.Genre
data50=read.csv(file.choose("1950-59"))
data50
bpm50=data50$Beats.Per.Minute..BPM.
bpm50
er50=data50$Energy
er50
da50=data50$Danceability
da50
lo50=data50$Loudness..dB.
lo50
li50=data50$Liveness
li50
va50=data50$Valence
va50
ld50=data50$Length..Duration.
ld50
ac50=data50$Acousticness
ac50
sp50=data50$Speechiness
sp50
po50=data50$Popularity
po50
data60=read.csv(file.choose("1960-69"))
data60
bpm60=data60$Beats.Per.Minute..BPM.
bpm60
er60=data60$Energy
er60
da60=data60$Danceability
da60
lo60=data60$Loudness..dB.
lo60
li60=data60$Liveness
li60
va60=data60$Valence
va60
ld60=data60$Length..Duration.
ld60
ac60=data60$Acousticness
ac60
sp60=data60$Speechiness
sp60
po60=data60$Popularity
po60
data70=read.csv(file.choose("1970-79"))
data70
bpm70=data70$Beats.Per.Minute..BPM.
bpm70
er70=data70$Energy
er70
da70=data70$Danceability
da70
lo70=data70$Loudness..dB.
lo70
li70=data70$Liveness
li70
va70=data70$Valence
va70
ld70=data70$Length..Duration.
ld70
ac70=data70$Acousticness
ac70
sp70=data70$Speechiness
sp70
po70=data70$Popularity
po70
data80=read.csv(file.choose("1980-89"))
data80
bpm80=data80$Beats.Per.Minute..BPM.
bpm80
er80=data80$Energy
er80
da80=data80$Danceability
da80
lo80=data80$Loudness..dB.
lo80
li80=data80$Liveness
li80
va80=data80$Valence
va80
ld80=data80$Length..Duration.
ld80
ac80=data80$Acousticness
ac80
sp80=data80$Speechiness
sp80
po80=data80$Popularity
po80
data90=read.csv(file.choose("1990-99"))
data90
bpm90=data90$Beats.Per.Minute..BPM.
bpm90
er90=data90$Energy
er90
da90=data90$Danceability
da90
lo90=data90$Loudness..dB.
lo90
li90=data90$Liveness
li90
va90=data90$Valence
va90
ld90=data90$Length..Duration.
ld90
ac90=data90$Acousticness
ac90
sp90=data90$Speechiness
sp90
po90=data90$Popularity
po90
data00=read.csv(file.choose("2000-09"))
data00
bpm00=data00$Beats.Per.Minute..BPM.
bpm00
er00=data00$Energy
er00
da00=data00$Danceability
da00
lo00=data00$Loudness..dB.
lo00
li00=data00$Liveness
li00
va00=data00$Valence
va00
ld00=data00$Length..Duration.
ld00
ac00=data00$Acousticness
ac00
sp00=data00$Speechiness
sp00
po00=data00$Popularity
po00
data10=read.csv(file.choose("2010-19"))
data10
bpm10=data10$Beats.Per.Minute..BPM.
bpm10
er10=data10$Energy
er10
da10=data10$Danceability
da10
lo10=data10$Loudness..dB.
lo10
li10=data10$Liveness
li10
va10=data10$Valence
va10
ld10=data10$Length..Duration.
ld10
ac10=data10$Acousticness
ac10
sp10=data10$Speechiness
sp10
po10=data10$Popularity
po10
pomean50=mean(po50)
pomean50
pomean60=mean(po60)
pomean60
pomean70=mean(po70)
pomean70
pomean80=mean(po80)
pomean80
pomean90=mean(po90)
pomean90
pomean00=mean(po00)
pomean00
pomean10=mean(po10)
pomean10
pop=c(pomean50,pomean60,pomean70,pomean80,pomean90,pomean00,pomean10)
pop
decade=c("1950-59","1960-69","1970-79","1980-89","1990-99","2000-09","2010-19")
popdec=data.frame(decade,pop)
popdec
barplot(popdec$pop,xlab = "decade", ylab = "popularity", main = "Popularity", ylim = c(0,70), names = decade , las = 1 , cex.names = 0.76)

#Is there a trend in genres preferred back in the day vs now?
tg50=data50$Top.Genre
tg50
# Genre "Adult Standards" was most popular in 1950-59
tg60=data60$Top.Genre
tg60
table_tg60=table(tg60)
table_tg60
# Genre "Album Rock" was most popular in 1960-69
tg70=data70$Top.Genre
tg70
dftg70=data.frame(table(tg70))
dftg70
# Genre "Album Rock" was most popular in 1970-79
tg80=data80$Top.Genre
tg80
dftg80=data.frame(table(tg80))
dftg80
# Genre "Album Rock" was most popular in 1980-89
tg90=data90$Top.Genre
tg90
dftg90=data.frame(table(tg90))
dftg90
# Genre "Alternative Rock" was most popular in 1990-99
tg00=data00$Top.Genre
tg00
dftg00=data.frame(table(tg00))
dftg00
# Genre "Alternative Metal" was most popular in 2000-09
tg10=data10$Top.Genre
tg10
dftg10=data.frame(table(tg10))
dftg10
# Genre "Dutch Pop" was most popular in 2010-19

#Is popularity dependent on danceability of the songs?
cor(data$Popularity,data$Danceability)
#Is popularity dependent on BPM of the songs?
cor(data$Popularity,data$Beats.Per.Minute..BPM.)
#Is popularity dependent on energy of the songs?
cor(data$Popularity,data$Energy)
#Is popularity dependent on loudness of the songs?
cor(data$Popularity,data$Loudness..dB.)
#Is popularity dependent on liveness of the songs?
cor(data$Popularity,data$Liveness)
#Is popularity dependent on valence of the songs?
cor(data$Popularity,data$Valence)
#Is popularity dependent on Length Duration of the songs?
cor(data$Popularity,data$Length..Duration.)
#Is popularity dependent on Acousticness of the songs?
cor(data$Popularity,data$Acousticness)
#Is popularity dependent on Speechiness of the songs?
cor(data$Popularity,data$Speechiness)

# So popularity of song mainly depend on Liveness, Loudness, Danceablilty and Speechiness

#What is the average tempo of songs compared over the years?
bpmmean50=mean(bpm50)
bpmmean50
bpmmean60=mean(bpm60)
bpmmean60
bpmmean70=mean(bpm70)
bpmmean70
bpmmean80=mean(bpm80)
bpmmean80
bpmmean90=mean(bpm90)
bpmmean90
bpmmean00=mean(bpm00)
bpmmean00
bpmmean10=mean(bpm10)
bpmmean10
bpm=c(bpmmean50,bpmmean60,bpmmean70,bpmmean80,bpmmean90,bpmmean00,bpmmean10)
bpm
decade=c("1950-59","1960-69","1970-79","1980-89","1990-99","2000-09","2010-19")
bpmdec=data.frame(decade,bpm)
bpmdec
barplot(bpmdec$bpm,xlab = "decade", ylab = "Beats Per Minute", main = "Beats Per Minute", ylim = c(0,160), names = decade , las = 1 , cex.names = 0.76)

#Average tempo was higher in 1950-59 than any other decades 

#Is there a trend of acoustic songs being popular back in 1960s than they are now?
po60
ac60
plot(po60,ac60)
cor.test(po60,ac60)
#Hence there is no trend of acoustic songs being popular back in1960s

#Which artist has the most trending songs?
table(data50$Artist)     # Elvis Presley  4
table(data60$Artist)    # The Beatles  27
table(data70$Artist)     #ABBA 15
table(data80$Artist)     # U2    14
table(data90$Artist)    # George Micheal 15
table(data00$Artist)     # Coldplay    16
table(data10$Artist)    # Ed Sheeran   12
# The beatles has the most trending songs.

#Which decade has the most trending songs?
# Decade 1960-69 has most trending songs

#Is there a correlation between energy and loudness of trending songs?
cor(data$Energy,data$Loudness..dB.)
#Is there a correlation between valence and danceability of trending songs?
cor(data$Valence,data$Danceability)  
#Is there a correlation between acousticness and energy of trending songs?
cor(data$Acousticness,data$Energy) 

data
datatg=data[data$Top.Genre=="Album Rock",]
datatg


#how length duration has changed over years?
ld=data$Length..Duration.
ld
plot(ld,type = "l")


ldmean50=mean(ld50)
ldmean50
ldmean60=mean(ld60)
ldmean60
ldmean70=mean(ld70)
ldmean70
ldmean80=mean(ld80)
ldmean80
ldmean90=mean(ld90)
ldmean90
ldmean00=mean(ld00)
ldmean00
ldmean10=mean(ld10)
ldmean10
lddf=c(ldmean50,ldmean60,ldmean70,ldmean80,ldmean90,ldmean00,ldmean10)
lddf
decade=c("1950-59","1960-69","1970-79","1980-89","1990-99","2000-09","2010-19")
lddec=data.frame(decade,lddf)
lddec
barplot(lddec$lddf,xlab = "decade", ylab = "Length Duration", main = "Length Duration", ylim = c(0,400), names = decade , las = 1 , cex.names = 0.76)



ermean50=mean(er50)
ermean50
ermean60=mean(er60)
ermean60
ermean70=mean(er70)
ermean70
ermean80=mean(er80)
ermean80
ermean90=mean(er90)
ermean90
ermean00=mean(er00)
ermean00
ermean10=mean(er10)
ermean10
erdf=c(ermean50,ermean60,ermean70,ermean80,ermean90,ermean00,ermean10)
erdf
decade=c("1950-59","1960-69","1970-79","1980-89","1990-99","2000-09","2010-19")
erdec=data.frame(decade,erdf)
erdec
barplot(erdec$erdf,xlab = "decade", ylab = "Energy", main = "Energy", ylim = c(0,70), names = decade , las = 1 , cex.names = 0.76)


damean50=mean(da50)
damean50
damean60=mean(da60)
damean60
damean70=mean(da70)
damean70
damean80=mean(da80)
damean80
damean90=mean(da90)
damean90
damean00=mean(da00)
damean00
damean10=mean(da10)
damean10
dadf=c(damean50,damean60,damean70,damean80,damean90,damean00,damean10)
dadf
decade=c("1950-59","1960-69","1970-79","1980-89","1990-99","2000-09","2010-19")
dadec=data.frame(decade,dadf)
dadec
barplot(dadec$dadf,xlab = "decade", ylab = "Danceability", main = "Danceability", ylim = c(0,60), names = decade , las = 1 , cex.names = 0.76)


limean50=mean(li50)
limean50
limean60=mean(li60)
limean60
limean70=mean(li70)
limean70
limean80=mean(li80)
limean80
limean90=mean(li90)
limean90
limean00=mean(li00)
limean00
limean10=mean(li10)
limean10
lidf=c(limean50,limean60,limean70,limean80,limean90,limean00,limean10)
lidf
decade=c("1950-59","1960-69","1970-79","1980-89","1990-99","2000-09","2010-19")
lidec=data.frame(decade,lidf)
lidec
barplot(lidec$lidf,xlab = "decade", ylab = "Liveness", main = "Liveness", ylim = c(0,25), names = decade , las = 1 , cex.names = 0.76)

vamean50=mean(va50)
vamean50
vamean60=mean(va60)
vamean60
vamean70=mean(va70)
vamean70
vamean80=mean(va80)
vamean80
vamean90=mean(va90)
vamean90
vamean00=mean(va00)
vamean00
vamean10=mean(va10)
vamean10
vadf=c(vamean50,vamean60,vamean70,vamean80,vamean90,vamean00,vamean10)
vadf
decade=c("1950-59","1960-69","1970-79","1980-89","1990-99","2000-09","2010-19")
vadec=data.frame(decade,vadf)
vadec
barplot(vadec$vadf,xlab = "decade", ylab = "Valence", main = "Valence", ylim = c(0,80), names = decade , las = 1 , cex.names = 0.76)

acmean50=mean(ac50)
acmean50
acmean60=mean(ac60)
acmean60
acmean70=mean(ac70)
acmean70
acmean80=mean(ac80)
acmean80
acmean90=mean(ac90)
acmean90
acmean00=mean(ac00)
acmean00
acmean10=mean(ac10)
acmean10
acdf=c(acmean50,acmean60,acmean70,acmean80,acmean90,acmean00,acmean10)
acdf
decade=c("1950-59","1960-69","1970-79","1980-89","1990-99","2000-09","2010-19")
acdec=data.frame(decade,acdf)
acdec
barplot(acdec$acdf,xlab = "decade", ylab = "Acousticness", main = "Acousticness", ylim = c(0,75), names = decade , las = 1 , cex.names = 0.76)




data.frame(table(data$Top.Genre))
data[data$Top.Genre == "album rock", ]
data2=data.frame(table(data$Top.Genre)>50)
data2
# adult standards as, album rock ar, alternative metal alm, alternative rock alr,
# classic rock cr, dutch pop dp, dutch indie di, dutch cabaret dc, dance pop, dance rock
data2[data2$table.data.Top.Genre....50 == "TRUE", ]

dataar=data[data$Top.Genre == "album rock",  ]
dataar
View(dataar)

dataas=data[data$Top.Genre == "adult standards",  ]
dataas
View(dataas)

dataalm=data[data$Top.Genre == "alternative metal",  ]
dataalm
View(dataalm)

dataalr=data[data$Top.Genre == "alternative rock",  ]
dataalr
View(dataalr)

datacr=data[data$Top.Genre == "classic rock",  ]
datacr
View(datacr)

datadr=data[data$Top.Genre == "dance rock",  ]
datadr
View(datadr)

datadp=data[data$Top.Genre == "dance pop",  ]
datadp
View(datadp)

datadup=data[data$Top.Genre == "dutch pop",  ]
datadup
View(datadup)

datadui=data[data$Top.Genre == "dutch indie",  ]
datadui
View(datadui)

dataduc=data[data$Top.Genre == "dutch cabaret",  ]
dataduc
View(dataduc)

nrow(datadup)

# adult standards, album rock, alternative metal, alternative rock,
# classic rock, dutch pop, dutch indie, dutch cabaret, dance pop, dance rock
bpmmeanar=mean(dataar$Beats.Per.Minute..BPM.)
bpmmeanar

bpmmeanas=mean(dataas$Beats.Per.Minute..BPM.)
bpmmeanas

bpmmeanalm=mean(dataalm$Beats.Per.Minute..BPM.)
bpmmeanalm

bpmmeanalr=mean(dataalr$Beats.Per.Minute..BPM.)
bpmmeanalr

bpmmeancr=mean(datacr$Beats.Per.Minute..BPM.)
bpmmeancr

bpmmeandup=mean(datadup$Beats.Per.Minute..BPM.)
bpmmeandup

bpmmeandui=mean(datadui$Beats.Per.Minute..BPM.)
bpmmeandui

bpmmeanduc=mean(dataduc$Beats.Per.Minute..BPM.)
bpmmeanduc

bpmmeandp=mean(datadp$Beats.Per.Minute..BPM.)
bpmmeandp

bpmmeandr=mean(datadr$Beats.Per.Minute..BPM.)
bpmmeandr

bpmgdf=c(bpmmeanalm,bpmmeanalr,bpmmeanar,bpmmeanas,bpmmeancr,bpmmeandp,bpmmeandr,bpmmeanduc,bpmmeandui,bpmmeandup)
bpmgdf
genre=c("alternative metal", "alternative rock", "album rock", "adult standards","classic rock", "dance pop", "dance rock", "dutch cabaret", "dutch indie", "dutch pop")
bpmg=data.frame(genre,bpmgdf)
bpmg
barplot(bpmg$bpmgdf,xlab = "popular genre", ylab = "BPM", main = "Beats Per Minute", ylim = c(0,140), names = genre , las = 2 , cex.names = 0.4)

ermeanar=mean(dataar$Energy)
ermeanar

ermeanas=mean(dataas$Energy)
ermeanas

ermeanalm=mean(dataalm$Energy)
ermeanalm

ermeanalr=mean(dataalr$Energy)
ermeanalr

ermeancr=mean(datacr$Energy)
ermeancr

ermeandup=mean(datadup$Energy)
ermeandup

ermeandui=mean(datadui$Energy)
ermeandui

ermeanduc=mean(dataduc$Energy)
ermeanduc

ermeandp=mean(datadp$Energy)
ermeandp

ermeandr=mean(datadr$Energy)
ermeandr

ergdf=c(ermeanalm,ermeanalr,ermeanar,ermeanas,ermeancr,ermeandp,ermeandr,ermeanduc,ermeandui,ermeandup)
ergdf
genre=c("alternative metal", "alternative rock", "album rock", "adult standards","classic rock", "dance pop", "dance rock", "dutch cabaret", "dutch indie", "dutch pop")
erg=data.frame(genre,ergdf)
erg
barplot(erg$ergdf,xlab = "popular genre", ylab = "Energy", main = "Energy", ylim = c(0,80), names = genre , las = 2 , cex.names = 0.5)


dameanar=mean(dataar$Danceability)
dameanar

dameanas=mean(dataas$Danceability)
dameanas

dameanalm=mean(dataalm$Danceability)
dameanalm

dameanalr=mean(dataalr$Danceability)
dameanalr

dameancr=mean(datacr$Danceability)
dameancr

dameandup=mean(datadup$Danceability)
dameandup

dameandui=mean(datadui$Danceability)
dameandui

dameanduc=mean(dataduc$Danceability)
dameanduc

dameandp=mean(datadp$Danceability)
dameandp

dameandr=mean(datadr$Danceability)
dameandr

dagdf=c(dameanalm,dameanalr,dameanar,dameanas,dameancr,dameandp,dameandr,dameanduc,dameandui,dameandup)
dagdf
genre=c("alternative metal", "alternative rock", "album rock", "adult standards","classic rock", "dance pop", "dance rock", "dutch cabaret", "dutch indie", "dutch pop")
dag=data.frame(genre,dagdf)
dag
barplot(dag$dagdf,xlab = "popular genre", ylab = "Danceability", main = "Danceability", ylim = c(0,70), names = genre , las = 2 , cex.names = 0.5)

lomeanar=mean(dataar$Loudness..dB.)
lomeanar

lomeanas=mean(dataas$Loudness..dB.)
lomeanas

lomeanalm=mean(dataalm$Loudness..dB.)
lomeanalm

lomeanalr=mean(dataalr$Loudness..dB.)
lomeanalr

lomeancr=mean(datacr$Loudness..dB.)
lomeancr

lomeandup=mean(datadup$Loudness..dB.)
lomeandup

lomeandui=mean(datadui$Loudness..dB.)
lomeandui

lomeanduc=mean(dataduc$Loudness..dB.)
lomeanduc

lomeandp=mean(datadp$Loudness..dB.)
lomeandp

lomeandr=mean(datadr$Loudness..dB.)
lomeandr

logdf=c(lomeanalm,lomeanalr,lomeanar,lomeanas,lomeancr,lomeandp,lomeandr,lomeanduc,lomeandui,lomeandup)
logdf
genre=c("alternative metal", "alternative rock", "album rock", "adult standards","classic rock", "dance pop", "dance rock", "dutch cabaret", "dutch indie", "dutch pop")
log=data.frame(genre,logdf)
log
barplot(log$logdf,xlab = "popular genre", ylab = "Loudness", main = "Loudness", ylim = c(0,-12), names = genre , las = 2 , cex.names = 0.5)


limeanar=mean(dataar$Liveness)
limeanar

limeanas=mean(dataas$Liveness)
limeanas

limeanalm=mean(dataalm$Liveness)
limeanalm

limeanalr=mean(dataalr$Liveness)
limeanalr

limeancr=mean(datacr$Liveness)
limeancr

limeandup=mean(datadup$Liveness)
limeandup

limeandui=mean(datadui$Liveness)
limeandui

limeanduc=mean(dataduc$Liveness)
limeanduc

limeandp=mean(datadp$Liveness)
limeandp

limeandr=mean(datadr$Liveness)
limeandr

ligdf=c(limeanalm,limeanalr,limeanar,limeanas,limeancr,limeandp,limeandr,limeanduc,limeandui,limeandup)
ligdf
genre=c("alternative metal", "alternative rock", "album rock", "adult standards","classic rock", "dance pop", "dance rock", "dutch cabaret", "dutch indie", "dutch pop")
lig=data.frame(genre,ligdf)
lig
barplot(lig$ligdf,xlab = "popular genre", ylab = "Liveness", main = "Liveness", ylim = c(0,25), names = genre , las = 2 , cex.names = 0.5)


vameanar=mean(dataar$Valence)
vameanar

vameanas=mean(dataas$Valence)
vameanas

vameanalm=mean(dataalm$Valence)
vameanalm

vameanalr=mean(dataalr$Valence)
vameanalr

vameancr=mean(datacr$Valence)
vameancr

vameandup=mean(datadup$Valence)
vameandup

vameandui=mean(datadui$Valence)
vameandui

vameanduc=mean(dataduc$Valence)
vameanduc

vameandp=mean(datadp$Valence)
vameandp

vameandr=mean(datadr$Valence)
vameandr

vagdf=c(vameanalm,vameanalr,vameanar,vameanas,vameancr,vameandp,vameandr,vameanduc,vameandui,vameandup)
vagdf
genre=c("alternative metal", "alternative rock", "album rock", "adult standards","classic rock", "dance pop", "dance rock", "dutch cabaret", "dutch indie", "dutch pop")
vag=data.frame(genre,vagdf)
vag
barplot(vag$vagdf,xlab = "popular genre", ylab = "Valence", main = "Valence", ylim = c(0,60), names = genre , las = 2 , cex.names = 0.5)



ldmeanar=mean(dataar$Length..Duration.)
ldmeanar

ldmeanas=mean(dataas$Length..Duration.)
ldmeanas

ldmeanalm=mean(dataalm$Length..Duration.)
ldmeanalm

ldmeanalr=mean(dataalr$Length..Duration.)
ldmeanalr

ldmeancr=mean(datacr$Length..Duration.)
ldmeancr

ldmeandup=mean(datadup$Length..Duration.)
ldmeandup

ldmeandui=mean(datadui$Length..Duration.)
ldmeandui

ldmeanduc=mean(dataduc$Length..Duration.)
ldmeanduc

ldmeandp=mean(datadp$Length..Duration.)
ldmeandp

ldmeandr=mean(datadr$Length..Duration.)
ldmeandr

ldgdf=c(ldmeanalm,ldmeanalr,ldmeanar,ldmeanas,ldmeancr,ldmeandp,ldmeandr,ldmeanduc,ldmeandui,ldmeandup)
ldgdf
genre=c("alternative metal", "alternative rock", "album rock", "adult standards","classic rock", "dance pop", "dance rock", "dutch cabaret", "dutch indie", "dutch pop")
ldg=data.frame(genre,ldgdf)
ldg
barplot(ldg$ldgdf,xlab = "popular genre", ylab = "Length Duration", main = "Length Duration", ylim = c(0,300), names = genre , las = 2 , cex.names = 0.5)



acmeanar=mean(dataar$Acousticness)
acmeanar

acmeanas=mean(dataas$Acousticness)
acmeanas

acmeanalm=mean(dataalm$Acousticness)
acmeanalm

acmeanalr=mean(dataalr$Acousticness)
acmeanalr

acmeancr=mean(datacr$Acousticness)
acmeancr

acmeandup=mean(datadup$Acousticness)
acmeandup

acmeandui=mean(datadui$Acousticness)
acmeandui

acmeanduc=mean(dataduc$Acousticness)
acmeanduc

acmeandp=mean(datadp$Acousticness)
acmeandp

acmeandr=mean(datadr$Acousticness)
acmeandr

acgdf=c(acmeanalm,acmeanalr,acmeanar,acmeanas,acmeancr,acmeandp,acmeandr,acmeanduc,acmeandui,acmeandup)
acgdf
genre=c("alternative metal", "alternative rock", "album rock", "adult standards","classic rock", "dance pop", "dance rock", "dutch cabaret", "dutch indie", "dutch pop")
acg=data.frame(genre,acgdf)
acg
barplot(acg$acgdf,xlab = "popular genre", ylab = "Acousticness", main = "Acousticness", ylim = c(0,60), names = genre , las = 2 , cex.names = 0.5)



spmeanar=mean(dataar$Speechiness)
spmeanar

spmeanas=mean(dataas$Speechiness)
spmeanas

spmeanalm=mean(dataalm$Speechiness)
spmeanalm

spmeanalr=mean(dataalr$Speechiness)
spmeanalr

spmeancr=mean(datacr$Speechiness)
spmeancr

spmeandup=mean(datadup$Speechiness)
spmeandup

spmeandui=mean(datadui$Speechiness)
spmeandui

spmeanduc=mean(dataduc$Speechiness)
spmeanduc

spmeandp=mean(datadp$Speechiness)
spmeandp

spmeandr=mean(datadr$Speechiness)
spmeandr

spgdf=c(spmeanalm,spmeanalr,spmeanar,spmeanas,spmeancr,spmeandp,spmeandr,spmeanduc,spmeandui,spmeandup)
spgdf
genre=c("alternative metal", "alternative rock", "album rock", "adult standards","classic rock", "dance pop", "dance rock", "dutch cabaret", "dutch indie", "dutch pop")
spg=data.frame(genre,spgdf)
spg
barplot(spg$spgdf,xlab = "popular genre", ylab = "Speechiness", main = "Speechiness", ylim = c(0,8), names = genre , las = 2 , cex.names = 0.5)


pomeanar=mean(dataar$Popularity)
pomeanar

pomeanas=mean(dataas$Popularity)
pomeanas

pomeanalm=mean(dataalm$Popularity)
pomeanalm

pomeanalr=mean(dataalr$Popularity)
pomeanalr

pomeancr=mean(datacr$Popularity)
pomeancr

pomeandup=mean(datadup$Popularity)
pomeandup

pomeandui=mean(datadui$Popularity)
pomeandui

pomeanduc=mean(dataduc$Popularity)
pomeanduc

pomeandp=mean(datadp$Popularity)
pomeandp

pomeandr=mean(datadr$Popularity)
pomeandr

pogdf=c(pomeanalm,pomeanalr,pomeanar,pomeanas,pomeancr,pomeandp,pomeandr,pomeanduc,pomeandui,pomeandup)
pogdf
genre=c("alternative metal", "alternative rock", "album rock", "adult standards","classic rock", "dance pop", "dance rock", "dutch cabaret", "dutch indie", "dutch pop")
pog=data.frame(genre,pogdf)
pog
barplot(pog$pogdf,xlab = "popular genre", ylab = "Popularity", main = "Popularity", ylim = c(0,80), names = genre , las = 2 , cex.names = 0.5)
 nrow(data)
 
data
cor.test(data$Beats.Per.Minute..BPM.,data$Popularity)

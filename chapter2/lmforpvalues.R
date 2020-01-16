X.master

x1 = X.master$completed.gap
x2 = X.master$completed.lec
x3 = X.master$completed.gfsi
x4 = X.master$completed.county
x5 = X.master$untreated
x6 = X.master$completed.none

y

fitlm = lm(y ~ x1 + x2 + x3 + x4 + x5 + x6)

summary(fitlm)




#Water testing
x1 = X.master$gc
x2 = X.master$Pak.Choy
x3 = X.master$completed.wksh
x4 = X.master$seedlings
x5 = X.master$completed.conf
x6 = X.master$Kohlrabi
x7 = X.master$Mizuna
x8 = X.master$osoil
x9 = X.master$speat
x10 = X.master$completed.con
x11 = X.master$rain
x12 = X.master$csoil
x13 = X.master$worm
x14 = X.master$completed.other
x15 = X.master$untreated
x16 = X.master$Tatsoi
x17 = X.master$Beet
x18 = X.master$completed.nofs
x19 = X.master$Radish

x20 = X.master$completed.own
x21 = X.master$fish
x22 = X.master$completed.haccp
x23 = X.master$completed.county
x24 = X.master$Nasturtium
x25 = X.master$wood
x26 = X.master$Celery
x27 = X.master$beef
x28 = X.master$municipal
x29 = X.master$na.med
x30 = X.master$reverse
x31 = X.master$Popcorn
x32 = X.master$completed.lec
x33 = X.master$flowers
x34 = X.master$completed.bk
x35 = X.master$Bok.Choy
x36 = X.master$completed.state
x37 = X.master$completed.psa
x38 = X.master$Amaranth
x39 = X.master$uvlight
x40 = X.master$completed.gfsi



fitlm2 = lm(y ~ x1+x2+x3+x4+x5+x6+x7+x8+x9+x10+x11+x12+x13+x14+x15+x16+x17+x18+x19+x20+x21+x22+x23+x24+x25+x26+x27+x28+x29+x30+x31+x32+x33+x34+x35+x36+x37+x38+x39+x40)

sumfit <- summary(fitlm2)

pvalues = round(sumfit$coefficients[,4],4)

pvalues = data.frame(pvalues)

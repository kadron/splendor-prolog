
working_directory(X, 'D:/Dropbox/Personal/Ersin/PHD/Cmpe540/TermProject').
[splendor].

[webplay].
startServer(5010).


%runGame([randomPlayer, human]).


setVerbose(10).
set_random(seed(15)). 
runGame([randomPlayer, human]).

runGame([randomPlayer, randomPlayer]).


[hello_world].

LINUX

working_directory(X, '/media/ersin/Data/Dropbox/Personal/Ersin/PHD/Cmpe540/TermProject').
[splendor].

 profile(time(splendor:testAll)).

+ token yetmese bile kart satil alinabiliyor, tokenlar da artiyorlar bir o kadar
+ token'i yetmedigi zaman satin almaya calisirsa rezerve edilecek, altin verilecegi belli edilecek. 
+ max 3 rezerve kart olabilir
+ rezerve kartlar ekranda gosterilecek. 
+ Fazla tasi oldugu zaman geri vermek icin mevcutlardan tas secme olacak. 
+ oyun bittigi zaman ekranda oyun bitti denecek, kazanan kaybeden gosterilecek
+ desteden rezerve etme olacak
+ altinla satin alabilme olacak
+ framework: noble tiles eklenecek
+ framework: reserve'den satil alma eklenecek
+ 4 kisi oynayabilme eklenecek
+ Arayuze kac oyunculu oyun istedigi eklenecek
+ reserve ederse ve gold'la birlikte 10'dan fazla gem'i olursa geri atma olmali.
+ oyun tur tamamlaninca bitecek, 15 olunca degil
+ puanlar ayni oldugunda en az kart satin alan birinci olacak
- 4 kiside, 1., 2., 3. ve 4. belli olacak 
- webplay'de kullanici ilk oyuncu da olabilir, ikinci oyuncu da, yani data.players[0] degil data.players[1] de olabilir (veya 2, 3)
- rakibin aldigi kart da animasyonla gidecek ve kartlarin yerleri degismeyecek, su anda hepsi kayiyor
- 125 nolu kartin gemleri yanlis, siyah gozukuyor, ama beyazlar

+ kart kalmayinca arayuzde kartlar gizlenecek. 

+ player: human gercekten human olacak. mevcut kod random player'a alinacak
+ gold random'da secilmeyecek (hem alirken, hem ararken)
+ randomGem problemi: eger geri atmasi gereken gemlerin en azindan ikisinin rengi ayni ise problem var.
+ framework: rezerve etme eklenecek





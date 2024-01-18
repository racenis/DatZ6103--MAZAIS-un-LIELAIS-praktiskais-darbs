# SIA Šņūkstings Bruņuvardes lielo darbu optimizators

# Palaišana

Vispirms vajag tikt pie Erlang instalācijas. Pēc tam vajag ielādēt repozitoriju,
aiznavigēties uz to un tajā palaist komandrindu. Tad ar `erl` komandu var
palaist erlang interpretatoru. Tad vajag nokompilēt programmu:
```
c(domain).
c(optimizer).
c(server).
```
Pēc tam vajag palaist optimizēšanas serveri un HTTP serveri:
```
optimizer:start_manager().
server:start().
```
Ja serveri met ārā kaut kādus kļūdu paziņojumus, tad dažkārt var vēl pāris reizes
mēģināt tos iestartēt un varbūt tad sanāks.
Tad kad komandrindā parādās paziņojums `Waiting for a connection...`, tad var vērt
vaļā pārlūkprogrammu un `http://127.0.0.1:8696` būs atrodams šņūkstu optimizators.

# Programmas instrukcija
## Darbplūsma
1. Nokonfigurēt šņūkstu sarakstu.
2. Iestatīt optimizatora iestatījumus.
3. Nosūtīt optimizācijas uzdevumu serverim.
4. Sagaidīt optimizācijas uzdevuma atrisinājumu.
5. Apskatīt optimizācijas rezultātu.
6. Novērtēt rezultātu.
7. Nepieciešamības gadījumā atgriezties punktā 2.

## Saskarne
Galvenie saskarnes elementi -- virsraksts, sarkanais brīdinājums, darba josla,
datu tabula (nav parādīta).

![attēls1.png](docs/attels1.png)

## Šņūkstu konfigurācija
Nospiest uz darbjoslas pogas "Šņūksti". Aizpildīt tabulu. Pēc katra ieraksta 
veikšanas, tabulas saturs tiks automātiski saglabāts pārlūkprogrammas kešatmiņā.

Programma neveic nekādu ievada validāciju. Lūdzu ievadīt tikai pareizu ievadu.
Gadījumā ja serveris pēc nepareiza ievada veic atteici, ievadīt servera
konsolē komandas `optimizer:start_manager().` un `server:start().` līdz
serveris beidz mest kļūdas.

*ID* laukā ir jāievada šņūksta identifikators, kas var būt jebkura
alfabētiska simbolu virkne, bez atstarpēm, t.i. simbolu virkne kas sastāv no
saprātīga daudzuma simbolu, kas redzami šajā sarakstā:
`abcdefghijklmnopqwrtyuizmnpcvxz`.

*Vārds* laukā ir jāievada šņūksta vārds. Atļauti jebkādi alfanumeriski
simboli, izņemot iekavas un dubultpēdiņas `"`, kas tāpat nav alfanumerisks simbols,
bet tas ir īpaši aizliegts.

*Netiek no* laukā ir jāievada laika moments, no kura šņūksts nebūs pieejams.
*Netiek uz* laukā ir jāievada laika moments, no kura šņūksts būs atkal pieejams.
Laika moments ir vesels skaitlis intervālā no `0` līdz `4000`. Darba dienas sākums ir
`0`, pusdienas iešņūkstēšanās notiek `1000`, normālās darba dienas beigas ir `2000`, 
virsstrāde atļauta līdz `4000`. Gadījumā ja šņūksts būs pieejams visu dienu, tad
abos laukumos ir jāieraksta `0`.

![attēls2.png](docs/attels2.png)

# Optimizatora konfigurēšana

Darbajoslas lejāmestnes izvēlnē *Alg.* izvēlēties optimizatora algoritmu.
Programmas testēšanas laikā atklājās ka vislabākais ir algoritms ar nosaukumu
"Kalnā Kāpšana".

Lauciņā ar nosaukumu *Iter.* ierakstīt algoritma iterāciju skaitu. Skaitam
jābūt veselam skaitlim no 1 līdz ļoti lielam skaitlim, bet vislabāk sanāk ap 10000. 

Lauciņā *Tags* var ierakstīt alfanumerisko optimizācijas darba identifikācijas
simbolu virkni, kas ļaus vieglāk identificēt palaistu optimizācijas darbu. Īpaši
nedrīkst izmantot simbolu `"`.

Kad visi parametri ir iestatīti, nospiest pogu "Optimizēt".

![attēls3.png](docs/attels3.png)

# Optimizēšanas gaidīšana

![attēls4.png](docs/attels4.png)

Nepieciešamības gadījumā ir iespējams nospiest pogu "Atcelt" lai atceltu
optimizācijas darbu. Nespiest pogu ja optimizācijas darbs ir tuvu beigšanai, jo
tad iespējams ka serverim atcelšanas pieprasījums kad darbs būs jau beidzies un
tad serveris apjukumā uztaisīs atteici.

# Rezultātu apskatīšana

![attēls5.png](docs/attels5.png)

Nospiest uz pogas "Skatīt" pie pabeigtā optimizācijas darba.

![attēls6.png](docs/attels6.png)

Tabulas augšējā pusē ir attēlots risinājuma novērtējums. Zem novērtējuma ir 
attēlots pilnais optimizācijas risinājuma saraksts ar katram šņūkstam
piešķirtajiem darbiem.

![attēls7.png](docs/attels7.png)

# Docker attēls

[https://hub.docker.com/r/racenis/sia-snukstings-brunuvardes](https://hub.docker.com/r/racenis/sia-snukstings-brunuvardes)

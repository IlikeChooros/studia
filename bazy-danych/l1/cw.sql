1:
select distinct Nazwisko_prezesa from Firma;

2:
select distinct Nazwa from Firma where Data_zalozenia > '2000-01-01';

3:
select Nazwa_firmy from Zatrudnienie z group by Nazwa_firm having count(Id_pracownika) >= 10;


4:
select Nazwisko_prezesa from Firma
join Pracownicy p on Pesel_prezesa=p.Pesel
join Zatrudnienie z on z.Id_pracownika=p.Id
where z.Nazwa_firmy=Nazwa;


5:


6:


7:
select f.Nazwa, f.Nazwisko_prezesa, f.Imie_prezesa from Firma
join Zatrudnienie z on z.Nazwa_firmy=f.Nazwa
join Pracownicy p on not p.Nazwisko='Fikcyjny';

8:
select f.Nazwa from Firma where f.Nazwa like '%sp. z o.o.%s';

9:
select f.Data_zalozenia from Firma where f.Data_zalozenia=
(select min(f.Data_zalozenia) from Firma);

10:


11:
select f.Nazwa from Firma f where f.Data_zalozenia < (select f.Data_zalozenia 
from Firma f where f.Nazwa='Korepetycje sp. z o.o.')

-- FROM HERE:

12:
select distinct p.Imie, p.Nazwisko from Pracownicy p
where p.Nazwisko='Kowalski' or p.Nazwisko='Nowak'

13: -- invalid
select f.Nazwa from Firma f 
join Obroty o on o.Nazwa_firmy=f.Nazwa and MAKEDATE(o.Rok, )

14:


15:
select f1.Nazwa, f2.Nazwa from Firma f1 
join Firma f2 on f1.Adres=f2.Adres and STRCMP(f1.Nazwa, f2.Nazwa) > 0 

16:

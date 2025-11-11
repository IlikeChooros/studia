-- 1: 
create database aparaty;
-- 'Smolnicki99!'
create user '283999' @'localhost' identified by 'smolnicki99';
GRANT select,
    insert,
    update on aparaty.* for '283999' @'localhost';
-- 2:
CREATE TABLE IF NOT EXISTS Producent (
    ID int NOT NULL AUTO_INCREMENT PRIMARY KEY,
    nazwa varchar(50) NOT NULL,
    kraj varchar(20) DEFAULT "nieznany",
    adresKorespondencyjny varchar(100) DEFAULT "nieznany"
);

CREATE TABLE IF NOT EXISTS Obiektyw (
    ID int NOT NULL AUTO_INCREMENT PRIMARY KEY,
    model varchar(30),
    minPrzeslona float,
    maxPrzeslona float,
    CHECK(minPrzeslona < maxPrzeslona)
);

CREATE TABLE IF NOT EXISTS Matryca (
    ID int NOT NULL AUTO_INCREMENT PRIMARY KEY,
    przekatna decimal(4,2),
    rozdzielczosc decimal(3,1),
    typ varchar(10)
);

ALTER TABLE Matryca AUTO_INCREMENT=100;

CREATE TABLE IF NOT EXISTS Aparat (
    model varchar(30) NOT NULL PRIMARY KEY,
    producent int,
    matryca int,
    obiektyw int,
    waga float NOT NULL DEFAULT 0,
    typ enum('kompaktowy', 'lustrzanka', 'profesjonalny', 'inny'),
    CONSTRAINT `FK_producent` FOREIGN KEY (producent) REFERENCES Producent(ID),
    CONSTRAINT `FK_matryca` FOREIGN KEY (matryca) REFERENCES Matryca(ID),
    CONSTRAINT `FK_obiektyw` FOREIGN KEY (obiektyw) REFERENCES Obiektyw(ID)
);
-- 3:
mysql -u 283999 -p
-- Smolnicki99!

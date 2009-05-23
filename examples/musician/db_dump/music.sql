# HeidiSQL Dump 
#
# --------------------------------------------------------
# Host:                 127.0.0.1
# Database:             music
# Server version:       5.0.51b-community
# Server OS:            Win32
# Target-Compatibility: Standard ANSI SQL
# HeidiSQL version:     3.2 Revision: 1129
# --------------------------------------------------------

/*!40100 SET CHARACTER SET latin1;*/
/*!40101 SET @OLD_SQL_MODE=@@SQL_MODE, SQL_MODE='ANSI';*/
/*!40014 SET @OLD_FOREIGN_KEY_CHECKS=@@FOREIGN_KEY_CHECKS, FOREIGN_KEY_CHECKS=0;*/


#
# Database structure for database 'music'
#

CREATE DATABASE /*!32312 IF NOT EXISTS*/ "music" /*!40100 DEFAULT CHARACTER SET latin1 */;

USE "music";


#
# Table structure for table 'musician'
#

CREATE TABLE /*!32312 IF NOT EXISTS*/ "musician" (
  "id" int(11) NOT NULL auto_increment,
  "name" varchar(20) default NULL,
  "birth_date" date default NULL,
  "instrument" enum('guitar','piano','drums','vocals') default NULL,
  "bio" text,
  PRIMARY KEY  ("id")
) AUTO_INCREMENT=5 /*!40100 DEFAULT CHARSET=latin1*/;



#
# Dumping data for table 'musician'
#

LOCK TABLES "musician" WRITE;
/*!40000 ALTER TABLE "musician" DISABLE KEYS;*/
REPLACE INTO "musician" ("id", "name", "birth_date", "instrument", "bio") VALUES
	(1,'John Lennon','1940-09-10','vocals','An iconic English 20th century
    rock and roll songwriter and singer...');
REPLACE INTO "musician" ("id", "name", "birth_date", "instrument", "bio") VALUES
	(2,'Paul McCartney','1942-06-18','piano','Sir James Paul McCartney
       is a popular Grammy Award-winning
       English artist...');
REPLACE INTO "musician" ("id", "name", "birth_date", "instrument", "bio") VALUES
	(3,'George Harrison','1943-02-24','guitar','George Harrison was a popular English
     musician best known as a member of The Beatles...');
REPLACE INTO "musician" ("id", "name", "birth_date", "instrument", "bio") VALUES
	(4,'Ringo Star','1940-07-07','drums','Richard Starkey, known by his stage name
     Ringo Starr, is an English popular musician,
     singer, and actor, best known as the
    drummer for The Beatles...');
/*!40000 ALTER TABLE "musician" ENABLE KEYS;*/
UNLOCK TABLES;
/*!40101 SET SQL_MODE=@OLD_SQL_MODE;*/
/*!40014 SET FOREIGN_KEY_CHECKS=@OLD_FOREIGN_KEY_CHECKS;*/

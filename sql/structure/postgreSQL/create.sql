CREATE DATABASE gilded_rose;

\connect gilded_rose;

CREATE TABLE item (
 name    CHARACTER VARYING(100) NOT NULL,
 sellIn  INTEGER,
 quality INTEGER                NOT NULL
);

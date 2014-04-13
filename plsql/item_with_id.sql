PROMPT Creating Table 'ITEM' with auto-increment primary key 'ID'
DROP TABLE item;
CREATE TABLE item
  (
    id      NUMBER(6) NOT NULL,
    name    VARCHAR2(100) NOT NULL,
    sell_in NUMBER(6) NOT NULL,
    quality NUMBER(6) NOT NULL
  );
/

ALTER TABLE item ADD (
  CONSTRAINT item_pk PRIMARY KEY (ID));
/

DROP SEQUENCE item_id_seq;
CREATE SEQUENCE item_id_seq 
  INCREMENT BY 1 
  START WITH 1 
  MAXVALUE 999999
  MINVALUE 1 
  NOCYCLE;
/

CREATE OR REPLACE TRIGGER item_bis_trg 
  BEFORE INSERT ON item 
  FOR EACH ROW 
BEGIN
    SELECT item_id_seq.NEXTVAL INTO :new.id FROM dual;
END;
/

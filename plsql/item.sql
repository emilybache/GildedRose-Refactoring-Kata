PROMPT Creating Table 'ITEM'
DROP TABLE item;
CREATE TABLE item
  (
    name    VARCHAR2(100) NOT NULL,
    sell_in NUMBER(6) NOT NULL,
    quality NUMBER(6) NOT NULL
  );
/

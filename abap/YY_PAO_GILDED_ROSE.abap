*&---------------------------------------------------------------------*
*&   Gilded Rose Requirements Specification
*&---------------------------------------------------------------------*
*&
*& Hi and welcome to team Gilded Rose. As you know, we are a small inn with
*& a prime location in a prominent city ran by a friendly innkeeper named
*& Allison. We also buy and sell only the finest goods. Unfortunately, our
*& goods are constantly degrading in quality as they approach their sell by
*& date. We have a system in place that updates our inventory for us. It
*& was developed by a no-nonsense type named Leeroy, who has moved on to
*& new adventures. Your task is to add the new feature to our system so that
*& we can begin selling a new category of items.
*&
*& First an introduction to our system:
*&
*& 	- All items have a Sell In value which denotes the number of
*&           days we have to sell the item
*& 	- All items have a Quality value which denotes how valuable the item is
*& 	- At the end of each day our system lowers both values for every item
*&
*& Seems pretty simple, right? Well this is where it gets interesting:
*&
*& 	- Once the sell by date has passed, Quality degrades twice as fast
*& 	- The Quality of an item is never negative
*& 	- "Aged Brie" actually increases in Quality the older it gets
*& 	- The Quality of an item is never more than 50
*& 	- "Sulfuras", being a legendary item, never has to be sold or
*&           decreases in Quality
*& 	- "Backstage passes", like aged brie, increases in Quality as its
*&           Sell In value approaches; Quality increases by 2 when there
*&           are 10 days or less and by 3 when there are 5 days or less
*&           but Quality drops to 0 after the concert
*&
*& We have recently signed a supplier of conjured items. This requires an
*& update to our system:
*&
*& 	- "Conjured" items degrade in Quality twice as fast as normal items
*&
*& Feel free to make any changes to the Update Quality method and add any new
*& code as long as everything still works correctly. However, do not alter
*& the Item class directly or Items table attribute as those belong to the
*& goblin in the corner who will insta-rage and one-shot you as he doesn't
*& believe in shared code ownership (you can make the Update Quality method
*& and Items property static if you must, we'll cover for you).
*&
*& Just for clarification, an item can never have its Quality increase
*& above 50, however "Sulfuras" is a legendary item and as such its Quality
*& is 80 and it never alters.

PROGRAM yy_pao_gilded_rose.


*& Production Code - Class Library
CLASS lcl_item DEFINITION DEFERRED.

CLASS lcl_gilded_rose DEFINITION FINAL.
  PUBLIC SECTION.
    TYPES:
      tt_items TYPE STANDARD TABLE OF REF TO lcl_item WITH EMPTY KEY.
    METHODS:
      constructor
        IMPORTING it_items TYPE tt_items,
      update_quality.

  PRIVATE SECTION.
    DATA:
      mt_items TYPE tt_items.
ENDCLASS.

CLASS lcl_item DEFINITION FINAL.
  PUBLIC SECTION.
    METHODS:
      constructor
        IMPORTING iv_name    TYPE string
                  iv_sell_in TYPE i
                  iv_quality TYPE i,
      description
        RETURNING VALUE(rv_string) TYPE string.
    DATA:
      mv_name    TYPE string,
      mv_sell_in TYPE i,
      mv_quality TYPE i.
ENDCLASS.

CLASS lcl_gilded_rose IMPLEMENTATION.

  METHOD constructor.
    mt_items = it_items.
  ENDMETHOD.

  METHOD update_quality.

    LOOP AT mt_items INTO DATA(lo_item).
      IF lo_item->mv_name <> |Aged Brie| AND
         lo_item->mv_name <> |Backstage passes to a TAFKAL80ETC concert|.
        IF lo_item->mv_quality > 0.
          IF lo_item->mv_name <> |Sulfuras, Hand of Ragnaros|.
            lo_item->mv_quality = lo_item->mv_quality - 1.
          ENDIF.
        ENDIF.
      ELSE.
        IF lo_item->mv_quality < 50.
          lo_item->mv_quality = lo_item->mv_quality + 1.

          IF lo_item->mv_name = |Backstage passes to a TAFKAL80ETC concert|.
            IF lo_item->mv_sell_in < 11.
              IF lo_item->mv_quality < 50.
                lo_item->mv_quality = lo_item->mv_quality + 1.
              ENDIF.
            ENDIF.

            IF lo_item->mv_sell_in < 6.
              IF lo_item->mv_quality < 50.
                lo_item->mv_quality = lo_item->mv_quality + 1.
              ENDIF.
            ENDIF.
          ENDIF.
        ENDIF.
      ENDIF.

      IF lo_item->mv_name <> |Sulfuras, Hand of Ragnaros|.
        lo_item->mv_sell_in = lo_item->mv_sell_in - 1.
      ENDIF.

      IF lo_item->mv_sell_in < 0.
        IF lo_item->mv_name <> |Aged Brie|.
          IF lo_item->mv_name <> |Backstage passes to a TAFKAL80ETC concert|.
            IF lo_item->mv_quality > 0.
              IF lo_item->mv_name <> |Sulfuras, Hand of Ragnaros|.
                lo_item->mv_quality = lo_item->mv_quality - 1.
              ENDIF.
            ENDIF.
          ELSE.
            lo_item->mv_quality = lo_item->mv_quality - lo_item->mv_quality.
          ENDIF.
        ELSE.
          IF lo_item->mv_quality < 50.
            lo_item->mv_quality = lo_item->mv_quality + 1.
          ENDIF.
        ENDIF.
      ENDIF.
    ENDLOOP.

  ENDMETHOD.

ENDCLASS.

CLASS lcl_item IMPLEMENTATION.

  METHOD constructor.
    mv_name    = iv_name.
    mv_sell_in = iv_sell_in.
    mv_quality = iv_quality.
  ENDMETHOD.

  METHOD description.
    rv_string = |{ mv_name }, { mv_sell_in }, { mv_quality }|.
  ENDMETHOD.

ENDCLASS.


*& Test Code - Executable Text Test Fixture
CLASS lth_texttest_fixture DEFINITION FINAL.
  PUBLIC SECTION.
    CLASS-METHODS main.
ENDCLASS.

CLASS lth_texttest_fixture IMPLEMENTATION.
  METHOD main.
    DATA(lo_out) = cl_demo_output=>new( )->write_text( |OMGHAI!| ).

    DATA(lt_items) = VALUE lcl_gilded_rose=>tt_items(
                        ( NEW #( iv_name    = |+5 Dexterity Vest|
                                 iv_sell_in = 10
                                 iv_quality = 20 ) )
                        ( NEW #( iv_name    = |Aged Brie|
                                 iv_sell_in = 2
                                 iv_quality = 0 ) )
                        ( NEW #( iv_name    = |Elixir of the Mongoose|
                                 iv_sell_in = 5
                                 iv_quality = 7 ) )
                        ( NEW #( iv_name    = |Sulfuras, Hand of Ragnaros|
                                 iv_sell_in = 0
                                 iv_quality = 80 ) )
                        ( NEW #( iv_name    = |Backstage passes to a TAFKAL80ETC concert|
                                 iv_sell_in = 15
                                 iv_quality = 20 ) )
                        ( NEW #( iv_name    = |Backstage passes to a TAFKAL80ETC concert|
                                 iv_sell_in = 10
                                 iv_quality = 49 ) )
                        ( NEW #( iv_name    = |Backstage passes to a TAFKAL80ETC concert|
                                 iv_sell_in = 5
                                 iv_quality = 49 ) )
                        "This conjured item does not work properly yet
                        ( NEW #( iv_name    = |Conjured Mana Cake|
                                 iv_sell_in = 3
                                 iv_quality = 6 ) ) ).

    DATA(lo_app) = NEW lcl_gilded_rose( it_items = lt_items ).

    DATA(lv_days) = 2.
    cl_demo_input=>request( EXPORTING text = |Number of Days?|
                            CHANGING field = lv_days ).

    DO lv_days TIMES.
      lo_out->next_section( |-------- day { sy-index } --------| ).
      lo_out->write_text( |Name, Sell_In, Quality| ).
      LOOP AT lt_items INTO DATA(lo_item).
        lo_out->write_text( lo_item->description( ) ).
      ENDLOOP.
      lo_app->update_quality( ).
    ENDDO.

    lo_out->display( ).
  ENDMETHOD.
ENDCLASS.


*& Test Code - Currently Broken
CLASS ltc_gilded_rose DEFINITION FINAL FOR TESTING RISK LEVEL HARMLESS.
  PRIVATE SECTION.
    METHODS:
      foo FOR TESTING.
ENDCLASS.

CLASS ltc_gilded_rose IMPLEMENTATION.

  METHOD foo.
    DATA(lt_items) = VALUE lcl_gilded_rose=>tt_items( ( NEW #( iv_name    = |foo|
                                                               iv_sell_in = 0
                                                               iv_quality = 0 ) ) ).

    DATA(lo_app) = NEW lcl_gilded_rose( it_items = lt_items ).
    lo_app->update_quality( ).

    cl_abap_unit_assert=>assert_equals(
                   act = CAST lcl_item( lt_items[ 1 ] )->mv_name
                   exp = |fixme| ).
  ENDMETHOD.

ENDCLASS.

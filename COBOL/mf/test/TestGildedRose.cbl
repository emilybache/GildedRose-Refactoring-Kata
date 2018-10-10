      *> Test Fixture for GildedRose, GildedRose

       copy "mfunit_prototypes.cpy".

       program-id. TestGildedRose.
       
       file-control.
           select in-items assign 'in-items'.
           select items assign 'items'.
           
       file section.
           fd in-items.
           01 in-item.
             02 sell-in pic 9(4).
             02 quality pic 9(4).
             02 name pic x(50).
           fd items.
           01 item.
             02 sell-in pic 9(4).
             02 quality pic 9(4).
             02 name pic x(50).
           
       working-storage section.
       copy "mfunit.cpy".
       78 TEST-TESTGILDEDROSE value "TestGildedRose".
       01 pp procedure-pointer.

      *> Program linkage data

       procedure division.
           goback returning 0
       .

       entry MFU-TC-PREFIX & TEST-TESTGILDEDROSE.
           open output in-items
               move "foo" to name in in-item
               move 0 to quality in in-item
               move 0 to sell-in in in-item
               write in-item
           close in-items
           call "GildedRose"
           open input items
           read items
           if name in item not equal to "fixme" then
               call MFU-ASSERT-FAIL-Z using z"item name was not fixme"
           close items
           goback
       .

      $region TestCase Configuration

       entry MFU-TC-SETUP-PREFIX & TEST-TESTGILDEDROSE.
       perform InitializeLinkageData
           *> Add any other test setup code here
           goback returning 0
       .

       InitializeLinkageData section.
           *> Load the library that is being tested
           set pp to entry "GildedRose"

           exit section
       .

      $end-region

       end program.

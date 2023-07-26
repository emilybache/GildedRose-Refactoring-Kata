# gilded-rose

## Setup

### Install quicklisp

To run this project install quicklisp (if not already done): 
 - download https://beta.quicklisp.org/quicklisp.lisp
 - load it with your common lisp implementation. The example with sbcl: 
   > sbcl --load quicklisp.lisp
 - run the command 
   > (quicklisp-quickstart:install) 
   in your common lisp implementation
 - run the command 
   > (ql:add-to-init-file) 
   in your common lisp implementation

### Install project
Copy the project-folder containing this file into /quicklisp/local-projects/ that has been created when installing quicklisp.
This is the root directory for quicklisp to search for the gilded-rose.asd file which defines the system (project) and its dependencies.
The quicklisp-folder is usually created in your home-directory.

### Work with the project

Now you can load the project with 
> (ql:quickload "gilded-rose") 
in your common lisp-implementation and run the tests with 
> (asdf:test-system "gilded-rose")
.

You can stub and mock functions and methods with the cl-mock-library which is already included in the system definition of the test-system:

(with-mocks ()
  (answer <your-method> (call-previous))
  (<your-method> <argument-list>)
  (is <your-testcase> (invocations '<your-method>)))

## Documentation

Hi and welcome to team Gilded Rose. As you know, we are a small inn
with a prime location in a prominent city ran by a friendly
innkeeper named Allison. We also buy and sell only the finest goods.
Unfortunately, our goods are constantly degrading in quality as they
approach their sell by date. We have a system in place that updates
our inventory for us. It was developed by a no-nonsense type named
Leeroy, who has moved on to new adventures. Your task is to add the
new feature to our system so that we can begin selling a new
category of items.

First an introduction to our system:
 - All items have a SellIn value which denotes the number of days we have to sell the item
 - All items have a Quality value which denotes how valuable the item is
 - At the end of each day our system lowers both values for every item
 
Pretty simple, right? Well this is where it gets interesting:
 - Once the sell by date has passed, Quality degrades twice as fast
The Quality of an item is never negative
 - "Aged Brie" actually increases in Quality the older it gets
 - The Quality of an item is never more than 50
 - "Sulfuras", being a legendary item, never has to be sold or decreases in Quality
 - "Backstage passes", like aged brie, increases in Quality as it's
   SellIn value approaches; Quality increases by 2 when there are 10
   days or less and by 3 when there are 5 days or less but Quality
   drops to 0 after the concert

We have recently signed a supplier of conjured items. This requires an update to our system:
 - "Conjured" items degrade in Quality twice as fast as normal items

Feel free to make any changes to the UpdateQuality method and add
any new code as long as everything still works correctly. However,
do not alter the Item class or Items property as those belong to the
goblin in the corner who will insta-rage and one-shot you as he
doesn't believe in shared code ownership (you can make the
UpdateQuality method and Items property static if you like, we'll
cover for you).
Just for clarification, an item can never have its Quality increase
above 50, however "Sulfuras" is a legendary item and as such its
Quality is 80 and it never alters.


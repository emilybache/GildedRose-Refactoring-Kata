# gilded rose

The requirements of gilded rose can be found here:
https://github.com/emilybache/GildedRose-Refactoring-Kata/blob/main/GildedRoseRequirements.txt

# Setup

## Install quicklisp

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

## Install project
Copy the project-folder containing this file into /quicklisp/local-projects/ that has been created when installing quicklisp.
This is the root directory for quicklisp to search for the gilded-rose.asd file which defines the system (project) and its dependencies.
The quicklisp-folder is usually created in your home-directory.

## Working with the project

Now you can load the project with 
> (ql:quickload "gilded-rose") 
in the common lisp implementation of your choice and run the tests with 
> (asdf:test-system "gilded-rose")
.

If you just want to run the tests
> (asdf:test-system "gilded-rose")
is sufficient.

You can mock functions and methods with the cl-mock-library which is already included in the system definition of the test-system:

(with-mocks ()
  (answer <your-method> (call-previous))
  (<your-method> <argument-list>)
  (is <your-testcase> (invocations '<your-method>)))
  
If you just want to stub functions you can replace 
(call-previous) 
with a return value of your choice and your test does not depend on 
(invocations '<your-method>)

## Running the texttest-fixture

If you don't want to work with the unit-tests you can test your program with the texttest-fixture.
After loading the project in the common lisp implementation of your choice with
> (ql:quickload "gilded-rose")
you can run the texttest-fixture with
> (gilded-rose::run-gilded-rose <number-of-days>)
where <number-of-days> is the number of days you want to simulate.

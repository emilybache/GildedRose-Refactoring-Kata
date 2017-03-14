#include <stdio.h>
#include <stdlib.h>
#include <check.h>

Suite *suite_rose(void);

int main(int argc, char **argv)
{
  Suite *s;
  SRunner *runner;
  int number_fails;
  int forkme = 1;

  if (argc > 1 && strcmp(argv[1], "--nofork") == 0) {
    forkme = 0;
  }

  s = suite_rose();
  runner = srunner_create(s);

  if (0 == forkme) {
    srunner_set_fork_status(runner, CK_NOFORK);
  }

  srunner_run_all(runner, CK_NORMAL);
  number_fails = srunner_ntests_failed(runner);

  srunner_free(runner);

  return number_fails;
}

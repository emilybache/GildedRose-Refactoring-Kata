CREATE OR REPLACE FUNCTION test_case_assertEquals_numeric_should_fail_if_not_equals() RETURNS VOID AS $$
DECLARE
  expected_message VARCHAR;
  error_message VARCHAR;
  is_equals BOOLEAN;
BEGIN
  BEGIN
    perform test_assertEquals(7, 5);
  EXCEPTION
    WHEN triggered_action_exception THEN
      expected_message := 'assertEquals failure: expect ''7'' instead of ''5''';
      error_message := SQLERRM;
      perform test_assertTrue(format('Expect message ''%s'' instead of ''%s'' ', expected_message, error_message), error_message = expected_message);
  END;
END;
$$ LANGUAGE plpgsql;

CREATE OR REPLACE FUNCTION test_case_assertEquals_numeric_should_success_if_equals() RETURNS VOID AS $$
BEGIN
  perform test_assertEquals(7, 7);
END;
$$ LANGUAGE plpgsql;

CREATE OR REPLACE FUNCTION test_case_assertEquals_text_should_fail_if_not_equals() RETURNS VOID AS $$
DECLARE
  expected_message VARCHAR;
  error_message VARCHAR;
  is_equals BOOLEAN;
BEGIN
  BEGIN
    perform test_assertEquals('hello'::VARCHAR, 'olleh');
  EXCEPTION
    WHEN triggered_action_exception THEN
      expected_message := 'assertEquals failure: expect ''hello'' instead of ''olleh''';
      error_message := SQLERRM;
      perform test_assertTrue(format('Expect message ''%s'' instead of ''%s'' ', expected_message, error_message), error_message = expected_message);
  END;
END;
$$ LANGUAGE plpgsql;

CREATE OR REPLACE FUNCTION test_case_assertEquals_text_should_success_if_equals() RETURNS VOID AS $$
BEGIN
  perform test_assertEquals('hello'::VARCHAR, 'hello');
END;
$$ LANGUAGE plpgsql;

CREATE OR REPLACE FUNCTION test_case_assertEquals_should_display_custom_message_if_defined() RETURNS VOID AS $$
DECLARE
  expected_message VARCHAR;
  error_message VARCHAR;
  is_equals BOOLEAN;
BEGIN
  BEGIN
    perform test_assertEquals('Test with custom message', 'hello'::VARCHAR, 'olleh');
  EXCEPTION
    WHEN triggered_action_exception THEN
      expected_message := 'Test with custom message: expect ''hello'' instead of ''olleh''';
      error_message := SQLERRM;
      perform test_assertTrue(format('Expect message ''%s'' instead of ''%s'' ', expected_message, error_message), error_message = expected_message);
  END;
END;
$$ LANGUAGE plpgsql;

CREATE OR REPLACE FUNCTION test_case_assertEqualsArray_should_success_if_equals() RETURNS VOID AS $$
BEGIN
  perform test_assertEqualsArray(ARRAY['1','2'], ARRAY['1','2']);
  perform test_assertEqualsArray(ARRAY['a','b'], ARRAY['a','b']);
END;
$$ LANGUAGE plpgsql;

CREATE OR REPLACE FUNCTION test_case_assertEqualsArray_should_display_diff_if_failed() RETURNS VOID AS $$
DECLARE
  expected_message VARCHAR;
  error_message VARCHAR;
  is_equals BOOLEAN;
BEGIN
  BEGIN
    perform test_assertEqualsArray(ARRAY['1','2','3','4'], ARRAY['1','2','4','4']);
  EXCEPTION
    WHEN triggered_action_exception THEN
      expected_message := CONCAT(
        'assertEqualsArray failure:', E'\n',
        '= 1', E'\n',
        '= 2', E'\n',
        '- 3', E'\n',
        '+ 4', E'\n',
        '= 4'
      );
      error_message := SQLERRM;
      perform test_assertTrue(format('Expect message ''%s'' instead of ''%s'' ', expected_message, error_message), error_message = expected_message);
  END;
END;
$$ LANGUAGE plpgsql;

SELECT * FROM test_run_all();

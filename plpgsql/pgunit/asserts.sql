CREATE OR REPLACE FUNCTION test_assertEquals(message TEXT, expected ANYELEMENT, result ANYELEMENT) RETURNS VOID AS $$
BEGIN
  IF expected = result THEN
    null;
  ELSE
    raise exception '%: expect ''%'' instead of ''%''', message, expected, result using errcode = 'triggered_action_exception';
  END IF;
END;
$$ LANGUAGE plpgsql;

CREATE OR REPLACE FUNCTION test_assertEquals(expected ANYELEMENT, result ANYELEMENT) RETURNS VOID AS $$
BEGIN
  perform test_assertEquals('assertEquals failure', expected, result);
END;
$$ LANGUAGE plpgsql;

CREATE OR REPLACE FUNCTION test_assertEqualsArray(expected VARCHAR[], result VARCHAR[]) RETURNS VOID AS $$
DECLARE
  line RECORD;
  error_message text;
BEGIN
  IF expected = result THEN
    null;
  ELSE
    error_message := 'assertEqualsArray failure:';
    FOR line IN SELECT expected_item, result_item FROM (SELECT unnest(expected) AS expected_item, unnest(result) AS result_item) x
    LOOP
      IF line.expected_item = line.result_item THEN
        error_message := CONCAT(error_message, E'\n', '= ', line.expected_item);
      ELSE
        error_message := CONCAT(error_message, E'\n', '- ', line.expected_item);
        error_message := CONCAT(error_message, E'\n', '+ ', line.result_item);
      END IF;
    END LOOP;

    raise exception '%', error_message using errcode = 'triggered_action_exception';
  END IF;
END;
$$ LANGUAGE plpgsql;

CREATE OR REPLACE FUNCTION test_assertEquals_golden_master(expected VARCHAR[], result VARCHAR[]) RETURNS VOID as $$
DECLARE
    golden TEXT;
    line VARCHAR;
BEGIN
  perform test_assertEqualsArray(expected, result);
EXCEPTION
  WHEN triggered_action_exception THEN
    golden := CONCAT(SQLERRM, E'\n\n', E'For update, copy:\n');
    golden := CONCAT(golden, E'expected := ARRAY[');
    FOREACH line IN ARRAY result
    LOOP
      golden := CONCAT(golden, E'\n', '''', line, ''',');
    END LOOP;
    golden := CONCAT(golden, E'\n', '];');

    raise exception '%', golden using errcode = 'triggered_action_exception';
END;
$$ LANGUAGE plpgsql;

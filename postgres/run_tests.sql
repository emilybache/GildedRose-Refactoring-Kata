create or replace function test_case_can_create_item() returns void as $$
begin
  perform test_assertTrue('hello', true);
end;
$$ language plpgsql;

select * from test_run_all();
create function pg_temp.create_role_if_not_exists(rolename name, opts character varying) RETURNS text
  LANGUAGE plpgsql
  AS $$
BEGIN
  IF NOT EXISTS (SELECT * FROM pg_roles WHERE rolname = rolename) THEN
    EXECUTE format('CREATE ROLE %I %s', rolename, opts);
    RETURN 'CREATE ROLE';
  ELSE
    RETURN format('ROLE ''%I'' ALREADY EXISTS', rolename);
  END IF;
END;
$$;

select pg_temp.create_role_if_not_exists('dbapi_anonymous', 'with nologin');
select pg_temp.create_role_if_not_exists('test_default_role', 'with nologin');

select pg_temp.create_role_if_not_exists('dbapi_test_author', 'with nologin');
select pg_temp.create_role_if_not_exists('dbapi_test_author_a', 'with nologin in role dbapi_test_author');
select pg_temp.create_role_if_not_exists('dbapi_test_author_b', 'with nologin in role dbapi_test_author');

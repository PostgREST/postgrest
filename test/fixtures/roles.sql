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

select pg_temp.create_role_if_not_exists('postgrest_anonymous', 'with nologin') as a
     , pg_temp.create_role_if_not_exists('test_default_role', 'with nologin') as b
     , pg_temp.create_role_if_not_exists('postgrest_test_author', 'with nologin') into temp shh;

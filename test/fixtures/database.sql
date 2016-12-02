CREATE EXTENSION IF NOT EXISTS pgcrypto;

ALTER DATABASE :db SET request.jwt.claim.id = '-1';

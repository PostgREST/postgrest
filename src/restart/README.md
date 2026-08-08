# restart

This directory contains the source code for the `restart` internal library.

The library provides a small framework for running an application that can
replace its own process. It is intentionally independent of PostgREST-specific
state: callers provide the application lifecycle actions, and the library
handles process mode detection, restart request coordination, and the
parent/child handover protocol where the platform supports it.

## Source Layout

- `common/System/Process/Restart.hs` is the public module. It re-exports the
  platform-independent API and the selected platform implementation.
- `common/System/Process/Restart/Shared.hs` defines public types shared by all
  platforms, including `ProcessConfig`, `StartupMode`, `AppRun`, `Ready`,
  and `HandoverError`.
- `posix/System/Process/Restart/Impl.hs` implements real process handover for
  POSIX platforms. It starts replacement processes, coordinates the private
  READY/COMMIT protocol over pipes, installs the optional SIGHUP restart
  handler, and integrates with systemd notifications when `NOTIFY_SOCKET` is
  available.
- `windows/System/Process/Restart/Impl.hs` preserves the same public API on
  Windows, but runs applications without replacement handover because the POSIX
  protocol is not available there.

The Cabal stanza always includes `src/restart/common` and adds either
`src/restart/posix` or `src/restart/windows` through platform-specific
`hs-source-dirs`.

# restart

This directory contains the source code for the `restart` library.

The library provides a small framework for running an application that can
replace its own process. It is intentionally independent of PostgREST-specific
state: callers provide the application lifecycle actions, and the library
handles process mode detection, restart request coordination, and the
parent/child handover protocol where the platform supports it.

## Source Layout

- `src/common/System/Process/Restart.hs` is the high-level public module. It
  provides `RestartOptions`, signal-handler configuration, and a
  `runRestartable` wrapper that resolves process configuration and installs
  configured signal handlers when the application marks itself ready.
- `src/common/System/Process/Restart/Shared.hs` defines public types shared by all
  platforms, including `ProcessConfig`, `StartupMode`, signal handler types,
  platform-level `AppRun` and `Ready`, and `HandoverError`.
- `src/posix/System/Process/Restart/Impl.hs` implements real process handover for
  POSIX platforms. It starts replacement processes, coordinates the private
  READY/COMMIT protocol over pipes, installs configured signal handlers, and
  integrates with systemd notifications when `NOTIFY_SOCKET` is available.
- `src/windows/System/Process/Restart/Impl.hs` preserves the same public API on
  Windows, but runs applications without replacement handover because the POSIX
  protocol is not available there.

The Cabal stanza always includes `src/common` and adds either `src/posix` or
`src/windows` through platform-specific `hs-source-dirs`.
`hs-source-dirs`.

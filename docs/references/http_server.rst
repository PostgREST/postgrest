.. _http_server:

HTTP Server
###########

The HTTP server is provided by `Warp <https://aosabook.org/en/posa/warp.html>`_.

Graceful shutdown
-----------------

PostgREST uses Warp's graceful shutdown, when a ``SIGTERM`` is received:

- It stops accepting new requests.
- Allows requests that are already in progress to finish.
- Closes idle ``Keep-Alive`` connections instead of waiting for them to expire.
- Responses sent during shutdown indicate that the connection should not be reused (e.g. for HTTP/1.x, it sends ``Connection: close``).

This allows PostgREST to shut down promptly without interrupting in-flight requests. Useful for zero-downtime upgrades and autoscaling/load-balancing under cloud environments (AWS ECS, Kubernetes).

Open file descriptors
---------------------

Every connection, from a client or to the database, takes a file descriptor. On startup, PostgREST raises its soft limit of open file descriptors to the hard limit, so it doesn't run out of them under load with the common default soft limit of 1024. This is safe as PostgREST doesn't use ``select()``, which can't handle file descriptors above 1024, as explained in `File Descriptor Limits <https://0pointer.net/blog/file-descriptor-limits.html>`_.

The hard limit stays as it is. To raise it, use your service manager or container runtime, e.g. ``LimitNOFILE`` in systemd or ``--ulimit nofile`` in Docker. When the system rejects the hard limit as the soft one, e.g. an unlimited hard limit on macOS, the soft limit is kept.

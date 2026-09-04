# Yolobox

Claude runs inside a yolobox container.
Use the `yolobox` skill for details about the current sandbox (mounts, network, env passthrough, etc.).

A Redis server is installed in the yolobox image.
Start it at the beginning of the session with `redis-server --daemonize yes --save ""` so that no `dump.rdb` file is created.
It is reachable via:

- `REDIS_HOST=127.0.0.1`
- `REDIS_PORT=6379`

Connect with `redis-cli -h "$REDIS_HOST" -p "$REDIS_PORT"`

Other repositories are mounted in `~/repositories/`

# Design Doc

## Goals

These goals are somewhat arbitrary because I'm having fun solving them, not
because of any real or expected constraints.

* Minimize latency for a game by having requests handled as close to player's
  region as possible.
  * Be able to pin a game to a process and have all relevant requests routed to
    it.
* Data loss of recent moves is acceptable.
* Non-game write requests are expected to be rare (registration?) and do not
  have to be region local.
* Be able to serve all read-only requests from a local region.
* Don't just use all the same tools I already know. This is a hobby project,
  I'm doing it to try new things.

## Decisions

* Game server does not need to have a confirmed write before ack'ing to client
  (prioritizing latency over persistence)
* Clients will maintain a persistent SSE connection to game server to receive
  game updates.
* Deploy using Fly.io to handle region routing (among many other things).


Persistence options:

* In-process sqlite, litestream backup.
  * This solves for accepting writes locally, and should also be fast enough
    that we can (locally, non-replicated) persist writes before ack'ing.
  * This ticks the "something new, fun and interesting" box.
* Still need to solve how to get eventually consistent "all data" local to a
  region.
  * Use a more standard distributed postgres setup?
    * Doing this with fly is a _little bit_ fun/interesting.
    * Need to sync writes to sqlite into postgres. Kinda gross?
    * SQLite doesn't really add anything in this case because we don't need the
      confirmed write, we can just background a write to postgres.
    * Hrm yeah sqlite seems too fun but not solving for constraints well enough.
    * Standard fly-replay approach won't work because we're not doing standard
      web requests. Use a second postgres pool that always points at
      `PRIMARY_REGION`.

      > The generated connection string uses port 5432 to connect to
      > PostgreSQL. This port always forwards you to a writable instance. Port
      > 5433 is direct to the PostgreSQL member, and used to connect to read
      > replicas directly.

      https://fly.io/docs/getting-started/multi-region-databases/


      Hrmm though actually _by default_ write requests should _only_ be able to
      come from the local region, so should "just work" going through 5433 for
      game writes.
  * Can we get safe-shutdown semantics for a regular deploy? (Stop accepting connections, flush all writes?) Maybe servant can get us this for free? Maybe fly.io does this?
    * Fly sends SIGINT but this can be customized.
    * warp can handle shutdowns: https://hackage.haskell.org/package/warp-3.3.20/docs/Network-Wai-Handler-Warp.html
* Alternative would be to use GCP/AWS managed postgres
  * Introduces latency, but more bulletproof management.
  * Definitely less fun!
  * Let's stick with fly.io database with ad-hoc backup solution.
  * fly.io volumes are already backed up, so think we just want to add PITR recovery.
    * WAL-G seems to be "the" way to do this https://gist.github.com/pohzipohzi/2f111d11ae0469266ddf50a5d71bfd60
    * https://supabase.com/blog/2020/08/02/continuous-postgresql-backup-walg
    * We don't need snapshots or anything to optimize recovery time, this is
      just for emergency cases - but that supabase article suggests might get
      both for free.

Doesn't look like anyone is deploying haskell to fly.io yet, so will need to
make our own buildpack.


### Routing Options

We need to make sure requests for a game always go the relevant "owner" process.

We can do this by including metadata on each game with the region and "number" to route to.

May as well encode that directly into the game ID? Probably not ideal, locks us
out of "migrating" games to different regions (though that's pretty
speculative). Can get best of both worlds by having primary region in database
(metadata), but then also encoding in URL to avoid a routing lookup. (In
pathological case, having wrong region would 404, so need a handler to
redirect. This works because all games will be read-replica'd to every region.)

For now let's reserve "number" for future use and only do region based routing. When a client connects:
* Determine which region a game is hosted out of.
* If not current region, respond with fly-replay to redo request.
  * Given we're using SSE which is HTTP, this _should_ just work? Client
    request will still be terminated at closest edge to them and backhauled to
    primary region.


But thinking through "number" anyway...

"number" would need to be implemented with multiple fly.io apps I think - I
don't think there's a way to route to a specific instance of an app within a
region? But maybe not actually we can use `fly-prefer-instance` header!?
  https://community.fly.io/t/is-it-possibly-for-a-client-to-connect-to-a-particular-instance/230/8

Need to check if instance ID is persistent (i.e. an app with 2 instances always
has 1 and 2) or is unique per boot. If the latter, bit trickier as every time
it boots would need a way to learn which games it should be claiming. (Pretty sure they're the latter)

  (Dodgy version: keep forwarding requests through remaining instances until it finds the right one?)

Dedicated app per number might just end up being easier. Eh, future problem.

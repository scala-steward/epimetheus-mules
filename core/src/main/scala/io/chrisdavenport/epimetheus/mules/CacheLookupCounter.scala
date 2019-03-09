package io.chrisdavenport.epimetheus
package mules

import internal._
import io.chrisdavenport.mules._
import cats.effect._
import cats._
import cats.implicits._
import shapeless.Sized

final class CacheLookupCounter[F[_]: Sync] private (private val c: UnlabelledCounter[F, CacheLookupCounterStatus]){
  import CacheLookupCounter._
  def lookup[K, V](l: Lookup[F, K, V], name: String): Lookup[F, K, V] = new LookupCounted[F, K, V](c, l, name)
  def cache[K, V](ca: Cache[F, K, V], name: String): Cache[F, K, V] = new CacheCounted[F, K, V](c, ca, name)
  def memoryCache[K, V](ca: MemoryCache[F, K, V], name: String): MemoryCache[F, K, V] = 
    ca.withOnCacheMiss(_ => c.label(CacheLookupCounterStatus(name, CacheMiss)).inc)
      .withOnCacheHit((_, _) => c.label(CacheLookupCounterStatus(name, CacheHit)).inc)

}

object CacheLookupCounter {


  def register[F[_]: Sync](
    cr: CollectorRegistry[F],
    name: Name = Name("mules_cache_lookup_total")
  ): F[CacheLookupCounter[F]] = 
    Counter.labelled(
      cr,
      name,
      "Cache Lookup Status Counter",
      Sized(Name("cache_name"), Name("status")),
      CacheLookupCounterStatus.cacheLookupStatShow
    ).map(new CacheLookupCounter(_))
  


  private class LookupCounted[F[_]: Monad, K, V](
    private val c: UnlabelledCounter[F, CacheLookupCounterStatus],
    private val innerL: Lookup[F, K, V],
    private val cacheName: String
  ) extends Lookup[F, K, V]{
      def lookup(k: K): F[Option[V]] = 
        innerL.lookup(k).flatMap{
          case s@Some(_) => c.label(CacheLookupCounterStatus(cacheName, CacheHit)).inc.as(s)
          case n@None => c.label(CacheLookupCounterStatus(cacheName, CacheMiss)).inc.as(n)
        }
  }

  private class CacheCounted[F[_]: Monad, K, V](
    private val c: UnlabelledCounter[F, CacheLookupCounterStatus],
    private val innerL: Cache[F, K, V],
    private val cacheName: String
  ) extends Cache[F, K, V]{
      def lookup(k: K): F[Option[V]] = 
        innerL.lookup(k).flatMap{
          case s@Some(_) => c.label(CacheLookupCounterStatus(cacheName, CacheHit)).inc.as(s)
          case n@None => c.label(CacheLookupCounterStatus(cacheName, CacheMiss)).inc.as(n)
        }
      def delete(k: K): F[Unit] = innerL.delete(k)
      def insert(k: K, v: V): F[Unit] = innerL.insert(k, v)
  }



}
package io.chrisdavenport.epimetheus
package mules

import internal._
import io.chrisdavenport.mules._
import cats.effect._
import cats._
import cats.syntax.all._
import shapeless.Sized

final class CacheLookupCounter[F[_]: Sync] private (private val c: UnlabelledCounter[F, CacheLookupCounterStatus]){
  import CacheLookupCounter._
  def meteredLookup[K, V](l: Lookup[F, K, V], name: String): Lookup[F, K, V] = new LookupCounted[F, K, V](c, l, name)
  def meteredCache[K, V](ca: Cache[F, K, V], name: String): Cache[F, K, V] = new CacheCounted[F, K, V](c, ca, name)
  def meteredMemoryCache[K, V](ca: MemoryCache[F, K, V], name: String): MemoryCache[F, K, V] = 
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
      "Cache Lookup Status Counter.",
      Sized(Label("cache_name"), Label("status")),
      CacheLookupCounterStatus.cacheLookupStatShow
    ).map(new CacheLookupCounter(_))


  def meteredMemoryCache[F[_]: Sync, K, V](
    cr: CollectorRegistry[F],
    name: Name,
    mc: MemoryCache[F, K, V]): F[MemoryCache[F, K, V]] = 
      Counter.labelled(
        cr,
        name,
        "Cache Lookup Status Counter.",
        Sized(Label("status")),
        {c: CacheLookupStatus => Sized(CacheLookupStatus.statusValue(c))}
      ).map(c => 
        mc.withOnCacheMiss(_ => c.label(CacheMiss).inc)
          .withOnCacheHit((_, _) => c.label(CacheHit).inc)
      )

  def meteredLookup[F[_]: Sync, K, V](
    cr: CollectorRegistry[F],
    name: Name,
    lookup: Lookup[F, K, V]
  ): F[Lookup[F, K, V]] =
    Counter.labelled(
      cr,
      name,
      "Cache Lookup Status Counter.",
      Sized(Label("status")),
      {c: CacheLookupStatus => Sized(CacheLookupStatus.statusValue(c))}
    ).map(new SingleLookupCounted(_, lookup))

  def meteredCache[F[_]: Sync, K, V](
    cr: CollectorRegistry[F],
    name: Name,
    cache: Cache[F, K, V]
  ): F[Cache[F, K, V]] = 
    Counter.labelled(
      cr,
      name,
      "Cache Lookup Status Counter.",
      Sized(Label("status")),
      {c: CacheLookupStatus => Sized(CacheLookupStatus.statusValue(c))}
    ).map(new SingleCacheCounted(_, cache))

  private class SingleLookupCounted[F[_]: Monad, K, V](
    private val c: UnlabelledCounter[F, CacheLookupStatus],
    private val innerL: Lookup[F, K, V]
  ) extends Lookup[F, K, V]{
    override def lookup(k: K): F[Option[V]] = 
      innerL.lookup(k).flatTap{
        case Some(_) => c.label(CacheHit).inc
        case None => c.label(CacheMiss).inc
      }
  }

  private class SingleCacheCounted[F[_]: Monad, K, V](
    private val c: UnlabelledCounter[F, CacheLookupStatus],
    private val innerL: Cache[F, K, V]
  ) extends Cache[F, K, V]{
    override def lookup(k: K): F[Option[V]] = 
      innerL.lookup(k).flatTap{
        case Some(_) => c.label(CacheHit).inc
        case None => c.label(CacheMiss).inc
      }

    override def delete(k: K): F[Unit] = innerL.delete(k)
    override def insert(k: K, v: V): F[Unit] = innerL.insert(k, v)
  }

  private class LookupCounted[F[_]: Monad, K, V](
    private val c: UnlabelledCounter[F, CacheLookupCounterStatus],
    private val innerL: Lookup[F, K, V],
    private val cacheName: String
  ) extends Lookup[F, K, V]{
      def lookup(k: K): F[Option[V]] = 
        innerL.lookup(k).flatMap{
          case s@Some(_) => c.label(CacheLookupCounterStatus(cacheName, CacheHit)).inc.as(s)
          case None => c.label(CacheLookupCounterStatus(cacheName, CacheMiss)).inc.as(None)
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
          case None => c.label(CacheLookupCounterStatus(cacheName, CacheMiss)).inc.as(None)
        }
      def delete(k: K): F[Unit] = innerL.delete(k)
      def insert(k: K, v: V): F[Unit] = innerL.insert(k, v)
  }



}
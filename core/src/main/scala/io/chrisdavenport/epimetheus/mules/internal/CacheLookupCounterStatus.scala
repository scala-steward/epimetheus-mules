package io.chrisdavenport.epimetheus.mules.internal

import shapeless.Sized

private[mules] final case class CacheLookupCounterStatus(cacheName: String, lookupStatus: CacheLookupStatus)

private[mules] object CacheLookupCounterStatus {
  def cacheLookupStatShow(c: CacheLookupCounterStatus) = {
    val lookupStatusValue = c.lookupStatus match {
      case CacheHit => "hit"
      case CacheMiss => "miss"
    }
    Sized(c.cacheName, lookupStatusValue)
  }
}

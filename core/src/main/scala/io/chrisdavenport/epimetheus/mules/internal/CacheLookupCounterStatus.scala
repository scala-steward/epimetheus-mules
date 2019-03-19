package io.chrisdavenport.epimetheus.mules.internal

import shapeless.Sized

private[mules] final case class CacheLookupCounterStatus(cacheName: String, lookupStatus: CacheLookupStatus)

private[mules] object CacheLookupCounterStatus {
  def cacheLookupStatShow(c: CacheLookupCounterStatus) = {
    Sized(c.cacheName, CacheLookupStatus.statusValue(c.lookupStatus))
  }
}

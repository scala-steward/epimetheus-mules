package io.chrisdavenport.epimetheus.mules.internal

private[mules] sealed trait CacheLookupStatus
private[mules] case object CacheMiss extends CacheLookupStatus
private[mules] case object CacheHit extends CacheLookupStatus
private[mules] object CacheLookupStatus {
  def statusValue(c: CacheLookupStatus): String = c match {
    case CacheHit => "hit"
    case CacheMiss => "miss"
  }
}
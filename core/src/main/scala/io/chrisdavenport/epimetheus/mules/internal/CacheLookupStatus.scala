package io.chrisdavenport.epimetheus.mules.internal

private[mules] sealed trait CacheLookupStatus
private[mules] case object CacheMiss extends CacheLookupStatus
private[mules] case object CacheHit extends CacheLookupStatus
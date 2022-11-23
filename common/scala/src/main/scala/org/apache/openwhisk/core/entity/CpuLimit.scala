/*
 * Created by Prasoon Sinha
 */

package org.apache.openwhisk.core.entity

import org.apache.openwhisk.core.ConfigKeys
import pureconfig._
import pureconfig.generic.auto._
import scala.util.Failure
import scala.util.Success
import scala.util.Try
import spray.json._

case class CpuLimitConfig(min: Int, max: Int, std: Int)

/**
 * CpuLimit encapsulates allowed cpu cores in a single container for an action. The limit must be within a
 * permissible range (by default [2, 256]). 
 * 
 * It is a value type (hence == is .equals, immutable and cannot be assigned null).
 * The constructor is private so that argument requirements are checked and normalized
 * before creating a new instance.
 *
 * @param cores the cpu limit in number of cores for the action
 */
protected[entity] class CpuLimit private (val cores: Int) extends AnyVal

protected[core] object CpuLimit extends ArgNormalizer[CpuLimit] {
    val config = loadConfigOrThrow[CpuLimitConfig](ConfigKeys.cpu)

  /** These values are set once at the beginning. Dynamic configuration updates are not supported at the moment. */
  protected[core] val MIN_CPU: Int = config.min
  protected[core] val MAX_CPU: Int = config.max
  protected[core] val STD_CPU: Int = config.std

  /** A singleton CpuLimit with default value */
  protected[core] val standardCpuLimit = CpuLimit(STD_CPU)

  /** Gets CpuLimit with default value */
  protected[core] def apply(): CpuLimit = standardCpuLimit

  /**
   * Creates CpuLimit for limit, iff limit is within permissible range.
   *
   * @param cores the limit, must be within permissible range
   * @return CpuLimit with limit set
   * @throws IllegalArgumentException if limit does not conform to requirements
   */
  @throws[IllegalArgumentException]
  protected[core] def apply(cores: Int): CpuLimit = {
    require(cores >= MIN_CPU, s"cores $cores below allowed threshold of $MIN_CPU")
    require(cores <= MAX_CPU, s"cores $cores exceeds allowed threshold of $MAX_CPU")
    new CpuLimit(cores)
  }

  override protected[core] implicit val serdes = new RootJsonFormat[CpuLimit] {
    def write(m: CpuLimit) = JsNumber(m.cores)

    def read(value: JsValue) = {
      Try {
        val JsNumber(c) = value
        require(c.isWhole, "cpu limit must be whole number")

        CpuLimit(c.toInt)
      } match {
        case Success(limit)                       => limit
        case Failure(e: IllegalArgumentException) => deserializationError(e.getMessage, e)
        case Failure(e: Throwable)                => deserializationError("cpu limit malformed", e)
      }
    }
  }
}
